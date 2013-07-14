module FSharpRefactor.Refactorings.AddArgument

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.CodeTransforms.CodeTransforms
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Engine.ValidityChecking
open FSharpRefactor.Refactorings.Rename

let DefaultBindingRange source (tree : Ast.AstNode) (position : pos) =
    let range = mkRange "test.fs" position position
    let rec tryFindDeepestBinding trees =
        let candidateBinding = List.tryPick (TryFindBindingAroundRange range) trees
        if Option.isNone candidateBinding then None
        else
            let children = Ast.GetChildren (Ast.AstNode.Binding candidateBinding.Value)
            if Option.isNone children then candidateBinding
            else
                let nestedBinding =
                    tryFindDeepestBinding (Ast.GetChildren (Ast.AstNode.Binding candidateBinding.Value)).Value
                if Option.isNone nestedBinding then candidateBinding else nestedBinding

    let deepestBinding = tryFindDeepestBinding [tree]
    if Option.isNone deepestBinding then None
    else Ast.GetRange (Ast.AstNode.Binding deepestBinding.Value)

let tryFindFunctionName (binding:SynBinding) =
    match binding with
        | SynBinding.Binding(_,_,_,_,_,_,_,p,_,_,_,_) ->
            match Ast.AstNode.Pattern p with
                | DeclaredIdent(name,range) -> Some name
                | _ -> None

let GetErrorMessage (position:(int*int) option, argumentName:string option, defaultValue:string option) (source:string) (filename:string) =
    let pos = PosFromPositionOption position
    let binding =
        lazy
            let tree = (Ast.Parse source).Value
            TryFindBindingAroundPos pos.Value tree

    let checkPosition (line, col) =
        let bindingAtRange =
            Option.isSome (binding.Force())
        let bindingIsFunction =
            bindingAtRange && (Option.isSome (tryFindFunctionName (binding.Value.Value)))
        //TODO: disallow mutable bindings

        match bindingAtRange, bindingIsFunction with
            | false,_ ->
                Some "No binding found around the given position"
            | _,false -> Some "Binding was not a function"
            | _,_ -> None

    IsSuccessful checkPosition position
    |> fun (l:Lazy<_>) -> l.Force()

let IsValid (position:(int*int) option, argumentName:string option, defaultValue:string option) (source:string) (filename:string) =
    GetErrorMessage (position, argumentName, defaultValue) source filename
    |> Option.isNone
    
let AddArgumentToBinding (bindingRange : range) argumentName : Refactoring<unit,Identifier> =
    let transform (source, ()) =
        let tree = (Ast.Parse source).Value
        let identEndRange =
            match FindBindingAtRange bindingRange tree with
                | SynBinding.Binding(_,_,_,_,_,_,_,
                                     SynPat.LongIdent(functionName,_,_,_,_,_),_,_,_,_) -> functionName.Range.EndRange
                | SynBinding.Binding(_,_,_,_,_,_,_,
                                     SynPat.Named(_,valueName,_,_,_),_,_,_,_) -> valueName.idRange.EndRange
                | b -> raise (RefactoringFailure("Binding did not have the right form:" + (sprintf "%A" b)))
        let argumentIdentifier =
            createIdentifier (identEndRange.End.Line, (identEndRange.End.Column+1)) argumentName bindingRange.FileName
        source, [identEndRange, " " + argumentName], argumentIdentifier
    
    let getErrorMessage (source,()) =
        GetErrorMessage (Some (bindingRange.Start.Line, bindingRange.Start.Column+1), Some argumentName, None) source "test.fs"

    { analysis = (fun (_,_) -> true);
      transform = transform;
      getErrorMessage = getErrorMessage }

//TODO: Only add brackets around usage if needed
let AddArgumentToFunctionUsage source (argument : string) (identRange : range) =
    let ident = TextOfRange source identRange
    { analysis = (fun (_,_) -> true);
      transform = fun (s,_) -> (s,[identRange, sprintf "(%s %s)" ident argument],());
      getErrorMessage = fun (s,_) -> None }

let FindFunctionUsageRanges source (tree : Ast.AstNode) (bindingRange : range) (functionName : string) =
    let bindingContainsNode scopeTree =
        match scopeTree with
            | Declaration(is,_) -> List.exists (fun (_,r) -> rangeContainsRange bindingRange r) is
            | Usage(_,r) -> rangeContainsRange bindingRange r

    let nodeDeclaresIdentifier scopeTree =
        match scopeTree with
            | Declaration(is,_) -> List.exists (fun (n,_) -> functionName = n) is
            | _ -> false

    let rec findDeclarationOfFunction scopeTrees =
        match scopeTrees with
            | (Declaration(is,ts1) as d)::ts2 ->
                if nodeDeclaresIdentifier d && bindingContainsNode d then ts1
                else findDeclarationOfFunction (ts1 @ ts2)
            | _::ts -> findDeclarationOfFunction ts
            | _ -> failwith (sprintf "%A, %A" bindingRange (makeScopeTrees tree))

    let rec findUsagesInScopeTrees scopeTrees =
        match scopeTrees with
            | (Declaration(is,ts1) as d)::ts2 ->
                if nodeDeclaresIdentifier d then findUsagesInScopeTrees ts2
                else (findUsagesInScopeTrees ts1) @ (findUsagesInScopeTrees ts2)
            | Usage(n,r)::ts2 ->
                if functionName = n then r::(findUsagesInScopeTrees ts2) else findUsagesInScopeTrees ts2
            | [] -> []

    makeScopeTrees tree
    |> findDeclarationOfFunction
    |> findUsagesInScopeTrees

let findFunctionName source (tree : Ast.AstNode) (bindingRange : range) =
    let binding = FindBindingAtRange bindingRange tree
    match tryFindFunctionName binding with
        | Some name -> name
        | None -> raise (RefactoringFailure("Binding was not a function"))

let CanAddArgument source (tree : Ast.AstNode) (bindingRange : range) (defaultValue : string) =
    try findFunctionName source tree bindingRange |> ignore; Valid with
        | :? KeyNotFoundException -> Invalid("No binding found at the given range")
        | RefactoringFailure(m) -> Invalid(m)

//TODO: Check arguments such as argumentName or defaultValue have a valid form
let AddTempArgument doCheck (bindingRange : range) defaultValue : Refactoring<unit,Identifier> =
    let analysis (source, ()) =
        if doCheck then
            IsValid (Some (bindingRange.Start.Line, bindingRange.Start.Column+1), None, Some defaultValue) source "test.fs"
        else
            true
    let transform (source, ()) =
        let tree = (Ast.Parse source).Value
        let argumentName = FindUnusedName tree
        let usageRefactorings =
            findFunctionName source tree bindingRange
            |> FindFunctionUsageRanges source tree bindingRange
            |> List.map (AddArgumentToFunctionUsage source defaultValue)
        let bindingRefactoring = AddArgumentToBinding bindingRange argumentName
        (List.fold interleave bindingRefactoring usageRefactorings).transform (source, ())
        
    let getErrorMessage (source, ()) =
        GetErrorMessage (Some (bindingRange.Start.Line, bindingRange.Start.Column+1), None, Some defaultValue) source "test.fs"
    { analysis = analysis;
      transform = transform;
      getErrorMessage = getErrorMessage }

let AddArgument doCheck (bindingRange : range) argumentName defaultValue : Refactoring<unit,unit> =
    let addTempArgumentRefactoring = AddTempArgument doCheck bindingRange defaultValue
    sequence addTempArgumentRefactoring (Rename doCheck argumentName)

let DoAddArgument source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) (defaultValue : string) =
    RunRefactoring (AddArgument true bindingRange argumentName defaultValue) () source

let Transform ((line, col):int*int, argumentName:string, defaultValue:string) (source:string) (filename:string) =
    let pos = mkPos line (col-1)
    let tree = (Ast.Parse source).Value
    let bindingRange =
        (Ast.GetRange (Ast.Binding (FindBindingAroundPos pos tree))).Value
    DoAddArgument source tree bindingRange argumentName defaultValue