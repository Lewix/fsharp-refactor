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
open FSharpRefactor.Refactorings

let defaultBindingRange source (tree : Ast.AstNode) (position : pos) =
    let range = mkRange "test.fs" position position
    let rec tryFindDeepestBinding trees =
        let candidateBinding = List.tryPick (TryFindBindingAroundRange range) trees
        if Option.isNone candidateBinding then None
        else
            let children = Ast.GetChildren (Ast.AstNode.Binding candidateBinding.Value)
            if Option.isNone children then candidateBinding
            else
                let nestedBinding =
                    tryFindDeepestBinding children.Value
                if Option.isNone nestedBinding then candidateBinding else nestedBinding

    let deepestBinding = tryFindDeepestBinding [tree]
    (Ast.GetRange (Ast.AstNode.Binding deepestBinding.Value)).Value

let tryFindFunctionName (binding:SynBinding) =
    match binding with
        | SynBinding.Binding(_,_,_,_,_,_,_,p,_,_,_,_) ->
            match Ast.AstNode.Pattern p with
                | DeclaredIdent(name,range) -> Some name
                | _ -> None
    
let addArgumentToBinding (bindingRange : range) argumentName : Refactoring<unit,Identifier> =
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
    
    { analysis = (fun (_,_) -> true);
      transform = transform;
      getErrorMessage = fun _ -> None }

//TODO: Only add brackets around usage if needed
let addArgumentToFunctionUsage source (argument : string) (identRange : range) =
    let ident = TextOfRange source identRange
    { analysis = (fun (_,_) -> true);
      transform = fun (s,_) -> (s,[identRange, sprintf "(%s %s)" ident argument],());
      getErrorMessage = fun _ -> None }

let findFunctionUsageRanges source (tree : Ast.AstNode) (bindingRange : range) (functionName : string) =
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

//TODO: Check arguments such as argumentName or defaultValue have a valid form
let addTempArgument doCheck (bindingRange : range) defaultValue : Refactoring<unit,Identifier> =
    let transform (source, ()) =
        let tree = (Ast.Parse source).Value
        let argumentName = FindUnusedName tree
        let usageRefactorings =
            findFunctionName source tree bindingRange
            |> findFunctionUsageRanges source tree bindingRange
            |> List.map (addArgumentToFunctionUsage source defaultValue)
        let bindingRefactoring = addArgumentToBinding bindingRange argumentName
        (List.fold interleave bindingRefactoring usageRefactorings).transform (source, ())

    { analysis = fun _ -> true;
      transform = transform;
      getErrorMessage = fun _ -> None }

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

    let checkPositionNameAndValue (position, name, defaultValue) =
        let bindingRange =
            (Ast.GetRange (Ast.AstNode.Binding (binding.Value.Value))).Value
        let oldSource, changes, (_, identifierRange) =
            (addTempArgument true bindingRange defaultValue).transform (source,())
        let sourceWithIdentifier = ChangeTextOf oldSource changes
        Rename.GetErrorMessage (Some (identifierRange.Start.Line, identifierRange.Start.Column+1), Some name) sourceWithIdentifier filename
            
    IsSuccessful checkPosition position
    |> Andalso (IsSuccessful checkPositionNameAndValue (TripleOptions (position, argumentName, defaultValue)))
    |> fun (l:Lazy<_>) -> l.Force()

let IsValid (position:(int*int) option, argumentName:string option, defaultValue:string option) (source:string) (filename:string) =
    GetErrorMessage (position, argumentName, defaultValue) source filename
    |> Option.isNone

let AddArgument doCheck (bindingRange : range) argumentName defaultValue : Refactoring<unit,unit> =
    let analysis (source, ()) =
        if doCheck then
            IsValid (Some (bindingRange.Start.Line, bindingRange.Start.Column+1), Some argumentName, Some defaultValue) source "test.fs"
        else
            true
            
    let getErrorMessage (source, ()) =
        GetErrorMessage (Some (bindingRange.Start.Line, bindingRange.Start.Column+1), Some argumentName, Some defaultValue) source "test.fs"
        
    let addTempArgumentRefactoring = addTempArgument doCheck bindingRange defaultValue
    let addArgumentRefactoring = sequence addTempArgumentRefactoring (Rename.Rename doCheck argumentName)
    { addArgumentRefactoring with analysis = analysis; getErrorMessage = getErrorMessage }

let Transform ((line, col):int*int, argumentName:string, defaultValue:string) (source:string) (filename:string) =
    let pos = mkPos line (col-1)
    let tree = (Ast.Parse source).Value
    let bindingRange = defaultBindingRange source tree pos
    RunRefactoring (AddArgument true bindingRange argumentName defaultValue) () source
