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

let TryFindDefaultBinding source (tree : Ast.AstNode) (position : pos) filename =
    let range = mkRange filename position position
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

    tryFindDeepestBinding [tree]

let TryFindFunctionIdentifier binding =
    match binding with
        | SynBinding.Binding(_,_,_,_,_,_,_,
                             SynPat.LongIdent(LongIdentWithDots([functionIdentifier],_),_,_,_,_,_),_,_,_,_) ->
                                Some (functionIdentifier.idText, functionIdentifier.idRange)
        | SynBinding.Binding(_,_,_,_,_,_,_,
                             SynPat.Named(_,valueName,_,_,_),_,_,_,_) ->
                                Some (valueName.idText, valueName.idRange)
        | _ -> None

let tryFindFunctionName (binding:SynBinding) =
    match binding with
        | SynBinding.Binding(_,_,_,_,_,_,_,p,_,_,_,_) ->
            match Ast.AstNode.Pattern p with
                | DeclaredIdent(name,range) -> Some name
                | _ -> None
    
let addArgumentToFunctionDeclaration (functionName, functionRange:range) argumentName filename : Refactoring<unit,Identifier> =
    let transform (source, ()) =
        let tree = (Ast.Parse source filename).Value
        let identEndRange = functionRange.EndRange
        let argumentIdentifier =
            createIdentifier (identEndRange.End.Line, (identEndRange.End.Column+1)) argumentName functionRange.FileName
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

let findFunctionUsageRanges source (tree : Ast.AstNode) (functionName, functionRange) =
    FindDeclarationScope (makeScopeTrees tree) (functionName, functionRange)
    |> FindDeclarationReferences (functionName, functionRange)
    |> List.filter ((<>) functionRange)

let findFunctionName source (tree : Ast.AstNode) (bindingRange : range) =
    let binding = FindBindingAtRange bindingRange tree
    match tryFindFunctionName binding with
        | Some name -> name
        | None -> raise (RefactoringFailure("Binding was not a function"))

//TODO: Check arguments such as argumentName or defaultValue have a valid form
let addTempArgument (functionIdentifier:Identifier) defaultValue filename : Refactoring<unit,Identifier> =
    let transform (source, ()) =
        let tree = (Ast.Parse source filename).Value
        let argumentName = FindUnusedName tree
        let usageRefactorings =
            findFunctionUsageRanges source tree functionIdentifier
            |> List.map (addArgumentToFunctionUsage source defaultValue)
        let bindingRefactoring = addArgumentToFunctionDeclaration functionIdentifier argumentName filename
        (List.fold interleave bindingRefactoring usageRefactorings).transform (source, ())

    { analysis = fun _ -> true;
      transform = transform;
      getErrorMessage = fun _ -> None }

let GetErrorMessage (position:(int*int) option, argumentName:string option, defaultValue:string option) (source:string) (filename:string) =
    let pos = PosFromPositionOption position
    let binding =
        lazy
            let tree = (Ast.Parse source filename).Value
            TryFindDefaultBinding source tree pos.Value filename

    let checkPosition (line, col) =
        let bindingAtRange =
            Option.isSome (binding.Force())
        let bindingIsFunction =
            Option.bind TryFindFunctionIdentifier binding.Value
            |> Option.isSome
        //TODO: disallow mutable bindings

        match bindingAtRange, bindingIsFunction with
            | false,_ ->
                Some "No binding found around the given position"
            | _,false -> Some "Binding was not a function"
            | _,_ -> None

    let checkPositionNameAndValue (position, name, defaultValue) =
        let functionIdentifier =
            (TryFindFunctionIdentifier (binding.Force().Value)).Value
        let oldSource, changes, (_, identifierRange) =
            (addTempArgument functionIdentifier defaultValue filename).transform (source,())
        let sourceWithIdentifier = ChangeTextOf oldSource changes
        Rename.GetErrorMessage (Some (identifierRange.Start.Line, identifierRange.Start.Column+1), Some name) sourceWithIdentifier filename
            
    IsSuccessful checkPosition position
    |> Andalso (IsSuccessful checkPositionNameAndValue (TripleOptions (position, argumentName, defaultValue)))
    |> fun (l:Lazy<_>) -> l.Force()

let IsValid (position:(int*int) option, argumentName:string option, defaultValue:string option) (source:string) (filename:string) =
    GetErrorMessage (position, argumentName, defaultValue) source filename
    |> Option.isNone

let AddArgument (functionIdentifier:Identifier) argumentName defaultValue filename : Refactoring<unit,unit> =
    let _, functionRange = functionIdentifier
    let analysis (source, ()) =
            IsValid (Some (functionRange.Start.Line, functionRange.Start.Column+1), Some argumentName, Some defaultValue) source filename
            
    let getErrorMessage (source, ()) =
        GetErrorMessage (Some (functionRange.Start.Line, functionRange.Start.Column+1), Some argumentName, Some defaultValue) source filename
        
    let addTempArgumentRefactoring = addTempArgument functionIdentifier defaultValue filename
    let addArgumentRefactoring = sequence addTempArgumentRefactoring (Rename.Rename argumentName filename)
    { addArgumentRefactoring with analysis = analysis; getErrorMessage = getErrorMessage }

let Transform ((line, col):int*int, argumentName:string, defaultValue:string) (source:string) (filename:string) =
    let pos = mkPos line (col-1)
    let tree = (Ast.Parse source filename).Value
    let binding = TryFindDefaultBinding source tree pos filename
    let functionIdentifier = TryFindFunctionIdentifier binding.Value
    RunRefactoring (AddArgument functionIdentifier.Value argumentName defaultValue filename) () source
