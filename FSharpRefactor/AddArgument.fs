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
open FSharpRefactor.Engine
open FSharpRefactor.Refactorings

let TryFindDefaultBinding (project:Project) (tree : Ast.AstNode) (position : pos) =
    let range = mkRange project.CurrentFile position position
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
    
let addArgumentToFunctionDeclaration (functionName, functionRange:range) argumentName : Refactoring<unit,Identifier> =
    let transform (project:Project, ()) =
        let tree = (Ast.Parse project.CurrentFileContents project.CurrentFile).Value
        let identEndRange = functionRange.EndRange
        let argumentIdentifier =
            createIdentifier (identEndRange.End.Line, (identEndRange.End.Column+1)) argumentName functionRange.FileName
        project, [identEndRange, " " + argumentName], argumentIdentifier
    
    { analysis = (fun (_,_) -> true);
      transform = transform;
      getErrorMessage = fun _ -> None }

//TODO: Only add brackets around usage if needed
let addArgumentToFunctionUsage (project:Project) (argument : string) (identRange : range) =
    let ident = TextOfRange project.CurrentFileContents identRange
    { analysis = (fun (_,_) -> true);
      transform = fun (s,_) -> (s,[identRange, sprintf "(%s %s)" ident argument],());
      getErrorMessage = fun _ -> None }

let findFunctionUsageRanges (project:Project) (tree : Ast.AstNode) (functionName, functionRange) =
    IdentifierScope((functionName, functionRange), project).FindReferences()
    |> List.filter ((<>) functionRange)

//TODO: Check arguments such as argumentName or defaultValue have a valid form
let addTempArgument (functionIdentifier:Identifier) defaultValue : Refactoring<unit,Identifier> =
    let transform (project:Project, ()) =
        let tree = (Ast.Parse project.CurrentFileContents project.CurrentFile).Value
        let argumentName = FindUnusedName tree
        let usageRefactorings =
            findFunctionUsageRanges project tree functionIdentifier
            |> List.map (addArgumentToFunctionUsage project defaultValue)
        let bindingRefactoring = addArgumentToFunctionDeclaration functionIdentifier argumentName
        (List.fold interleave bindingRefactoring usageRefactorings).transform (project, ())

    { analysis = fun _ -> true;
      transform = transform;
      getErrorMessage = fun _ -> None }

let GetErrorMessage (position:(int*int) option, argumentName:string option, defaultValue:string option) (project:Project) =
    let pos = PosFromPositionOption position
    let binding =
        lazy
            let tree = (Ast.Parse project.CurrentFileContents project.CurrentFile).Value
            TryFindDefaultBinding project tree pos.Value

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
        let oldProject, changes, (_, identifierRange) =
            (addTempArgument functionIdentifier defaultValue).transform (project,())
        let sourceWithIdentifier = ChangeTextOf oldProject.CurrentFileContents changes
        let projectWithIdentifier = project.UpdateCurrentFileContents sourceWithIdentifier
        Rename.GetErrorMessage (Some (identifierRange.Start.Line, identifierRange.Start.Column+1), Some name) projectWithIdentifier
            
    IsSuccessful checkPosition position
    |> Andalso (IsSuccessful checkPositionNameAndValue (TripleOptions (position, argumentName, defaultValue)))
    |> fun (l:Lazy<_>) -> l.Force()

let IsValid (position:(int*int) option, argumentName:string option, defaultValue:string option) project =
    GetErrorMessage (position, argumentName, defaultValue) project
    |> Option.isNone

let AddArgument (functionIdentifier:Identifier) argumentName defaultValue : Refactoring<unit,unit> =
    let _, functionRange = functionIdentifier
    let analysis (project, ()) =
            IsValid (Some (functionRange.Start.Line, functionRange.Start.Column+1), Some argumentName, Some defaultValue) project
            
    let getErrorMessage (project, ()) =
        GetErrorMessage (Some (functionRange.Start.Line, functionRange.Start.Column+1), Some argumentName, Some defaultValue) project
        
    let addTempArgumentRefactoring = addTempArgument functionIdentifier defaultValue
    let addArgumentRefactoring = sequence addTempArgumentRefactoring (Rename.Rename argumentName)
    { addArgumentRefactoring with analysis = analysis; getErrorMessage = getErrorMessage }

let Transform ((line, col):int*int, argumentName:string, defaultValue:string) (project:Project) =
    let pos = mkPos line (col-1)
    let tree = (Ast.Parse project.CurrentFileContents project.CurrentFile).Value
    let binding = TryFindDefaultBinding project tree pos
    let functionIdentifier = TryFindFunctionIdentifier binding.Value
    RunRefactoring (AddArgument functionIdentifier.Value argumentName defaultValue) () project
