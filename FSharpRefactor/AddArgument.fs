module FSharpRefactor.Refactorings.AddArgument

open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.Modules
open FSharpRefactor.Engine.RangeAnalysis
open FSharpRefactor.Engine.ScopeAnalysis
open FSharpRefactor.Engine.CodeTransforms.CodeTransforms
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Engine.ValidityChecking
open FSharpRefactor.Engine.Scoping
open FSharpRefactor.Engine.Projects
open FSharpRefactor.Engine
open FSharpRefactor.Refactorings

let TryFindDefaultBinding (project:Project) filename (tree : Ast.AstNode) (position : pos) =
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
    
let addArgumentToFunctionDeclaration (functionName, functionRange:range) argumentName : Refactoring<unit,Identifier> =
    let transform (project:Project, filename, ()) =
        let tree = GetParseTree project filename
        let identEndRange = functionRange.EndRange
        let argumentIdentifier =
            createIdentifier (identEndRange.End.Line, (identEndRange.End.Column+1)) argumentName functionRange.FileName
        project, [identEndRange, " " + argumentName], argumentIdentifier
    
    { analysis = (fun _ -> true);
      transform = transform;
      getErrorMessage = fun _ -> None }

//TODO: Only add brackets around usage if needed
let addArgumentToFunctionUsage (project:Project) filename (argument : string) (identRange : range) =
    let ident = TextOfRange (project.GetContents filename) identRange
    { analysis = (fun _ -> true);
      transform = fun (s,_,_) -> (s,[identRange, sprintf "(%s %s)" ident argument],());
      getErrorMessage = fun _ -> None }

let findFunctionUsageRanges (project:Project) (tree : Ast.AstNode) (functionName, functionRange) =
    (GetIdentifierScope project ((functionName, functionRange), [])).FindReferences()
    |> List.filter ((<>) functionRange)

//TODO: Check arguments such as argumentName or defaultValue have a valid form
let addTempArgument (functionIdentifier:Identifier) defaultValue : Refactoring<unit,Identifier> =
    let transform (project:Project, filename, ()) =
        let tree = GetParseTree project filename
        let argumentName = FindUnusedName project tree
        let usageRefactorings =
            findFunctionUsageRanges project tree functionIdentifier
            |> List.map (addArgumentToFunctionUsage project filename defaultValue)
        let bindingRefactoring = addArgumentToFunctionDeclaration functionIdentifier argumentName
        (List.fold interleave bindingRefactoring usageRefactorings).transform (project, filename, ())

    { analysis = fun _ -> true;
      transform = transform;
      getErrorMessage = fun _ -> None }

let GetErrorMessage (position:(int*int) option, argumentName:string option, defaultValue:string option) (project:Project) filename =
    let filename = Path.GetFullPath filename
    let pos = PosFromPositionOption position
    let binding =
        lazy
            let tree = GetParseTree project filename
            TryFindDefaultBinding project filename tree pos.Value

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
            (addTempArgument functionIdentifier defaultValue).transform (project, filename, ())
        let sourceWithIdentifier = ChangeTextOf (oldProject.GetContents filename) changes
        let projectWithIdentifier = project.UpdateContents filename sourceWithIdentifier
        Rename.GetErrorMessage (Some (identifierRange.Start.Line, identifierRange.Start.Column+1), Some name) projectWithIdentifier filename
            
    IsSuccessful checkPosition position
    |> Andalso (IsSuccessful checkPositionNameAndValue (TripleOptions (position, argumentName, defaultValue)))
    |> fun (l:Lazy<_>) -> l.Force()

let IsValid (position:(int*int) option, argumentName:string option, defaultValue:string option) project filename =
    let filename = Path.GetFullPath filename
    GetErrorMessage (position, argumentName, defaultValue) project filename
    |> Option.isNone

let AddArgument (functionIdentifier:Identifier) argumentName defaultValue : Refactoring<unit,unit> =
    let _, functionRange = functionIdentifier
    let analysis (project, filename, ()) =
            IsValid (Some (functionRange.Start.Line, functionRange.Start.Column+1), Some argumentName, Some defaultValue) project filename
            
    let getErrorMessage (project, filename, ()) =
        GetErrorMessage (Some (functionRange.Start.Line, functionRange.Start.Column+1), Some argumentName, Some defaultValue) project filename
        
    let addTempArgumentRefactoring = addTempArgument functionIdentifier defaultValue
    let addArgumentRefactoring = sequence addTempArgumentRefactoring (Rename.Rename argumentName)
    { addArgumentRefactoring with analysis = analysis; getErrorMessage = getErrorMessage }

let Transform ((line, col):int*int, argumentName:string, defaultValue:string) (project:Project) filename =
    let filename = Path.GetFullPath filename
    let pos = mkPos line (col-1)
    let tree = GetParseTree project filename
    let binding = TryFindDefaultBinding project filename tree pos
    let functionIdentifier = TryFindFunctionIdentifier binding.Value
    RunRefactoring (AddArgument functionIdentifier.Value argumentName defaultValue) () project filename
