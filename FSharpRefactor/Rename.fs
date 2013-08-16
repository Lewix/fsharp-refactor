module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis
open FSharpRefactor.Engine.RangeAnalysis
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Engine.ValidityChecking
open FSharpRefactor.Engine.Scoping
open FSharpRefactor.Engine.Projects
open FSharpRefactor.Engine

//TODO: these probably need to be put in an .fsi file
let GetErrorMessage (position:(int*int) option, newName:string option) (project:Project) =
    let pos = PosFromPositionOption position
    let identifier =
        lazy Option.bind (TryFindIdentifier project project.CurrentFile) pos
    let identifierScope =
        lazy Option.bind (fun (longIdentifier:Identifier * Identifier list) -> (TryGetIdentifierScope project longIdentifier)) (identifier.Force())

    let checkPosition (line, col) =
        match Option.isSome (identifier.Force()), Option.isSome (identifierScope.Force()) with
            | false,_ -> Some("No identifier found at the given range")
            | _,false ->
                let identifierName, _ = identifier.Value.Value
                Some(sprintf "The identifier %A was not declared in the given project" identifierName)
            | _ -> None

    let checkName newName =
        //TODO: check newName is a valid name
        None

    let checkPositionAndName (position, newName) =
        let oldName = identifierScope.Force().Value.IdentifierName
        let newNameIsBound =
            identifierScope.Value.Value.IsDeclaredInBinding newName
            |> (&&) (oldName <> newName)

        let newNameIsFree =
            identifierScope.Value.Value.IsFree newName

        let oldNameIsFree =
            identifierScope.Value.Value.FindNestedDeclarations newName
            |> List.map (fun scope -> scope.IsFree oldName)
            |> List.fold (||) false

        match newNameIsBound, newNameIsFree, oldNameIsFree with
            | true,_,_ -> Some(sprintf "%s is already declared in that pattern" newName)
            | _,true,_ -> Some(sprintf "%s is free in the scope of %s" newName oldName)
            | _,_,true -> Some(sprintf "%s is free in the scope of a %s defined in its scope" oldName newName)
            | _ -> None

    IsSuccessful checkPosition position
    |> Andalso (IsSuccessful checkName newName)
    |> Andalso (IsSuccessful checkPositionAndName (PairOptions (position, newName)))
    |> fun (l:Lazy<_>) -> l.Force()

let IsValid (position:(int*int) option, newName:string option) project =
    GetErrorMessage (position, newName) project
    |> Option.isNone

let Rename newName : Refactoring<Identifier,unit> =
    let analysis (project:Project, identifier:Identifier) =
        let identifierScope = GetIdentifierScope project (identifier, [])
        IsValid (Some (identifierScope.DeclarationRange.StartLine, identifierScope.DeclarationRange.StartColumn+1), Some newName) project

    let transform (project:Project, identifier:Identifier) =
        let identifierScope = GetIdentifierScope project (identifier, [])
        let changes =
            identifierScope.FindReferences ()
            |> List.map (fun r -> (r,newName))
        project, changes, ()

    let getErrorMessage (project:Project, identifier:Identifier) =
        let identifierScope = GetIdentifierScope project (identifier, [])
        GetErrorMessage (Some (identifierScope.DeclarationRange.StartLine, identifierScope.DeclarationRange.StartColumn+1), Some newName) project
    { analysis = analysis; transform = transform; getErrorMessage = getErrorMessage }

let Transform ((line:int, col:int), newName:string) (project:Project) =
    let position = mkPos line (col-1)
    let tree = GetParseTree project project.CurrentFile
    let identifierScope = GetIdentifierScope project (FindIdentifier project project.CurrentFile position)
    RunRefactoring (Rename newName) identifierScope.IdentifierDeclaration project