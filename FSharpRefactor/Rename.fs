module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Engine.ValidityChecking
open FSharpRefactor.Engine

//TODO: these probably need to be put in an .fsi file
let GetErrorMessage (position:(int*int) option, newName:string option) (source:string) (filename:string) =
    let pos = PosFromPositionOption position
    let identifier =
        lazy Option.bind (TryFindIdentifier source filename) pos
    let identifierScope =
        lazy Option.bind (fun (identifier:Identifier) -> Some (new IdentifierScope(identifier, source))) (identifier.Force())

    let checkPosition (line, col) =
        match Option.isSome (identifier.Force()), Option.isSome (identifierScope.Force()) with
            | false,_ -> Some("No identifier found at the given range")
            | _,false ->
                let identifierName, _ = identifier.Value.Value
                Some(sprintf "The identifier %A was not declared in the given source" identifierName)
            | _ -> None

    let checkName newName =
        //TODO: check newName is a valid name
        None

    let checkPositionAndName (position, newName) =
        let oldName = identifierScope.Force().Value.IdentifierName
        let newNameIsBound =
            identifierScope.Value.Value.NamesDeclaredInBinding
            |> IsDeclared newName
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

let IsValid (position:(int*int) option, newName:string option) (source:string) (filename:string) =
    GetErrorMessage (position, newName) source filename
    |> Option.isNone

let Rename newName filename : Refactoring<Identifier,unit> =
    let analysis (source:string, identifier:Identifier) =
        let identifierScope = new IdentifierScope(identifier, source)
        IsValid (Some (identifierScope.DeclarationRange.StartLine, identifierScope.DeclarationRange.StartColumn+1), Some newName) source filename

    let transform (source:string, identifier:Identifier) =
        let identifierScope = new IdentifierScope(identifier, source)
        let changes =
            identifierScope.FindReferences ()
            |> List.map (fun r -> (r,newName))
        source, changes, ()

    let getErrorMessage (source:string, identifier:Identifier) =
        let identifierScope = new IdentifierScope(identifier, source)
        GetErrorMessage (Some (identifierScope.DeclarationRange.StartLine, identifierScope.DeclarationRange.StartColumn+1), Some newName) source filename
    { analysis = analysis; transform = transform; getErrorMessage = getErrorMessage }

let Transform ((line:int, col:int), newName:string) (source:string) (filename:string) =
    let position = mkPos line (col-1)
    let tree = (Ast.Parse source filename).Value
    let identifierScope = new IdentifierScope(FindIdentifier source filename position, source)
    RunRefactoring (Rename newName filename) identifierScope.IdentifierDeclaration source