module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Engine.ValidityChecking

//TODO: these probably need to be put in an .fsi file
let GetErrorMessage (position:(int*int) option, newName:string option) (source:string) (filename:string) =
    let pos = PosFromPositionOption position
    let scopeTrees =
        lazy (makeScopeTrees (Ast.Parse source filename).Value)
    let identifier =
        lazy (Option.bind (TryFindIdentifier source filename) pos)
    let identifierDeclaration =
        lazy
            let tryFindDeclaration =
                TryFindIdentifierDeclaration (scopeTrees.Force())
            Option.bind tryFindDeclaration (identifier.Force())
    let declarationScope =
        lazy
            let tryFindDeclarationScope =
                TryFindDeclarationScope (scopeTrees.Force())
            Option.bind tryFindDeclarationScope (identifierDeclaration.Force())

    let checkPosition (line, col) =
        match Option.isSome (identifier.Force()), Option.isSome (identifierDeclaration.Force()) with
            | false,_ -> Some("No identifier found at the given range")
            | _,false ->
                let identifierName, _ = identifier.Value.Value
                Some(sprintf "The identifier %A was not declared in the given source" identifierName)
            | _ -> None

    let checkName newName =
        //TODO: check newName is a valid name
        None

    let checkPositionAndName (position, newName) =
        let oldName, _ = identifierDeclaration.Force().Value
        let newNameIsBound =
            DeclaredNames (declarationScope.Force().Value)
            |> IsDeclared newName
            |> (&&) (oldName <> newName)

        let newNameIsFree =
            IsFree newName (declarationScope.Value.Value)

        let oldNameIsFree =
            GetShallowestDeclarations newName (declarationScope.Value.Value)
            |> List.map (IsFree oldName)
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
    let analysis (source, (_, identifierRange) : Identifier) =
        IsValid (Some (identifierRange.Start.Line, identifierRange.Start.Column+1), Some newName) source filename

    let transform (source, identifier) =
        let tree = (Ast.Parse source filename).Value
        let declarationScope =
            FindDeclarationScope (makeScopeTrees tree) identifier
        let changes =
            FindDeclarationReferences identifier declarationScope
            |> List.map (fun r -> (r,newName))
        source, changes, ()

    let getErrorMessage (source, (_, range : range)) =
        let pos = range.Start
        GetErrorMessage (Some (pos.Line, pos.Column+1), Some newName) source filename
    { analysis = analysis; transform = transform; getErrorMessage = getErrorMessage }

let Transform ((line:int, col:int), newName:string) (source:string) (filename:string) =
    let position = mkPos line (col-1)
    let tree = (Ast.Parse source filename).Value
    let declarationIdentifier =
        FindIdentifierDeclaration (makeScopeTrees (Ast.Parse source filename).Value) (FindIdentifier source filename position)
    RunRefactoring (Rename newName filename) declarationIdentifier source