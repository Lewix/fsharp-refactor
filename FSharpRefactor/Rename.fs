module FSharpRefactor.Refactorings.Rename

open System.IO
open System.Text.RegularExpressions
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
let GetErrorMessage (position:(int*int) option, newName:string option) (project:Project) (filename:string) =
    let filename = Path.GetFullPath filename
    let pos = PosFromPositionOption position
    let identifier =
        lazy Option.bind (TryFindIdentifier project filename) pos
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
        let letter = "\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}"
        let digit = "\p{Nd}"
        let connectingChar = "\p{Pc}"
        let combiningChar = "\p{Mn}\p{Mc}"
        let formattingChar = "\p{Cf}"
        let startChar = sprintf "[_%s]" letter
        let identChar = sprintf "['%s%s%s%s%s]" letter digit connectingChar combiningChar formattingChar
        let ident = sprintf "%s%s*" startChar identChar
        
        let isValidShortIdent =
            let identMatch = Regex.Match(newName, ident)
            String.length newName = identMatch.Length
        let isValidLongIdent =
            if String.length newName > 4 then
                newName.StartsWith "``"
                |> (&&) (newName.EndsWith "``")
                |> (&&) (not (newName.[2..(String.length newName)-3].Contains "``"))
            else false
            
        match isValidShortIdent, isValidLongIdent with
            | false, false -> Some "The given name is not a valid identifier"
            | _ -> None

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

let IsValid (position:(int*int) option, newName:string option) project filename =
    let filename = Path.GetFullPath filename
    GetErrorMessage (position, newName) project filename
    |> Option.isNone

let Rename newName : Refactoring<Identifier,unit> =
    let analysis (project:Project, filename, identifier:Identifier) =
        let identifierScope = GetIdentifierScope project (identifier, [])
        IsValid (Some (identifierScope.DeclarationRange.StartLine, identifierScope.DeclarationRange.StartColumn+1), Some newName) project filename

    let transform (project:Project, filename, identifier:Identifier) =
        let identifierScope = GetIdentifierScope project (identifier, [])
        let changes =
            identifierScope.FindReferences ()
            |> List.map (fun r -> (r,newName))
        project, changes, ()

    let getErrorMessage (project:Project, filename, identifier:Identifier) =
        let identifierScope = GetIdentifierScope project (identifier, [])
        GetErrorMessage (Some (identifierScope.DeclarationRange.StartLine, identifierScope.DeclarationRange.StartColumn+1), Some newName) project filename
    { analysis = analysis; transform = transform; getErrorMessage = getErrorMessage }

let Transform ((line:int, col:int), newName:string) (project:Project) filename =
    let filename = Path.GetFullPath filename
    let position = mkPos line (col-1)
    let identifierScope = GetIdentifierScope project (FindIdentifier project filename position)
    RunRefactoring (Rename newName) identifierScope.IdentifierDeclaration project filename