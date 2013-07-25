module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Engine.ValidityChecking
 
let rec findDeclarationInScopeTrees trees (name, declarationRange) =
    match trees with
        | [] -> None
        | Usage(_,_)::ds -> findDeclarationInScopeTrees ds (name, declarationRange)
        | Declaration(is, ts)::ds ->
            let isDeclaration = (fun (n,r) -> n = name && rangeContainsRange r declarationRange)
            if List.exists isDeclaration is then Some(Declaration(is, ts))
            else findDeclarationInScopeTrees (List.append ts ds) (name, declarationRange)

let rec rangesToReplace (name, declarationRange) tree =
    let rangeOfIdent (name : string) (identifiers : Identifier list) =
        let identifier = List.tryFind (fun (n,_) -> n = name) identifiers
        if Option.isNone identifier then None else Some(snd identifier.Value)
    let isNestedDeclaration idents =
        List.exists (fun (n,r) -> n = name && not (rangeContainsRange r declarationRange)) idents
        
    match tree with
        | Usage(n, r) -> if n = name then [r] else []
        | Declaration(is, ts) ->
            if isNestedDeclaration is then []
            else
                let remainingRanges = List.concat (Seq.map (rangesToReplace (name, declarationRange)) ts)
                let declarationRange = rangeOfIdent name is
                if Option.isSome declarationRange then declarationRange.Value::remainingRanges
                else remainingRanges

//TODO: these probably need to be put in an .fsi file
let GetErrorMessage (position:(int*int) option, newName:string option) (source:string) (filename:string) =
    let rec isFree targetName tree =
        match tree with
            | Usage(n,_) -> n = targetName
            | Declaration(is, ts) ->
                if IsDeclared targetName is then false
                else List.fold (||) false (List.map (isFree targetName) ts)

    let rec getTopLevelDeclarations targetName tree =
        match tree with
            | Declaration(is, ts) as declaration->
                if IsDeclared targetName is
                then [declaration]
                else List.collect (getTopLevelDeclarations targetName) ts
            | _ -> []

    let pos = PosFromPositionOption position
    let scopeTrees =
        lazy (makeScopeTrees (Ast.Parse source).Value)
    let identifier =
        lazy (Option.bind (TryFindIdentifier source) pos)
    let identifierDeclaration =
        lazy
            let tryFindDeclaration =
                TryFindIdentifierDeclaration (scopeTrees.Force())
            Option.bind tryFindDeclaration (identifier.Force())
    let declarationScope =
        lazy
            let tryFindDeclarationScope =
                findDeclarationInScopeTrees (scopeTrees.Force())
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
        let newNameIsNotBound =
            match declarationScope.Force().Value with
                | Declaration(is,ts) ->
                    newName = oldName || not (IsDeclared newName is)
                | _ -> true
        let newNameIsNotFree =
            not (isFree newName (declarationScope.Value.Value))

        let oldNameIsNotFree =
            getTopLevelDeclarations newName (declarationScope.Value.Value)
            |> List.map (isFree oldName)
            |> List.fold (||) false
            |> not

        match newNameIsNotBound, newNameIsNotFree, oldNameIsNotFree with
            | false,_,_ -> Some(sprintf "%s is already declared in that pattern" newName)
            | _,false,_ -> Some(sprintf "%s is free in the scope of %s" newName oldName)
            | _,_,false -> Some(sprintf "%s is free in the scope of a %s defined in its scope" oldName newName)
            | _ -> None

    IsSuccessful checkPosition position
    |> Andalso (IsSuccessful checkName newName)
    |> Andalso (IsSuccessful checkPositionAndName (PairOptions (position, newName)))
    |> fun (l:Lazy<_>) -> l.Force()

let IsValid (position:(int*int) option, newName:string option) (source:string) (filename:string) =
    GetErrorMessage (position, newName) source filename
    |> Option.isNone

let Rename newName : Refactoring<Identifier,unit> =
    let analysis (source, (_, identifierRange) : Identifier) =
        IsValid (Some (identifierRange.Start.Line, identifierRange.Start.Column+1), Some newName) source "test.fs"

    let transform (source, identifier) =
        let tree = (Ast.Parse source).Value
        let declarationScope =
            findDeclarationInScopeTrees (makeScopeTrees tree) identifier
            |> Option.get
        let changes =
            rangesToReplace identifier declarationScope
            |> List.map (fun r -> (r,newName))
        source, changes, ()

    let getErrorMessage (source, (_, range : range)) =
        let pos = range.Start
        GetErrorMessage (Some (pos.Line, pos.Column+1), Some newName) source "test.fs"
    { analysis = analysis; transform = transform; getErrorMessage = getErrorMessage }

let Transform ((line:int, col:int), newName:string) (source:string) (filename:string) =
    let position = mkPos line (col-1)
    let tree = (Ast.Parse source).Value
    let declarationIdentifier =
        FindIdentifierDeclaration (makeScopeTrees (Ast.Parse source).Value) (FindIdentifier source position)
    RunRefactoring (Rename newName) declarationIdentifier source