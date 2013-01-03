module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.RefactoringWorkflow


let isDeclared (name : string) (identifiers : Identifier list) =
    List.exists (fun (n,_) -> n = name) identifiers

let rangeOfIdent (name : string) (identifiers : Identifier list) =
    let identifier = List.tryFind (fun (n,_) -> n = name) identifiers
    if Option.isNone identifier then None else Some(snd identifier.Value)
    
let rec findDeclarationInScopeTrees trees (name, declarationRange) =
    match trees with
        | [] -> None
        | Usage(_,_)::ds -> findDeclarationInScopeTrees ds (name, declarationRange)
        | Declaration(is, ts)::ds ->
            let isDeclaration = (fun (n,r) -> n = name && rangeContainsRange r declarationRange)
            if List.exists isDeclaration is then Some(Declaration(is, ts))
            else findDeclarationInScopeTrees ds (name, declarationRange)

let rec rangesToReplace (name, declarationRange) tree =
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

let CanRename (tree : Ast.AstNode) (name : string, declarationRange : range) (newName : string) =
    // Check if targetName is free in tree
    // Call onDeclarationFun if declaration of targetName encountered
    let newNameIsFree = sprintf "%s is free in the scope of %s" newName name
    let oldNameIsFree = sprintf "%s is free in the scope of a %s defined in its scope" name newName
    let rec isFree targetName tree =
        match tree with
            | Usage(n,_) -> n = targetName
            | Declaration(is, ts) ->
                if isDeclared targetName is then false
                else List.fold (||) false (List.map (isFree targetName) ts)
    let rec getTopLevelDeclarations targetName tree =
        match tree with
            | Declaration(is, ts) as declaration->
                if isDeclared targetName is
                then [declaration]
                else List.collect (getTopLevelDeclarations targetName) ts
            | _ -> []

    let declarationScope =
        findDeclarationInScopeTrees (makeScopeTrees tree) (name, declarationRange)
        
    if Option.isSome declarationScope then
        let isNameBoundTwice =
            match declarationScope.Value with
                | Declaration(is,ts) -> isDeclared newName is
                | _ -> false
        if isNameBoundTwice then Invalid(sprintf "%s is already declared in that pattern" newName)
        else
            if isFree newName declarationScope.Value then Invalid(newNameIsFree)
            else
                let isOldNameFree =
                    getTopLevelDeclarations newName declarationScope.Value
                    |> List.map (isFree name)
                    |> List.fold (||) false
                if isOldNameFree then Invalid(oldNameIsFree)
                else Valid
    else Invalid("Could not find a declaration at the given range")


let Rename source (tree: Ast.AstNode) (declarationIdentifier : Identifier) (newName : string) =
    let valid = CanRename tree declarationIdentifier newName 
    refactoring source valid {
        let declarationScope =
            findDeclarationInScopeTrees (makeScopeTrees tree) declarationIdentifier
        if Option.isSome declarationScope
        then 
            let ranges = rangesToReplace declarationIdentifier declarationScope.Value
            for range in ranges do yield (range,newName)
    }


let DoRename source (tree: Ast.AstNode) (declarationIdentifier : Identifier) (newName : string) =
    RunRefactoring (Rename source tree declarationIdentifier newName)
