module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.CodeTransforms

let isDeclared (name : string) (identifiers : Identifier list) =
    List.exists (fun (n,_) -> n = name) identifiers

let rangeOfIdent (name : string) (identifiers : Identifier list) =
    let identifier = List.tryFind (fun (n,_) -> n = name) identifiers
    if Option.isNone identifier then None else Some(snd identifier.Value)
    
let rec findDeclarationInScopeTrees trees name declarationRange =
    match trees with
        | [] -> None
        | Usage(_,_)::ds -> findDeclarationInScopeTrees ds name declarationRange
        | Declaration(is, ts)::ds ->
            let isDeclaration = (fun (n,r) -> n = name && rangeContainsRange r declarationRange)
            if List.exists isDeclaration is then Some(Declaration(is, ts))
            else findDeclarationInScopeTrees ds name declarationRange

let CanRename (tree : Ast.AstNode) (name : string, declarationRange : range) (newName : string) =
    // Check if targetName is free in tree
    // Call onDeclarationFun if declaration of targetName encountered
    let rec isFree (onDeclarationFun : ScopeTree -> bool) targetName tree =
        match tree with
            | Usage(n,_) -> n <> targetName
            | Declaration(is, ts) ->
                if isDeclared targetName is
                then onDeclarationFun (Declaration(is, ts))
                else List.fold (fun state t -> state && isFree onDeclarationFun targetName t) true ts

    let declarationScope =
        findDeclarationInScopeTrees (makeScopeTrees tree) name declarationRange
    if Option.isSome declarationScope
    then isFree (isFree (fun _ -> true) name) newName declarationScope.Value else false

let DoRename source (tree: Ast.AstNode) (name : string, declarationRange : range) (newName : string) =
    let isNestedDeclaration idents =
        List.exists (fun (n,r) -> n = name && not (rangeContainsRange r declarationRange)) idents
    let rec rangesToReplace tree =
        match tree with
            | Usage(n, r) -> if n = name then [r] else []
            | Declaration(is, ts) ->
                if isNestedDeclaration is then []
                else
                    let declarationRange = rangeOfIdent name is
                    let remainingRanges = List.concat (Seq.map rangesToReplace ts)
                    if Option.isSome declarationRange then declarationRange.Value::remainingRanges
                    else remainingRanges

    let declarationScope =
        findDeclarationInScopeTrees (makeScopeTrees tree) name declarationRange
    if Option.isNone declarationScope
    then source
    else
        let ranges = rangesToReplace declarationScope.Value
        CodeTransforms.ChangeTextOf source
                                    (List.zip ranges (List.map (fun _ -> newName) [1..(ranges.Length)]))
   
