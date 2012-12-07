module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis
open FSharpRefactor.Engine.CodeTransforms

type ScopeTree =
    | Declaration of (string * range) list * ScopeTree list
    | Usage of string * range

let isDeclared (name : string) (identifiers : (string * range) list) =
    List.exists (fun (n,_) -> n = name) identifiers

let rangeOfIdent (name : string) (identifiers : (string * range) list) =
    let identifier = List.tryFind (fun (n,_) -> n = name) identifiers
    if Option.isNone identifier then None else Some(snd identifier.Value)
    
let rec getDeclarations p =
    match p with
        | CodeAnalysis.Declaration(text, range) -> [(text, range)]
        | Ast.Children cs -> List.concat (Seq.map getDeclarations cs)
        | _ -> []

//TODO: mutually recursive functions with "and"
let rec makeScopeTree (tree : Ast.AstNode) =
    match tree with
        | Ast.AstNode.Expression(SynExpr.LetOrUse(_,_,[b],e,_)) ->
            let l1,v =
                match b with
                    | SynBinding.Binding(_,_,_,_,_,_,_,p,_,e,_,_) ->
                        (makeScopeTree (Ast.AstNode.Expression e),
                         getDeclarations (Ast.AstNode.Pattern p))
            let l2 = makeScopeTree (Ast.AstNode.Expression e)
            Declaration(v,l2)::l1
        | Ast.Children(cs) -> List.concat (Seq.map makeScopeTree cs)
        | CodeAnalysis.Usage(text, range) -> [Usage(text, range)]
        | _ -> []

let CanRename (tree : Ast.AstNode) (name : string, declarationRange : range) (newName : string) =
    let rec findDeclarationInScopeTrees trees =
        match trees with
            | [] -> None
            | Usage(_,_)::ds -> findDeclarationInScopeTrees ds
            | Declaration(is, ts)::ds ->
                let isDeclaration = (fun (n,r) -> n = name && rangeContainsRange r declarationRange)
                if List.exists isDeclaration is then Some(Declaration(is, ts))
                else findDeclarationInScopeTrees ds

    // Check if targetName is free in tree
    // Call onDeclarationFun if declaration of targetName encountered
    let rec isFree (onDeclarationFun : ScopeTree -> bool) targetName tree =
        match tree with
            | Usage(n,_) -> n <> targetName
            | Declaration(is, ts) ->
                if isDeclared targetName is
                then onDeclarationFun (Declaration(is, ts))
                else List.fold (fun state t -> state && isFree onDeclarationFun targetName t) true ts

    let declarationScope = findDeclarationInScopeTrees (makeScopeTree tree)
    if Option.isSome declarationScope
    then isFree (isFree (fun _ -> true) name) newName declarationScope.Value else false

//TODO: make some generic tree walking function which can be used for most of this
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

    let ranges = List.concat (Seq.map rangesToReplace (makeScopeTree tree))
    CodeTransforms.ChangeTextOf source (List.zip ranges (List.map (fun _ -> newName) [1..(ranges.Length)]))
   
