module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis

type ScopeTree =
    | Declaration of (string * range) list * ScopeTree list
    | Usage of string * range

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
            | Declaration(vs, ts)::ds ->
                let isDeclaration = (fun (n,r) -> n = name && rangeContainsRange r declarationRange)
                if List.exists isDeclaration vs then Some(Declaration(vs, ts))
                else findDeclarationInScopeTrees ds

    // Check if targetName is free in tree
    // Call onDeclarationFun if declaration of targetName encountered
    let rec isFree (onDeclarationFun : ScopeTree -> bool) targetName tree =
        match tree with
            | Usage(n,_) -> n <> targetName
            | Declaration(vs, ts) ->
                if List.exists (fun (n,_) -> n = targetName) vs
                then onDeclarationFun (Declaration(vs, ts))
                else List.fold (fun state t -> state && isFree onDeclarationFun targetName t) true ts

    let declarationScope = findDeclarationInScopeTrees (makeScopeTree tree)
    if Option.isSome declarationScope
    then isFree (isFree (fun _ -> true) name) newName declarationScope.Value else false
