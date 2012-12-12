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

let addChildren (tree : ScopeTree) (children : ScopeTree list) =
    if List.isEmpty children then tree else
    match tree with
        | Usage(text,range) -> Declaration([text,range],children)
        | Declaration(is, cs) -> Declaration(is, List.append cs children)

//TODO: mutually recursive functions with "and" (multiple bindings per let)
//TODO: rename makeScopeTree to makeScopeTrees
let rec makeScopeTree (tree : Ast.AstNode) =
    let identifiersFromBinding binding =
        match binding with
            | SynBinding.Binding(_,_,_,_,_,_,_,p,_,_,_,_) ->
                getDeclarations (Ast.AstNode.Pattern p)
    let scopeTreesFromBinding binding =
        match binding with
            | SynBinding.Binding(_,_,_,_,_,_,_,_,_,e,_,_) ->
                makeScopeTree (Ast.AstNode.Expression e)
    let rec makeNestedScopeTrees ds =
        match ds with
            | [] -> []
            | d::ds ->
                let headScopeTree = makeScopeTree d
                let tailScopeTree = makeNestedScopeTrees ds
                (addChildren (List.head headScopeTree) tailScopeTree)::(List.tail headScopeTree)
        
    match tree with
        | Ast.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_,_,ds,_,_,_,_)) ->
            makeNestedScopeTrees (List.map Ast.AstNode.Module ds)
        | Ast.AstNode.Module(SynModuleDecl.Let(_,[b],_)) ->
            Declaration(identifiersFromBinding b, [])::(scopeTreesFromBinding b)
        | Ast.AstNode.Expression(SynExpr.LetOrUse(_,_,[b],e,_)) ->
            let is = identifiersFromBinding b
            let l1 = scopeTreesFromBinding b
            let l2 = makeScopeTree (Ast.AstNode.Expression e)
            Declaration(is,l2)::l1
        | Ast.AstNode.MatchClause(Clause(p,we,e,_,_)) ->
            [Declaration(getDeclarations (Ast.AstNode.Pattern p),
                         makeScopeTree (Ast.AstNode.Expression e))]
        | Ast.Children(cs) -> List.concat (Seq.map makeScopeTree cs)
        | CodeAnalysis.Usage(text, range) -> [Usage(text, range)]
        | _ -> []

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

    let declarationScope = findDeclarationInScopeTrees (makeScopeTree tree) name declarationRange
    if Option.isSome declarationScope
    then isFree (isFree (fun _ -> true) name) newName declarationScope.Value else false

//TODO: make some generic tree walking function which can be used for most of this?
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

    let declarationScope = findDeclarationInScopeTrees (makeScopeTree tree) name declarationRange
    if Option.isNone declarationScope
    then source
    else
        let ranges = rangesToReplace declarationScope.Value
        CodeTransforms.ChangeTextOf source
                                    (List.zip ranges (List.map (fun _ -> newName) [1..(ranges.Length)]))
   
