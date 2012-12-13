namespace FSharpRefactor.Engine.CodeAnalysis

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
                          
module ScopeAnalysis =
    type Identifier = string * range
    
    type ScopeTree =
        | Declaration of Identifier list * ScopeTree list
        | Usage of Identifier

    //TODO: Consider LongIdent and LongIdentWithDots
    let (|UsedIdent|_|) (node : Ast.AstNode) =
        match node with
            | Ast.AstNode.Expression(SynExpr.Ident(i)) -> Some(i.idText, i.idRange)
            | _ -> None

    let (|DeclaredIdent|_|) (node : Ast.AstNode) =
        match node with
            | Ast.Pattern(SynPat.Named(_,i,_,_,_)) -> Some(i.idText, i.idRange)
            | _ -> None

    let GetFreeIdentifiers (trees : ScopeTree list) (declared : Set<string>) =
        let rec freeIdentifiersInSingleTree foundFree declared tree =
            match tree with
                | Usage(n,_) ->
                    if Set.contains n declared then foundFree
                    else Set.add n foundFree
                | Declaration(is, ts) ->
                    let updatedDeclared = Set.union declared (Set(List.map fst is))
                    Set.unionMany (Seq.map (freeIdentifiersInSingleTree foundFree updatedDeclared) ts)
                    
        Set.unionMany (Seq.map (freeIdentifiersInSingleTree (Set []) declared) trees)

    let GetDeclarations (trees : ScopeTree list) = Set []

    let rec getDeclarations p =
        match p with
            | DeclaredIdent(text, range) -> [(text, range)]
            | Ast.Children cs -> List.concat (Seq.map getDeclarations cs)
            | _ -> []

    let addChildren (tree : ScopeTree) (children : ScopeTree list) =
        if List.isEmpty children then tree else
        match tree with
            | Usage(text,range) -> Declaration([text,range],children)
            | Declaration(is, cs) -> Declaration(is, List.append cs children)

    //TODO: mutually recursive functions with "and" (multiple bindings per let)
    let rec makeScopeTrees (tree : Ast.AstNode) =
        let identifiersFromBinding binding =
            match binding with
                | SynBinding.Binding(_,_,_,_,_,_,_,p,_,_,_,_) ->
                    getDeclarations (Ast.AstNode.Pattern p)
        let scopeTreesFromBinding binding =
            match binding with
                | SynBinding.Binding(_,_,_,_,_,_,_,_,_,e,_,_) ->
                    makeScopeTrees (Ast.AstNode.Expression e)
        let rec makeNestedScopeTrees ds =
            match ds with
                | [] -> []
                | d::ds ->
                    let headScopeTree = makeScopeTrees d
                    let tailScopeTree = makeNestedScopeTrees ds
                    (addChildren (List.head headScopeTree) tailScopeTree)::(List.tail headScopeTree)

        match tree with
            | Ast.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_,_,ds,_,_,_,_)) ->
                makeNestedScopeTrees (List.map Ast.AstNode.ModuleDeclaration ds)
            | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(_,[b],_)) ->
                Declaration(identifiersFromBinding b, [])::(scopeTreesFromBinding b)
            | Ast.AstNode.Expression(SynExpr.LetOrUse(_,_,[b],e,_)) ->
                let is = identifiersFromBinding b
                let l1 = scopeTreesFromBinding b
                let l2 = makeScopeTrees (Ast.AstNode.Expression e)
                Declaration(is,l2)::l1
            | Ast.AstNode.MatchClause(Clause(p,we,e,_,_)) ->
                [Declaration(getDeclarations (Ast.AstNode.Pattern p),
                             makeScopeTrees (Ast.AstNode.Expression e))]
            | Ast.Children(cs) -> List.concat (Seq.map makeScopeTrees cs)
            | UsedIdent(text, range) -> [Usage(text, range)]
            | _ -> []
