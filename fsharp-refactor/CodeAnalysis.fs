namespace FSharpRefactor.Engine.CodeAnalysis

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast

module RangeAnalysis =
    let rec ListNodes (tree : Ast.AstNode) =
        match tree with
            | Ast.Children cs -> tree::(List.concat (Seq.map ListNodes cs))
            | _ -> [tree]
            
    let rec FindNodesWithRange range (tree : Ast.AstNode) =
        let allNodes = ListNodes tree
        let hasRange node =
            Option.isSome (Ast.GetRange node) && (Ast.GetRange node).Value = range
        List.filter hasRange allNodes

    let FindAstNodeAtRange range (tree : Ast.AstNode) isNode =
        List.find isNode (FindNodesWithRange range tree)

    let FindExpressionAtRange range (tree : Ast.AstNode)  =
        let isExpression node =
            match node with
                | Ast.AstNode.Expression _ -> true
                | _ -> false
        FindAstNodeAtRange range tree isExpression

    let FindBindingAtRange range (tree : Ast.AstNode) =
        let isBinding node  =
            match node with
                | Ast.AstNode.Binding _ -> true
                | _ -> false
        FindAstNodeAtRange range tree isBinding


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
            //TODO: Make everything work with LongIdentWithDots
            | Ast.Pattern(SynPat.LongIdent(LongIdentWithDots(i::_,_),_,_,_,_,_)) ->
                Some(i.idText, i.idRange)
            | _ -> None

    let DefaultDeclared = Set ["op_Addition"]

    let rec ListNodes tree =
        match tree with
            | Declaration(is, ts) as d -> d::(List.collect ListNodes ts)
            | usage -> [usage]

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

    let GetDeclarations (trees : ScopeTree list) =
        let rec declarationsInSingleTree tree =
            match tree with
                | Usage(n,_) -> Set []
                | Declaration(is, ts) ->
                    let declarationsInChildren =
                        Set.unionMany (Seq.map declarationsInSingleTree ts)
                    Set.union declarationsInChildren (Set(List.map fst is))

        Set.unionMany (Seq.map declarationsInSingleTree trees)

    let rec getDeclarations p =
        match p with
            | DeclaredIdent(text, range) -> [(text, range)]
            | Ast.Children cs -> List.concat (Seq.map getDeclarations cs)
            | _ -> []

    let isDeclaration (tree : ScopeTree) =
        match tree with
            | Declaration(_,_) -> true
            | _ -> false

    let isUsage (tree : ScopeTree) =
        match tree with
            | Usage(_,_) -> true
            | _ -> false

    let addChildren (tree : ScopeTree) (children : ScopeTree list) =
        if List.isEmpty children then tree else
        match tree with
            | Usage(text,range) -> Declaration([text,range],children)
            | Declaration(is, cs) -> Declaration(is, List.append cs children)

    let mergeBindings (bindingTrees : ScopeTree list list) =
        let rec mergeDeclarations declarations =
            match declarations with
                | [d] -> d
                | Declaration(is1, ts1)::Declaration(is2, ts2)::ts ->
                    mergeDeclarations (Declaration(List.append is1 is2, List.append ts1 ts2)::ts)

        let mainDeclarations = List.map List.head bindingTrees
        let rest = List.concat (List.map List.tail bindingTrees)

        mergeDeclarations mainDeclarations
        |> fun declaration -> addChildren declaration rest
        
    let rec makeScopeTrees (tree : Ast.AstNode) =
        let rec makeNestedScopeTrees declarations =
            match declarations with
                | [] -> []
                | d::ds ->
                    let headScopeTree = makeScopeTrees d
                    let tailScopeTree = makeNestedScopeTrees ds
                    (addChildren (List.head headScopeTree) tailScopeTree)::(List.tail headScopeTree)

        match tree with
            | Ast.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_,_,ds,_,_,_,_)) ->
                makeNestedScopeTrees (List.map Ast.AstNode.ModuleDeclaration ds)
            | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(false,bs,_)) ->
                makeNestedScopeTrees (List.map Ast.AstNode.Binding bs)
            | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(true,bs,_)) ->
                let scopeTreesFromBindings =
                    List.map makeScopeTrees (List.map Ast.AstNode.Binding bs)
                [mergeBindings scopeTreesFromBindings]
            | Ast.AstNode.Expression(SynExpr.LetOrUse(false,_,bs,e,_)) ->
                let bindingScopeTrees = makeNestedScopeTrees (List.map Ast.AstNode.Binding bs)
                let expressionScopeTrees = makeScopeTrees (Ast.AstNode.Expression e)
                (addChildren (List.head bindingScopeTrees) expressionScopeTrees)::(List.tail bindingScopeTrees)
            | Ast.AstNode.Expression(SynExpr.LetOrUse(true,_,bs,e,_)) ->
                let scopeTreesFromBindings =
                    List.map makeScopeTrees (List.map Ast.AstNode.Binding bs)
                let bindingScopeTree = mergeBindings scopeTreesFromBindings
                let expressionScopeTrees = makeScopeTrees (Ast.AstNode.Expression e)
                [addChildren bindingScopeTree expressionScopeTrees]
            | Ast.AstNode.MatchClause(Clause(p,we,e,_,_)) ->
                [Declaration(getDeclarations (Ast.AstNode.Pattern p),
                             makeScopeTrees (Ast.AstNode.Expression e))]
            | Ast.AstNode.Binding(SynBinding.Binding(_,_,_,_,_,_,_,pattern,_,expression,_,_)) ->
                let idsDeclaredInBinding = getDeclarations (Ast.AstNode.Pattern pattern)
                let scopeTreesFromBinding = makeScopeTrees (Ast.AstNode.Expression expression)
                match pattern with
                    | SynPat.LongIdent(functionIdent,_,_,arguments,_,_) ->
                        let idsFromArgument a = getDeclarations (Ast.AstNode.Pattern a)
                        let idsFromArguments = List.concat (Seq.map idsFromArgument arguments)
                        [Declaration(idsDeclaredInBinding, []);
                         Declaration(idsFromArguments, makeScopeTrees (Ast.AstNode.Expression expression))]
                    | _ -> Declaration(idsDeclaredInBinding, [])::scopeTreesFromBinding
            | Ast.Children(cs) -> List.concat (Seq.map makeScopeTrees cs)
            | UsedIdent(text, range) -> [Usage(text, range)]
            | _ -> []
