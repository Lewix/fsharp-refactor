namespace FSharpRefactor.Engine.CodeAnalysis

open System
open System.Collections.Generic
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
            //TODO: Make everything work with LongIdentWithDots
            | Ast.Pattern(SynPat.LongIdent(LongIdentWithDots(i::_,_),_,_,_,_,_)) ->
                Some(i.idText, i.idRange)
            | _ -> None

    let DefaultDeclared = Set ["op_Addition"]

    let rec ListNodes tree =
        match tree with
            | Declaration(is, ts) as d -> d::(List.collect ListNodes ts)
            | usage -> [usage]

    let rec ListIdentifiers trees =
        match trees with
            | [] -> []
            | Usage(name, range)::rest -> (name,range)::(ListIdentifiers rest)
            | Declaration(is,ts)::rest -> List.append is (ListIdentifiers (List.append ts rest))

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

    let IsDeclared (name : string) (identifiers : Identifier list) =
        List.exists (fun (n,_) -> n = name) identifiers

    let TryFindIdentifierWithName (trees : ScopeTree list) (name : string) =
        let rec tryFindIdentifierWithName tree =
            match tree with
                | Declaration(is, ts) ->
                    if IsDeclared name is then Some (List.find (fun (n,r) -> n = name) is)
                    else List.tryPick tryFindIdentifierWithName ts
                | _ -> None
        List.tryPick tryFindIdentifierWithName trees

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
                    let headScopeTrees = makeScopeTrees d
                    let tailScopeTrees = makeNestedScopeTrees ds
                    if List.isEmpty headScopeTrees then
                        tailScopeTrees
                    else
                        (addChildren (List.head headScopeTrees) tailScopeTrees)::(List.tail headScopeTrees)

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


    let TryFindExpressionAtRange range (tree : Ast.AstNode)  =
        let isExpression node =
            match node with
                | Ast.AstNode.Expression _ -> true
                | _ -> false
        List.tryFind isExpression (FindNodesWithRange range tree)

    let chooseBinding node  =
        match node with
            | Ast.AstNode.Binding b -> Some b
            | _ -> None
                
    let FindBindingAtRange range (tree : Ast.AstNode) =
        List.pick chooseBinding (FindNodesWithRange range tree)

    let rec FindNodesAroundRange range (tree : Ast.AstNode) =
        let treeContainsRange tree =
            Option.isSome (Ast.GetRange tree) && rangeContainsRange (Ast.GetRange tree).Value range
        ListNodes tree
        |> List.filter treeContainsRange

    let TryFindBindingAroundRange range (tree : Ast.AstNode) =
        FindNodesAroundRange range tree
        |> List.tryPick chooseBinding

    let TryFindExpressionAroundRange range (tree : Ast.AstNode) =
        let chooseExpression node =
            match node with
                | Ast.AstNode.Expression e -> Some e
                | _ -> None
        FindNodesAroundRange range tree
        |> List.tryPick chooseExpression
        
    let FindIdentifierAtRange range (tree : Ast.AstNode) =
        let binding = FindBindingAtRange range tree
        let pattern =
            match binding with
                | SynBinding.Binding(_,_,_,_,_,_,_,pattern,_,_,_,_) -> Ast.AstNode.Pattern pattern
        match pattern with
            | ScopeAnalysis.DeclaredIdent id -> id
            | _ -> raise (new KeyNotFoundException("Couldn't find an identifier declaration at that range"))

    let FindIdentifierName source (position : pos) =
        let nameIfContainsPos (name,range) =
            if rangeContainsPos range position then Some name else None

        (Ast.Parse source).Value
        |> ScopeAnalysis.makeScopeTrees
        |> ScopeAnalysis.ListIdentifiers
        |> List.tryPick nameIfContainsPos
