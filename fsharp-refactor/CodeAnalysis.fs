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
            //TODO: Make everything work with LongIdentWithDots
            | Ast.Pattern(SynPat.LongIdent(LongIdentWithDots(i::is,_),_,_,_,_,_) as longIdent) ->
                Some(i.idText, longIdent.Range)
            | _ -> None

    let DefaultDeclared = Set ["op_Addition"]

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

    let mergeTrees (trees : ScopeTree list) =
        let rec mergeDeclarations declarations =
            match declarations with
                | [d] -> d
                | Declaration(is1, ts1)::Declaration(is2, ts2)::ts ->
                    mergeDeclarations (Declaration(List.append is1 is2, List.append ts1 ts2)::ts)

        let usages = List.filter isUsage trees
        let declarations = List.filter isDeclaration trees
        assert (declarations <> [])
       
        mergeDeclarations declarations
        |> fun declaration -> addChildren declaration usages
        
    //TODO: for recursive definitions, id is in its own scope
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
                    List.concat (Seq.map makeScopeTrees (List.map Ast.AstNode.Binding bs))
                [mergeTrees scopeTreesFromBindings]
            | Ast.AstNode.Expression(SynExpr.LetOrUse(false,_,bs,e,_)) ->
                let bindingScopeTrees = makeNestedScopeTrees (List.map Ast.AstNode.Binding bs)
                let expressionScopeTrees = makeScopeTrees (Ast.AstNode.Expression e)
                (addChildren (List.head bindingScopeTrees) expressionScopeTrees)::(List.tail bindingScopeTrees)
            | Ast.AstNode.Expression(SynExpr.LetOrUse(true,_,bs,e,_)) ->
                // let rec id1 = e1 [and id2 = e1 [...]] in e
                let scopeTreesFromBindings =
                    List.concat (Seq.map makeScopeTrees (List.map Ast.AstNode.Binding bs))
                let bindingScopeTree = mergeTrees scopeTreesFromBindings
                let expressionScopeTrees = makeScopeTrees (Ast.AstNode.Expression e)
                //TODO: mergeTrees needs to be fixed, as does addChildren
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
