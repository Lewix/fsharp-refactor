module FSharpRefactor.Engine.ScopeAnalysis

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast

type ScopeTree<'declaration, 'usage> =
    | TopLevelDeclaration of 'declaration  * ScopeTree<'declaration, 'usage> list
    | Declaration of 'declaration * ScopeTree<'declaration, 'usage> list
    | Usage of 'usage

type Identifier = string * range
type IdentifierScopeTree = ScopeTree<Identifier list, Identifier>

let (|UsedIdent|_|) (node : Ast.AstNode) =
    match node with
        | Ast.AstNode.Expression(SynExpr.Ident(i)) -> Some(i.idText, i.idRange)
        | Ast.AstNode.Expression(SynExpr.LongIdent(_,LongIdentWithDots(i::_,_),_,_)) ->
            Some(i.idText, i.idRange)
        | _ -> None

let (|DeclaredIdent|_|) (node : Ast.AstNode) =
    match node with
        | Ast.Pattern(SynPat.Named(_,i,_,_,_)) -> Some(i.idText, i.idRange)
        | Ast.Pattern(SynPat.LongIdent(LongIdentWithDots([_;_],_),_,_,_,_,_)) ->
            // This is a member declaration of the form self.id, ignore both idents
            None
        | Ast.Pattern(SynPat.LongIdent(LongIdentWithDots(i::_,_),_,_,_,_,_)) ->
            Some(i.idText, i.idRange)
        | Ast.SimplePattern(SynSimplePat.Id(i,_,_,_,_,_)) ->
            Some(i.idText, i.idRange)
        | _ -> None

let rec ListIdentifiers trees =
    match trees with
        | [] -> []
        | Usage(name, range)::rest -> (name,range)::(ListIdentifiers rest)
        | TopLevelDeclaration(is,ts)::rest
        | Declaration(is,ts)::rest -> List.append is (ListIdentifiers (List.append ts rest))

let isDeclaration (tree : IdentifierScopeTree) =
    match tree with
        | TopLevelDeclaration(_,_)
        | Declaration(_,_) -> true
        | _ -> false

let isUsage (tree : IdentifierScopeTree) =
    match tree with
        | Usage(_,_) -> true
        | _ -> false

let addChildren (tree : IdentifierScopeTree) (children : IdentifierScopeTree list) =
    if List.isEmpty children then tree else
    match tree with
        | Usage(text,range) ->
            printfn "transforming usage into decl"
            Declaration([text,range],children)
        | TopLevelDeclaration(is, cs) -> TopLevelDeclaration(is, List.append cs children)
        | Declaration(is, cs) -> Declaration(is, List.append cs children)

let mergeBindings (bindingTrees : IdentifierScopeTree list list) =
    let rec mergeDeclarations declarations =
        match declarations with
            | [] -> Declaration([],[])
            | [TopLevelDeclaration(_,_) | Declaration(_,_) as d] -> d
            | TopLevelDeclaration(is1, ts1)::(TopLevelDeclaration(is2, ts2) | Declaration(is2, ts2))::ts
            | (TopLevelDeclaration(is1, ts1) | Declaration(is1, ts1))::TopLevelDeclaration(is2, ts2)::ts ->
                mergeDeclarations (TopLevelDeclaration(List.append is1 is2, List.append ts1 ts2)::ts)
            | Declaration(is1, ts1)::Declaration(is2, ts2)::ts ->
                mergeDeclarations (Declaration(List.append is1 is2, List.append ts1 ts2)::ts)
            | Usage(_,_)::ts -> mergeDeclarations ts
            | (TopLevelDeclaration(_,_) | Declaration(_,_) as d)::Usage(_,_)::ts ->
                mergeDeclarations (d::ts)

    let mainDeclarations = List.map List.head bindingTrees
    let rest = List.concat (List.map List.tail bindingTrees)

    let declaration = mergeDeclarations mainDeclarations
    addChildren declaration rest
    
let rec makeScopeTreesAtLevel isTopLevel (tree : Ast.AstNode) =
    let rec makeNestedScopeTrees isTopLevel declarations =
        match declarations with
            | [] -> []
            | d::ds ->
                let headScopeTrees = makeScopeTreesAtLevel isTopLevel d
                let tailScopeTrees = makeNestedScopeTrees isTopLevel ds
                let headDeclarations = List.filter isDeclaration headScopeTrees
                let headUsages = List.filter isUsage headScopeTrees

                if List.isEmpty headDeclarations then
                    List.append headUsages tailScopeTrees
                else
                    (addChildren (List.head headDeclarations) tailScopeTrees)::(List.append headUsages (List.tail headDeclarations))

    let declarationsFromMatchPattern pattern =
        let rec declarationsFromAstNode node =
            match node with
                | Ast.AstNode.Pattern(SynPat.LongIdent(patternDiscriminator,_,_,arguments,_,_)) ->
                    List.collect declarationsFromAstNode (List.map Ast.AstNode.Pattern arguments)
                | DeclaredIdent(text,range) -> [text,range]
                | Ast.Children cs -> List.collect declarationsFromAstNode cs
                | _ -> []
        declarationsFromAstNode (Ast.AstNode.Pattern pattern)

    let rec getDeclarations pattern =
        match pattern with
            | DeclaredIdent(text,range) -> [text,range]
            | Ast.Children cs -> List.collect getDeclarations cs
            | _ -> []

    let tryGetSelfIdentifier identWithDots =
        match identWithDots with
            | LongIdentWithDots([i;_],_) -> Some (i.idText, i.idRange)
            | _ -> None

    let declarationsFromFunctionPatterns patterns =
        List.collect getDeclarations (List.map Ast.AstNode.Pattern patterns)

    let declarationsFromSimplePatterns patterns =
        List.collect getDeclarations (List.map Ast.AstNode.SimplePattern patterns)

    let rec flattenSimplePatterns patterns =
        match patterns with
            | SynSimplePats.SimplePats(ps,_) -> ps
            | SynSimplePats.Typed(ps,_,_) -> flattenSimplePatterns ps

    match tree with
        | Ast.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_,_,ds,_,_,_,_)) ->
            makeNestedScopeTrees isTopLevel (List.map Ast.AstNode.ModuleDeclaration ds)
        | Ast.TypeDefinitionRepresentation(SynTypeDefnRepr.ObjectModel(_,ms,_)) ->
            makeNestedScopeTrees isTopLevel (List.map Ast.MemberDefinition ms)
        | Ast.MemberDefinition(SynMemberDefn.ImplicitCtor(_,_,ps,selfId,_)) ->
            let idsDeclaredInPatterns = declarationsFromSimplePatterns ps
            let idsInScopeInCtor =
                if Option.isNone selfId then idsDeclaredInPatterns
                else (selfId.Value.idText, selfId.Value.idRange)::idsDeclaredInPatterns
            [Declaration(idsInScopeInCtor, [])]
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(false,bs,_)) ->
            makeNestedScopeTrees true (List.map Ast.AstNode.Binding bs)
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(true,bs,_)) ->
            let scopeTreesFromBindings =
                List.map (makeScopeTreesAtLevel true) (List.map Ast.AstNode.Binding bs)
            [mergeBindings scopeTreesFromBindings]
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.NestedModule(_,ds,_,_)) ->
            makeNestedScopeTrees true (List.map Ast.AstNode.ModuleDeclaration ds)
        | Ast.AstNode.Expression(SynExpr.LetOrUse(false,_,bs,e,_)) ->
            let bindingScopeTrees = makeNestedScopeTrees false (List.map Ast.AstNode.Binding bs)
            let expressionScopeTrees = makeScopeTreesAtLevel false (Ast.AstNode.Expression e)
            (addChildren (List.head bindingScopeTrees) expressionScopeTrees)::(List.tail bindingScopeTrees)
        | Ast.AstNode.Expression(SynExpr.LetOrUse(true,_,bs,e,_)) ->
            let scopeTreesFromBindings =
                List.map (makeScopeTreesAtLevel false) (List.map Ast.AstNode.Binding bs)
            let bindingScopeTree = mergeBindings scopeTreesFromBindings
            let expressionScopeTrees = makeScopeTreesAtLevel false (Ast.AstNode.Expression e)
            [addChildren bindingScopeTree expressionScopeTrees]
        | Ast.AstNode.Expression(SynExpr.Lambda(_,_,ps,e,_)) ->
            let simplePatterns = flattenSimplePatterns ps
            let idsDeclaredInPatterns = declarationsFromSimplePatterns simplePatterns
            [Declaration(idsDeclaredInPatterns,
                         makeScopeTreesAtLevel false (Ast.AstNode.Expression e))]
        | Ast.AstNode.MatchClause(Clause(p,we,e,_,_)) ->
            [Declaration(declarationsFromMatchPattern p,
                         makeScopeTreesAtLevel false (Ast.AstNode.Expression e))]
        | Ast.AstNode.Binding(SynBinding.Binding(_,_,_,_,_,_,_,pattern,_,expression,_,_)) ->
            let idsDeclaredInBinding = declarationsFromFunctionPatterns [pattern]
            let scopeTreesFromBinding = makeScopeTreesAtLevel false (Ast.AstNode.Expression expression)
            match isTopLevel, pattern with
                | _, SynPat.LongIdent(functionIdent,_,_,arguments,_,_) ->
                    let selfIdentifier = tryGetSelfIdentifier functionIdent
                    let idsFromArguments =
                        declarationsFromFunctionPatterns arguments
                    let idsInScopeInExpression =
                        if Option.isNone selfIdentifier then idsFromArguments
                        else selfIdentifier.Value::idsFromArguments
                    let argumentsScopeTrees =
                        if idsInScopeInExpression = [] then scopeTreesFromBinding
                        else [Declaration(idsInScopeInExpression, scopeTreesFromBinding)]
                    if idsDeclaredInBinding = [] then argumentsScopeTrees
                    else
                        if isTopLevel then TopLevelDeclaration(idsDeclaredInBinding, [])::argumentsScopeTrees
                        else Declaration(idsDeclaredInBinding, [])::argumentsScopeTrees
                | true, _ -> TopLevelDeclaration(idsDeclaredInBinding, [])::scopeTreesFromBinding
                | false, _ -> Declaration(idsDeclaredInBinding, [])::scopeTreesFromBinding
        | UsedIdent(text,range) -> [Usage(text,range)]
        | Ast.Children(cs) -> List.concat (Seq.map (makeScopeTreesAtLevel isTopLevel) cs)
        | _ -> []

let makeScopeTrees (tree : Ast.AstNode) =
    makeScopeTreesAtLevel false tree