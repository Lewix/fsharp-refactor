module FSharpRefactor.Engine.ScopeAnalysis

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.Modules

type ScopeTree<'declaration, 'usage> =
    | Declaration of 'declaration * ScopeTree<'declaration, 'usage> list
    | Usage of 'usage

type Identifier = string * range
type IdentifierScopeTree = ScopeTree<Identifier list, Identifier * Identifier list>

let (|UsedIdent|_|) (node : Ast.AstNode) =
    match node with
        | Ast.AstNode.Expression(SynExpr.Ident(i)) -> Some [i.idText, i.idRange]
        | Ast.AstNode.Expression(SynExpr.LongIdent(_,LongIdentWithDots(is,_),_,_)) ->
            Some (List.map (fun (i:Ident) -> i.idText, i.idRange) is)
        | _ -> None

let rec ListIdentifiers trees =
    match trees with
        | [] -> []
        | Usage((name,range),_)::rest -> (name,range)::(ListIdentifiers rest)
        | Declaration(is,ts)::rest -> List.append is (ListIdentifiers (List.append ts rest))

let isDeclaration (tree : ScopeTree<'declaration,'usage>) =
    match tree with
        | Declaration(_,_) -> true
        | _ -> false

let isUsage (tree : ScopeTree<'declaration, 'usage>) =
    match tree with
        | Usage(_) -> true
        | _ -> false

let addChildren (tree : ScopeTree<'declaration, 'usage>) (children : ScopeTree<'declaration, 'usage> list) =
    if List.isEmpty children then tree else
    match tree with
        | Usage(_) ->
            failwith "transforming usage into decl"
        | Declaration(is, cs) -> Declaration(is, List.append cs children)

let mergeBindings (bindingTrees : IdentifierScopeTree list list) =
    let rec mergeDeclarations declarations =
        match declarations with
            | [] -> Declaration([],[])
            | [Declaration(_,_) as d] -> d
            | Declaration(is1, ts1)::Declaration(is2, ts2)::ts ->
                mergeDeclarations (Declaration(List.append is1 is2, List.append ts1 ts2)::ts)
            | Usage(_)::ts -> mergeDeclarations ts
            | (Declaration(_,_) as d)::Usage(_)::ts ->
                mergeDeclarations (d::ts)

    let mainDeclarations = List.map List.head bindingTrees
    let rest = List.concat (List.map List.tail bindingTrees)

    let declaration = mergeDeclarations mainDeclarations
    addChildren declaration rest


let makeNestedScopeTrees makeScopeTreesFunction childrenScopeTrees nodes =
    let reversedNodes = List.rev nodes
    Seq.fold makeScopeTreesFunction childrenScopeTrees reversedNodes
            
let rec makeScopeTreesWithChildren (project:Project) modules childrenScopeTrees (tree : Ast.AstNode) =
    let makeScopeTreesWithChildren = makeScopeTreesWithChildren project modules
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
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.NestedModule(_,ds,_,_))
        | Ast.ModuleOrNamespace(SynModuleOrNamespace.SynModuleOrNamespace(_,_,ds,_,_,_,_)) ->
            let shouldNestScopes = function
                | Ast.AstNode.ModuleDeclaration(SynModuleDecl.NestedModule(_,_,_,_)) -> true
                | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Types(_,_)) -> true
                | _ -> false
            let nestedDeclarations, otherDeclarations = 
                List.map Ast.AstNode.ModuleDeclaration ds
                |> List.partition shouldNestScopes
            List.append (List.collect (makeScopeTreesWithChildren []) nestedDeclarations)
                        (makeNestedScopeTrees makeScopeTreesWithChildren childrenScopeTrees otherDeclarations)
        | Ast.TypeDefinitionRepresentation(SynTypeDefnRepr.ObjectModel(_,ms,_)) ->
            makeNestedScopeTrees makeScopeTreesWithChildren childrenScopeTrees (List.map Ast.MemberDefinition ms)
        | Ast.MemberDefinition(SynMemberDefn.ImplicitCtor(_,_,ps,selfId,_)) ->
            let idsDeclaredInPatterns = declarationsFromSimplePatterns ps
            let idsInScopeInCtor =
                if Option.isNone selfId then idsDeclaredInPatterns
                else (selfId.Value.idText, selfId.Value.idRange)::idsDeclaredInPatterns
            [Declaration(idsInScopeInCtor, childrenScopeTrees)]
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Open(LongIdentWithDots(i::is,_), r)) ->
            //TODO: proper name resolution
            let fullName = i.idText, List.map (fun (i:Ident) -> i.idText) is
            let openedModule = Seq.tryFind (fun (m:Module) -> m.fullName = fullName) modules
            let declarations = if Option.isSome openedModule then openedModule.Value.declarations else []
            [Declaration(declarations,childrenScopeTrees)]
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(false,bs,_)) ->
            makeNestedScopeTrees makeScopeTreesWithChildren childrenScopeTrees (List.map Ast.AstNode.Binding bs)
        | Ast.AstNode.ModuleDeclaration(SynModuleDecl.Let(true,bs,_)) ->
            let scopeTreesFromBindings =
                List.map (makeScopeTreesWithChildren childrenScopeTrees) (List.map Ast.AstNode.Binding bs)
            [mergeBindings scopeTreesFromBindings]
        | Ast.AstNode.Expression(SynExpr.LetOrUse(false,_,bs,e,_)) ->
            let expressionScopeTrees = makeScopeTreesWithChildren childrenScopeTrees (Ast.AstNode.Expression e)
            makeNestedScopeTrees makeScopeTreesWithChildren expressionScopeTrees (List.map Ast.AstNode.Binding bs)
        | Ast.AstNode.Expression(SynExpr.LetOrUse(true,_,bs,e,_)) ->
            let scopeTreesFromBindings =
                List.map (makeScopeTreesWithChildren []) (List.map Ast.AstNode.Binding bs)
            let bindingScopeTree = mergeBindings scopeTreesFromBindings
            let expressionScopeTrees = makeScopeTreesWithChildren childrenScopeTrees (Ast.AstNode.Expression e)
            [addChildren bindingScopeTree expressionScopeTrees] //FIXME: don't call addChildren
        | Ast.AstNode.Expression(SynExpr.Lambda(_,_,ps,e,_)) ->
            let simplePatterns = flattenSimplePatterns ps
            let idsDeclaredInPatterns = declarationsFromSimplePatterns simplePatterns
            [Declaration(idsDeclaredInPatterns,
                         makeScopeTreesWithChildren childrenScopeTrees (Ast.AstNode.Expression e))]
        | Ast.AstNode.MatchClause(Clause(p,we,e,_,_)) ->
            let children =
                List.concat [childrenScopeTrees;
                            (if Option.isNone we then []
                             else makeScopeTreesWithChildren [] (Ast.AstNode.Expression we.Value));
                            (makeScopeTreesWithChildren [] (Ast.AstNode.Expression e))]
            [Declaration(declarationsFromMatchPattern p, children)]
        | Ast.AstNode.Binding(SynBinding.Binding(_,_,_,_,_,_,_,pattern,_,expression,_,_)) ->
            let idsDeclaredInBinding = declarationsFromFunctionPatterns [pattern]
            let scopeTreesFromBinding = makeScopeTreesWithChildren [] (Ast.AstNode.Expression expression)
            match pattern with
                | SynPat.LongIdent(functionIdent,a,b,arguments,c,d) ->
                    let functionScopeTrees =
                        let functionIdentifiers =
                            declarationsFromFunctionPatterns [SynPat.LongIdent(functionIdent,a,b,[],c,d)]
                        if functionIdentifiers = [] then childrenScopeTrees
                        else [Declaration(functionIdentifiers, childrenScopeTrees)]
                    let argumentsScopeTrees =
                        let selfIdentifier = tryGetSelfIdentifier functionIdent
                        let idsFromArguments =
                            declarationsFromFunctionPatterns arguments
                        let idsInScopeInExpression =
                            if Option.isNone selfIdentifier then idsFromArguments
                            else selfIdentifier.Value::idsFromArguments
                        if idsInScopeInExpression = [] then scopeTreesFromBinding
                        else [Declaration(idsInScopeInExpression, scopeTreesFromBinding)]
                    if idsDeclaredInBinding = [] then argumentsScopeTrees
                    else
                        functionScopeTrees @ argumentsScopeTrees
                |  _ -> Declaration(idsDeclaredInBinding, childrenScopeTrees)::scopeTreesFromBinding
        | UsedIdent(idents) -> Usage(List.head idents, List.tail idents)::childrenScopeTrees
        | Ast.Children(cs) -> List.concat (Seq.map (makeScopeTreesWithChildren childrenScopeTrees) cs)
        | _ -> childrenScopeTrees

let makeProjectScopeTrees project tree =
    let modules = GetModules project
    makeScopeTreesWithChildren project modules [] tree