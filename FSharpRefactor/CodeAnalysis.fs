namespace FSharpRefactor.Engine.CodeAnalysis

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast

module ScopeAnalysis =
    type Identifier = string * range

    type ScopeTree =
        | TopLevelDeclaration of Identifier list * ScopeTree list
        | Declaration of Identifier list * ScopeTree list
        | Usage of Identifier

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

    let IsDeclared (name : string) (identifiers : Identifier list) =
        List.exists (fun (n,_) -> n = name) identifiers

    let GetFreeIdentifierUsages (trees : ScopeTree list) =
        let rec freeIdentifiersInSingleTree foundFree declared tree =
            match tree with
                | Usage(n,r) ->
                    if Set.contains n declared then foundFree
                    else (n,r)::foundFree
                | TopLevelDeclaration(is, ts)
                | Declaration(is, ts) ->
                    let updatedDeclared = Set.union declared (Set(List.map fst is))
                    List.collect (freeIdentifiersInSingleTree foundFree updatedDeclared) ts
                    
        List.collect (freeIdentifiersInSingleTree [] Set.empty<string>) trees

    let rec IsFree targetName tree =
        let hasTargetName (n,_) = n = targetName
            
        GetFreeIdentifierUsages [tree]
        |> List.exists hasTargetName

    let TryFindIdentifierDeclaration (trees : ScopeTree list) ((name, range) : Identifier) =
        let isDeclaration (n,r) = n = name && r = range
        let isSameName (n,r) = n = name
            
        let rec tryFindIdentifierAndDeclaration previousDeclaration tree =
            match tree with
                | Usage(n,r) -> if n = name && r = range then previousDeclaration else None
                | TopLevelDeclaration(is, ts)
                | Declaration(is, ts) ->
                    if List.exists isDeclaration is then
                        List.tryFind isDeclaration is
                    elif List.exists isSameName is then
                        List.tryPick (tryFindIdentifierAndDeclaration (List.tryFind isSameName is)) ts
                    else
                        List.tryPick (tryFindIdentifierAndDeclaration previousDeclaration) ts

        List.tryPick (tryFindIdentifierAndDeclaration None) trees
        
    let FindIdentifierDeclaration (trees : ScopeTree list) ((name, range) : Identifier) =
        TryFindIdentifierDeclaration trees (name, range)
        |> Option.get

    let TryFindIdentifierWithName (trees : ScopeTree list) (name : string) =
        let rec tryFindIdentifierWithName tree =
            match tree with
                | TopLevelDeclaration(is, ts)
                | Declaration(is, ts) ->
                    if IsDeclared name is then Some (List.find (fun (n,r) -> n = name) is)
                    else List.tryPick tryFindIdentifierWithName ts
                | Usage(_, _) -> None
        List.tryPick tryFindIdentifierWithName trees

    let GetDeclarations (trees : ScopeTree list) =
        let rec declarationsInSingleTree tree =
            match tree with
                | Usage(n,_) -> Set []
                | TopLevelDeclaration(is, ts)
                | Declaration(is, ts) ->
                    let declarationsInChildren =
                        Set.unionMany (Seq.map declarationsInSingleTree ts)
                    Set.union declarationsInChildren (Set(List.map fst is))

        Set.unionMany (Seq.map declarationsInSingleTree trees)

    let isDeclaration (tree : ScopeTree) =
        match tree with
            | TopLevelDeclaration(_,_)
            | Declaration(_,_) -> true
            | _ -> false

    let isUsage (tree : ScopeTree) =
        match tree with
            | Usage(_,_) -> true
            | _ -> false

    let addChildren (tree : ScopeTree) (children : ScopeTree list) =
        if List.isEmpty children then tree else
        match tree with
            | Usage(text,range) ->
                printfn "transforming usage into decl"
                Declaration([text,range],children)
            | TopLevelDeclaration(is, cs) -> TopLevelDeclaration(is, List.append cs children)
            | Declaration(is, cs) -> Declaration(is, List.append cs children)

    let mergeBindings (bindingTrees : ScopeTree list list) =
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

    let FindUnusedName (tree : Ast.AstNode) =
        let scopeTrees = makeScopeTrees tree
        let usedNames = GetDeclarations scopeTrees
        let randomNumberGenerator = new System.Random()

        let rec generateWhileUsed () =
            let name = "tmpFunction" + string (randomNumberGenerator.Next())
            if Set.contains name usedNames then generateWhileUsed ()
            else name

        generateWhileUsed ()


module RangeAnalysis =           
    let CountLines body =
        1+(String.length (String.collect (fun c -> if c = '\n' then "\n" else "") body))

    let filterNodesOnRange acceptNode range (tree : Ast.AstNode) =
        let nodeContainsRange node =
            rangeContainsRange ((Ast.GetRange node).Value) range
        let rec findNodesWithRangeInChildren trees foundNodes =
            match trees with
                | [] -> foundNodes
                | tree::ts ->
                    let foundNodes =
                        if acceptNode tree then tree::foundNodes else foundNodes
                    let childrenContainingRange =
                        if Option.isNone (Ast.GetChildren tree) then []
                        else List.filter nodeContainsRange ((Ast.GetChildren tree).Value)
                    findNodesWithRangeInChildren (List.append childrenContainingRange ts) foundNodes

        findNodesWithRangeInChildren [tree] []

    let FindNodesWithRange range (tree : Ast.AstNode) =
        let hasRange node =
            Option.isSome (Ast.GetRange node) && (Ast.GetRange node).Value = range

        filterNodesOnRange hasRange range tree
        
    let rec FindNodesAroundRange range (tree : Ast.AstNode) =
        let treeContainsRange tree =
            Option.isSome (Ast.GetRange tree) && rangeContainsRange (Ast.GetRange tree).Value range

        filterNodesOnRange treeContainsRange range tree

    let TryFindExpressionAtRange range (tree : Ast.AstNode)  =
        let isExpression node =
            match node with
                | Ast.AstNode.Expression _ -> true
                | _ -> false
        let nodes = FindNodesWithRange range tree
        List.tryFind isExpression (FindNodesWithRange range tree)

    let chooseBinding node  =
        match node with
            | Ast.AstNode.Binding b -> Some b
            | _ -> None
                
    let FindBindingAtRange range (tree : Ast.AstNode) =
        List.pick chooseBinding (FindNodesWithRange range tree)

    let TryFindBindingAroundRange range (tree : Ast.AstNode) =
        FindNodesAroundRange range tree
        |> List.tryPick chooseBinding
        
    let TryFindBindingAroundPos pos filename (tree : Ast.AstNode) =
        let range = mkRange filename pos pos
        TryFindBindingAroundRange range tree

    let FindExpressionsAroundRange range (tree : Ast.AstNode) =
        let isExpression node =
            match node with
                | Ast.AstNode.Expression _ -> true
                | _ -> false
        FindNodesAroundRange range tree
        |> List.filter isExpression

    let TryFindIdentifier source filename (position : pos) =
        let containsPos (name,range) =
            // Identifiers' ranges extend past the end of the text
            // so avoid range.End for cases like b in a+b
            rangeContainsPos range position && range.End <> position

        (Ast.Parse source filename).Value
        |> ScopeAnalysis.makeScopeTrees
        |> ScopeAnalysis.ListIdentifiers
        |> List.tryFind containsPos

    let FindIdentifier source filename (position : pos) =
        TryFindIdentifier source filename position
        |> Option.get

    let FindIdentifierName source filename (line, col) =
        let name, _ = FindIdentifier source filename (mkPos line (col-1))
        name
        
    let RangeToTuple (range : range) =
        (range.StartLine, range.StartColumn+1), (range.EndLine, range.EndColumn+1)
        
    let GetEndPosition (source : string) =
        let lines = source.Split([|'\n'|])
        let lineCount = Array.length lines
        lineCount, String.length (lines.[lineCount-1])
