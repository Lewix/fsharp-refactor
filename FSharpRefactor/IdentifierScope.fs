namespace FSharpRefactor.Engine
open System
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis

type ExpressionScope (scopeTrees:ScopeTree list, project:Project) =
    new(expression:Ast.AstNode, project:Project) =
        ExpressionScope(makeScopeTrees expression, project)

    member self.IsFree identifierName =
        let hasTargetName (n,_) = n = identifierName            
        self.FindFreeIdentifiers ()
        |> List.exists hasTargetName
    member self.FindFreeIdentifiers () =
        let rec freeIdentifiersInSingleTree foundFree declared tree =
            match tree with
                | Usage(n,r) ->
                    if Set.contains n declared then foundFree
                    else (n,r)::foundFree
                | TopLevelDeclaration(is, ts)
                | Declaration(is, ts) ->
                    let updatedDeclared = Set.union declared (Set(List.map fst is))
                    List.collect (freeIdentifiersInSingleTree foundFree updatedDeclared) ts
                    
        List.collect (freeIdentifiersInSingleTree [] Set.empty<string>) scopeTrees
    member self.FindNestedDeclarations identifierName =
        let isDeclared (name : string) (identifiers : Identifier list) =
            List.exists (fun (n,_) -> n = name) identifiers
        let rec getShallowestDeclarations targetName tree =
            match tree with
                | TopLevelDeclaration(is, ts) when isDeclared targetName is -> [is, ts, true]
                | Declaration(is, ts) when isDeclared targetName is -> [is, ts, false]
                | TopLevelDeclaration(is, ts) 
                | Declaration(is, ts) as declaration ->
                    List.collect (getShallowestDeclarations targetName) ts
                | _ -> []

        List.collect (getShallowestDeclarations identifierName) scopeTrees
        |> List.map (fun (identifiers, trees, isTopLevel) -> new IdentifierScope(identifiers, trees, isTopLevel, project))
    
    override self.ToString () =
        sprintf "%A" scopeTrees


and IdentifierScope (identifier:Identifier, identifierScope:ScopeTree, project:Project) =
    inherit ExpressionScope([identifierScope], project)

    new(identifiers, trees, isTopLevel, project:Project) =
        let identifierScope =
            if isTopLevel then TopLevelDeclaration(identifiers, trees)
            else Declaration(identifiers, trees)
        IdentifierScope(List.head identifiers, identifierScope, project)

    override self.Equals(other) =
        match other with
            | :? IdentifierScope ->
                (other :?> IdentifierScope).IdentifierDeclaration = self.IdentifierDeclaration
            | _ -> false

    override self.GetHashCode () =
        self.IdentifierDeclaration.GetHashCode ()

    member self.IdentifierDeclaration with get() = identifier
    member self.DeclarationRange with get() = snd identifier
    member self.IdentifierName with get() = fst identifier
    member self.NamesDeclaredInBinding
        with get() =
            match identifierScope with
                | TopLevelDeclaration(is,ts)
                | Declaration(is,ts) -> List.map fst is
                | _ -> []
    member self.IsDeclaredInBinding identifierName =
        List.exists ((=) identifierName) self.NamesDeclaredInBinding
    member self.FindReferences () =
        let rangeOfIdent (name : string) (identifiers : Identifier list) =
            let identifier = List.tryFind (fun (n,_) -> n = name) identifiers
            if Option.isNone identifier then None else Some(snd identifier.Value)
        let isNestedDeclaration idents =
            List.exists (fun (n,r) -> n = self.IdentifierName && not (rangeContainsRange r self.DeclarationRange)) idents
        
        let rec findReferencesInTree tree =
            match tree with
                | Usage(n, r) when n = self.IdentifierName -> [r]
                | TopLevelDeclaration(is, ts)
                | Declaration(is, ts) when not (isNestedDeclaration is) ->
                    let remainingRanges = List.concat (Seq.map findReferencesInTree ts)
                    let declarationRange = rangeOfIdent self.IdentifierName is
                    if Option.isSome declarationRange then declarationRange.Value::remainingRanges
                    else remainingRanges
                | _ -> []
        findReferencesInTree identifierScope


module Scoping =
    let tryFindIdentifierDeclaration (trees : ScopeTree list) ((name, range) : Identifier) =
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

    let rec tryFindDeclarationScope trees (name, declarationRange) =
        match trees with
            | [] -> None
            | Usage(_,_)::ds -> tryFindDeclarationScope ds (name, declarationRange)
            | (TopLevelDeclaration(is, ts) as d)::ds
            | (Declaration(is, ts) as d)::ds ->
                let isDeclaration = (fun (n,r) -> n = name && rangeContainsRange r declarationRange)
                if List.exists isDeclaration is then Some d
                else tryFindDeclarationScope (List.append ts ds) (name, declarationRange)
                
    let getDeclarations (trees : ScopeTree list) =
        let rec declarationsInSingleTree tree =
            match tree with
                | Usage(n,_) -> Set []
                | TopLevelDeclaration(is, ts)
                | Declaration(is, ts) ->
                    let declarationsInChildren =
                        Set.unionMany (Seq.map declarationsInSingleTree ts)
                    Set.union declarationsInChildren (Set(List.map fst is))

        Set.unionMany (Seq.map declarationsInSingleTree trees)

    let FindUnusedName (tree : Ast.AstNode) =
        let scopeTrees = makeScopeTrees tree
        let usedNames = getDeclarations scopeTrees
        let randomNumberGenerator = new System.Random()

        let rec generateWhileUsed () =
            let name = "tmpFunction" + string (randomNumberGenerator.Next())
            if Set.contains name usedNames then generateWhileUsed ()
            else name

        generateWhileUsed ()

    let TryGetIdentifierScope (project:Project) (identifier:Identifier) =
        let _, range = identifier
        let scopeTrees = makeScopeTrees (Ast.Parse project.CurrentFileContents range.FileName).Value
        let identifierDeclaration = tryFindIdentifierDeclaration scopeTrees identifier
        let identifierScope =
            Option.bind (tryFindDeclarationScope scopeTrees) identifierDeclaration
        if Option.isSome identifierScope then Some (new IdentifierScope(identifierDeclaration.Value, identifierScope.Value, project))
        else None
        
    let GetIdentifierScope (project:Project) (identifier:Identifier) =
        TryGetIdentifierScope project identifier
        |> Option.get