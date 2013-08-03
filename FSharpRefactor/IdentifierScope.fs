namespace FSharpRefactor.Engine
open System
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis

type ExpressionScope (scopeTrees:IdentifierScopeTree list, project:Project) =
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


and IdentifierScope (identifier:Identifier, identifierScope:IdentifierScopeTree, project:Project) =
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

