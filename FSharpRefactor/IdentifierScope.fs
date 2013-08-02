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
        let rec getShallowestDeclarations targetName tree =
            match tree with
                | TopLevelDeclaration(is, ts) when IsDeclared targetName is -> [is, ts, true]
                | Declaration(is, ts) when IsDeclared targetName is -> [is, ts, false]
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
                | Declaration(is,ts) -> is
                | _ -> []

    member self.FindReferences () =
        FindDeclarationReferences identifier identifierScope

module Scoping =
    let rec TryFindDeclarationScope trees (name, declarationRange) =
        match trees with
            | [] -> None
            | Usage(_,_)::ds -> TryFindDeclarationScope ds (name, declarationRange)
            | (TopLevelDeclaration(is, ts) as d)::ds
            | (Declaration(is, ts) as d)::ds ->
                let isDeclaration = (fun (n,r) -> n = name && rangeContainsRange r declarationRange)
                if List.exists isDeclaration is then Some d
                else TryFindDeclarationScope (List.append ts ds) (name, declarationRange)
    
    let FindDeclarationScope trees declarationIdentifier =
        TryFindDeclarationScope trees declarationIdentifier
        |> Option.get

    let TryGetIdentifierScope (project:Project) (identifier:Identifier) =
        let _, range = identifier
        let scopeTrees = makeScopeTrees (Ast.Parse project.CurrentFileContents range.FileName).Value
        let identifierDeclaration = TryFindIdentifierDeclaration scopeTrees identifier
        let identifierScope =
            Option.bind (TryFindDeclarationScope scopeTrees) identifierDeclaration
        if Option.isSome identifierScope then Some (new IdentifierScope(identifierDeclaration.Value, identifierScope.Value, project))
        else None
        
    let GetIdentifierScope (project:Project) (identifier:Identifier) =
        TryGetIdentifierScope project identifier
        |> Option.get