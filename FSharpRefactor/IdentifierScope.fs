namespace FSharpRefactor.Engine
open System
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis

type ExpressionScope (scopeTrees:IdentifierScopeTree list, project:Project) =
    new(expression:Ast.AstNode, project:Project) =
        ExpressionScope(makeProjectScopeTrees project expression, project)

    member self.IsFree identifierName =
        let hasTargetName (n,_) = n = identifierName            
        self.FindFreeIdentifiers ()
        |> List.exists hasTargetName
    member self.FindFreeIdentifiers () =
        let rec freeIdentifiersInSingleTree foundFree declared tree =
            match tree with
                | Usage((n,r),_) ->
                    if Set.contains n declared then foundFree
                    else (n,r)::foundFree
                | Declaration(is, ts) ->
                    let updatedDeclared = Set.union declared (Set(List.map fst is))
                    List.collect (freeIdentifiersInSingleTree foundFree updatedDeclared) ts
                    
        List.collect (freeIdentifiersInSingleTree [] Set.empty<string>) scopeTrees
    member self.FindNestedDeclarations identifierName =
        let isDeclared (name : string) (identifiers : Identifier list) =
            List.exists (fun (n,_) -> n = name) identifiers
        let rec getShallowestDeclarations targetName tree =
            match tree with
                | Declaration(is, ts) when isDeclared targetName is -> [is, ts]
                | Declaration(is, ts) as declaration ->
                    List.collect (getShallowestDeclarations targetName) ts
                | _ -> []

        List.collect (getShallowestDeclarations identifierName) scopeTrees
        |> List.map (fun (identifiers, trees) -> new IdentifierScope(identifiers, trees, project))
    
    override self.ToString () =
        sprintf "%A" scopeTrees


and IdentifierScope (identifier:Identifier, identifierScope:IdentifierScopeTree, project:Project) =
    inherit ExpressionScope([identifierScope], project)

    new(identifiers, trees, project:Project) =
        let identifierScope =
            Declaration(identifiers, trees)
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
                | Declaration(is,ts) -> List.map fst is
                | _ -> []
    member self.IsDeclaredInBinding identifierName =
        List.exists ((=) identifierName) self.NamesDeclaredInBinding
    member self.FindReferences () =
        let isLocalIdentifier = false
        let referenceRanges =
            if isLocalIdentifier then
                ReferenceFinder.FindDeclarationReferencesInFile identifierScope self.IdentifierDeclaration
            else
                ReferenceFinder.FindDeclarationReferences project self.IdentifierDeclaration
                |> Seq.map snd |> Seq.toList
                
        self.DeclarationRange::referenceRanges