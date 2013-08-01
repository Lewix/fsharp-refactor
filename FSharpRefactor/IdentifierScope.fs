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
        List.foldBack (IsFree identifierName >> (||)) scopeTrees false
    member self.FindFreeIdentifiers () =
        GetFreeIdentifierUsages scopeTrees
    member self.FindNestedDeclarations identifierName =
        List.collect (GetShallowestDeclarations identifierName) scopeTrees
        |> List.map (fun (identifiers, trees, isTopLevel) -> new IdentifierScope(identifiers, trees, isTopLevel, project))
    
    override self.ToString () =
        sprintf "%A" scopeTrees

and IdentifierScope (identifier:Identifier, identifierScope:ScopeTree, project:Project) =
    inherit ExpressionScope([identifierScope], project)

    new(identifier:Identifier, project:Project) =
        let _, range = identifier
        let scopeTrees = makeScopeTrees (Ast.Parse project.CurrentFileContents range.FileName).Value
        let identifierDeclaration = FindIdentifierDeclaration scopeTrees identifier
        let identifierScope = FindDeclarationScope scopeTrees identifierDeclaration
        IdentifierScope(identifierDeclaration, identifierScope, project)

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
    member self.NamesDeclaredInBinding with get() = DeclaredNames identifierScope
    member self.FindReferences () =
        FindDeclarationReferences identifier identifierScope

module Scoping =
    let TryGetIdentifierScope (source:Project) (identifier:Identifier) =
        let _, range = identifier
        let scopeTrees = makeScopeTrees (Ast.Parse source.CurrentFileContents range.FileName).Value
        let identifierDeclaration = TryFindIdentifierDeclaration scopeTrees identifier
        let identifierScope =
            Option.bind (TryFindDeclarationScope scopeTrees) identifierDeclaration
        if Option.isSome identifierScope then Some (new IdentifierScope(identifierDeclaration.Value, identifierScope.Value, source))
        else None