namespace FSharpRefactor.Engine
open System
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis

type IdentifierScope private (identifier:Identifier, identifierScope:ScopeTree) =    
    new(identifier:Identifier, source) =
        let _, range = identifier
        let scopeTrees = makeScopeTrees (Ast.Parse source range.FileName).Value
        let identifierDeclaration = FindIdentifierDeclaration scopeTrees identifier
        let identifierScope = FindDeclarationScope scopeTrees identifierDeclaration
        IdentifierScope(identifierDeclaration, identifierScope)

    private new(identifiers, trees, isTopLevel) =
        let identifierScope =
            if isTopLevel then TopLevelDeclaration(identifiers, trees)
            else Declaration(identifiers, trees)
        IdentifierScope(List.head identifiers, identifierScope)

    member self.IdentifierDeclaration with get() = identifier
    member self.DeclarationRange with get() = snd identifier
    member self.IdentifierName with get() = fst identifier
    member self.NamesDeclaredInBinding with get() = DeclaredNames identifierScope
    member self.FindReferences () =
        FindDeclarationReferences identifier identifierScope
    member self.IsFree identifierName =
        IsFree identifierName identifierScope
    member self.FindNestedDeclarations identifierName =
        GetShallowestDeclarations identifierName identifierScope
        |> List.map (fun (identifiers, trees, isTopLevel) -> new IdentifierScope(identifiers, trees, isTopLevel))