namespace FSharpRefactor.Engine
open System
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis

type IdentifierScope(identifier:Identifier, source) =
    let _, range = identifier
    let scopeTrees = makeScopeTrees (Ast.Parse source range.FileName).Value
    let identifierDeclaration = FindIdentifierDeclaration scopeTrees identifier
    let identifierScope = FindDeclarationScope scopeTrees identifierDeclaration
    
    member self.IsFree identifierName =
        IsFree identifierName identifierScope