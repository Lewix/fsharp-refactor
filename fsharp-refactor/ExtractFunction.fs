module FSharpRefactor.Refactorings.ExtractFunction

open Microsoft.FSharp.Compiler.Range

let CanExtractFunction (tree : Ast.AstNode) (expressionRange : range) (functionName : string) = true

let DoExtractFunction source (tree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    source
