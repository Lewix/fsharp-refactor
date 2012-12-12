module FSharpRefactor.Refactorings.ExtractFunction

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast

let findExpressionAtRange (tree : Ast.AstNode) range = tree

let CanExtractFunction (tree : Ast.AstNode) (expressionRange : range) (functionName : string) = true

let DoExtractFunction source (tree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    source
