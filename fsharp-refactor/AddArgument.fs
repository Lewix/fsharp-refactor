module FSharpRefactor.Refactorings.AddArgument

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast

let AddArgumentToBinding source (tree : Ast.AstNode) (bindingRange : range) (argumentName : string) = source

let AddArgument source (bindingRange : range) (argumentName : string) (defaultValue : string) = source
