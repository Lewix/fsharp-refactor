module FSharpRefactor.Refactorings.Rename

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast

type ScopeTree =
    | Declaration of string * range * ScopeTree list
    | Usage of string * range
    | Empty

// Returns a scope tree rooted at the first declaration in tree or empty if there are none
let makeScopeTree (tree : Ast.AstNode) = Empty
