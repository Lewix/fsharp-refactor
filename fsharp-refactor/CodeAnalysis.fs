namespace FSharpRefactor.Engine.CodeAnalysis

open System
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast

module CodeAnalysis =
    // Count the Wild nodes
    let rec CountDeclarations (tree : Ast.AstNode) =
        match tree with
            | Ast.Pattern(SynPat.Wild r) -> 1
            | Ast.Children c -> List.sum (List.map CountDeclarations c)
            | _ -> 0
    
