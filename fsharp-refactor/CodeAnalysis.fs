namespace FSharpRefactor.Engine.CodeAnalysis

open System
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast

module CodeAnalysis =
    let rec CountDeclarations (tree : Ast.AstNode) =
        match tree with
            | Ast.Pattern(SynPat.Wild r) -> 1
            | Ast.Children c -> List.sum (List.map CountDeclarations c)
            | _ -> 0
    

    let rec CountUsages (tree : Ast.AstNode) =
        0
