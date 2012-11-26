namespace FSharpRefactor.Engine.CodeAnalysis

open System
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast

module CodeAnalysis =
    let rec CountDeclarations (tree : Ast.AstNode) =
        match tree with
            | Ast.Pattern(SynPat.Wild _) -> 1
            | Ast.Children c -> List.sum (List.map CountDeclarations c)
            | _ -> 0
    

    let rec CountUsages (tree : Ast.AstNode) =
        match tree with
            | Ast.Expression(SynExpr.LongIdent(_,_,_,_)) -> 1
            | Ast.Expression(SynExpr.Ident(_)) -> 1
            | Ast.Pattern(SynPat.LongIdent(_,_,_,_,_,_)) -> 1
            | Ast.Children c -> List.sum (List.map CountDeclarations c)
            | _ -> 0
                             
