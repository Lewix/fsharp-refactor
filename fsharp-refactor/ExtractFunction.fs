module FSharpRefactor.Refactorings.ExtractFunction

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast

let rec findNodesWithRange range (tree : Ast.AstNode) =
    let nodeRange = Ast.GetRange(tree)
    let remainingRanges =
        match tree with
            | Ast.Children cs -> List.concat (Seq.map (findNodesWithRange range) cs)
            | _ -> []
    if Option.isSome nodeRange && range = nodeRange.Value
    then tree::remainingRanges else remainingRanges


let findExpressionAtRange range (tree : Ast.AstNode)  =
    let nodesWithRange = findNodesWithRange range tree
    let isExpression node =
        match node with
            | Ast.AstNode.Expression _ -> true
            | _ -> false
    List.find isExpression nodesWithRange

let CanExtractFunction (tree : Ast.AstNode) (expressionRange : range) (functionName : string) = true

let DoExtractFunction source (tree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    source
