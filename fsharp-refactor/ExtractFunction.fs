module FSharpRefactor.Refactorings.ExtractFunction

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms

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

let CreateFunction source (inScopeTree : Ast.AstNode) (functionName : string) (arguments : string list) (body : string) (isRecursive : bool) =
    let letString = if isRecursive then "let rec" else "let"
    let functionStrings = List.concat (seq [[letString; functionName]; arguments; ["="; body; "in "]])
    let functionString = String.concat " " functionStrings
    let range = (Ast.GetRange inScopeTree).Value.StartRange

    CodeTransforms.ChangeTextOf source [range,functionString]
    

let CanExtractFunction (tree : Ast.AstNode) (expressionRange : range) (functionName : string) = true

let DoExtractFunction source (tree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    source
