module FSharpRefactor.Refactorings.ExtractFunction

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis

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

let rec stripBrackets (body : string) =
    if body.[0] = '(' && body.[(String.length body)-1] = ')'
    then stripBrackets (body.[1..(String.length body)-2])
    else body

//TODO: make a workflow for code transformations with ChangeTextOf (do all the transformations at the end)
let CreateFunction source (inScopeTree : Ast.AstNode) (functionName : string) (arguments : string list) (body : string) (isRecursive : bool) =
    let letString = if isRecursive then "let rec" else "let"
    let functionStrings = List.concat (seq [[letString; functionName]; arguments; ["="; stripBrackets body; "in "]])
    let functionString = String.concat " " functionStrings
    let range = (Ast.GetRange inScopeTree).Value.StartRange

    [range,functionString]
    
let CallFunction source (functionName : string) (arguments : string list) (range : range) =
    let argumentString = String.concat " " arguments
    //TODO: don't always put brackets around function body
    let callString = "(" + functionName + " " + argumentString + ")"
    
    [range,callString]

let CanExtractFunction (tree : Ast.AstNode) (expressionRange : range) (functionName : string) = true

let DoExtractFunction source (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    let body = CodeTransforms.TextOfRange source expressionRange
    let bodyExpression = findExpressionAtRange expressionRange inScopeTree
    let arguments =
        GetFreeIdentifiers (makeScopeTrees inScopeTree) DefaultDeclared
        |> Set.difference (GetFreeIdentifiers (makeScopeTrees bodyExpression) DefaultDeclared)
        |> Set.toList

    CreateFunction source inScopeTree functionName arguments body false
    |> List.append (CallFunction source functionName arguments expressionRange)
    |> CodeTransforms.ChangeTextOf source
