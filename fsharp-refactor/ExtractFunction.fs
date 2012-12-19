module FSharpRefactor.Refactorings.ExtractFunction

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow

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
    //TODO: extract template and ranges
    let recRange = mkRange "/home/lewis/test.fs" (mkPos 1 3) (mkPos 1 3)
    let nameRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 16)
    let parameterRange = mkRange "/home/lewis/test.fs" (mkPos 1 17) (mkPos 1 27)
    let bodyRange = mkRange "/home/lewis/test.fs" (mkPos 1 30) (mkPos 1 34)

    refactoring "let functionName parameters = body in " {
        if isRecursive then yield (recRange, " rec")
        yield (nameRange, functionName)
        yield (parameterRange, String.concat " " arguments)
        yield (bodyRange, stripBrackets body)
    }
    
let CallFunction source (functionName : string) (arguments : string list) =
    let argumentString = String.concat " " arguments
    //TODO: don't always put brackets around function body
    "(" + functionName + " " + argumentString + ")"

let CanExtractFunction (tree : Ast.AstNode) (expressionRange : range) (functionName : string) = true

let DoExtractFunction source (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    refactoring source {
        let body = CodeTransforms.TextOfRange source expressionRange
        let bodyExpression = findExpressionAtRange expressionRange inScopeTree
        let arguments =
            GetFreeIdentifiers (makeScopeTrees inScopeTree) DefaultDeclared
            |> Set.difference (GetFreeIdentifiers (makeScopeTrees bodyExpression) DefaultDeclared)
            |> Set.toList

        yield ((Ast.GetRange inScopeTree).Value.StartRange, CreateFunction source inScopeTree functionName arguments body false)
        yield (expressionRange, CallFunction source functionName arguments)
    }
