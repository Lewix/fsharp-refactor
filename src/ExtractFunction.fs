module FSharpRefactor.Refactorings.ExtractFunction

open System.Collections.Generic
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow
open FSharpRefactor.Engine.CodeTemplates
open FSharpRefactor.Refactorings.Rename

let rec stripBrackets (body : string) =
    if body.[0] = '(' && body.[(String.length body)-1] = ')'
    then stripBrackets (body.[1..(String.length body)-2])
    else body

let DefaultInScopeTree source (tree : Ast.AstNode) (expressionRange : range) =
    let outermostBinding = TryFindBindingAroundRange expressionRange tree
    let outermostExpression = TryFindExpressionAroundRange expressionRange tree
    if Option.isSome outermostBinding then
        match outermostBinding.Value with
            | SynBinding.Binding(_,_,_,_,_,_,_,_,_,expression,_,_) -> Some(expression)
    else outermostExpression

let CreateFunction source (inScopeTree : Ast.AstNode) (functionName : string) (arguments : string list) (body : string) (isRecursive : bool) =
    RunRefactoring (refactoring FunctionDefinition.Template Valid {
        if isRecursive then yield (FunctionDefinition.RecRange, FunctionDefinition.RecTemplate)
        yield (FunctionDefinition.NameRange, functionName)
        if List.isEmpty arguments then yield (FunctionDefinition.ParameterRange, "")
        else yield (FunctionDefinition.ParameterRange, " " + (String.concat " " arguments))
        yield (FunctionDefinition.BodyRange, stripBrackets body)
    })
    
let CallFunction source (functionName : string) (arguments : string list) =
    //TODO: don't always put brackets around function body
    RunRefactoring (refactoring FunctionCall.Template Valid {
        yield (FunctionCall.NameRange, functionName)
        if List.isEmpty arguments then yield (FunctionCall.ParameterRange, "")
        else yield (FunctionCall.ParameterRange, " " + (String.concat " " arguments))
    })

let CanExtractFunction source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    let expressionRangeIsInInScopeTree =
        if rangeContainsRange (Ast.GetRange inScopeTree).Value expressionRange then Valid
        else Invalid("The expression is not contained within the specified scope")
    let expressionRangeIsValid =
        if Option.isSome (TryFindExpressionAtRange expressionRange tree) then Valid
        else Invalid("No expression found at the given range") 
    let expressionIsInfix =
        match TryFindExpressionAtRange expressionRange tree with
            | Some(Ast.AstNode.Expression(SynExpr.App(_,true,_,_,_))) -> Invalid("The expression is a partial application of an infix function")
            | _ -> Valid
    List.reduce CombineValidity
                [expressionRangeIsValid; expressionRangeIsInInScopeTree; expressionIsInfix]

let ExtractTempFunction source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    let valid = CanExtractFunction source tree inScopeTree expressionRange functionName
    refactoring source valid {
        let body = CodeTransforms.TextOfRange source expressionRange
        let bodyExpression = TryFindExpressionAtRange expressionRange inScopeTree
        let arguments =
            GetFreeIdentifiers (makeScopeTrees inScopeTree) DefaultDeclared
            |> Set.difference (GetFreeIdentifiers (makeScopeTrees bodyExpression.Value) DefaultDeclared)
            |> Set.toList

        yield ((Ast.GetRange inScopeTree).Value.StartRange, CreateFunction source inScopeTree functionName arguments body false)
        yield (expressionRange, CallFunction source functionName arguments)
    }

let ExtractFunction source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    let unusedName = FindUnusedName tree
    let sourceWithTempFunction = RunRefactoring (ExtractTempFunction source tree inScopeTree expressionRange unusedName)
    let tree = (Ast.Parse sourceWithTempFunction).Value
    let identifier = (TryFindIdentifierWithName (makeScopeTrees tree) unusedName).Value
    Rename sourceWithTempFunction tree identifier functionName
    

let DoExtractFunction source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    RunRefactoring (ExtractFunction source tree inScopeTree expressionRange functionName)
