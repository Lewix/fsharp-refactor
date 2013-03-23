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

let DefaultInScopeTree (tree : Ast.AstNode) (expressionRange : range) =
    let outermostBinding = TryFindBindingAroundRange expressionRange tree
    let outermostExpression = TryFindExpressionAroundRange expressionRange tree
    if Option.isSome outermostBinding then
        match outermostBinding.Value with
            | SynBinding.Binding(_,_,_,_,_,_,_,_,_,expression,_,_) -> Some(expression)
    else outermostExpression

let CreateFunction (functionName : string) (arguments : string list) (body : string) (isMultiLine : bool) =
    RunRefactoring (refactoring (FunctionDefinition.Template isMultiLine) Valid {
        yield (FunctionDefinition.NameRange isMultiLine, functionName)
        
        if List.isEmpty arguments then
            yield (FunctionDefinition.ParameterRange isMultiLine, "")
        else
            yield (FunctionDefinition.ParameterRange isMultiLine, " " + (String.concat " " arguments))

        if isMultiLine then
            yield (FunctionDefinition.BodyRange isMultiLine, CodeTransforms.Indent (stripBrackets body) "    ")
        else
            yield (FunctionDefinition.BodyRange isMultiLine, stripBrackets body)
    })
    
let CallFunction (functionName : string) (arguments : string list) =
    //TODO: don't always put brackets around function body
    RunRefactoring (refactoring FunctionCall.Template Valid {
        yield (FunctionCall.NameRange, functionName)
        if List.isEmpty arguments then yield (FunctionCall.ParameterRange, "")
        else yield (FunctionCall.ParameterRange, " " + (String.concat " " arguments))
    })

let CanExtractFunction (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
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

let ExtractTempFunction doCheck source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    let valid =
        if doCheck then CanExtractFunction tree inScopeTree expressionRange functionName else Valid
    refactoring source valid {
        let body = CodeTransforms.TextOfRange source expressionRange
        let bodyExpression = TryFindExpressionAtRange expressionRange inScopeTree
        let arguments =
            GetFreeIdentifiers (makeScopeTrees inScopeTree) DefaultDeclared
            |> Set.difference (GetFreeIdentifiers (makeScopeTrees bodyExpression.Value) DefaultDeclared)
            |> Set.toList

        let inScopeRange = (Ast.GetRange inScopeTree).Value
        if inScopeRange.EndLine = inScopeRange.StartLine then
            let functionDefinition = CreateFunction functionName arguments body false
            yield ((Ast.GetRange inScopeTree).Value.StartRange, functionDefinition)
        else
            let unindentedBody = 
                (String.replicate (expressionRange.StartColumn) " ") + body
                |> CodeTransforms.RemoveLeading ' '
            let functionDefinition = CreateFunction functionName arguments unindentedBody true
            let inScopeTreeStart = (Ast.GetRange inScopeTree).Value.StartRange
            let startOfLine = mkRange "test.fs" (mkPos inScopeTreeStart.StartLine 0) inScopeTreeStart.End
            let indentString = String.replicate inScopeTreeStart.StartColumn " "
            yield (startOfLine, CodeTransforms.Indent functionDefinition indentString)

        yield (expressionRange, CallFunction functionName arguments)
    }

let ExtractFunction doCheck source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    let unusedName = FindUnusedName tree
    let sourceWithTempFunction = RunRefactoring (ExtractTempFunction doCheck source tree inScopeTree expressionRange unusedName)
    let tree = (Ast.Parse sourceWithTempFunction).Value
    let identifier = (TryFindIdentifierWithName (makeScopeTrees tree) unusedName).Value
    refactor (Rename doCheck identifier functionName) sourceWithTempFunction 
    

let DoExtractFunction source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    RunNewRefactoring (ExtractFunction true source tree inScopeTree expressionRange functionName)
