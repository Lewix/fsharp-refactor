module FSharpRefactor.Refactorings.ExtractFunction

open System.Collections.Generic
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms.CodeTransforms
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

let CreateFunction functionName arguments body isMultiLine =
    let transform (source,()) =
        let parametersChange =
            if List.isEmpty arguments then
                FunctionDefinition.ParameterRange isMultiLine, ""
            else
                FunctionDefinition.ParameterRange isMultiLine, " " + (String.concat " " arguments);
        let bodyChange =
            if isMultiLine then
                FunctionDefinition.BodyRange isMultiLine, Indent (stripBrackets body) "    "
            else
                FunctionDefinition.BodyRange isMultiLine, stripBrackets body
        let nameChange = FunctionDefinition.NameRange isMultiLine, functionName

        source, [parametersChange; bodyChange; nameChange], ()

    RunNewRefactoring (refactor { analysis = (fun (_,_) -> Valid); transform = transform } () (FunctionDefinition.Template isMultiLine))
    
let CallFunction functionName arguments =
    //TODO: don't always put brackets around function body
    let transform (source,()) =
        let parameterChange =
            if List.isEmpty arguments then
                FunctionCall.ParameterRange, ""
            else
                FunctionCall.ParameterRange, " " + (String.concat " " arguments)
        source, [(FunctionCall.NameRange, functionName); parameterChange], ()

    RunNewRefactoring (refactor { analysis = (fun (_,_) -> Valid); transform = transform } () FunctionCall.Template)

let CanExtractFunction (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) =
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

let ExtractTempFunction doCheck inScopeTree (expressionRange : range) : NewRefactoring<unit,Identifier> =
    let analysis (source,()) =
        let tree = (Ast.Parse source).Value
        CanExtractFunction tree inScopeTree expressionRange
    let transform (source,()) =
        let tree = (Ast.Parse source).Value
        let functionName = FindUnusedName tree
        let body = TextOfRange source expressionRange
        let bodyExpression = TryFindExpressionAtRange expressionRange inScopeTree
        let arguments =
            GetFreeIdentifiers (makeScopeTrees inScopeTree) DefaultDeclared
            |> Set.difference (GetFreeIdentifiers (makeScopeTrees bodyExpression.Value) DefaultDeclared)
            |> Set.toList

        let inScopeRange = (Ast.GetRange inScopeTree).Value
        let definitionChange =
            if inScopeRange.EndLine = inScopeRange.StartLine then
                let functionDefinition = CreateFunction functionName arguments body false
                (Ast.GetRange inScopeTree).Value.StartRange, functionDefinition
            else
                let unindentedBody = 
                    (String.replicate (expressionRange.StartColumn) " ") + body
                    |> RemoveLeading ' '
                let functionDefinition = CreateFunction functionName arguments unindentedBody true
                let inScopeTreeStart = (Ast.GetRange inScopeTree).Value.StartRange
                let startOfLine = mkRange "test.fs" (mkPos inScopeTreeStart.StartLine 0) inScopeTreeStart.End
                let indentString = String.replicate inScopeTreeStart.StartColumn " "
                startOfLine, Indent functionDefinition indentString
        let callChange =
            expressionRange, CallFunction functionName arguments

        //TODO: function declaration's identifier
        source, [callChange; definitionChange], createIdentifier (1,0) "todo" expressionRange.FileName
    { analysis = analysis; transform = transform }

let ExtractFunction doCheck inScopeTree expressionRange functionName : NewRefactoring<unit,unit> =
    let extractTempRefactoring = ExtractTempFunction doCheck inScopeTree expressionRange
    sequence extractTempRefactoring (Rename doCheck functionName)
    
let DoExtractFunction source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    RunNewRefactoring (refactor (ExtractFunction true inScopeTree expressionRange functionName) () source)
