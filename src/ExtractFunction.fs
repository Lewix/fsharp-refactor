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

let CreateFunction functionName arguments body isMultiLine indentString (declarationRange : range) : Refactoring<unit,Identifier> =
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
    let declarationSource =
        RunRefactoring { analysis = (fun (_,_) -> Valid); transform = transform } () (FunctionDefinition.Template isMultiLine)

    let transform (source,()) =
        let nameRange =
            mkRange "test.fs" (mkPos 1 4) (mkPos 1 16)
        let startColumn, startLine =
            declarationRange.StartLine, declarationRange.StartColumn+(String.length indentString)+nameRange.StartColumn
        let endLine =
            startLine + (String.length functionName)
        let identifierRange =
            mkRange declarationRange.FileName (mkPos startColumn startLine) (mkPos startColumn endLine)
        source, [declarationRange, Indent declarationSource indentString], (functionName, identifierRange)
    { analysis = (fun (_,_) -> Valid); transform = transform }
 
let CallFunction functionName arguments callRange : Refactoring<unit,unit> =
    //TODO: don't always put brackets around function body
    //TODO: this is contrived, just get rid of the templates...
    let transform (source,()) =
        let parameterChange =
            if List.isEmpty arguments then
                FunctionCall.ParameterRange, ""
            else
                FunctionCall.ParameterRange, " " + (String.concat " " arguments)
        source, [(FunctionCall.NameRange, functionName); parameterChange], ()

    let callSource =
        RunRefactoring { analysis = (fun (_,_) -> Valid); transform = transform } () FunctionCall.Template

    { analysis = (fun (_,_) -> Valid); transform = (fun (s,_) -> s, [callRange, callSource], ()) }

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

let ExtractTempFunction doCheck inScopeTree (expressionRange : range) : Refactoring<unit,Identifier> =
    let analysis (source,()) =
        let tree = (Ast.Parse source).Value
        CanExtractFunction tree inScopeTree expressionRange
    let transform (source,()) =
        let tree = (Ast.Parse source).Value
        let functionName = FindUnusedName tree
        let unindentedBody = 
            (String.replicate (expressionRange.StartColumn) " ") + (TextOfRange source expressionRange)
            |> RemoveLeading ' '
        let bodyExpression = TryFindExpressionAtRange expressionRange inScopeTree
        let arguments =
            GetFreeIdentifiers (makeScopeTrees inScopeTree) DefaultDeclared
            |> Set.difference (GetFreeIdentifiers (makeScopeTrees bodyExpression.Value) DefaultDeclared)
            |> Set.toList

        let inScopeRange = (Ast.GetRange inScopeTree).Value
        let definitionRefactoring =
            if inScopeRange.StartLine = inScopeRange.EndLine then
                CreateFunction functionName arguments unindentedBody false "" inScopeRange.StartRange
            else
                let inScopeTreeStart = inScopeRange.StartRange
                let startOfLine = mkRange "test.fs" (mkPos inScopeTreeStart.StartLine 0) inScopeTreeStart.End
                let indentString = String.replicate inScopeTreeStart.StartColumn " "
                CreateFunction functionName arguments unindentedBody true indentString startOfLine
        let callRefactoring =
            CallFunction functionName arguments expressionRange

        (interleave definitionRefactoring callRefactoring).transform (source, ())

    { analysis = analysis; transform = transform }

let ExtractFunction doCheck inScopeTree expressionRange functionName : Refactoring<unit,unit> =
    let extractTempRefactoring = ExtractTempFunction doCheck inScopeTree expressionRange
    sequence extractTempRefactoring (Rename doCheck functionName)
    
let DoExtractFunction source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
    RunRefactoring (ExtractFunction true inScopeTree expressionRange functionName) () source
