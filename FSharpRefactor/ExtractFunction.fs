module FSharpRefactor.Refactorings.ExtractFunction

open System.Collections.Generic
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms.CodeTransforms
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Engine.ValidityChecking
open FSharpRefactor.Refactorings

let rec stripBrackets (body : string) =
    if body.[0] = '(' && body.[(String.length body)-1] = ')'
    then stripBrackets (body.[1..(String.length body)-2])
    else body

let formatArguments functionName arguments =
    List.map (fun s -> " " + s) arguments
    |> List.fold (+) functionName

let defaultInScopeTree (tree : Ast.AstNode) (expressionRange : range) =
    let chooseOutermost node1 node2 =
        if rangeContainsRange (Ast.GetRange node1).Value (Ast.GetRange node2).Value
        then node1 else node2
    let outermostBinding = TryFindBindingAroundRange expressionRange tree
    let expressionsAroundRange = FindExpressionsAroundRange expressionRange tree
    let outermostExpression =
        if List.isEmpty expressionsAroundRange then None
        else Some (List.reduce chooseOutermost expressionsAroundRange)
    if Option.isSome outermostBinding then
        match outermostBinding.Value with
            | SynBinding.Binding(_,_,_,_,_,_,_,_,_,expression,_,_) ->
                Some(Ast.AstNode.Expression expression)
    else outermostExpression

let createFunction functionName arguments body isMultiLine indentString (declarationRange : range) : Refactoring<unit,Identifier> =
    let declarationSource =
        let parameters =
            formatArguments functionName arguments
        let body =
            if isMultiLine then
                Indent (stripBrackets body) "    "
            else
                sprintf "(%s)" body
        if isMultiLine then
            sprintf "let %s =\n%s\n" parameters body
        else
            sprintf "let %s = %s in " parameters body

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
    { analysis = (fun _ -> true); transform = transform; getErrorMessage = fun _ -> None }
 
let callFunction functionName arguments callRange : Refactoring<unit,unit> =
    //TODO: don't always put brackets around function body
    let transform (source, ()) =
        let functionCall =
            formatArguments functionName arguments
        if List.isEmpty arguments then
            source, [callRange, functionCall], ()
        else
            source, [callRange, sprintf "(%s)" functionCall], ()
            
    { analysis = (fun _ -> true); transform = transform; getErrorMessage = fun _ -> None }

let extractTempFunctionTransform source (expressionRange : range) inScopeTree =
        let tree = (Ast.Parse source).Value
        let functionName = FindUnusedName tree
        let unindentedBody = 
            (String.replicate (expressionRange.StartColumn) " ") + (TextOfRange source expressionRange)
            |> RemoveLeading ' '
        let bodyExpression = TryFindExpressionAtRange expressionRange inScopeTree

        let getFreeIdentifierDeclarations expression =
            GetFreeIdentifierUsages (makeScopeTrees expression)
            |> List.map (TryFindIdentifierDeclaration (makeScopeTrees inScopeTree))
            |> List.collect Option.toList
            // Can't compare ranges or positions, so use a tuple of ints...
            |> List.map (fun (n,r) -> (n, (r.StartLine, r.StartColumn))) 
            |> Set.ofList

        let arguments =
            getFreeIdentifierDeclarations inScopeTree
            |> Set.difference (getFreeIdentifierDeclarations bodyExpression.Value)
            |> Set.toList
            |> List.map fst

        let inScopeRange = (Ast.GetRange inScopeTree).Value
        let definitionRefactoring =
            if inScopeRange.StartLine = inScopeRange.EndLine then
                createFunction functionName arguments unindentedBody false "" inScopeRange.StartRange
            else
                let inScopeTreeStart = inScopeRange.StartRange
                let startOfLine = mkRange "test.fs" (mkPos inScopeTreeStart.StartLine 0) inScopeTreeStart.End
                let indentString = String.replicate inScopeTreeStart.StartColumn " "
                createFunction functionName arguments unindentedBody true indentString startOfLine
        let callRefactoring =
            callFunction functionName arguments expressionRange

        (interleave definitionRefactoring callRefactoring).transform (source, ())


let extractTempFunction inScopeTree (expressionRange : range) : Refactoring<unit,Identifier> =
    let transform (source,()) =
        extractTempFunctionTransform source expressionRange inScopeTree
    { analysis = (fun _ -> true); transform = transform; getErrorMessage = fun _ -> None }


let GetErrorMessage (range:((int*int)*(int*int)) option, functionName:string option) (source:string) (filename:string) =
    let tree = (Ast.Parse source).Value

    let checkRange ((startLine, startCol), (endLine, endCol)) =
        let range = mkRange "test.fs" (mkPos startLine (startCol-1)) (mkPos endLine (endCol-1))
        let expressionAtRange = Option.isSome (TryFindExpressionAtRange range tree)
        let expressionIsNotInfix =
            match TryFindExpressionAtRange range tree with
                | Some(Ast.AstNode.Expression(SynExpr.App(_,true,_,_,_))) -> false
                | _ -> true
                
        match expressionAtRange, expressionIsNotInfix with
            | false,_ -> Some "No expression found at the given range"
            | _,false -> Some "The expression is a partial application of an infix function"
            | _ -> None


    let checkRangeAndName (((startLine, startCol), (endLine, endCol)), name) =
        let range = mkRange "test.fs" (mkPos startLine (startCol-1)) (mkPos endLine (endCol-1))
        let inScopeTree = defaultInScopeTree tree range
        let oldSource, changes, (_, identifierRange) =
            extractTempFunctionTransform source range inScopeTree.Value
        let sourceWithIdentifier = ChangeTextOf oldSource changes
        Rename.GetErrorMessage (Some (identifierRange.Start.Line, identifierRange.Start.Column+1), Some name)
                               sourceWithIdentifier "test.fs"

    IsSuccessful checkRange range
    |> Andalso (IsSuccessful checkRangeAndName (PairOptions (range, functionName)))
    |> fun (l:Lazy<_>) -> l.Force()

let IsValid (range:((int*int)*(int*int)) option, functionName:string option) (source:string) (filename:string) =
    Option.isNone (GetErrorMessage (range, functionName) source filename)

let ExtractFunction inScopeTree (expressionRange : range) functionName : Refactoring<unit,unit> =
    let analysis (source,()) =
        IsValid (Some ((expressionRange.StartLine, expressionRange.StartColumn+1), 
                       (expressionRange.EndLine, expressionRange.EndColumn+1)),
                 Some functionName)
                source "test.fs"
    let getErrorMessage (source,()) =
        GetErrorMessage (Some ((expressionRange.StartLine, expressionRange.StartColumn+1),
                               (expressionRange.EndLine, expressionRange.EndColumn+1)),
                         Some functionName)
                        source "test.fs"
    let extractTempRefactoring = extractTempFunction inScopeTree expressionRange
    let extractFunctionRefactoring = sequence extractTempRefactoring (Rename.Rename functionName)
    { extractFunctionRefactoring with analysis = analysis; getErrorMessage = getErrorMessage }

let Transform (((startLine, startColumn), (endLine, endColumn)), functionName) source filename =
    let tree = (Ast.Parse source).Value
    let expressionRange = mkRange "test.fs" (mkPos startLine (startColumn-1)) (mkPos endLine (endColumn-1))
    let inScopeTree = (defaultInScopeTree tree expressionRange).Value
    RunRefactoring (ExtractFunction inScopeTree expressionRange functionName) () source