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
open FSharpRefactor.Engine.Scoping
open FSharpRefactor.Engine
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
        let startColumn, startLine =
            declarationRange.StartLine, declarationRange.StartColumn+(String.length indentString)+4
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

let extractTempFunctionTransform source (expressionRange : range) inScopeTree filename =
        let tree = (Ast.Parse source filename).Value
        let functionName = FindUnusedName tree
        let unindentedBody = 
            (String.replicate (expressionRange.StartColumn) " ") + (TextOfRange source expressionRange)
            |> RemoveLeading ' '
        let bodyExpressionScope = ExpressionScope(FindExpressionAtRange expressionRange inScopeTree)
        let debug = makeScopeTrees inScopeTree
        let inScopeScope = ExpressionScope(inScopeTree)

        let getFreeIdentifierDeclarations (expressionScope:ExpressionScope) =
            expressionScope.FindFreeIdentifiers ()
            |> List.map (TryGetIdentifierScope source)
            |> List.collect Option.toList

        let arguments =
            let freeIdentifiersInScope = getFreeIdentifierDeclarations inScopeScope
            getFreeIdentifierDeclarations bodyExpressionScope
            |> List.filter (fun id -> not (List.exists ((=) id) freeIdentifiersInScope))
            |> List.map (fun id -> id.IdentifierName)
            |> Set.ofList |> Set.toList

        let inScopeRange = (Ast.GetRange inScopeTree).Value
        let definitionRefactoring =
            if inScopeRange.StartLine = inScopeRange.EndLine then
                createFunction functionName arguments unindentedBody false "" inScopeRange.StartRange
            else
                let inScopeTreeStart = inScopeRange.StartRange
                let startOfLine = mkRange filename (mkPos inScopeTreeStart.StartLine 0) inScopeTreeStart.End
                let indentString = String.replicate inScopeTreeStart.StartColumn " "
                createFunction functionName arguments unindentedBody true indentString startOfLine
        let callRefactoring =
            callFunction functionName arguments expressionRange

        (interleave definitionRefactoring callRefactoring).transform (source, ())


let extractTempFunction inScopeTree (expressionRange : range) filename : Refactoring<unit,Identifier> =
    let transform (source,()) =
        extractTempFunctionTransform source expressionRange inScopeTree filename
    { analysis = (fun _ -> true); transform = transform; getErrorMessage = fun _ -> None }


let GetErrorMessage (range:((int*int)*(int*int)) option, functionName:string option) (source:string) (filename:string) =
    let tree = (Ast.Parse source filename).Value

    let checkRange ((startLine, startCol), (endLine, endCol)) =
        let range = mkRange filename (mkPos startLine (startCol-1)) (mkPos endLine (endCol-1))
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
        let range = mkRange filename (mkPos startLine (startCol-1)) (mkPos endLine (endCol-1))
        let inScopeTree = defaultInScopeTree tree range
        let oldSource, changes, (_, identifierRange) =
            extractTempFunctionTransform source range inScopeTree.Value filename
        let sourceWithIdentifier = ChangeTextOf oldSource changes
        Rename.GetErrorMessage (Some (identifierRange.Start.Line, identifierRange.Start.Column+1), Some name)
                               sourceWithIdentifier filename

    IsSuccessful checkRange range
    |> Andalso (IsSuccessful checkRangeAndName (PairOptions (range, functionName)))
    |> fun (l:Lazy<_>) -> l.Force()

let IsValid (range:((int*int)*(int*int)) option, functionName:string option) (source:string) (filename:string) =
    Option.isNone (GetErrorMessage (range, functionName) source filename)

let ExtractFunction inScopeTree (expressionRange : range) functionName filename : Refactoring<unit,unit> =
    let analysis (source,()) =
        IsValid (Some ((expressionRange.StartLine, expressionRange.StartColumn+1), 
                       (expressionRange.EndLine, expressionRange.EndColumn+1)),
                 Some functionName)
                source filename
    let getErrorMessage (source,()) =
        GetErrorMessage (Some ((expressionRange.StartLine, expressionRange.StartColumn+1),
                               (expressionRange.EndLine, expressionRange.EndColumn+1)),
                         Some functionName)
                        source filename
    let extractTempRefactoring = extractTempFunction inScopeTree expressionRange filename
    let extractFunctionRefactoring = sequence extractTempRefactoring (Rename.Rename functionName filename)
    { extractFunctionRefactoring with analysis = analysis; getErrorMessage = getErrorMessage }

let Transform (((startLine, startColumn), (endLine, endColumn)), functionName) source filename =
    let tree = (Ast.Parse source filename).Value
    let expressionRange = mkRange filename (mkPos startLine (startColumn-1)) (mkPos endLine (endColumn-1))
    let inScopeTree = (defaultInScopeTree tree expressionRange).Value
    RunRefactoring (ExtractFunction inScopeTree expressionRange functionName filename) () source