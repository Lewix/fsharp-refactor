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
open FSharpRefactor.Refactorings.Rename

let rec stripBrackets (body : string) =
    if body.[0] = '(' && body.[(String.length body)-1] = ')'
    then stripBrackets (body.[1..(String.length body)-2])
    else body

let formatArguments functionName arguments =
    List.map (fun s -> " " + s) arguments
    |> List.fold (+) functionName

let DefaultInScopeTree (tree : Ast.AstNode) (expressionRange : range) =
    let outermostBinding = TryFindBindingAroundRange expressionRange tree
    let outermostExpression = TryFindExpressionAroundRange expressionRange tree
    if Option.isSome outermostBinding then
        match outermostBinding.Value with
            | SynBinding.Binding(_,_,_,_,_,_,_,_,_,expression,_,_) -> Some(expression)
    else outermostExpression

let CreateFunction functionName arguments body isMultiLine indentString (declarationRange : range) : Refactoring<unit,Identifier> =
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
    { analysis = (fun (_,_) -> Valid); transform = transform }
 
let CallFunction functionName arguments callRange : Refactoring<unit,unit> =
    //TODO: don't always put brackets around function body
    let transform (source, ()) =
        let functionCall =
            formatArguments functionName arguments
        if List.isEmpty arguments then
            source, [callRange, functionCall], ()
        else
            source, [callRange, sprintf "(%s)" functionCall], ()
            
    { analysis = (fun (_,_) -> Valid); transform = transform }

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
        if doCheck then
            let tree = (Ast.Parse source).Value
            CanExtractFunction tree inScopeTree expressionRange
        else
            Valid
    let transform (source,()) =
        let tree = (Ast.Parse source).Value
        let functionName = FindUnusedName tree
        let unindentedBody = 
            (String.replicate (expressionRange.StartColumn) " ") + (TextOfRange source expressionRange)
            |> RemoveLeading ' '
        let bodyExpression = TryFindExpressionAtRange expressionRange inScopeTree

        let getFreeIdentifierDeclarations expression =
            GetFreeIdentifierUsages (makeScopeTrees expression) DefaultDeclared
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

let GetErrorMessage (range:((int*int)*(int*int)) option, functionName:string option) (source:string) (filename:string) =
    let checkRange ((startLine, startCol), (endLine, endCol)) =
        let tree = (Ast.Parse source).Value
        let range = mkRange "test.fs" (mkPos startLine (startCol-1)) (mkPos endLine (endCol-1))
        if Option.isSome (TryFindExpressionAtRange range tree)
        then None else Some "No expression found at the given range"

    IsSuccessful checkRange range
    |> fun (l:Lazy<_>) -> l.Force()
        

let IsValid (range:((int*int)*(int*int)) option, functionName:string option) (source:string) (filename:string) =
    Option.isNone (GetErrorMessage (range, functionName) source filename)
