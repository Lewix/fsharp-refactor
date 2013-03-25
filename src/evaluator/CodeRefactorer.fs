module FSharpRefactor.Evaluator.CodeRefactorer

open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Refactorings.Rename
open FSharpRefactor.Refactorings.AddArgument
open FSharpRefactor.Refactorings.ExtractFunction
open FSharpRefactor.Evaluator.GenerationState

exception CouldNotRefactor

//TODO: multiline code
let posFromIndex code index = mkPos 1 index

let (|Ident|_|) code =
    let m = Regex("[^i]*(ident[0-9]*)(.*)").Match(code)
    if m.Success then
        let identGroup = m.Groups.[1]
        let rest = m.Groups.[2]
        Some (identGroup.Value, identGroup.Index, rest.Value)
    else
        None

let getIdentifiers code =
    let rec getIdentifiersTc remainingCode index identifiers =
        match remainingCode with
            | Ident(ident, indexInRemainingCode, rest) ->
                let identIndex = indexInRemainingCode + index
                let identRange =
                    mkRange "test.fs"
                            (posFromIndex code identIndex)
                            (posFromIndex code (identIndex + ident.Length))
                getIdentifiersTc rest
                                 (identIndex + ident.Length)
                                 ((ident,identRange)::identifiers)
            | _ -> identifiers
    getIdentifiersTc code 0 []

let tryRefactoring refactoring =
    try
       true, refactoring true, None
    with
       | RefactoringFailure message -> false, refactoring false, Some message

let randomRename code newName identifierIndex =
    let identifiers = getIdentifiers code
    let identifier = identifiers.[identifierIndex % identifiers.Length]
    let tree = (Ast.Parse code).Value

    tryRefactoring (fun check -> RunRefactoring (Rename check newName) identifier code)

let randomAddArgument code argumentName defaultValue bindingIndex =
    let tree = (Ast.Parse code).Value
    let isBinding node = match node with | Ast.AstNode.Binding b -> true | _ -> false
    let bindings =
        ListNodes tree
        |> List.filter isBinding
    
    if List.isEmpty bindings then raise CouldNotRefactor
    else
        let bindingRange = (Ast.GetRange (bindings.[bindingIndex % bindings.Length])).Value
        tryRefactoring (fun check -> RunRefactoring (AddArgument check bindingRange argumentName (string defaultValue)) () code)

let randomExtractFunction source functionName expressionIndex scopeIndex =
    let tree = (Ast.Parse source).Value
    let isExpression node = match node with | Ast.AstNode.Expression e -> true | _ -> false
    let expressions = List.filter isExpression (ListNodes tree)

    if List.isEmpty expressions then raise CouldNotRefactor 
    else
        let expressionRange = (Ast.GetRange (expressions.[expressionIndex % expressions.Length])).Value
        let potentialScopes = FindNodesAroundRange expressionRange tree
        if List.isEmpty potentialScopes then raise CouldNotRefactor
        else
            let inScopeTree = potentialScopes.[scopeIndex % potentialScopes.Length]
            tryRefactoring (fun check -> RunRefactoring (ExtractFunction check inScopeTree expressionRange functionName) () source)
