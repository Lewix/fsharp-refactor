module FSharpRefactor.Evaluator.CodeRefactorer

open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow
open FSharpRefactor.Refactorings.Rename
open FSharpRefactor.Refactorings.AddArgument
open FSharpRefactor.Refactorings.ExtractFunction
open FSharpRefactor.Evaluator.GenerationState

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
       Some(refactoring ())
    with
       | RefactoringFailure _ -> None

let randomRename code newName identifierIndex =
    let identifiers = getIdentifiers code
    let identifier = identifiers.[identifierIndex % identifiers.Length]
    let tree = (Ast.Parse code).Value

    tryRefactoring (fun () -> DoRename code tree identifier newName)

let randomAddArgument code argumentName defaultValue bindingIndex =
    let tree = (Ast.Parse code).Value
    let isBinding node = match node with | Ast.AstNode.Binding b -> true | _ -> false
    let bindings =
        ListNodes tree
        |> List.filter isBinding
    
    if List.isEmpty bindings then None
    else
        let bindingRange = (Ast.GetRange (bindings.[bindingIndex % bindings.Length])).Value
        tryRefactoring (fun () -> DoAddArgument code tree bindingRange argumentName (string defaultValue))

let randomExtractFunction source functionName expressionIndex scopeIndex =
    let tree = (Ast.Parse source).Value
    let isExpression node = match node with | Ast.AstNode.Expression e -> true | _ -> false
    let expressions = List.filter isExpression (ListNodes tree)

    if List.isEmpty expressions then None 
    else
        let expressionRange = (Ast.GetRange (expressions.[expressionIndex % expressions.Length])).Value
        let potentialScopes = FindNodesAroundRange expressionRange tree
        if List.isEmpty potentialScopes then None
        else
            let inScopeTree = potentialScopes.[scopeIndex % potentialScopes.Length]
            tryRefactoring (fun () -> DoExtractFunction source tree inScopeTree expressionRange functionName)
