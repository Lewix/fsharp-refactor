module FSharpRefactor.Engine.RangeAnalysis

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.ScopeAnalysis

let CountLines body =
    1+(String.length (String.collect (fun c -> if c = '\n' then "\n" else "") body))

let filterNodesOnRange acceptNode range (tree : Ast.AstNode) =
    let nodeContainsRange node =
        rangeContainsRange ((Ast.GetRange node).Value) range
    let rec findNodesWithRangeInChildren trees foundNodes =
        match trees with
            | [] -> foundNodes
            | tree::ts ->
                let foundNodes =
                    if acceptNode tree then tree::foundNodes else foundNodes
                let childrenContainingRange =
                    if Option.isNone (Ast.GetChildren tree) then []
                    else List.filter nodeContainsRange ((Ast.GetChildren tree).Value)
                findNodesWithRangeInChildren (List.append childrenContainingRange ts) foundNodes

    findNodesWithRangeInChildren [tree] []

let FindNodesWithRange range (tree : Ast.AstNode) =
    let hasRange node =
        Option.isSome (Ast.GetRange node) && (Ast.GetRange node).Value = range

    filterNodesOnRange hasRange range tree
    
let rec FindNodesAroundRange range (tree : Ast.AstNode) =
    let treeContainsRange tree =
        Option.isSome (Ast.GetRange tree) && rangeContainsRange (Ast.GetRange tree).Value range

    filterNodesOnRange treeContainsRange range tree

let TryFindExpressionAtRange range (tree : Ast.AstNode) =
    let isExpression node =
        match node with
            | Ast.AstNode.Expression _ -> true
            | _ -> false
    List.tryFind isExpression (FindNodesWithRange range tree)
    
let FindExpressionAtRange range (tree : Ast.AstNode) =
    TryFindExpressionAtRange range tree
    |> Option.get

let chooseBinding node  =
    match node with
        | Ast.AstNode.Binding b -> Some b
        | _ -> None
            
let FindBindingAtRange range (tree : Ast.AstNode) =
    List.pick chooseBinding (FindNodesWithRange range tree)

let TryFindBindingAroundRange range (tree : Ast.AstNode) =
    FindNodesAroundRange range tree
    |> List.tryPick chooseBinding
    
let TryFindBindingAroundPos pos filename (tree : Ast.AstNode) =
    let range = mkRange filename pos pos
    TryFindBindingAroundRange range tree

let FindExpressionsAroundRange range (tree : Ast.AstNode) =
    let isExpression node =
        match node with
            | Ast.AstNode.Expression _ -> true
            | _ -> false
    FindNodesAroundRange range tree
    |> List.filter isExpression

let TryFindIdentifier source filename (position : pos) =
    let containsPos (name,range) =
        // Identifiers' ranges extend past the end of the text
        // so avoid range.End for cases like b in a+b
        rangeContainsPos range position && range.End <> position

    (Ast.Parse source filename).Value
    |> makeScopeTrees
    |> ListIdentifiers
    |> List.tryFind containsPos

let FindIdentifier source filename (position : pos) =
    TryFindIdentifier source filename position
    |> Option.get

let FindIdentifierName source filename (line, col) =
    let name, _ = FindIdentifier source filename (mkPos line (col-1))
    name
    
let RangeToTuple (range : range) =
    (range.StartLine, range.StartColumn+1), (range.EndLine, range.EndColumn+1)
    
let GetEndPosition (source : string) =
    let lines = source.Split([|'\n'|])
    let lineCount = Array.length lines
    lineCount, String.length (lines.[lineCount-1])