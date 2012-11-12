namespace FSharpRefactor.Engine.TreeTransforms

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast

module TreeTransforms =
    exception InvalidRange
    
    let ChangeTextOf (source : string) (nodeTextPairsToChange : (Ast.AstNode * string) list) =
        // Expect the nodes to have a range. If not, raise an exception
        let getRange node =
            let range = Ast.GetRange node
            if range.IsSome then range.Value
            else raise InvalidRange
                
        let sortFunction (node1, _) (node2, _) =
            -rangeOrder.Compare(getRange node1, getRange node2)
            
        let sortedPairs =
            List.sortWith sortFunction nodeTextPairsToChange

        let replaceOne (source : string) (node, replacementText) =
            let range = getRange node
            let startColumn = range.StartColumn
            let endColumn = range.EndColumn
            let startLine = range.StartLine
            let endLine = range.EndLine
            let lines = source.Split('\n')
            
            Seq.fold (+) "" (Seq.skip endLine lines)
            |> (+) lines.[endLine-1].[endColumn..]
            |> (+) replacementText
            |> (+) lines.[startLine-1].[0..startColumn-1]
            |> (+) (Seq.fold (+) "" (Seq.take (startLine-1) lines))

        let rec processPairs modifiedSource remainingNodeTextPairs =
            match remainingNodeTextPairs with
                | [] -> modifiedSource
                | p::ps -> processPairs (replaceOne modifiedSource p) ps

        processPairs source nodeTextPairsToChange
