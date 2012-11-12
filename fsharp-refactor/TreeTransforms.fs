namespace FSharpRefactor.Engine.TreeTransforms

open System
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

        let sortedPairs =
            let sortFunction (node1, _) (node2, _) =
                -rangeOrder.Compare(getRange node1, getRange node2)

            List.sortWith sortFunction nodeTextPairsToChange

        let replaceOne (source : string) (node, replacementText) =
            let notLineSep = fun c -> c <> '\n'
            let rec takeAroundPos before after (line, column) =
                // Lines are indexed from 1, columns from 0
                match (line, column) with
                    | 1,c -> (Seq.append before (Seq.take column after), Seq.skip column after)
                    | n,_ ->
                        takeAroundPos (Seq.concat [before; (Seq.takeWhile notLineSep after); seq['\n']])
                                      (Seq.skip 1 (Seq.skipWhile notLineSep after))
                                      (line-1, column)
                                        
            let range = getRange node
            let before, _ = takeAroundPos "" source (range.StartLine, range.StartColumn)
            let _, after = takeAroundPos "" source (range.EndLine, range.EndColumn)

            (Seq.fold (+) "" (Seq.map string before)) + replacementText + (Seq.fold (+) "" (Seq.map string after))

            
        let rec processPairs modifiedSource remainingNodeTextPairs =
            match remainingNodeTextPairs with
                | [] -> modifiedSource
                | p::ps -> processPairs (replaceOne modifiedSource p) ps

        processPairs source sortedPairs
