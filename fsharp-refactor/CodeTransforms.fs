namespace FSharpRefactor.Engine.CodeTransforms

open System
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast

module CodeTransforms =
    exception InvalidRange
    exception InvalidNode

    let replaceRange (source : string) (range : range, replacementText : string) =
        let notLineSep = fun c -> c <> '\n'
        let rec takeAroundPos before after (line, column) =
            // Lines are indexed from 1, columns from 0
            match (line, column) with
                | 1,c -> (Seq.append before (Seq.take column after), Seq.skip column after)
                | n,_ ->
                    takeAroundPos (Seq.concat [before; (Seq.takeWhile notLineSep after); seq['\n']])
                                  (Seq.skip 1 (Seq.skipWhile notLineSep after))
                                  (line-1, column)

        let before, _ = takeAroundPos "" source (range.StartLine, range.StartColumn)
        let _, after = takeAroundPos "" source (range.EndLine, range.EndColumn)

        (Seq.fold (+) "" (Seq.map string before)) + replacementText + (Seq.fold (+) "" (Seq.map string after))

    // Expect the nodes to have a range. If not, raise an exception
    let getRange node =
        let range = Ast.GetRange node
        if range.IsSome then range.Value
        else raise InvalidRange


    let ChangeTextOf (source : string) (rangeTextPairsToChange : (range * string) list) =
        let sortedPairs =
            let sortFunction (range1, _) (range2, _) =
                -rangeOrder.Compare(range1, range2)

            List.sortWith sortFunction rangeTextPairsToChange


        let rec processPairs modifiedSource remainingRangeTextPairs =
            match remainingRangeTextPairs with
                | [] -> modifiedSource
                | (r,t)::ps -> processPairs (replaceRange modifiedSource (r, t)) ps

        processPairs source sortedPairs
    
    let TextOfRange (source : string) (range : range) =
        let lines = source.Split('\n')
        let startLine = lines.[range.StartLine-1].[range.StartColumn..]
        let endLine = lines.[range.EndLine-1].[range.StartColumn..]
        let rec getLines line =
            if line < range.EndLine-1 then lines.[line]::(getLines (line+1))
            else if line = range.EndLine-1 then [endLine]
            else [] // StartLine and EndLine are equal

        startLine::(getLines range.StartLine)
        |> Seq.fold (+) ""
