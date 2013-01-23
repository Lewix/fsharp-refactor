namespace FSharpRefactor.Engine.CodeTransforms

open System
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis

module CodeTransforms =
    exception InvalidRange
    exception InvalidNode


    let takeAroundPos source (line, column) =
        let notLineSep = fun c -> c <> '\n'
        let makeString charSeq = Seq.fold (+) "" (Seq.map string charSeq)
        // Lines are indexed from 1, columns from 0
        let rec takeAroundPosSeq before after (line, column) =
            match (line, column) with
                | 1,c -> (Seq.append before (Seq.take column after), Seq.skip column after)
                | n,_ ->
                    takeAroundPosSeq (Seq.concat [before; (Seq.takeWhile notLineSep after); seq['\n']])
                                  (Seq.skip 1 (Seq.skipWhile notLineSep after))
                                  (line-1, column)
        let before,after = takeAroundPosSeq "" source (line, column)
        makeString before, makeString after

    let Indent (body : string) =
        let indentString = "    "
        let indentLine body line =
            let before, after = takeAroundPos body (line, 0)
            before + indentString + after
        List.fold indentLine body [1..(CountLines body)]

    let replaceRange (source : string) (range : range, replacementText : string) =
        let before, _ = takeAroundPos source (range.StartLine, range.StartColumn)
        let _, after = takeAroundPos source (range.EndLine, range.EndColumn)

        before + replacementText + after

    // Expect the nodes to have a range. If not, raise an exception
    let getRange node =
        let range = Ast.GetRange node
        if range.IsSome then range.Value
        else raise InvalidRange


    let ChangeTextOf (source : string) (rangeTextPairsToChange : (range * string) list) =
        let sortedPairs =
            let sortFunction (range1, _) (range2, _) =
                let compared = -rangeOrder.Compare(range1, range2)
                if compared = 0 then -posOrder.Compare(range1.End, range2.End)
                else compared

            List.sortWith sortFunction rangeTextPairsToChange


        let rec processPairs modifiedSource remainingRangeTextPairs =
            match remainingRangeTextPairs with
                | [] -> modifiedSource
                | (r,t)::ps -> processPairs (replaceRange modifiedSource (r, t)) ps

        processPairs source sortedPairs

    let TextOfRange (source : string) (range : range) =
        let _, after = takeAroundPos source (range.StartLine, range.StartColumn)
        let endPosInAfter =
            if range.StartLine = range.EndLine then (1, range.EndColumn-range.StartColumn)
            else (1 + range.EndLine - range.StartLine, range.EndColumn)
        fst (takeAroundPos after endPosInAfter)
