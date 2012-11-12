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

        let joinLines s1 s2 =
            match (s1,s2) with
                | ("",s2) -> s2
                | (s1,"") -> s1
                | (s1,s2) -> s1 + "\n" + s2

        let addNewLine i s = if i = 0 then s else "\n" + s
                                
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
            
            let before = Seq.fold joinLines "" (Seq.take (startLine-1) lines)
            let after = Seq.fold joinLines "" (Seq.skip endLine lines)
            let replacement = lines.[startLine-1].[0..startColumn-1]
                              |> (fun s -> s + replacementText)
                              |> (fun s -> s + lines.[endLine-1].[endColumn..])

            Seq.fold joinLines "" [before; replacement; after]

        let rec processPairs modifiedSource remainingNodeTextPairs =
            match remainingNodeTextPairs with
                | [] -> modifiedSource
                | p::ps -> processPairs (replaceOne modifiedSource p) ps

        processPairs source sortedPairs
