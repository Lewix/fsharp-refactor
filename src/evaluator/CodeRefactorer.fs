module FSharpRefactor.Evaluator.CodeRefactorer

open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Refactorings.Rename
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

let randomRename code newName identifierIndex =
    let identifier =
        (getIdentifiers code).[identifierIndex]
    let tree = (Ast.Parse code).Value

    code, DoRename code tree identifier newName
