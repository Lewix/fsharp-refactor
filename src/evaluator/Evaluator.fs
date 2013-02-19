module FSharpRefactor.Evaluator.Evaluator

open System
open System.IO
open FSharpRefactor.Evaluator.CodeGenerator
open FSharpRefactor.Evaluator.GenerationState
open FSharpRefactor.Evaluator.BehaviourChecker
open FSharpRefactor.Evaluator.CodeRefactorer

let last ls = List.reduceBack (fun _ l -> l) ls

let randomIdent (random : Random) =
    { defaultState with randomNumbers = Seq.initInfinite (fun i -> random.Next()) }
    |> generateIdent Int
    |> fst

let evaluateExtractFunction source (random : Random) =
    randomExtractFunction source (randomIdent random) (random.Next()) (random.Next())

let evaluateAddArgument source (random : Random) =
    randomAddArgument source (randomIdent random) (random.Next()) (random.Next())

let evaluateRename source (random : Random) =
    randomRename source (randomIdent random) (random.Next())

let evaluateRefactoring refactoring =
    let entryPoint = "f"
    let codeTemplate, code = generateEntryPoint entryPoint defaultState
    let random = new Random()
    let refactoringResult = refactoring code random

    if Option.isSome refactoringResult then
        let before = codeTemplate code
        let after = codeTemplate refactoringResult.Value
        Some(BehaviourHasChanged entryPoint before after, before, after)
    else
        None

let evaluateRefactorings refactoring iterations (resultsFile : string) =
    let evaluations = Seq.init iterations (fun i -> evaluateRefactoring refactoring)
    let fileWriter = new StreamWriter(resultsFile)

    let writeResultLine result =
        if Option.isSome result then
            let changed, before, after = result.Value
            ignore (fprintfn fileWriter "%A,%A,%A" changed before after)
        else
            ignore (fprintfn fileWriter "failed,,")
        fileWriter.Flush()

    Seq.iter writeResultLine evaluations
    fileWriter.Close()
