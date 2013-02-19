module FSharpRefactor.Evaluator.Evaluator

open System
open System.IO
open FSharpRefactor.Evaluator.CodeGenerator
open FSharpRefactor.Evaluator.GenerationState
open FSharpRefactor.Evaluator.BehaviourChecker
open FSharpRefactor.Evaluator.CodeRefactorer

let last ls = List.reduceBack (fun _ l -> l) ls

let evaluateRename () =
    let entryPoint = "f"
    let code = generateEntryPoint entryPoint defaultState
    let random = new Random()
    let newName, _ =
        { defaultState with randomNumbers = Seq.initInfinite(fun _ -> random.Next()) }
        |> generateIdent Int
    let renameResult = randomRename code newName (random.Next())

    if Option.isSome renameResult then
        let after = renameResult.Value
        Some(BehaviourHasChanged (entryPoint) code after, code, after)
    else
        None

let evaluateRenames iterations (resultsFile : string) =
    let evaluations = Seq.init iterations (fun i -> evaluateRename())
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
