module FSharpRefactor.Evaluator.Evaluator

open System
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
    let before, after = randomRename code newName (random.Next())

    BehaviourHasChanged (entryPoint) before after
