module FSharpRefactor.Evaluator.EvaluatorCommandLine

open FSharpRefactor.Evaluator.Evaluator
open FSharpRefactor.Evaluator.BehaviourChecker

[<EntryPoint>]
let main(args : string[]) =
    sprintf "%A" (evaluateRefactorings evaluateExtractFunction "/mnt/media/git/university/personalproj/evaluation_results/results4.csv") |> ignore
    0
