module FSharpRefactor.Evaluator.Evaluator

open System
open System.IO
open FSharpRefactor.Evaluator.CodeGenerator
open FSharpRefactor.Evaluator.GenerationState
open FSharpRefactor.Evaluator.BehaviourChecker
open FSharpRefactor.Evaluator.CodeRefactorer

type Status =
    | Succeeded
    | Failed
    | Exception

type RefactoringResult = {
    changed : bool;
    status : Status;
    sourceBefore : string;
    sourceAfter : string option;
    refactoring : string;
    time : DateTime
    }

let last ls = List.reduceBack (fun _ l -> l) ls

let randomIdent (random : Random) =
    { defaultState with randomNumbers = Seq.initInfinite (fun i -> random.Next()) }
    |> generateIdent Int
    |> fst

let tryRefactoring refactoring =
    try
        let result = refactoring ()
        if Option.isSome result then
            Succeeded, result
        else
            Failed, result
    with
        | _ -> Exception, None

let evaluateExtractFunction source (random : Random) =
    let status, sourceAfter =
        tryRefactoring (fun () -> randomExtractFunction source (randomIdent random) (random.Next()) (random.Next()))
    { status = status;
      sourceBefore = source;
      sourceAfter = sourceAfter;
      refactoring = "extract-function";
      time = DateTime.Now;
      changed = false }

let evaluateAddArgument source (random : Random) =
    let status, sourceAfter =
        tryRefactoring (fun () -> randomAddArgument source (randomIdent random) (random.Next()) (random.Next()))
    { status = status;
      sourceBefore = source;
      sourceAfter = sourceAfter;
      refactoring = "add-argument";
      time = DateTime.Now;
      changed = false }

let evaluateRename source (random : Random) =
    let status, sourceAfter =
        tryRefactoring (fun () -> randomRename source (randomIdent random) (random.Next()))
    { status = status;
      sourceBefore = source;
      sourceAfter = sourceAfter;
      refactoring = "rename";
      time = DateTime.Now;
      changed = false }

let evaluateRefactoring refactoring =
    let entryPoint = "f"
    let codeTemplate, code = generateEntryPoint entryPoint defaultState
    let random = new Random()
    let refactoringResult = refactoring code random
    let changed =
        if Option.isSome refactoringResult.sourceAfter then
            BehaviourHasChanged entryPoint refactoringResult.sourceBefore refactoringResult.sourceAfter.Value
        else
            false

    { refactoringResult with changed = changed }

let evaluateRefactorings refactoring iterations (resultsFile : string) =
    let evaluations = Seq.init iterations (fun i -> evaluateRefactoring refactoring)
    let fileWriter = new StreamWriter(resultsFile, true)
    ignore (fprintfn fileWriter "status,before,after,refactoring,time")

    let writeResultLine result =
        fprintfn fileWriter "%A,%A,%A,%A,%A,%A" result.status
                                                result.changed
                                                result.sourceBefore
                                                result.sourceAfter
                                                result.refactoring
                                                result.time
        fileWriter.Flush()

    Seq.iter writeResultLine evaluations
    fileWriter.Close()
