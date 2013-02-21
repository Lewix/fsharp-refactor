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
    time : DateTime;
    errorMessage : string option;
    identThreshold : int
    }

let last ls = List.reduceBack (fun _ l -> l) ls

let randomIdent (random : Random) =
    { defaultState with randomNumbers = Seq.initInfinite (fun i -> random.Next()) }
    |> generateIdent Int
    |> fst

let tryRefactoring refactoring =
    try
        let didCheck, result, message = refactoring ()
        if didCheck then
            Succeeded, Some result, message
        else
            Failed, Some result, message
    with
        | CouldNotRefactor -> raise CouldNotRefactor
        | e -> Exception, None, Some e.Message

let evaluateExtractFunction idents source (random : Random) =
    let status, sourceAfter, message =
        tryRefactoring (fun () -> randomExtractFunction source (randomIdent random) (random.Next()) (random.Next()))
    { status = status;
      sourceBefore = source;
      sourceAfter = sourceAfter;
      refactoring = "extract-function";
      time = DateTime.Now;
      changed = false;
      errorMessage = message;
      identThreshold = idents }

let evaluateAddArgument idents source (random : Random) =
    let status, sourceAfter, message =
        tryRefactoring (fun () -> randomAddArgument source (randomIdent random) (random.Next()) (random.Next()))
    { status = status;
      sourceBefore = source;
      sourceAfter = sourceAfter;
      refactoring = "add-argument";
      time = DateTime.Now;
      changed = false;
      errorMessage = message;
      identThreshold = idents }

let evaluateRename idents source (random : Random) =
    let status, sourceAfter, message =
        tryRefactoring (fun () -> randomRename source (randomIdent random) (random.Next()))
    { status = status;
      sourceBefore = source;
      sourceAfter = sourceAfter;
      refactoring = "rename";
      time = DateTime.Now;
      changed = false;
      errorMessage = message;
      identThreshold = idents }

let evaluateRefactoring idents refactoring =
    try
        let entryPoint = "f"
        let codeTemplate, code = generateEntryPoint entryPoint { defaultState with identThreshold = idents }
        let random = new Random()
        let refactoringResult = refactoring idents code random
        let changed =
            if Option.isSome refactoringResult.sourceAfter then
                BehaviourHasChanged entryPoint (codeTemplate refactoringResult.sourceBefore) (codeTemplate refactoringResult.sourceAfter.Value)
            else
                false

        Some { refactoringResult with changed = changed }
    with
        | CouldNotRefactor -> None

let evaluateRefactorings refactoring iterations (resultsFile : string) =
    let identsOnIteration i = 5 * (i/1000 + 1)
    let evaluations = Seq.init iterations (fun i -> evaluateRefactoring (identsOnIteration i) refactoring)
    let fileWriter = new StreamWriter(resultsFile, true)
    ignore (fprintfn fileWriter "status,changed,before,after,refactoring,time,error message,ident threshold")

    let writeResultLine result =
        if Option.isSome result then
            fprintfn fileWriter "%A,%A,%A,%A,%A,%A,%A,%A" result.Value.status
                                                    result.Value.changed
                                                    result.Value.sourceBefore
                                                    result.Value.sourceAfter
                                                    result.Value.refactoring
                                                    result.Value.time
                                                    result.Value.errorMessage
                                                    result.Value.identThreshold
            fileWriter.Flush()

    Seq.iter writeResultLine evaluations
    fileWriter.Close()
