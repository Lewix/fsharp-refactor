module FSharpRefactor.Engine.RefactoringWorkflow

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.CodeTransforms

type ErrorMessage = string
type Change = range * string
type Source = string


type RefactoringValidity =
    | Valid
    | Invalid of ErrorMessage

type Refactoring =
    | Success of Source * Change list
    | Failure of ErrorMessage

let CombineValidity validity1 validity2 =
    match validity1, validity2 with
        | Valid, Valid -> Valid
        | Invalid msg, _ -> Invalid msg
        | _, Invalid msg -> Invalid msg

exception RefactoringFailure of ErrorMessage

type RefactoringBuilder(source,analysis) =
    member this.source = source
    member this.analysis = analysis
        
    member this.Yield(change) : Refactoring =
        match this.analysis with
            | Valid -> Success(this.source,[change])
            | Invalid message -> Failure(message)

    member this.YieldFrom(refactoring) : Refactoring = refactoring

    member this.Combine(refactoring1,refactoring2) =
        match refactoring1,refactoring2 with
            | Success(_,changes1),Success(_,changes2) ->
                Success(this.source,List.append changes1 changes2)
            | Failure(message),_ | _,Failure(message) -> Failure(message)

    member this.Delay(delayedChanges) = delayedChanges()

    member this.For(changes, changeFunc) =
        let refactorings = List.map changeFunc changes
        if List.isEmpty refactorings then this.Zero()
        else List.reduce (fun r1 r2 -> this.Combine(r1,r2)) refactorings

    member this.Zero() = Success(this.source,[])

let RunRefactoring refactoring =
    match refactoring with
        | Success(source,changes) -> CodeTransforms.ChangeTextOf source changes
        | Failure(message) -> raise (RefactoringFailure message)
        
let refactoring source valid = new RefactoringBuilder(source,valid)







type RefactoringResult<'T> =
    | Success of Source * 'T
    | Failure of ErrorMessage

type NewRefactoring<'T,'U> = {
    analysis : Source * 'T -> RefactoringValidity;
    transform : Source * 'T -> Change list * 'U
    }

let RunNewRefactoring refactoringResult =
    match refactoringResult with
        | Success(source, _) -> source
        | Failure(message) -> raise (RefactoringFailure message)

let refactor (refactoring : NewRefactoring<_,_>) args source =
    let validity = refactoring.analysis (source, args)
    match validity with
        | Invalid(message) -> Failure(message)
        | Valid ->
            let changes, output = refactoring.transform (source, args)
            let resultingSource = CodeTransforms.ChangeTextOf source changes
            Success(resultingSource, output)

let interleave (r1 : NewRefactoring<unit,_>) (r2 : NewRefactoring<unit,_>) =
    let interleavedTransform (source, ()) =
        let source1, _ = r1.transform (source, ())
        let source2, _ = r2.transform (source, ())
        (List.append source1 source2, ())
    let interleavedAnalysis (source, ()) =
        let validity1 = r1.analysis (source, ())
        let validity2 = r2.analysis (source, ())
        CombineValidity validity1 validity2
    { analysis = interleavedAnalysis; transform = interleavedTransform }

//TODO: exception handling? Error handling in general
let sequence (r1 : NewRefactoring<unit,'T>) (r2 : NewRefactoring<'T,_>) =
    let analysis (source, args) =
        let r1Analysis = r1.analysis (source, args)
        if r1Analysis = Valid then
            let _, r2Args = r1.transform (source, args)
            let r1Result = refactor r1 () source
            match r1Result with
                | Success(r1Source, r2Args) -> r2.analysis (r1Source, r2Args)
                | Failure(message) -> Invalid(message)
        else r1Analysis
    let transform (source, args) =
        let r1Result = refactor r1 () source
        match r1Result with
            | Success(r1Source, r2Args) -> r2.transform (r1Source, r2Args)
            | Failure(message) -> raise (RefactoringFailure message)

    { analysis = analysis; transform = transform }
