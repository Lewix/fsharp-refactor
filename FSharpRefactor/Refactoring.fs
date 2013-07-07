module FSharpRefactor.Engine.Refactoring

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.CodeTransforms

type ErrorMessage = string
type Change = range * string
type Source = string


type RefactoringValidity =
    | Valid
    | Invalid of ErrorMessage

type RefactoringResult<'T> =
    | Success of Source * 'T
    | Failure of ErrorMessage

let CombineValidity validity1 validity2 =
    match validity1, validity2 with
        | Valid, Valid -> Valid
        | Invalid msg, _ -> Invalid msg
        | _, Invalid msg -> Invalid msg

exception RefactoringFailure of ErrorMessage

type Refactoring<'T,'U> = {
    analysis : Source * 'T -> RefactoringValidity;
    transform : Source * 'T -> Source * Change list * 'U
    }

let refactor (refactoring : Refactoring<_,_>) args source =
    let validity = refactoring.analysis (source, args)
    match validity with
        | Invalid(message) -> Failure(message)
        | Valid ->
            let source, changes, output = refactoring.transform (source, args)
            let resultingSource = CodeTransforms.ChangeTextOf source changes
            Success(resultingSource, output)

let RunRefactoring refactoring args source =
    let refactoringResult = refactor refactoring args source
    match refactoringResult with
        | Success(source, _) -> source
        | Failure(message) -> raise (RefactoringFailure message)

let interleave (r1 : Refactoring<unit,_>) (r2 : Refactoring<unit,_>) =
    let interleavedTransform (source, ()) =
        let source1, changes1, output = r1.transform (source, ())
        let source2, changes2, _ = r2.transform (source, ())
        assert (source1 = source2)
        source1, List.append changes1 changes2, output

    let interleavedAnalysis (source, ()) =
        let validity1 = r1.analysis (source, ())
        let validity2 = r2.analysis (source, ())
        CombineValidity validity1 validity2

    { analysis = interleavedAnalysis; transform = interleavedTransform }

//TODO: exception handling? Error handling in general
let sequence (r1 : Refactoring<unit,'T>) (r2 : Refactoring<'T,_>) =
    let analysis (source, args) =
        let r1Analysis = r1.analysis (source, args)
        if r1Analysis = Valid then
            let r1Result = refactor r1 () source
            match r1Result with
                | Success(r1Source, r2Args) -> r2.analysis (r1Source, r2Args)
                | Failure(message) -> Invalid(message)
        else r1Analysis
    let transform (source, args) =
        let r1Result = refactor r1 () source
        match r1Result with
            | Success(r1Source, r2Args) ->
                r2.transform (r1Source, r2Args)
            | Failure(message) -> raise (RefactoringFailure message)

    { analysis = analysis; transform = transform }
