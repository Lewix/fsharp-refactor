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
    analysis : Source * 'T -> bool;
    transform : Source * 'T -> Source * Change list * 'U;
    getErrorMessage : Source * 'T -> ErrorMessage option
    }

let refactor (refactoring : Refactoring<_,_>) args source =
    let isValid = refactoring.analysis (source, args)
    if isValid then
        let source, changes, output = refactoring.transform (source, args)
        CodeTransforms.ChangeTextOf source changes, output
    else
        let errorMessage = (refactoring.getErrorMessage (source, args)).Value
        raise (RefactoringFailure errorMessage)

let RunRefactoring refactoring args source =
    let source, _ = refactor refactoring args source
    source

let interleave (r1 : Refactoring<unit,_>) (r2 : Refactoring<unit,_>) =
    let interleavedTransform (source, ()) =
        let source1, changes1, output = r1.transform (source, ())
        let source2, changes2, _ = r2.transform (source, ())
        assert (source1 = source2)
        source1, List.append changes1 changes2, output

    let interleavedAnalysis (source, ()) =
        let validity1 = r1.analysis (source, ())
        let validity2 = r2.analysis (source, ())
        validity1 && validity2

    { analysis = interleavedAnalysis; transform = interleavedTransform; getErrorMessage = r1.getErrorMessage }

//TODO: exception handling? Error handling in general
let sequence (r1 : Refactoring<unit,'T>) (r2 : Refactoring<'T,_>) =
    let analysis (source, args) =
        let r1Analysis = r1.analysis (source, args)
        if r1Analysis then
            let r1Source, r2Args = refactor r1 () source
            r2.analysis (r1Source, r2Args)
        else r1Analysis
        
    let transform (source, args) =
        let r1Source, r2Args = refactor r1 () source
        r2.transform (r1Source, r2Args)

    { analysis = analysis; transform = transform; getErrorMessage = r1.getErrorMessage }
