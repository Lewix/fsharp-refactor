module FSharpRefactor.Engine.Refactoring

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine

type ErrorMessage = string
type Change = range * string

type RefactoringResult<'T> =
    | Success of Project * 'T
    | Failure of ErrorMessage

exception RefactoringFailure of ErrorMessage

type Refactoring<'T,'U> = {
    analysis : Project * 'T -> bool;
    transform : Project * 'T -> Project * Change list * 'U;
    getErrorMessage : Project * 'T -> ErrorMessage option
    }

let refactor (refactoring : Refactoring<_,_>) args project =
    let isValid = refactoring.analysis (project, args)
    if isValid then
        let project, changes, output = refactoring.transform (project, args)
        let updatedContents =
            CodeTransforms.ChangeTextOf project.CurrentFileContents changes
        project.UpdateCurrentFileContents updatedContents, output
    else
        let errorMessage = (refactoring.getErrorMessage (project, args)).Value
        raise (RefactoringFailure errorMessage)

let RunRefactoring refactoring args project =
    let project, _ = refactor refactoring args project
    project

let interleave (r1 : Refactoring<unit,_>) (r2 : Refactoring<unit,_>) =
    let interleavedTransform (project, ()) =
        let project1, changes1, output = r1.transform (project, ())
        let project2, changes2, _ = r2.transform (project, ())
        assert (project1 = project2)
        project1, List.append changes1 changes2, output

    let interleavedAnalysis (project, ()) =
        let validity1 = r1.analysis (project, ())
        let validity2 = r2.analysis (project, ())
        validity1 && validity2

    { analysis = interleavedAnalysis; transform = interleavedTransform; getErrorMessage = r1.getErrorMessage }

//TODO: exception handling? Error handling in general
let sequence (r1 : Refactoring<unit,'T>) (r2 : Refactoring<'T,_>) =
    let analysis (project, args) =
        let r1Analysis = r1.analysis (project, args)
        if r1Analysis then
            let r1Project, r2Args = refactor r1 () project
            r2.analysis (r1Project, r2Args)
        else r1Analysis
        
    let transform (project, args) =
        let r1Project, r2Args = refactor r1 () project
        r2.transform (r1Project, r2Args)

    { analysis = analysis; transform = transform; getErrorMessage = r1.getErrorMessage }
