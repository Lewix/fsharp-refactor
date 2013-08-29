module FSharpRefactor.Engine.Refactoring

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine

type Filename = string
type ErrorMessage = string
type Change = range * string

type RefactoringResult<'T> =
    | Success of Project * 'T
    | Failure of ErrorMessage

exception RefactoringFailure of ErrorMessage

type Refactoring<'T,'U> = {
    analysis : Project * Filename * 'T -> bool;
    transform : Project * Filename * 'T -> Project * Change list * 'U;
    getErrorMessage : Project * Filename * 'T -> ErrorMessage option
    }

let refactor (refactoring : Refactoring<_,_>) args project filename =
    let isValid = refactoring.analysis (project, filename, args)
    if isValid then
        let project, changes, output = refactoring.transform (project, filename, args)
        let updatedProject =
            CodeTransforms.PerformChanges project changes
        updatedProject, output
    else
        let errorMessage = (refactoring.getErrorMessage (project, filename, args)).Value
        raise (RefactoringFailure errorMessage)

let RunRefactoring refactoring args project filename =
    let project, _ = refactor refactoring args project filename
    project

let interleave (r1 : Refactoring<unit,_>) (r2 : Refactoring<unit,_>) =
    let interleavedTransform (project, filename, ()) =
        let project1, changes1, output = r1.transform (project, filename, ())
        let project2, changes2, _ = r2.transform (project, filename, ())
        assert (project1 = project2)
        project1, List.append changes1 changes2, output

    let interleavedAnalysis (project, filename, ()) =
        let validity1 = r1.analysis (project, filename, ())
        let validity2 = r2.analysis (project, filename, ())
        validity1 && validity2

    { analysis = interleavedAnalysis; transform = interleavedTransform; getErrorMessage = r1.getErrorMessage }

//TODO: exception handling? Error handling in general
let sequence (r1 : Refactoring<unit,'T>) (r2 : Refactoring<'T,_>) =
    let analysis (project, filename, args) =
        let r1Analysis = r1.analysis (project, filename, args)
        if r1Analysis then
            let r1Project, r2Args = refactor r1 () project filename
            r2.analysis (r1Project, filename, r2Args)
        else r1Analysis
        
    let transform (project, filename, args) =
        let r1Project, r2Args = refactor r1 () project filename
        r2.transform (r1Project, filename, r2Args)

    { analysis = analysis; transform = transform; getErrorMessage = r1.getErrorMessage }
