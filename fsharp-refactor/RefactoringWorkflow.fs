module FSharpRefactor.Engine.RefactoringWorkflow

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.CodeTransforms

type ErrorMessage = string
type Change = range * string
type Source = string

type Refactoring =
    | Success of Source * Change list
    | Failure of ErrorMessage

type RefactoringValidity =
    | Valid
    | Invalid of ErrorMessage

exception RefactoringFailure of ErrorMessage

type RefactoringBuilder(source,validity) =
    member this.source = source
    member this.validity = validity
        
    member this.Yield(change) : Refactoring =
        match this.validity with
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
        List.reduce (fun r1 r2 -> this.Combine(r1,r2)) refactorings

    member this.Zero() = Success(this.source,[])

let RunRefactoring refactoring =
    match refactoring with
        | Success(source,changes) -> CodeTransforms.ChangeTextOf source changes
        | Failure(message) -> raise (RefactoringFailure message)
        
let refactoring source valid = new RefactoringBuilder(source,valid)
