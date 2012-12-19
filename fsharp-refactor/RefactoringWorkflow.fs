module FSharpRefactor.Engine.RefactoringWorkflow

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.CodeTransforms

type Refactoring = (range * string) list

type RefactoringBuilder(source) =
    member this.source = source
    member this.Yield(change) : Refactoring = [change]

    member this.Combine(changes1,changes2) =
        List.append changes1 changes2

    member this.Delay(delayedChanges) = delayedChanges()

    member this.For(changes, changeFunc) =
        List.concat (Seq.map changeFunc changes)

    member this.Zero() =
        []

    member this.Run(changes) = CodeTransforms.ChangeTextOf this.source changes
//CodeTransforms.ChangeTextOf this.source changes

let refactoring source = new RefactoringBuilder(source)
