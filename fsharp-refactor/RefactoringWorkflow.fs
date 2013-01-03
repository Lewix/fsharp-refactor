module FSharpRefactor.Engine.RefactoringWorkflow

open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.CodeTransforms

type Refactoring =
    { source : string;
      changes : (range * string) list }

type RefactoringBuilder(source) =
    member this.source = source
    member this.Yield(change) : Refactoring =
        { source = this.source;
          changes = [change] }

    member this.YieldFrom(refactoring) : Refactoring = refactoring

    member this.Combine(refactoring1,refactoring2) =
        { source = this.source;
          changes = List.append refactoring1.changes refactoring2.changes }

    member this.Delay(delayedChanges) = delayedChanges()

    member this.For(changes, changeFunc) =
        let refactoringFunc = fun c -> (changeFunc c).changes
        { source = this.source;
          changes = List.collect refactoringFunc changes }

    member this.Zero() =
        { source = this.source;
          changes = [] }

let RunRefactoring refactoring = CodeTransforms.ChangeTextOf refactoring.source refactoring.changes
let refactoring source = new RefactoringBuilder(source)
