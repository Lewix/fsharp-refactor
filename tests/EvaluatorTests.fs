module FSharpRefactor.Tests.EvaluatorTests

open NUnit.Framework
open FSharpRefactor.Evaluator.BehaviourChecker

[<TestFixture>]
type BehaviourCheckerModule() =
    let hasChanged source1 source2 = BehaviourHasChanged "arg" source1 source2

    [<Test>]
    member this.``Can figure out that behaviour is different when both programs are valid``() =
        Assert.IsFalse(hasChanged "let f a = a" "let f a = arg")
