module FSharpRefactor.Tests.EvaluatorTests

open NUnit.Framework
open FSharpRefactor.Evaluator.BehaviourChecker

[<TestFixture>]
type BehaviourCheckerModule() =
    let hasChanged source1 source2 = BehaviourHasChanged "arg" source1 source2

    [<Test>]
    member this.``Can compile some source code without errors``() =
        let _, results = compile "let f a = 1"
        Assert.IsFalse(results.Errors.HasErrors)

    [<Test>]
    member this.``Can figure out that behaviour is different when both programs are valid``() =
        Assert.IsFalse(hasChanged "let f a = a" "let f a = arg")
