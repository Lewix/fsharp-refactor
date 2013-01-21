module FSharpRefactor.Tests.EvaluatorTests

open NUnit.Framework
open FSharpRefactor.Evaluator.BehaviourChecker

[<TestFixture>]
type BehaviourCheckerModule() =
    let hasChanged source1 source2 = BehaviourHasChanged "f" source1 source2

    [<Test>]
    member this.``Can compile some source code without errors``() =
        let _, results = compile "let f a = 1"
        Assert.IsFalse(results.Errors.HasErrors)

    [<Test>]
    member this.``Can figure out that behaviour is different when both programs are valid``() =
        Assert.IsTrue(hasChanged "let f (a:int) = a" "let f (a:int) = 1")

    [<Test>]
    member this.``Can conjecture out that behaviour is unchanged when both programs are valid``() =
        Assert.IsFalse(hasChanged "let f (a:int) = a" "let f (a:int) = 0 + 5*a - a*4")

    [<Test>]
    member this.``Can figure out that behaviour is unchanged when neither program is valid``() =
        Assert.IsFalse(hasChanged "let f (a:int) = 1+\"a\"" "let f (a:int) = 1.0 + 1")

    [<Test>]
    member this.``Can figure out that behaviour has changed if only one program is valid``() =
        Assert.IsTrue(hasChanged "let f (a:int) = 1 + 1" "let f (a:int) = 1 + 1.0")
