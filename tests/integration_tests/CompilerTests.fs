module FSharpRefactor.Tests.IntegrationTests.EvaluatorTests

open NUnit.Framework
open FSharpRefactor.Evaluator.BehaviourChecker

[<TestFixture>]
type CompilerModule() =
    [<Test>]
    member this.``Can compile some source code without errors``() =
        let results, _ = compile "let f a = 1"
        Assert.IsFalse(results.Errors.HasErrors)
