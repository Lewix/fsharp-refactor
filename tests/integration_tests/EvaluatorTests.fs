module FSharpRefactor.Tests.IntegrationTests.EvaluatorTests

open NUnit.Framework
open FSharpRefactor.Evaluator.BehaviourChecker

[<TestFixture>]
type CompilerModule() =
    [<Test>]
    member this.``Can compile some source code without errors``() =
        let results, _ = compile "let f a = 1"
        Assert.IsFalse(results.Errors.HasErrors)

[<TestFixture>]
type EvaluatorModule() =
    [<Test>]
    member this.``Can rename a random identifier in a piece of code``() =
        let code = "let ident0 ident1 = (fun ident3 -> 1) in ident0 1"
        Assert.AreEqual((code,"let f ident1 = (fun ident3 -> 1) in f 1"),
                        randomRename code "f" 3)
