module FSharpRefactor.Tests.EvaluatorTests

open System.IO
open System.CodeDom.Compiler
open NUnit.Framework
open FSharpRefactor.Evaluator.BehaviourChecker
open FSharpRefactor.Evaluator.CodeGenerator

[<TestFixture>]
type BehaviourCheckerModule() =
    let hasChanged resultsAndAssembly1 resultsAndAssembly2 =
        resultsBehaviourHasChanged "f" resultsAndAssembly1 resultsAndAssembly2

    let noErrors = new CompilerResults(new TempFileCollection())
    let errors = new CompilerResults(new TempFileCollection())
    do
        ignore (errors.Errors.Add(new CompilerError()))

    [<Test>]
    member this.``Can compile some source code without errors``() =
        let results, _ = compile "let f a = 1"
        Assert.IsFalse(results.Errors.HasErrors)

    [<Test>]
    member this.``Can figure out that behaviour is different when both programs are valid``() =
        Assert.IsTrue(hasChanged (noErrors, "../tests/compiler_tests/success1.dll")
                                 (noErrors, "../tests/compiler_tests/success2.dll"))

    [<Test>]
    member this.``Can conjecture out that behaviour is unchanged when both programs are valid``() =
        Assert.IsFalse(hasChanged (noErrors, "../tests/compiler_tests/success1.dll")
                                 (noErrors, "../tests/compiler_tests/success1b.dll"))

    [<Test>]
    member this.``Can figure out that behaviour is unchanged when neither program is valid``() =
        Assert.IsFalse(hasChanged (errors, "") (errors, "doesnt/exist"))

    [<Test>]
    member this.``Can figure out that behaviour has changed if only one program is valid``() =
        Assert.IsTrue(hasChanged (noErrors, "../mnt/media/git/university/personalproj/fsharp-refactor/tests/compiler_tests/success1.dll") (errors, ""))

[<TestFixture>]
type CodeGenerationModule() =
    [<Test>]
    member this.``Can generate identifier and integer terminals``() =
        Assert.AreEqual("10", fst (generateInteger (seq [110])))
        Assert.AreEqual("ident10", fst (generateIdent (seq [110])))

    [<Test>]
    member this.``Can generate lists if identifiers for functions``() =
        Assert.AreEqual("ident5", fst (generateIdentList (seq [0;5])))
        Assert.AreEqual("ident1 ident5 ident11", fst (generateIdentList (seq [2;101;5;11;1])))
