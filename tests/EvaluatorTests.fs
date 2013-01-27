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
    let generateInteger = generateInteger (Set<string> [])
    let generateIdent = generateIdent (Set<string> [])
    let generateIdentList = generateIdentList (Set<string> [])
    let generateExpressionEmpty = generateExpression (Set<string> [])

    let getString (s,_,_) = s

    [<Test>]
    member this.``Can generate identifier and integer terminals``() =
        Assert.AreEqual("10", getString (generateInteger (seq [110])))
        Assert.AreEqual("ident10", getString (generateIdent (seq [110])))

    [<Test>]
    member this.``Can generate lists of identifiers for functions``() =
        Assert.AreEqual("", getString (generateIdentList (seq [0;12;34;5])))
        Assert.AreEqual("ident5", getString (generateIdentList (seq [1;5])))
        Assert.AreEqual("ident1 ident5 ident11", getString (generateIdentList (seq [3;101;5;11;1])))

    [<Test>]
    member this.``Can generate an expression``() =
        // 0 : int, 1 : ident, 2 : e + e, 3 : (ident ident_list), 4 : let ident_list = e in e
        Assert.AreEqual("1", getString (generateExpressionEmpty (seq [0;1])))
        Assert.AreEqual("ident5", getString (generateExpression (Set ["ident5"]) (seq [1;5])))
        Assert.AreEqual("1 + 2", getString (generateExpressionEmpty (seq [2;0;1;0;2])))
        Assert.AreEqual("(ident0 ident2 ident3)",
                        getString (generateExpression 
                            (Set ["ident0";"ident1";"ident2";"ident3"]) (seq [3;1;0;2;1;2;1;3])))
        Assert.AreEqual("(let ident0 = 1 in 2)", getString (generateExpressionEmpty (seq [4;1;0;0;1;0;2])))
        Assert.AreEqual("1", getString (generateExpressionEmpty (seq [10;1])))

    [<Test>]
    member this.``Can generate a list of expressions``() =
        Assert.AreEqual("1 2 3", getString (generateExpressionList (Set<string> []) (seq [3;0;1;0;2;0;3])))
        Assert.AreEqual("ident2", getString (generateExpressionList (Set<string> ["ident2"]) (seq [1;1;3])))

    [<Test>]
    member this.``Can generate declared identifier``() =
        Assert.AreEqual(Some "ident5", getString (generateDeclaredIdent (Set ["ident0"; "ident3"; "ident5"])
                                                                  (seq [11])))
        Assert.AreEqual(None, getString (generateDeclaredIdent (Set []) (seq [5])))

    [<Test>]
    member this.``Can avoid using idents which aren't declared``() =
        Assert.AreEqual("(let ident0 = 1 in ident0)", getString (generateExpressionEmpty (seq [4;1;0;0;1;1;2])),
                        "Don't use ident2, use ident0 because it's the only available one")
        Assert.AreEqual("(let ident0 = 1 in ident0)", getString (generateExpressionEmpty (seq [4;1;0;1;0;1;1;5])),
                        "If a number indicates an ident should be used when state is empty, just discard that number")
