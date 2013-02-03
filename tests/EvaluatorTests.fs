module FSharpRefactor.Tests.EvaluatorTests

open System.IO
open System.CodeDom.Compiler
open NUnit.Framework
open FSharpRefactor.Evaluator.BehaviourChecker
open FSharpRefactor.Evaluator.CodeGenerator
open FSharpRefactor.Evaluator.GenerationState

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
[<Category("Evaluation")>]
type CodeGenerationModule() =
    let emptyState = { identifierTypes = Map []; genericTypes = Map []; randomNumbers = seq [] }
    let generateInteger = generateInteger Int 
    let generateIdent = generateIdent Int 
    let generateIdentList = generateIdentList Int 
    let generateExpressionEmpty = generateExpression (Map []) Type.Int 1 

    let getString (s,_) = s

    [<Test>]
    member this.``Can generate identifier and integer terminals``() =
        Assert.AreEqual("10", getString (generateInteger {emptyState with randomNumbers = (seq [110])}))
        Assert.AreEqual("ident10", getString (generateIdent {emptyState with randomNumbers = (seq [110])}))

    [<Test>]
    member this.``Can generate lists of identifiers for functions``() =
        Assert.AreEqual("", getString (generateIdentList {emptyState with randomNumbers = (seq [0;12;34;5])}))
        Assert.AreEqual("ident5", getString (generateIdentList {emptyState with randomNumbers = (seq [1;5])}))
        Assert.AreEqual("ident1 ident5 ident11", getString (generateIdentList {emptyState with randomNumbers = (seq [3;101;5;11;1])}))

    [<Test>]
    member this.``Can generate an expression``() =
        // 0 : int, 1 : ident, 2 : e + e, 3 : (ident ident_list), 4 : let ident_list = e in e
        Assert.AreEqual("1", getString (generateExpressionEmpty {emptyState with randomNumbers = (seq [0;1])}))

        let state = { identifierTypes = (Map ["ident5", Type.Int]); genericTypes = Map []; randomNumbers = seq [] }
        Assert.AreEqual("ident5", getString (generateExpression (Map<int,Type> []) Type.Int 1 {state with randomNumbers = (seq [1;5])}))

        Assert.AreEqual("1 + 2", getString (generateExpressionEmpty {emptyState with randomNumbers = (seq [1;0;1;0;2])}))
        //TODO: Test app and let
        //Assert.AreEqual("(ident0 ident2)",
        //                getString (generateExpression (Map<int,Type> []) Type.Int 1
        //                    (Map ["ident0",Type.Fun(Type.Int,Type.Int);
        //                          "ident1",Type.Int;"ident2",Type.Int])
        //                    (seq [3;0;0;0;1;0])))
        //Assert.AreEqual("(let ident0 = 1 in 2)", getString (generateExpressionEmpty (seq [3;0;0;0;1;0;2])))

    [<Test>]
    member this.``Can generate declared identifier``() =
        let state = { identifierTypes = (Map ["ident0",Type.Int; "ident3",Type.Int; "ident5",Type.Int]); genericTypes = Map []; randomNumbers = seq [] }
        Assert.AreEqual("ident5", getString (generateDeclaredIdent (Map<int,Type> []) Type.Int {state with randomNumbers = (seq [11])}))

    [<Test>]
    member this.``Can avoid using idents which aren't declared``() =
        Assert.AreEqual("(let ident0 = 1 in ident0)", getString (generateExpressionEmpty {emptyState with randomNumbers = (seq [3;0;0;0;1;1;2])}), "Don't use ident2, use ident0 because it's the only available one")

    [<Test>]
    member this.``Can cutoff at a certain depth to avoid exponential growth``() =
        let state = { identifierTypes = (Map ["ident1",Type.Int]); genericTypes = (Map []); randomNumbers = seq [] }
        Assert.AreEqual("1 + 1 + 1 + 1 + 1 + 1", getString (generateExpression (Map<int,Type> []) Type.Int 0 {state with randomNumbers = (seq [2;0;1;2;0;1;2;0;1;2;0;1;2;0;1;2;1;1])}))

    [<Test>]
    member this.``Can generate a declared identifier of a specified type``() =
        let state = { identifierTypes = (Map["ident1",Type.Int;"ident3",Type.Fun(Type.Int,Type.Int)]); genericTypes = (Map []); randomNumbers = seq [] }
        Assert.AreEqual("ident3",
            getString (generateDeclaredIdent (Map<int,Type> []) (Type.Fun(Type.Int,Type.Int)) {state with randomNumbers = (seq [0])}))

    [<Test>]
    member this.``Can compare generic types``() =
        Assert.AreEqual((Map [1,Type.Int], true), typesAreEquivalent (Map<int,Type> []) Type.Int (Generic 1))
        Assert.AreEqual((Map [1,Generic 2], true), typesAreEquivalent (Map<int,Type> []) (Generic 1) (Generic 2))
        Assert.AreEqual((Map [1,Type.Int;2,Type.Int], true), typesAreEquivalent (Map [1,Type.Int;2,Type.Int]) (Generic 2) (Generic 1))
        Assert.AreEqual((Map [1,Type.Int], false), typesAreEquivalent (Map [1,Type.Int]) (Generic 1) (Type.Fun(Type.Int,Type.Int)))
