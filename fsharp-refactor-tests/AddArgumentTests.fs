namespace FSharpRefactor.Tests.AddArgumentTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.RefactoringWorkflow
open FSharpRefactor.Refactorings.AddArgument

[<TestFixture>]
type AddArgumentModule() =
    [<Test>]
    member this.``Can add an argument to a binding``() =
        let source = "let f a b = a+b"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 15)
        let expected = "let f c a b = a+b"

        Assert.AreEqual(expected, RunRefactoring (AddArgumentToBinding source tree bindingRange "c"))

    [<Test>]
    member this.``Can add an argument to a function call``() =
        let source = "f a \"b\" 3"
        let tree = (Ast.Parse source).Value
        let usageRange = mkRange "/home/lewis/test.fs" (mkPos 1 0) (mkPos 1 1)
        let expected ="f \"arg\" a \"b\" 3"

        Assert.AreEqual(expected, RunRefactoring (AddArgumentToFunctionUsage source tree usageRange "\"arg\""))

    [<Test>]
    member this.``Can find all the App nodes calling a certain function``() =
        let source = "(let f a b c = 1 in (f 1 2 3) + ((f 2) 2) + (1 + (2 + (f 3 3 4)))) + (f 1)"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "/home/lewis/test.fs" (mkPos 1 5) (mkPos 1 16)
        let functionUsageRanges = FindFunctionUsageRanges source tree bindingRange "f"

        Assert.AreEqual([mkRange "/home/lewis/test.fs" (mkPos 1 21) (mkPos 1 22); mkRange "/home/lewis/test.fs" (mkPos 1 34) (mkPos 1 35); mkRange "/home/lewis/test.fs" (mkPos 1 55) (mkPos 1 56)], functionUsageRanges)

    [<Test>]
    member this.``Can find App nodes calling a function without duplicates``() =
        let source = "let f a b = 1 in f f f"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 12)
        let functionUsageRanges = FindFunctionUsageRanges source tree bindingRange "f"

        Assert.AreEqual([mkRange "/home/lewis/test.fs" (mkPos 1 17) (mkPos 1 18); mkRange "/home/lewis/test.fs" (mkPos 1 19) (mkPos 1 20); mkRange "/home/lewis/test.fs" (mkPos 1 21) (mkPos 1 22)], functionUsageRanges)

    [<Test>]
    member this.``Can add an argument to a function``() =
        let source = "(let f a b c = 1 in (f 1 2 3) + ((f 2) 2) + (1 + (2 + (f 3 3 4)))) + (f 1)"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "/home/lewis/test.fs" (mkPos 1 5) (mkPos 1 16)
        let expected = "(let f arg a b c = 1 in (f 0 1 2 3) + ((f 0 2) 2) + (1 + (2 + (f 0 3 3 4)))) + (f 1)"

        Assert.AreEqual(expected, AddArgument source tree bindingRange "arg" "0")

    [<Test>]
    member this.``Can add an argument to a function, even if it is not being applied``() =
        let source = "let f a = 1 in let g a = f in g 1 1"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 11)
        let expected = "let f arg a = 1 in let g a = f \"value\" in g 1 1"

        Assert.AreEqual(expected, AddArgument source tree bindingRange "arg" "\"value\"")
        
    [<Test>]
    member this.``Can find the name of a function given its binding's range``() =
        let source = "let f a b = a+b"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 15)

        Assert.AreEqual("f", findFunctionName source tree bindingRange)
