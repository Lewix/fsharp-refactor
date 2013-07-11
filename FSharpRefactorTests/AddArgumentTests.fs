namespace FSharpRefactor.Tests.AddArgumentTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Refactorings.AddArgument

[<TestFixture>]
type AddArgumentModule() =
    [<Test>]
    member this.``Can get changes``() =
        let source = "let f a = 1"
        let expected = [(1,1),(1,11),"let f b a = 1"]
        Assert.AreEqual(expected, GetChanges ((1,7), "b", "0") source "test.fs", sprintf "%A" (GetChanges ((1,7), "b", "0") source "test.fs"))

    [<Test>]
    member this.``Can check arguments separately``() =
        Assert.IsTrue(IsValid (Some(1,10), None, None) "let f a = 1" "test.fs", sprintf "Valid position")
        Assert.IsFalse(IsValid (Some(1,1), None, None) "1" "test.fs", "No binding around position")
        Assert.IsFalse(IsValid (Some(1,6), None, None) "let a,b = 1,2" "test.fs", "Position not a function")
        //TODO: renaming checks
        //Assert.IsTrue(IsValid (Some(1,10), Some "b", None) "let f a = 1" "test.fs", sprintf "Valid name and position")
        //Assert.IsFalse(IsValid (Some(1,10), Some "a", None) "let f a = 1" "test.fs", sprintf "Invalid name and position")
        //TODO: check default value and name

    [<Test>]
    member this.``Can add an argument to a binding``() =
        let source = "let f a b = a+b"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 15)
        let expected = "let f c a b = a+b"

        Assert.AreEqual(expected, RunRefactoring (AddArgumentToBinding bindingRange "c") () source)

    [<Test>]
    member this.``Can add an argument to a value binding``() =
        let source = "let x = 1+2"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 11)
        let expected = "let x arg = 1+2"

        Assert.AreEqual(expected, RunRefactoring (AddArgumentToBinding bindingRange "arg") () source)

    [<Test>]
    member this.``Can add an argument to a function call``() =
        let source = "f a \"b\" 3"
        let tree = (Ast.Parse source).Value
        let usageRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 1)
        let expected ="(f \"arg\") a \"b\" 3"

        Assert.AreEqual(expected, RunRefactoring (AddArgumentToFunctionUsage source "\"arg\"" usageRange) () source)

    [<Test>]
    member this.``Can find all the App nodes calling a certain function``() =
        let source = "(let f a b c = 1 in (f 1 2 3) + ((f 2) 2) + (1 + (2 + (f 3 3 4)))) + (f 1)"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "test.fs" (mkPos 1 5) (mkPos 1 16)
        let functionUsageRanges = FindFunctionUsageRanges source tree bindingRange "f"

        Assert.AreEqual([mkRange "test.fs" (mkPos 1 21) (mkPos 1 22); mkRange "test.fs" (mkPos 1 34) (mkPos 1 35); mkRange "test.fs" (mkPos 1 55) (mkPos 1 56)], functionUsageRanges)

    [<Test>]
    member this.``Can find App nodes calling a function without duplicates``() =
        let source = "let f a b = 1 in f f f"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 12)
        let functionUsageRanges = FindFunctionUsageRanges source tree bindingRange "f"

        Assert.AreEqual([mkRange "test.fs" (mkPos 1 17) (mkPos 1 18); mkRange "test.fs" (mkPos 1 19) (mkPos 1 20); mkRange "test.fs" (mkPos 1 21) (mkPos 1 22)], functionUsageRanges)

    [<Test>]
    member this.``Can stop finding App nodes when an identifier is redefined``() =
        let source = "let f = 2\n\nlet f = 1 in (fun f -> f)"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "test.fs" (mkPos 3 4) (mkPos 3 8)
        let functionUsageRanges = FindFunctionUsageRanges source tree bindingRange "f"

        Assert.AreEqual([], functionUsageRanges)

    [<Test>]
    member this.``Can add an argument to a function``() =
        let source = "(let f a b c = 1 in (f 1 2 3) + ((f 2) 2) + (1 + (2 + (f 3 3 4)))) + (f 1)"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "test.fs" (mkPos 1 5) (mkPos 1 16)
        let expected = "(let f arg a b c = 1 in ((f 0) 1 2 3) + (((f 0) 2) 2) + (1 + (2 + ((f 0) 3 3 4)))) + (f 1)"

        Assert.AreEqual(expected, DoAddArgument source tree bindingRange "arg" "0")

    [<Test>]
    member this.``Can add an argument to a function, even if it is not being applied``() =
        let source = "let f a = 1 in let g a = f in g 1 1"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 11)
        let expected = "let f arg a = 1 in let g a = (f \"value\") in g 1 1"

        Assert.AreEqual(expected, DoAddArgument source tree bindingRange "arg" "\"value\"")
        
    [<Test>]
    member this.``Can find the name of a function given its binding's range``() =
        let source = "let f a b = a+b"
        let tree = (Ast.Parse source).Value
        let bindingRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 15)

        Assert.AreEqual("f", findFunctionName source tree bindingRange)

    [<Test>]
    member this.``Can find a sensible default binding range for a given position``() =
        let source = "let f a b =\n  let x = 3+4+5"
        let tree = (Ast.Parse source).Value
        let position1 = mkPos 2 11
        let position2 = mkPos 2 1
        let expected1 = "x = 3+4+5"
        let expected2 = "f a b =\n  let x = 3+4+5"

        Assert.AreEqual(expected1, CodeTransforms.TextOfRange source (DefaultBindingRange source tree position1).Value)
        Assert.AreEqual(expected2, CodeTransforms.TextOfRange source (DefaultBindingRange source tree position2).Value)

    [<Test>]
    member this.``Cannot add an argument if there is no binding at the given range``() =
        let source = "let f a b = a+b"
        let tree = (Ast.Parse source).Value
        let range = mkRange "test.fs" (mkPos 1 3) (mkPos 1 5)
        let valid = CanAddArgument source tree range "0"

        Assert.AreEqual(Invalid("No binding found at the given range"),
                        valid,
                        sprintf "Extract function validity was incorrect: %A" valid)                       

    [<Test>]
    member this.``Cannot add an argument if there is already on with the same name``() =
        let source1 = "let f a b = a+b"
        let source2 = "let rec f a b = f(a+b)"
        let tree1 = (Ast.Parse source1).Value
        let tree2 = (Ast.Parse source2).Value
        
        let range1 = mkRange "test.fs" (mkPos 1 4) (mkPos 1 15)
        let range2 = mkRange "test.fs" (mkPos 1 8) (mkPos 1 22)

        Assert.AreEqual("let f f a b = a+b", DoAddArgument source1 tree1 range1 "f" "0")
        Assert.Throws<RefactoringFailure>(fun () -> ignore (DoAddArgument source1 tree1 range1 "a" "0")) |> ignore
        Assert.Throws<RefactoringFailure>(fun () -> ignore (DoAddArgument source2 tree2 range2 "f" "0")) |> ignore

    [<Test>]
    member this.``Can turn off add argument checks``() =
        let source = "let rec f a = f a"
        let tree = (Ast.Parse source).Value
        let range = mkRange "test.fs" (mkPos 1 8) (mkPos 1 17)

        Assert.AreEqual("let rec f a a = (f 0) a", RunRefactoring (AddArgument false range "a" "0") () source)
