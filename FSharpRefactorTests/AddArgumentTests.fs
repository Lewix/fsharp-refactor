namespace FSharpRefactor.Tests.AddArgumentTests

open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Refactorings.AddArgument

[<TestFixture>]
type AddArgumentModule() =
    let parse (source:string) (filename:string) =
        Ast.Parse (new Project(source, filename)) filename

    [<Test>]
    member this.``Can get changes``() =
        let source = "let f a = 1"
        let expected = "let f b a = 1"
        Assert.AreEqual(expected, Transform ((1,7), "b", "0") (new Project(source, "test.fs")))

    [<Test>]
    member this.``Can check arguments separately``() =
        Assert.IsTrue(IsValid (Some(1,10), None, None) (new Project("let f a = 1", "test.fs")), sprintf "Valid position")
        Assert.IsFalse(IsValid (Some(1,1), None, None) (new Project("1", "test.fs")), "No binding around position")
        Assert.IsFalse(IsValid (Some(1,6), None, None) (new Project("let a,b = 1,2", "test.fs")), "Position not a function")
        //TODO: renaming checks
        //Assert.IsTrue(IsValid (Some(1,10), Some "b", None) "let f a = 1" "test.fs", sprintf "Valid name and position")
        //Assert.IsFalse(IsValid (Some(1,10), Some "a", None) "let f a = 1" "test.fs", sprintf "Invalid name and position")
        //TODO: check default value and name

    [<Test>]
    member this.``Can add an argument to a binding``() =
        let source = "let f a b = a+b"
        let tree = (parse source "test.fs").Value
        let functionIdentifier = "f", mkRange "test.fs" (mkPos 1 4) (mkPos 1 5)
        let expected = "let f c a b = a+b"

        Assert.AreEqual(expected, RunRefactoring (addArgumentToFunctionDeclaration functionIdentifier "c") () (new Project(source, "test.fs")))

    [<Test>]
    member this.``Can add an argument to a value binding``() =
        let source = "let x = 1+2"
        let tree = (parse source "test.fs").Value
        let bindingRange = "x", mkRange "test.fs" (mkPos 1 4) (mkPos 1 5)
        let expected = "let x arg = 1+2"

        Assert.AreEqual(expected, RunRefactoring (addArgumentToFunctionDeclaration bindingRange "arg") () (new Project(source, "test.fs")))

    [<Test>]
    member this.``Can add an argument to a function call``() =
        let project = new Project("f a \"b\" 3", "test.fs")
        let tree = (parse project.CurrentFileContents "test.fs").Value
        let usageRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 1)
        let expected ="(f \"arg\") a \"b\" 3"

        Assert.AreEqual(expected, RunRefactoring (addArgumentToFunctionUsage project "\"arg\"" usageRange) () project)

    [<Test>]
    member this.``Can find all the App nodes calling a certain function``() =
        let source = "(let f a b c = 1 in (f 1 2 3) + ((f 2) 2) + (1 + (2 + (f 3 3 4)))) + (f 1)"
        let tree = (parse source "test.fs").Value
        let bindingRange = "f", mkRange "test.fs" (mkPos 1 5) (mkPos 1 6)
        let functionUsageRanges = findFunctionUsageRanges (new Project(source, "test.fs")) tree bindingRange

        Assert.AreEqual([mkRange "test.fs" (mkPos 1 21) (mkPos 1 22); mkRange "test.fs" (mkPos 1 34) (mkPos 1 35); mkRange "test.fs" (mkPos 1 55) (mkPos 1 56)], functionUsageRanges, sprintf "%A" functionUsageRanges)

    [<Test>]
    member this.``Can find App nodes calling a function without duplicates``() =
        let source = "let f a b = 1 in f f f"
        let tree = (parse source "test.fs").Value
        let bindingRange = "f", mkRange "test.fs" (mkPos 1 4) (mkPos 1 5)
        let functionUsageRanges = findFunctionUsageRanges (new Project(source, "test.fs")) tree bindingRange

        Assert.AreEqual([mkRange "test.fs" (mkPos 1 17) (mkPos 1 18); mkRange "test.fs" (mkPos 1 19) (mkPos 1 20); mkRange "test.fs" (mkPos 1 21) (mkPos 1 22)], functionUsageRanges)

    [<Test>]
    member this.``Can stop finding App nodes when an identifier is redefined``() =
        let source = "let f = 2\n\nlet f = 1 in (fun f -> f)"
        let tree = (parse source "test.fs").Value
        let bindingRange = "f", mkRange "test.fs" (mkPos 3 4) (mkPos 3 5)
        let functionUsageRanges = findFunctionUsageRanges (new Project(source, "test.fs")) tree bindingRange

        Assert.AreEqual([], functionUsageRanges)

    [<Test>]
    member this.``Can add an argument to a function``() =
        let source = "(let f a b c = 1 in (f 1 2 3) + ((f 2) 2) + (1 + (2 + (f 3 3 4)))) + (f 1)"
        let tree = (parse source "test.fs").Value
        let bindingRange = mkRange "test.fs" (mkPos 1 5) (mkPos 1 16)
        let expected = "(let f arg a b c = 1 in ((f 0) 1 2 3) + (((f 0) 2) 2) + (1 + (2 + ((f 0) 3 3 4)))) + (f 1)"

        Assert.AreEqual(expected, Transform ((1,6), "arg", "0") (new Project(source, "test.fs")))

    [<Test>]
    member this.``Can add an argument to a function, even if it is not being applied``() =
        let source = "let f a = 1 in let g a = f in g 1 1"
        let tree = (parse source "test.fs").Value
        let bindingRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 11)
        let expected = "let f arg a = 1 in let g a = (f \"value\") in g 1 1"

        Assert.AreEqual(expected, Transform ((1,5), "arg", "\"value\"") (new Project(source, "test.fs")))

    [<Test>]
    member this.``Can find a sensible default binding range for a given position``() =
        let source = "let f a b =\n  let x = 3+4+5"
        let tree = (parse source "test.fs").Value
        let position1 = mkPos 2 11
        let position2 = mkPos 2 1
        let expected1 = "x = 3+4+5"
        let expected2 = "f a b =\n  let x = 3+4+5"

        Assert.AreEqual(expected1, CodeTransforms.TextOfRange source (Ast.GetRange (Ast.AstNode.Binding (TryFindDefaultBinding (new Project(source, "test.fs")) tree position1).Value)).Value)
        Assert.AreEqual(expected2, CodeTransforms.TextOfRange source (Ast.GetRange (Ast.AstNode.Binding (TryFindDefaultBinding (new Project(source, "test.fs")) tree position2).Value)).Value)

    [<Test>]
    member this.``Cannot add an argument if there is no binding at the given range``() =
        let source = "let f a b = a+b"
        let tree = (parse source "test.fs").Value
        let range = mkRange "test.fs" (mkPos 1 3) (mkPos 1 5)
        let valid = IsValid (Some (1,3), None, Some "0") (new Project(source, "test.fs"))

        Assert.IsFalse(valid, sprintf "Extract function validity was incorrect: %A" valid)                       

    [<Test>]
    member this.``Cannot add an argument if there is already on with the same name``() =
        let source1 = "let f a b = a+b"
        let source2 = "let rec f a b = f(a+b)"
        let tree1 = (parse source1 "test.fs").Value
        let tree2 = (parse source2 "test.fs").Value
        
        let range1 = mkRange "test.fs" (mkPos 1 4) (mkPos 1 15)
        let range2 = mkRange "test.fs" (mkPos 1 8) (mkPos 1 22)

        Assert.AreEqual("let f f a b = a+b", Transform ((1,5), "f", "0") (new Project(source1, "test.fs")))
        Assert.IsFalse(IsValid (Some (1,5), Some "a", Some "0") (new Project(source1, "test.fs")))
        Assert.IsFalse(IsValid (Some (1,9), Some "f", Some "0") (new Project(source2, "test.fs")))
