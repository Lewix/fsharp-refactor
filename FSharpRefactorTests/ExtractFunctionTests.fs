namespace FSharpRefactor.Tests.ExtractFunctionTests

open System.IO
open NUnit.Framework
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open FSharpRefactor.Engine
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.RangeAnalysis
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.Refactoring
open FSharpRefactor.Refactorings.ExtractFunction

[<TestFixture>]
type ExtractFunctionAnalysisModule() =
    let parse (source:string) (filename:string) =
        Ast.Parse (new Project(source, filename)) filename
    
    let mkRange filename startPos endPos = mkRange (Path.GetFullPath filename) startPos endPos

    [<Test>]
    member this.``Can check arguments separately``() =
        Assert.IsTrue(IsValid (Some ((1,9),(1,12)),None) (new Project("let x = 1+1", "test.fs")), "Valid range")
        Assert.IsFalse(IsValid (Some ((1,3),(1,12)),None) (new Project("let x = 1+1", "test.fs")), "Invalid range")

    [<Test>]
    member this.``Cannot extract a function if there is no expression at expressionRange``() =
        let source = "(2+3)+(3+4)"
        let valid = IsValid (Some ((1,5),(1,7)), None) (new Project(source, "test.fs"))

        Assert.IsFalse(valid)

    [<Test>]
    member this.``Cannot extract an expression which is an application of an infix expression``() =
        let source = "1+2+3"
        let valid = IsValid (Some ((1,1),(1,5)), None) (new Project(source, "test.fs"))

        Assert.IsFalse(valid)

    [<Test>]
    member this.``Cannot extract a function with a taken name``() =
        let source = "let f a = 1 in (f 1)+(1+2)"
        let valid = IsValid (Some ((1,22), (1,27)), Some "f") (new Project(source, "test.fs"))

        Assert.IsFalse(valid)

    [<Test>]
    member this.``Can find a suitable default inScopeTree from expressionRange``() =
        let source1 = "let f a b = 5*a + b"
        let source2 = "let f a b =\n  let x = 5*a + b"
        let expressionRange1 = mkRange "test.fs" (mkPos 1 13) (mkPos 1 16)
        let expressionRange2 = mkRange "test.fs" (mkPos 1 24) (mkPos 1 27)
        let expected1 = "5*a + b"
        let expected2 = "let x = 5*a + b"

        Assert.AreEqual(expected1,
                        CodeTransforms.TextOfRange source1 (Ast.GetRange (defaultInScopeTree (parse source1 "test.fs").Value expressionRange1).Value).Value)
        Assert.AreEqual(expected2,
                        CodeTransforms.TextOfRange source2 (Ast.GetRange (defaultInScopeTree (parse source2 "test.fs").Value expressionRange2).Value).Value)

    [<Test>]
    member this.``Can find a default inScopeTree if there are no bindings in the source``() =
        let source = "1+2+3"
        let tree = (parse source "test.fs").Value
        let expressionRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 5)

        Assert.AreEqual(source,
                        CodeTransforms.TextOfRange source (Ast.GetRange (defaultInScopeTree tree expressionRange).Value).Value)

[<TestFixture>]
type ExtractFunctionTransformModule() =
    let parse (source:string) (filename:string) =
        Ast.Parse (new Project(source, filename)) filename

    let DoExtractFunction source (tree : Ast.AstNode) (inScopeTree : Ast.AstNode) (expressionRange : range) (functionName : string) =
        RunRefactoring (ExtractFunction inScopeTree expressionRange functionName) () source
        
    let mkRange filename startPos endPos = mkRange (Path.GetFullPath filename) startPos endPos
    let files = ["test.fs"]

    [<SetUp>]
    member this.CreateFiles () =
        List.map (fun (f:string) -> new StreamWriter(f)) files
        |> List.map (fun (s:StreamWriter) -> s.Close())
        |> ignore
    [<TearDown>]
    member this.DeleteFiles () =
        List.map File.Delete files
        |> ignore

    [<Test>]
    member this.``Can get changes``() =
        let source = "let f a = 1+1"
        let expected = "let f a = let g = (1+1) in g"
        Assert.AreEqual(expected, Transform (((1,11),(1,14)), "g") (new Project(source, "test.fs")))
        
    [<Test>]
    member this.``Can extract an expression into a value, if it needs no arguments``() =
        let source = "1+3+4+4"
        let expected = "let a = (1+3+4+4) in a"

        Assert.AreEqual(expected, Transform (((1,1),(1,8)), "a") (new Project(source, "test.fs")))

    [<Test>]
    member this.``Can disambiguate between identifiers with the same name``() = 
        let source = "let f a = (let a = 1+a in a+2)"
        let expected = "let f a = let g a = (a+2) in (let a = 1+a in (g a))"

        Assert.AreEqual(expected, Transform (((1,27),(1,30)), "g") (new Project(source, "test.fs")))
    
    [<Test>]
    member this.``Can extract an expression into a function around a LetOrUse expression``() =
        let source = "let c = 1 in let a b = 1+(b+c)+4"
        let expected = "let c = 1 in let f b = ((b+c)) in let a b = 1+(f b)+4"
        let tree = (parse source "test.fs").Value
        let letTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 13) (mkPos 1 32)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 1 25) (mkPos 1 30)

        Assert.AreEqual(expected, DoExtractFunction (new Project(source, "test.fs")) tree letTree expressionRange "f")
        
    [<Test>]
    member this.``Can extract an expression into a function around a Let expression``() =
        let source = "let a b = b+b"
        let expected = "let double b = (b+b) in let a b = (double b)"
        let tree = (parse source "test.fs").Value
        let letTree =
            List.head (FindNodesWithRange (mkRange "test.fs" (mkPos 1 0) (mkPos 1 13)) tree)
        let expressionRange = mkRange "test.fs" (mkPos 1 10) (mkPos 1 13)

        Assert.AreEqual(expected, DoExtractFunction (new Project(source, "test.fs")) tree letTree expressionRange "double")
        
        
    [<Test>]
    member this.``Can indent a multiline function definition before inserting it``() =
        let source = "let f a =\n    match a with\n        | Some(x) -> x\n        | None -> 0"
        let expected = "let f a =\n    let g =\n        match a with\n            | Some(x) -> x\n            | None -> 0\n    g"

        Assert.AreEqual(expected, Transform (((2,5),(4,20)), "g") (new Project(source, "test.fs")))

[<TestFixture>]
type CreateFunctionModule() =
    [<Test>]
    member this.``Can add a function to an expression``() =
        let expected = "let f a b =\n    a+b\n"
        let insertRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 0)
        Assert.AreEqual(expected, RunRefactoring (createFunction "f" ["a";"b"] "a+b" true "" insertRange) () (new Project("", "test.fs")))

    [<Test>]
    member this.``Can add a function with multiple lines in its body to an expression``() =
        let expected = "let f a b =\n    match a,b with\n        | (a,b) -> 1\n"
        let insertRange = mkRange "test.fs" (mkPos 1 0) (mkPos 1 0)
        Assert.AreEqual(expected, RunRefactoring (createFunction "f" ["a";"b"] "match a,b with\n    | (a,b) -> 1" true "" insertRange) () (new Project("", "test.fs")))
