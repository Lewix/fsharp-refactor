namespace FSharpRefactor.Tests.EngineTests

open System
open NUnit.Framework
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeTransforms
open FSharpRefactor.Engine.RangeAnalysis


[<TestFixture>]
type AstModule() =
    [<Test>]
    member this.``Can instantiate a AstNode from the result of parsing source code``() =
        let source = "let a = 1"
        let rootNode = Ast.Parse source "test.fs"
        let expectNamespace node =
            match node with
                | Some(Ast.AstNode.File(_)) -> true
                | _ -> false
        Assert.IsTrue(expectNamespace rootNode, "Should get a namespace or module from MakeAstNode")

        let binding (node : Ast.AstNode option) =
            Ast.GetChildren(List.head(Ast.GetChildren(node.Value).Value))
        let expectModule (node : Ast.AstNode option) =
            match binding node with
                | Some(Ast.AstNode.ModuleDeclaration(_)::_) -> true
                | _ -> false
        Assert.IsTrue(expectModule rootNode)

[<TestFixture>]
type CodeTransformsModule() =
    [<Test>]
    member this.``Can indent a string``() =
        let indent body = CodeTransforms.Indent body "    "
        Assert.AreEqual("    hello", indent "hello")
        Assert.AreEqual("    \n    ", indent "\n")
        Assert.AreEqual("    line one\n    line two\n    ", indent "line one\nline two\n")
        Assert.AreEqual("aaaaahello", CodeTransforms.Indent "hello" "aaaaa")

    [<Test>]
    member this.``Can remove leading characters from lines if they all the same``() =
        Assert.AreEqual(" Hello\nWorld", CodeTransforms.RemoveLeading ' ' "    Hello\n   World")
        Assert.AreEqual("Hello\nWorld", CodeTransforms.RemoveLeading ' ' "Hello\nWorld")

    [<Test>]
    member this.``Can change the text corresponding to an ast node``() =
        let source = "let a = 1\n\n"
        let tree = (Ast.Parse source "test.fs").Value
        let a = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.Head).Value.Head).Value.Head
        let expected = "let b = 1\n\n"

        Assert.AreEqual(expected, CodeTransforms.ChangeTextOf source [((Ast.GetRange a).Value,"b")])

    [<Test>]
    member this.``Can change the text for two ast nodes``() =
        let source = "\nlet a = 1\nlet b = 2"
        let tree = (Ast.Parse source "test.fs").Value
        let a = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.Head).Value.Head).Value.Head
        let b = Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(Ast.GetChildren(tree).Value.Head).Value.[1]).Value.Head).Value.Head
        let expected = "\nlet b = 1\nlet a2 = 2"

        Assert.AreEqual(expected, CodeTransforms.ChangeTextOf source [((Ast.GetRange a).Value,"b");((Ast.GetRange b).Value,"a2")])

    [<Test>]
    member this.``Can get text at a given range``() =
        let source = "let a = 1+(2+3)+4"
        let expected = "(2+3)"
        let range = mkRange "test.fs" (mkPos 1 10) (mkPos 1 15)
        
        Assert.AreEqual(expected, CodeTransforms.TextOfRange source range)

        let source = "let a = 1+(2\n+3)+4"
        let expected = "(2\n+3)"
        let range = mkRange "test.fs" (mkPos 1 10) (mkPos 2 3)

        Assert.AreEqual(expected, CodeTransforms.TextOfRange source range)

    [<Test>]
    member this.``Can change text for two ranges with same start point, if one is of length 0``() =
        let source = "1"
        let expected = "5+2"
        let range1= mkRange "test.fs" (mkPos 1 0) (mkPos 1 1)
        let range2 = mkRange "test.fs" (mkPos 1 0) (mkPos 1 0)

        Assert.AreEqual(expected, CodeTransforms.ChangeTextOf source [(range2,"5+");(range1,"2")])

    [<Test>]
    member this.``Can compute output declarationIdentifier from input one``() =
        let source = "let a = 1 in let b = 2 in a"
        let expected = "longIdentifier", mkRange "test.fs" (mkPos 1 17) (mkPos 1 31)
        let declarationRange = mkRange "test.fs" (mkPos 1 17) (mkPos 1 18)
        Assert.AreEqual(expected, CodeTransforms.updateIdentifier ("b",declarationRange) "longIdentifier")

    [<Test>]
    member this.``Can create an identifier starting at a given position``() =
        let expected = "longIdentifier", mkRange "test.fs" (mkPos 1 17) (mkPos 1 31)
        Assert.AreEqual(expected, CodeTransforms.createIdentifier (1, 17) "longIdentifier" "test.fs")


[<TestFixture>]
type RangeAnalysisModule() =
    [<Test>]
    member this.``Can find identifier at position``() =
        let filename = "test.fs"
        let source = "let functio a b c = a+b+c in functio 1 2 3"
        let aDeclarationRange = mkRange "test.fs" (mkPos 1 12) (mkPos 1 13)
        let fDeclarationRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 11)

        Assert.AreEqual(Some("a",aDeclarationRange), TryFindIdentifier source filename aDeclarationRange.Start)
        Assert.AreEqual(Some("functio",fDeclarationRange), TryFindIdentifier source filename fDeclarationRange.Start)

    [<Test>]
    member this.``Can find identifier position when identifiers are just next to each other``() =
        let source = "a+b"
        let range = mkRange "test.fs" (mkPos 1 2) (mkPos 1 3)

        Assert.AreEqual(Some("b", range), TryFindIdentifier source "test.fs" range.Start)
    
    [<Test>]
    member this.``Can find the AstNode.Expression corresponding to a range``() =
        let source = "let a = 1+(2+3)+4"
        let tree = (Ast.Parse source "test.fs").Value
        let expressionRange = mkRange "test.fs" (mkPos 1 10) (mkPos 1 15)
        let expression = TryFindExpressionAtRange expressionRange tree

        match expression with
            | Some(Ast.AstNode.Expression(SynExpr.Paren(SynExpr.App(_,_,SynExpr.App(_,_,_,SynExpr.Const(_,_),_),SynExpr.Const(_,_),_),_,_,_))) -> ()
            | _ -> Assert.Fail("The AstNode was not the one for (2+3): " + (sprintf "%A" expression))

    [<Test>]
    member this.``Can find the binding from the binding's range``() =
        let source = "let f a b = a+b"
        let tree = (Ast.Parse source "test.fs").Value
        let bindingRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 15)
        let binding = FindBindingAtRange bindingRange tree

        match binding with
            | SynBinding.Binding(_,_,_,_,_,_,_,SynPat.LongIdent(_,_,_,_,_,_),_,_,_,_) -> ()
            | _ -> Assert.Fail("The AstNode was not the one for the binding 'let f a b = a+b': " + (sprintf "%A" binding))
