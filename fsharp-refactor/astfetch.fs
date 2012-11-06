namespace FSharpRefactor.Engine

open System 
open Microsoft.FSharp.Compiler.SourceCodeServices

module ASTFetcher =
    let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty(fun _ -> ()))
    let filename = "/home/lewis/test.fs"
    let source = "let a = 1"
    let options = checker.GetCheckOptionsFromScriptRoot(filename, source, DateTime.Now, [| |])
    let untypedParseInfo = checker.UntypedParse(filename, source, options)
    let parseTree = untypedParseInfo.ParseTree
    
    let Parse source = checker.UntypedParse(filename, source, options).ParseTree
