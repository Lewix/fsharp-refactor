namespace FSharpRefactor.Engine

open System 
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

module ASTFetcher =
    let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty(fun _ -> ()))
    let filename = "/home/lewis/test.fs"
    let options source = checker.GetCheckOptionsFromScriptRoot(filename, source, DateTime.Now, [| |])
    
    let Parse source = checker.UntypedParse(filename, source, options source).ParseTree

    let Height tree = 0
