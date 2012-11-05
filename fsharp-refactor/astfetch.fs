namespace FSharpRefactor.Engine

open System 
open Microsoft.FSharp.Compiler.SourceCodeServices

module ASTFetcher =
    let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty(fun _ -> ()))
    let filename = "/home/lewis/test.fs"
    let source = "let a = 1"
    let options = {ProjectFileName = "/mnt/media/git/university/personalproj/fsharp-refactor/fsharp-refactor/fsharp-refactor.fsproj"
                   ProjectFileNames = [|""|]
                   ProjectOptions = [||]
                   IsIncompleteTypeCheckEnvironment = true
                   UseScriptResolutionRules = false
                   LoadTime = System.DateTime.Now
                   UnresolvedReferences = None}
    let untypedParseInfo = checker.UntypedParse(filename, source, options)
    let parseTree = untypedParseInfo.ParseTree
