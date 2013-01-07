module FSharpRefactor.Engine.CodeTemplates

open Microsoft.FSharp.Compiler.Range

type FunctionCall =
    static member Template = "(functionName parameters)"
    static member NameRange = mkRange "/home/lewis/test.fs" (mkPos 1 1) (mkPos 1 13)
    static member ParameterRange = mkRange "/home/lewis/test.fs" (mkPos 1 13) (mkPos 1 24)

type FunctionDefinition =
    static member Template = "let functionName parameters = body in "
    static member RecRange = mkRange "/home/lewis/test.fs" (mkPos 1 3) (mkPos 1 3)
    static member RecTemplate = " rec" 
    static member NameRange = mkRange "/home/lewis/test.fs" (mkPos 1 4) (mkPos 1 16)
    static member ParameterRange = mkRange "/home/lewis/test.fs" (mkPos 1 16) (mkPos 1 27)
    static member BodyRange = mkRange "/home/lewis/test.fs" (mkPos 1 30) (mkPos 1 34)
