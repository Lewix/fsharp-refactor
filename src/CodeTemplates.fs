module FSharpRefactor.Engine.CodeTemplates

open Microsoft.FSharp.Compiler.Range

type FunctionCall =
    static member Template = "(functionName parameters)"
    static member NameRange = mkRange "test.fs" (mkPos 1 1) (mkPos 1 13)
    static member ParameterRange = mkRange "test.fs" (mkPos 1 13) (mkPos 1 24)

type SingleLineFunctionDefinition =
    static member Template = "let functionName parameters = body in "
    static member RecRange = mkRange "test.fs" (mkPos 1 3) (mkPos 1 3)
    static member RecTemplate = " rec" 
    static member NameRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 16)
    static member ParameterRange = mkRange "test.fs" (mkPos 1 16) (mkPos 1 27)
    static member BodyRange = mkRange "test.fs" (mkPos 1 30) (mkPos 1 34)

type MultilineFunctionDefinition =
    static member Template = "let functionName parameters =\nbody\n"
    static member RecRange = mkRange "test.fs" (mkPos 1 3) (mkPos 1 3)
    static member RecTemplate = " rec" 
    static member NameRange = mkRange "test.fs" (mkPos 1 4) (mkPos 1 16)
    static member ParameterRange = mkRange "test.fs" (mkPos 1 16) (mkPos 1 27)
    static member BodyRange = mkRange "test.fs" (mkPos 2 0) (mkPos 2 4)

type FunctionDefinition =
    static member Template lines =
        if lines = 1 then SingleLineFunctionDefinition.Template
        else MultilineFunctionDefinition.Template
    static member RecRange lines =
        if lines = 1 then SingleLineFunctionDefinition.RecRange
        else MultilineFunctionDefinition.RecRange
    static member RecTemplate lines =
        if lines = 1 then SingleLineFunctionDefinition.RecTemplate
        else MultilineFunctionDefinition.RecTemplate
    static member NameRange lines =
        if lines = 1 then SingleLineFunctionDefinition.NameRange
        else MultilineFunctionDefinition.NameRange
    static member ParameterRange lines =
        if lines = 1 then SingleLineFunctionDefinition.ParameterRange
        else MultilineFunctionDefinition.ParameterRange
    static member BodyRange lines =
        if lines = 1 then SingleLineFunctionDefinition.BodyRange
        else MultilineFunctionDefinition.BodyRange
