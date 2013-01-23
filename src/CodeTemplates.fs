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
    static member Template isMultiLine =
        if isMultiLine then MultilineFunctionDefinition.Template
        else SingleLineFunctionDefinition.Template
    static member RecRange isMultiLine =
        if isMultiLine then MultilineFunctionDefinition.RecRange
        else SingleLineFunctionDefinition.RecRange
    static member RecTemplate isMultiLine =
        if isMultiLine then MultilineFunctionDefinition.RecTemplate
        else SingleLineFunctionDefinition.RecTemplate
    static member NameRange isMultiLine =
        if isMultiLine then MultilineFunctionDefinition.NameRange
        else SingleLineFunctionDefinition.NameRange
    static member ParameterRange isMultiLine =
        if isMultiLine then MultilineFunctionDefinition.ParameterRange
        else SingleLineFunctionDefinition.ParameterRange
    static member BodyRange isMultiLine =
        if isMultiLine then MultilineFunctionDefinition.BodyRange
        else SingleLineFunctionDefinition.BodyRange
