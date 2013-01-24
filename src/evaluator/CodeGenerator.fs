module FSharpRefactor.Evaluator.CodeGenerator

type GenerationConfig =
    static member IntegerThreshold = 100
    static member IdentThreshold = 100

let generateInteger (randomNumbers : seq<int>) =
    GenerationConfig.IntegerThreshold
    |> (%) (Seq.head randomNumbers)
    |> string

let generateIdent (randomNumbers : seq<int>) =
    GenerationConfig.IdentThreshold
    |> (%) (Seq.head randomNumbers)
    |> string
    |> (+) "ident"
