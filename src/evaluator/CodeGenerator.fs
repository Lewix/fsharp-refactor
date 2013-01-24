module FSharpRefactor.Evaluator.CodeGenerator

type GenerationConfig =
    static member IntegerThreshold = 100
    static member IdentThreshold = 100
    static member IdentListLengthThreshold = 5

let rec generateInteger (randomNumbers : seq<int>) =
    let integer =
        GenerationConfig.IntegerThreshold
        |> (%) (Seq.head randomNumbers)
        |> string
    integer, Seq.skip 1 randomNumbers

and generateIdent (randomNumbers : seq<int>) =
    let ident =
        GenerationConfig.IdentThreshold
        |> (%) (Seq.head randomNumbers)
        |> string
        |> (+) "ident"
    ident, Seq.skip 1 randomNumbers

and generateIdentList (randomNumbers : seq<int>) =
    let remaining = (Seq.head randomNumbers) % GenerationConfig.IdentListLengthThreshold
    let randomNumbers = Seq.skip 1 randomNumbers
    let ident, randomNumbers = generateIdent randomNumbers
    if remaining = 0 then
        ident, randomNumbers
    else
        let arguments, randomNumbers = generateIdentList (Seq.append (seq [remaining-1]) randomNumbers)
        ident + " " + arguments, randomNumbers
