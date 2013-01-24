module FSharpRefactor.Evaluator.CodeGenerator

type GenerationConfig =
    static member IntegerThreshold = 100
    static member IdentThreshold = 100
    static member IdentListLengthThreshold = 5
    static member ExpressionFormsCount = 5

type ExpressionForm =
    | Integer = 0
    | Ident = 1
    | Addition = 2
    | Application = 3
    | Let = 4

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

//TODO: threshold expression form
and generateExpression (randomNumbers : seq<int>) =
    let expressionForm, randomNumbers =
        enum<ExpressionForm>(Seq.head randomNumbers), Seq.skip 1 randomNumbers
    match expressionForm with
        | ExpressionForm.Integer -> generateInteger randomNumbers
        | ExpressionForm.Ident -> generateIdent randomNumbers
        | ExpressionForm.Addition ->
            let e1, randomNumbers = generateExpression randomNumbers
            let e2, randomNumbers = generateExpression randomNumbers
            sprintf "%s + %s" e1 e2, randomNumbers
        | ExpressionForm.Application ->
            let ident, randomNumbers = generateIdent randomNumbers
            let identList, randomNumbers = generateIdentList randomNumbers
            sprintf "(%s %s)" ident identList, randomNumbers
        | ExpressionForm.Let ->
            let identList, randomNumbers = generateIdentList randomNumbers
            let e1, randomNumbers = generateExpression randomNumbers
            let e2, randomNumbers = generateExpression randomNumbers
            sprintf "(let %s = %s in %s)" identList e1 e2, randomNumbers
        | _ ->
            let newExpressionForm = (int expressionForm) % GenerationConfig.ExpressionFormsCount 
            generateExpression (Seq.append (seq [newExpressionForm]) randomNumbers)
