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

let rec generateInteger state (randomNumbers : seq<int>) =
    let integer =
        GenerationConfig.IntegerThreshold
        |> (%) (Seq.head randomNumbers)
        |> string
    integer, state, Seq.skip 1 randomNumbers

and generateIdent (state : Set<string>) (randomNumbers : seq<int>) =
    let ident =
        GenerationConfig.IdentThreshold
        |> (%) (Seq.head randomNumbers)
        |> string
        |> (+) "ident"
    ident, state.Add ident, Seq.skip 1 randomNumbers

and generateDeclaredIdent (state : Set<string>) (randomNumbers : seq<int>) =
    if state.Count = 0 then
        None, state, randomNumbers
    else
        let ident = Seq.nth ((Seq.head randomNumbers) % state.Count) state
        Some ident, state, Seq.skip 1 randomNumbers

and generateIdentList state (randomNumbers : seq<int>) =
    let remaining = (Seq.head randomNumbers) % GenerationConfig.IdentListLengthThreshold
    let randomNumbers = Seq.skip 1 randomNumbers
    let ident, state, randomNumbers = generateIdent state randomNumbers
    if remaining = 0 then
        ident, state, randomNumbers
    else
        let arguments, state, randomNumbers =
            generateIdentList state (Seq.append (seq [remaining-1]) randomNumbers)
        ident + " " + arguments, state, randomNumbers

and generateExpression state (randomNumbers : seq<int>) =
    let expressionForm, randomNumbers =
        enum<ExpressionForm>(Seq.head randomNumbers), Seq.skip 1 randomNumbers
    match expressionForm with
        | ExpressionForm.Integer -> generateInteger state randomNumbers
        | ExpressionForm.Ident ->
            let i, state, randomNumbers = generateDeclaredIdent state randomNumbers
            if Option.isSome i then
                i.Value, state, randomNumbers
            else
                generateExpression state (Seq.skip 1 randomNumbers)
        | ExpressionForm.Addition ->
            let e1, _, randomNumbers = generateExpression state randomNumbers
            let e2, _, randomNumbers = generateExpression state randomNumbers
            sprintf "%s + %s" e1 e2, state, randomNumbers
        | ExpressionForm.Application ->
        //TODO: use expressions instead of idents
            let ident, _, randomNumbers = generateIdent state randomNumbers
            let identList, _, randomNumbers = generateIdentList state randomNumbers
            sprintf "(%s %s)" ident identList, state, randomNumbers
        | ExpressionForm.Let ->
            let identList, new_state, randomNumbers = generateIdentList state randomNumbers
            let e1, _, randomNumbers = generateExpression new_state randomNumbers
            let e2, _, randomNumbers = generateExpression new_state randomNumbers
            sprintf "(let %s = %s in %s)" identList e1 e2, new_state, randomNumbers
        | _ ->
            let newExpressionForm = (int expressionForm) % GenerationConfig.ExpressionFormsCount 
            generateExpression state (Seq.append (seq [newExpressionForm]) randomNumbers)
