module FSharpRefactor.Evaluator.CodeGenerator

type GenerationConfig =
    static member IntegerThreshold = 100
    static member IdentThreshold = 100
    static member IdentListLengthThreshold = 5
    static member ExpressionFormsCount = 5
    static member ExpressionListLengthThreshold = 5
    static member CutoffDepth = 5

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

and generateList state (randomNumbers : seq<int>) generationFunction lengthThreshold =
    let length = (Seq.head randomNumbers) % (lengthThreshold+1)
    let randomNumbers = Seq.skip 1 randomNumbers
    if length = 0 then
        "", state, randomNumbers
    elif length = 1 then
        let item, state, randomNumbers = generationFunction state randomNumbers
        item, state, randomNumbers
    else
        let item, state, randomNumbers = generationFunction state randomNumbers
        let items, state, randomNumbers =
            generateList state (Seq.append (seq [length-1]) randomNumbers) generationFunction lengthThreshold
        item + " " + items, state, randomNumbers

and generateIdentList state (randomNumbers : seq<int>) =
    generateList state randomNumbers generateIdent GenerationConfig.IdentListLengthThreshold

and generateExpressionList depth state (randomNumbers : seq<int>) =
    generateList state randomNumbers (generateExpression depth) GenerationConfig.ExpressionListLengthThreshold

and generateExpression depth state (randomNumbers : seq<int>) =
    let expressionForm, randomNumbers =
        enum<ExpressionForm>(Seq.head randomNumbers), Seq.skip 1 randomNumbers
    let expressionForm =
        if depth >= GenerationConfig.CutoffDepth then
            enum<ExpressionForm> ((int expressionForm) % 2)
        else
            expressionForm
    let depth = depth+1
    match expressionForm with
        | ExpressionForm.Integer -> generateInteger state randomNumbers
        | ExpressionForm.Ident ->
            let i, state, randomNumbers = generateDeclaredIdent state randomNumbers
            if Option.isSome i then
                i.Value, state, randomNumbers
            else
                generateExpression depth state randomNumbers
        | ExpressionForm.Addition ->
            let e1, _, randomNumbers = generateExpression depth state randomNumbers
            let e2, _, randomNumbers = generateExpression depth state randomNumbers
            sprintf "%s + %s" e1 e2, state, randomNumbers
        | ExpressionForm.Application ->
            let expression, _, randomNumbers = generateExpression depth state randomNumbers
            let expressionList, _, randomNumbers = generateExpressionList depth state randomNumbers
            sprintf "(%s %s)" expression expressionList, state, randomNumbers
        | ExpressionForm.Let ->
            let ident, inScopeState, randomNumbers = generateIdent state randomNumbers
            let identList, bodyState, randomNumbers = generateIdentList state randomNumbers
            let e1, _, randomNumbers = generateExpression depth bodyState randomNumbers
            let e2, _, randomNumbers = generateExpression depth inScopeState randomNumbers
            if identList = "" then
                sprintf "(let %s = %s in %s)" ident e1 e2, state, randomNumbers
            else
                sprintf "(let %s %s = %s in %s)" ident identList e1 e2, state, randomNumbers
        | _ ->
            let newExpressionForm = (int expressionForm) % GenerationConfig.ExpressionFormsCount 
            generateExpression depth state (Seq.append (seq [newExpressionForm]) randomNumbers)
