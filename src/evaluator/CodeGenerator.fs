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

type Type =
    | Int
    | Fun of Type * Type

let rec generateInteger targetType state (randomNumbers : seq<int>) =
    let integer =
        GenerationConfig.IntegerThreshold
        |> (%) (Seq.head randomNumbers)
        |> string
    integer, state, Seq.skip 1 randomNumbers

and generateIdent targetType (state : Map<string,Type>) (randomNumbers : seq<int>) =
    let ident =
        GenerationConfig.IdentThreshold
        |> (%) (Seq.head randomNumbers)
        |> string
        |> (+) "ident"
    ident, state.Add (ident, targetType), Seq.skip 1 randomNumbers

and generateDeclaredIdent targetType (state : Map<string,Type>) (randomNumbers : seq<int>) =
    let targetTypeIdents = Map.filter (fun i t -> t = targetType) state
    if targetTypeIdents.Count = 0 then
        None, targetTypeIdents, randomNumbers
    else
        let ident, _ = Seq.nth ((Seq.head randomNumbers) % targetTypeIdents.Count) (Map.toSeq targetTypeIdents)
        Some ident, targetTypeIdents, Seq.skip 1 randomNumbers

and generateList targetType state (randomNumbers : seq<int>) generationFunction lengthThreshold =
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
            generateList Int state (Seq.append (seq [length-1]) randomNumbers) generationFunction lengthThreshold
        item + " " + items, state, randomNumbers

and generateIdentList targetType state (randomNumbers : seq<int>) =
    generateList Int state randomNumbers (generateIdent Int) GenerationConfig.IdentListLengthThreshold

and generateExpressionList targetType depth state (randomNumbers : seq<int>) =
    generateList Int state randomNumbers (generateExpression Int depth) GenerationConfig.ExpressionListLengthThreshold

and generateExpression targetType depth state (randomNumbers : seq<int>) =
    let expressionForm, randomNumbers =
        enum<ExpressionForm>(Seq.head randomNumbers), Seq.skip 1 randomNumbers
    let expressionForm =
        if depth >= GenerationConfig.CutoffDepth then
            enum<ExpressionForm> ((int expressionForm) % 2)
        else
            expressionForm
    let depth = depth+1
    match expressionForm with
        | ExpressionForm.Integer -> generateInteger Int state randomNumbers
        | ExpressionForm.Ident ->
            let i, state, randomNumbers = generateDeclaredIdent Int state randomNumbers
            if Option.isSome i then
                i.Value, state, randomNumbers
            else
                generateExpression Int depth state randomNumbers
        | ExpressionForm.Addition ->
            let e1, _, randomNumbers = generateExpression Int depth state randomNumbers
            let e2, _, randomNumbers = generateExpression Int depth state randomNumbers
            sprintf "%s + %s" e1 e2, state, randomNumbers
        | ExpressionForm.Application ->
            let expression, _, randomNumbers = generateExpression Int depth state randomNumbers
            let expressionList, _, randomNumbers = generateExpressionList Int depth state randomNumbers
            sprintf "(%s %s)" expression expressionList, state, randomNumbers
        | ExpressionForm.Let ->
            let ident, inScopeState, randomNumbers = generateIdent Int state randomNumbers
            let identList, bodyState, randomNumbers = generateIdentList int state randomNumbers
            let e1, _, randomNumbers = generateExpression Int depth bodyState randomNumbers
            let e2, _, randomNumbers = generateExpression Int depth inScopeState randomNumbers
            if identList = "" then
                sprintf "(let %s = %s in %s)" ident e1 e2, state, randomNumbers
            else
                sprintf "(let %s %s = %s in %s)" ident identList e1 e2, state, randomNumbers
        | _ ->
            let newExpressionForm = (int expressionForm) % GenerationConfig.ExpressionFormsCount 
            generateExpression Int depth state (Seq.append (seq [newExpressionForm]) randomNumbers)
