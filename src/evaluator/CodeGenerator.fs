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

let getTargetTypeExpressionForms targetType state =
    let typeInState t =
        Map.exists (fun _ t -> t = targetType) state

    List.filter snd [ExpressionForm.Integer, targetType = Int;
                     ExpressionForm.Ident, typeInState targetType;
                     ExpressionForm.Addition, targetType = Int;
                     ExpressionForm.Application, true;
                     ExpressionForm.Let, true]
    |> List.map fst

let chooseFrom (elements : list<'a>) randomNumbers =
    elements.[(Seq.head randomNumbers) % (List.length elements)], Seq.skip 1 randomNumbers

let rec generateInteger targetType state (randomNumbers : seq<int>) =
    let integers = List.map string [0..GenerationConfig.IntegerThreshold-1]
    let integer, randomNumbers = chooseFrom integers randomNumbers
    integer, state, randomNumbers

and generateIdent targetType (state : Map<string,Type>) (randomNumbers : seq<int>) =
    let idents = List.map (fun i -> "ident"+(string i)) [0..GenerationConfig.IdentThreshold-1]
    let ident, randomNumbers = chooseFrom idents randomNumbers
    ident, state.Add (ident, targetType), randomNumbers

and generateDeclaredIdent targetType (state : Map<string,Type>) (randomNumbers : seq<int>) =
    let targetTypeIdents =
        Map.filter (fun i t -> t = targetType) state
        |> Map.toList
        |> List.map fst
    if List.isEmpty targetTypeIdents then
        None, state, randomNumbers
    else
        let ident, randomNumbers = chooseFrom targetTypeIdents randomNumbers
        Some ident, state, randomNumbers

and generateList targetType state (randomNumbers : seq<int>) generationFunction lengthThreshold =
    let length, randomNumbers = chooseFrom [0..lengthThreshold] randomNumbers
    if length = 0 then
        "", state, randomNumbers
    elif length = 1 then
        let item, state, randomNumbers = generationFunction state randomNumbers
        item, state, randomNumbers
    else
        let item, state, randomNumbers = generationFunction state randomNumbers
        let items, state, randomNumbers =
            generateList targetType state (Seq.append (seq [length-1]) randomNumbers) generationFunction 
                         lengthThreshold
        item + " " + items, state, randomNumbers

and generateIdentList targetType state (randomNumbers : seq<int>) =
    generateList targetType state randomNumbers (generateIdent targetType) GenerationConfig.IdentListLengthThreshold

and generateExpressionList targetType depth state (randomNumbers : seq<int>) =
    generateList targetType state randomNumbers (generateExpression targetType depth)
                 GenerationConfig.ExpressionListLengthThreshold

and generateExpression targetType depth state (randomNumbers : seq<int>) =
    let terminalExpressionForms = [ExpressionForm.Integer; ExpressionForm.Ident]
    let targetTypeExpressionForms = getTargetTypeExpressionForms targetType state
    let expressionForm, randomNumbers =
        if depth >= GenerationConfig.CutoffDepth then
            chooseFrom terminalExpressionForms randomNumbers
        else
            chooseFrom targetTypeExpressionForms randomNumbers
    let depth = depth+1

    match expressionForm with
        | ExpressionForm.Integer -> generateInteger Int state randomNumbers
        | ExpressionForm.Ident ->
            let i, state, randomNumbers = generateDeclaredIdent targetType state randomNumbers
            if Option.isSome i then
                i.Value, state, randomNumbers
            else
                generateExpression targetType depth state randomNumbers
        | ExpressionForm.Addition ->
            let e1, _, randomNumbers = generateExpression Int depth state randomNumbers
            let e2, _, randomNumbers = generateExpression Int depth state randomNumbers
            sprintf "%s + %s" e1 e2, state, randomNumbers
        | ExpressionForm.Application ->
            //TODO: get the types right
            let expression, _, randomNumbers = generateExpression targetType depth state randomNumbers
            let expressionList, _, randomNumbers = generateExpressionList targetType depth state randomNumbers
            sprintf "(%s %s)" expression expressionList, state, randomNumbers
        | ExpressionForm.Let ->
            //TODO: get the types right
            let ident, inScopeState, randomNumbers = generateIdent targetType state randomNumbers
            let identList, bodyState, randomNumbers = generateIdentList targetType state randomNumbers
            let e1, _, randomNumbers = generateExpression targetType depth bodyState randomNumbers
            let e2, _, randomNumbers = generateExpression targetType depth inScopeState randomNumbers
            if identList = "" then
                sprintf "(let %s = %s in %s)" ident e1 e2, state, randomNumbers
            else
                sprintf "(let %s %s = %s in %s)" ident identList e1 e2, state, randomNumbers
        | _ ->
            let newExpressionForm = (int expressionForm) % GenerationConfig.ExpressionFormsCount 
            generateExpression targetType depth state (Seq.append (seq [newExpressionForm]) randomNumbers)
