module FSharpRefactor.Evaluator.CodeGenerator

open System

type GenerationConfig =
    static member IntegerThreshold = 100
    static member IdentThreshold = 100
    static member IdentListLengthThreshold = 5
    static member GenericTypeThreshold = 10
    static member ExpressionFormsCount = 5
    static member CutoffDepth = 5

type ExpressionForm =
    | Integer = 0
    | Ident = 1
    | Addition = 2
    | Application = 3
    | Let = 4

type Type =
    | Int
    | Generic of int
    | Fun of Type * Type

let rec typesAreEquivalent (genericTypes : Map<int,Type>) t1 t2 =
    match t1,t2 with
        | Generic i, Generic j ->
            match genericTypes.ContainsKey i, genericTypes.ContainsKey j with
                | true, true -> typesAreEquivalent genericTypes genericTypes.[i] genericTypes.[j]
                | true, false -> typesAreEquivalent genericTypes genericTypes.[i] (Generic j)
                | false, true -> typesAreEquivalent genericTypes (Generic i) genericTypes.[j]
                | false, false -> genericTypes, true
        | Generic _, _ | _, Generic _ -> genericTypes, true
        | Int, Int -> genericTypes, true
        | Fun(ta1,ta2),Fun(tb1,tb2) ->
            genericTypes, snd (typesAreEquivalent genericTypes ta1 ta2) && snd (typesAreEquivalent genericTypes tb1 tb2)
        | _ -> genericTypes, false

let getTargetTypeExpressionForms genericTypes targetType state =
    let typeInState t =
        Map.exists (fun _ t -> snd (typesAreEquivalent genericTypes targetType t)) state

    List.filter snd [ExpressionForm.Integer, snd(typesAreEquivalent genericTypes targetType Int);
                     ExpressionForm.Ident, typeInState targetType;
                     ExpressionForm.Addition, snd(typesAreEquivalent genericTypes targetType Int);
                     ExpressionForm.Application, true;
                     ExpressionForm.Let, true]
    |> List.map fst

let chooseFrom (elements : list<'a>) randomNumbers =
    elements.[(Seq.head randomNumbers) % (List.length elements)], Seq.skip 1 randomNumbers

let generateGeneric state randomNumbers =
    //TODO: distinguish used and unused generic types
    let genericNum, randomNumbers = chooseFrom [0..GenerationConfig.GenericTypeThreshold] randomNumbers
    Generic genericNum, randomNumbers

let rec generateInteger targetType state (randomNumbers : seq<int>) =
    let integers = List.map string [0..GenerationConfig.IntegerThreshold-1]
    let integer, randomNumbers = chooseFrom integers randomNumbers
    integer, state, randomNumbers

and generateIdent targetType (state : Map<string,Type>) (randomNumbers : seq<int>) =
    let idents = List.map (fun i -> "ident"+(string i)) [0..GenerationConfig.IdentThreshold-1]
    let ident, randomNumbers = chooseFrom idents randomNumbers
    ident, state.Add (ident, targetType), randomNumbers

and generateDeclaredIdent genericTypes targetType (state : Map<string,Type>) (randomNumbers : seq<int>) =
    let targetTypeIdents =
        Map.filter (fun i t -> snd (typesAreEquivalent genericTypes t targetType)) state
        |> Map.toList
        |> List.map fst
    let ident, randomNumbers = chooseFrom targetTypeIdents randomNumbers
    ident, state, randomNumbers

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

and generateApplication genericTypes targetType depth state (randomNumbers : seq<int>) =
    let argumentType, randomNumbers = generateGeneric state randomNumbers
    let e1, _, randomNumbers =
        generateExpression genericTypes (Fun(argumentType, targetType)) depth state randomNumbers
    let e2, _, randomNumbers = generateExpression genericTypes argumentType depth state randomNumbers
    sprintf "(%s %s)" e1 e2, state, randomNumbers

and generateLet genericTypes targetType depth state (randomNumbers : seq<int>) =
    let ident, inScopeState, randomNumbers = generateIdent targetType state randomNumbers
    let identList, bodyState, randomNumbers = generateIdentList targetType state randomNumbers
    let e1, _, randomNumbers = generateExpression genericTypes targetType depth bodyState randomNumbers
    let e2, _, randomNumbers = generateExpression genericTypes targetType depth inScopeState randomNumbers
    let letString =
        if identList = "" then
            sprintf "(let %s = %s in %s)" ident e1 e2
        else
            sprintf "(let %s %s = %s in %s)" ident identList e1 e2
    letString, state, randomNumbers

and generateExpression genericTypes targetType depth state (randomNumbers : seq<int>) =
    let terminalExpressionForms = [ExpressionForm.Integer; ExpressionForm.Ident]
    let targetTypeExpressionForms = getTargetTypeExpressionForms genericTypes targetType state
    let expressionForm, randomNumbers =
        if depth >= GenerationConfig.CutoffDepth then
            chooseFrom terminalExpressionForms randomNumbers
        else
            chooseFrom targetTypeExpressionForms randomNumbers
    let depth = depth+1

    match expressionForm with
        | ExpressionForm.Integer ->
            generateInteger Int state randomNumbers
        | ExpressionForm.Ident ->
            generateDeclaredIdent genericTypes targetType state randomNumbers
        | ExpressionForm.Addition ->
            let e1, _, randomNumbers = generateExpression genericTypes Int depth state randomNumbers
            let e2, _, randomNumbers = generateExpression genericTypes Int depth state randomNumbers
            sprintf "%s + %s" e1 e2, state, randomNumbers
        | ExpressionForm.Application ->
            generateApplication genericTypes targetType depth state randomNumbers
        | ExpressionForm.Let ->
            //TODO: get the types right
            generateLet genericTypes targetType depth state randomNumbers
        | _ ->
            let newExpressionForm = (int expressionForm) % GenerationConfig.ExpressionFormsCount 
            generateExpression genericTypes targetType depth state (Seq.append (seq [newExpressionForm]) randomNumbers)
