module FSharpRefactor.Evaluator.CodeGenerator

open System
open FSharpRefactor.Evaluator.GenerationState

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

let getTargetTypeExpressionForms targetType state =
    let typeInState t =
        Map.exists (fun _ t -> fst (typesAreEquivalent state targetType t)) state.identifierTypes

    List.filter snd [ExpressionForm.Integer, fst(typesAreEquivalent state targetType Type.Int);
                     ExpressionForm.Ident, typeInState targetType;
                     ExpressionForm.Addition, fst(typesAreEquivalent state targetType Type.Int);
                     ExpressionForm.Application, true;
                     ExpressionForm.Let, true]
    |> List.map fst

let generateGeneric state =
    let unusedGenerics =
        Set (usedGenerics state)
        |> Set.difference (Set [0..GenerationConfig.GenericTypeThreshold])
        |> Set.toList
    let genericNum, state = chooseFrom unusedGenerics state
    Type.Generic genericNum, state

let rec generateInteger targetType state =
    let integers = List.map string [0..GenerationConfig.IntegerThreshold-1]
    let integer, state = chooseFrom integers state
    integer, state

and generateIdent targetType (state : GenerationState)  =
    let idents = List.map (fun i -> "ident"+(string i)) [0..GenerationConfig.IdentThreshold-1]
    let ident, state = chooseFrom idents state
    ident, addIdentifierType state (ident, targetType)

and generateDeclaredIdent targetType (state : GenerationState) =
    let targetTypeIdents =
        Map.filter (fun _ t -> fst (typesAreEquivalent state t targetType)) state.identifierTypes
        |> Map.toList
        |> List.map fst
    let ident, state = chooseFrom targetTypeIdents state
    ident, snd (typesAreEquivalent state (state.identifierTypes.[ident]) targetType)

and generateList targetType state  generationFunction lengthThreshold =
    let length, state = chooseFrom [0..lengthThreshold] state
    if length = 0 then
        "", state
    elif length = 1 then
        let item, state = generationFunction state 
        item, state
    else
        let item, state = generationFunction state
        let items, state =
            let state = { state with randomNumbers = (Seq.append (seq [length-1]) state.randomNumbers) }
            generateList targetType state generationFunction 
                         lengthThreshold
        item + " " + items, state

and generateIdentList targetType state =
    generateList targetType state (generateIdent targetType) GenerationConfig.IdentListLengthThreshold

and generateApplication targetType depth state =
    let argumentType, state = generateGeneric state
    let e1, state =
        generateExpression (Type.Fun(argumentType, targetType)) depth state
    let e2, state = generateExpression argumentType depth state
    sprintf "(%s %s)" e1 e2, state

and generateLet targetType depth state =
    let identList, bodyState = generateIdentList targetType state
    let e1, state = generateExpression targetType depth bodyState
    let ident, inScopeState = generateIdent targetType state
    let e2, state = generateExpression targetType depth inScopeState
    let letString =
        if identList = "" then
            sprintf "(let %s = %s in %s)" ident e1 e2
        else
            sprintf "(let %s %s = %s in %s)" ident identList e1 e2
    letString, state

and generateExpression targetType depth (state : GenerationState) =
    let terminalExpressionForms = [ExpressionForm.Integer; ExpressionForm.Ident]
    let targetTypeExpressionForms = getTargetTypeExpressionForms targetType state
    let expressionForm, state =
        if depth >= GenerationConfig.CutoffDepth then
            chooseFrom terminalExpressionForms state
        else
            chooseFrom targetTypeExpressionForms state
    let depth = depth+1

    match expressionForm with
        | ExpressionForm.Integer ->
            generateInteger Type.Int state 
        | ExpressionForm.Ident ->
            generateDeclaredIdent targetType state 
        | ExpressionForm.Addition ->
            let e1, state = generateExpression Type.Int depth state 
            let e2, state = generateExpression Type.Int depth state 
            sprintf "%s + %s" e1 e2, state
        | ExpressionForm.Application ->
            generateApplication targetType depth state 
        | ExpressionForm.Let ->
            //TODO: get the types right
            generateLet targetType depth state 
        | _ ->
            let newExpressionForm = (int expressionForm) % GenerationConfig.ExpressionFormsCount 
            generateExpression targetType depth state
