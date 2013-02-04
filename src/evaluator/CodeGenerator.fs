module FSharpRefactor.Evaluator.CodeGenerator

open System
open FSharpRefactor.Evaluator.GenerationState

type GenerationConfig =
    static member IntegerThreshold = 100
    static member IdentThreshold = 100
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

and generateApplication targetType depth state =
    let argumentType, state = generateGeneric state
    let e1, state =
        generateExpression (Type.Fun(argumentType, targetType)) depth state
    let e2, state = generateExpression argumentType depth state
    sprintf "(%s %s)" e1 e2, state

and generateLet targetType depth state =
    let argumentType, state = generateGeneric state
    let bodyType, state = generateGeneric state
    let isFunction, state = chooseFrom [true;false] state

    let bodyState, argumentAndBodyString, functionType =
        if isFunction then
            let argumentName, bodyState = generateIdent argumentType state
            let e1, bodyState = generateExpression bodyType depth bodyState
            bodyState, sprintf "%s = %s" argumentName e1, Fun(argumentType,bodyType)
        else
            let e1, bodyState = generateExpression bodyType depth state
            bodyState, sprintf "= %s" e1, bodyType

    let state = { state with randomNumbers = bodyState.randomNumbers; genericTypes = bodyState.genericTypes }

    let functionName, inScopeState = generateIdent functionType state
    let e2, state = generateExpression targetType depth inScopeState

    let letString = sprintf "(let %s %s in %s)" functionName argumentAndBodyString e2
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
            generateLet targetType depth state 
        | _ ->
            let newExpressionForm = (int expressionForm) % GenerationConfig.ExpressionFormsCount 
            generateExpression targetType depth state
