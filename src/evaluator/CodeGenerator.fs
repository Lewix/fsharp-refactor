module FSharpRefactor.Evaluator.CodeGenerator

open System
open FSharpRefactor.Evaluator.GenerationState

type GenerationConfig =
    static member IntegerThreshold = 100
    static member ExpressionFormsCount = 5
    static member CutoffDepth = 5

type ExpressionForm =
    | Integer = 0
    | Ident = 1
    | Addition = 2
    | Application = 3
    | Let = 4
    | Lambda = 5

let getTargetTypeExpressionForms targetType state =
    let typeInState t =
        Map.exists (fun _ t -> targetType = t) state.identifierTypes
    let isFunction t =
        match t with Fun(_,_) -> true | _ -> false

    List.filter snd [ExpressionForm.Integer, targetType = Type.Int;
                     ExpressionForm.Ident, typeInState targetType;
                     ExpressionForm.Addition, targetType = Type.Int;
                     ExpressionForm.Application, true;
                     ExpressionForm.Let, true;
                     ExpressionForm.Lambda, isFunction targetType]
    |> List.map fst

let rec generateType state =
    let isFunction, state = chooseFromWeighted [false,4;true,1] state
    if isFunction then
        let t1, state = generateType state
        let t2, state = generateType state
        Fun(t1,t2), state
    else
        Int, state

and generateInteger targetType state =
    let integers = List.map string [0..GenerationConfig.IntegerThreshold-1]
    let integer, state = chooseFrom integers state
    integer, state

and generateIdent targetType (state : GenerationState)  =
    let idents = List.map (fun i -> "ident"+(string i)) [0..state.identThreshold-1]
    let ident, state = chooseFrom idents state
    ident, addIdentifierType state (ident, targetType)

and generateDeclaredIdent targetType (state : GenerationState) =
    let targetTypeIdents =
        Map.filter (fun _ t -> targetType = t) state.identifierTypes
        |> Map.toList
        |> List.map fst
    let ident, state = chooseFrom targetTypeIdents state
    ident, state

and generateApplication targetType depth state =
    let argumentType, state = generateType state
    let e1, state = generateExpression (Type.Fun(argumentType, targetType)) depth state
    let e2, state = generateExpression argumentType depth state
    sprintf "(%s %s)" e1 e2, state

and generateLambda targetType depth state =
    let oldIdentifierTypes = state.identifierTypes

    let argumentType, bodyType =
        match targetType with
            | Fun(t1,t2) -> t1,t2
            | _ -> failwith "Trying to generate a lambda with a non-function type"
    let argument, state = generateIdent argumentType state
    let body, state = generateExpression bodyType depth state

    sprintf "(fun %s -> %s)" argument body, { state with identifierTypes = oldIdentifierTypes }

and generateLet targetType depth state =
    // Save off identifierTypes so that the declared identifier is only in scope in the let expression
    let oldIdentifierTypes = state.identifierTypes

    let argumentType, state = generateType state
    let bodyType, state = generateType state
    let isFunction, state = chooseFrom [true;false] state

    let bodyState, argumentAndBodyString, functionType =
        if isFunction then
            let argumentName, bodyState = generateIdent argumentType state
            let e1, bodyState = generateExpression bodyType depth bodyState
            bodyState, sprintf "%s = %s" argumentName e1, Fun(argumentType,bodyType)
        else
            let e1, bodyState = generateExpression bodyType depth state
            bodyState, sprintf "= %s" e1, bodyType

    let state = { state with randomNumbers = bodyState.randomNumbers }

    let functionName, inScopeState = generateIdent functionType state
    let e2, state = generateExpression targetType depth inScopeState

    let letString = sprintf "(let %s %s in %s)" functionName argumentAndBodyString e2
    letString, { state with identifierTypes = oldIdentifierTypes }

and generateExpression targetType depth (state : GenerationState) =
    let targetTypeExpressionForms = getTargetTypeExpressionForms targetType state
    let terminalExpressionForms = [ExpressionForm.Integer; ExpressionForm.Ident; ExpressionForm.Lambda]

    let expressionForm, state =
        if depth >= GenerationConfig.CutoffDepth then
            let availableTerminalExpressionForms =
                Set terminalExpressionForms
                |> Set.intersect (Set targetTypeExpressionForms)
                |> Set.toList
            chooseFrom availableTerminalExpressionForms state
        elif not (Set.isEmpty (Set.difference (Set targetTypeExpressionForms) (Set terminalExpressionForms))) then
            let availableNonTerminalExpressionForms =
                Set.difference (Set targetTypeExpressionForms) (Set terminalExpressionForms)
                |> Set.toList
            chooseFrom availableNonTerminalExpressionForms state
        else
            chooseFrom targetTypeExpressionForms state
    let depth = depth+1

    match expressionForm with
        | ExpressionForm.Integer ->
            generateInteger targetType state 
        | ExpressionForm.Ident ->
            generateDeclaredIdent targetType state 
        | ExpressionForm.Addition ->
            let e1, state = generateExpression targetType depth state 
            let e2, state = generateExpression targetType depth state 
            sprintf "(%s + %s)" e1 e2, state
        | ExpressionForm.Application ->
            generateApplication targetType depth state 
        | ExpressionForm.Let ->
            generateLet targetType depth state 
        | ExpressionForm.Lambda ->
            generateLambda targetType depth state
        | _ ->
            let newExpressionForm = (int expressionForm) % GenerationConfig.ExpressionFormsCount 
            generateExpression targetType depth state

and generateEntryPoint functionName (state : GenerationState) =
    let ident, state = generateIdent Int state
    let e, _ = generateExpression Int 1 state
    sprintf "let %s (%s:int) = %s" functionName ident, e

let random = new Random()
let defaultType = Int
let defaultState = { identifierTypes = Map []; randomNumbers = Seq.initInfinite (fun _ -> random.Next()); identThreshold = 5 }
