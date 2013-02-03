module FSharpRefactor.Evaluator.GenerationState

type Type =
    | Int
    | Fun of Type * Type
    | Generic of int

type GenerationState = {
    identifierTypes : Map<string, Type>;
    genericTypes : Map<int, Type>;
    randomNumbers : seq<int>
    }

let chooseFrom (elements : list<'a>) (state : GenerationState) =
    let randomNumbers = state.randomNumbers
    let state = { state with randomNumbers = Seq.skip 1 randomNumbers }
    elements.[(Seq.head randomNumbers) % (List.length elements)], state

let addIdentifierType state (identifier, identifierType) =
    { state with identifierTypes = state.identifierTypes.Add(identifier, identifierType) }

let rec typesAreEquivalent state t1 t2 =
    let genericTypes = state.genericTypes
    match t1,t2 with
        | Type.Generic i, Type.Generic j ->
            match genericTypes.ContainsKey i, genericTypes.ContainsKey j with
                | true, true -> typesAreEquivalent state genericTypes.[i] genericTypes.[j]
        | Type.Generic _, _ | _, Type.Generic _ -> genericTypes, true
        | Type.Int, Type.Int -> genericTypes, true
        | Type.Fun(ta1,ta2),Type.Fun(tb1,tb2) ->
            genericTypes, snd (typesAreEquivalent state ta1 ta2) && snd (typesAreEquivalent state tb1 tb2)
        | _ -> genericTypes, false

