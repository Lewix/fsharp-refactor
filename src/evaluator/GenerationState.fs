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
