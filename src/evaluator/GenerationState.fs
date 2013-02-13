module FSharpRefactor.Evaluator.GenerationState

open System

type Type =
    | Int
    | Fun of Type * Type

type Range = {
    startPos : int * int;
    endPos : int * int
    }

type GenerationState = {
    identifierTypes : Map<string, Type>;
    randomNumbers : seq<int>;
    identifiers : List<string>
    }

let chooseFrom (elements : list<'a>) (state : GenerationState) =
    let randomNumbers = state.randomNumbers
    let state = { state with randomNumbers = Seq.skip 1 randomNumbers }
    elements.[(Seq.head randomNumbers) % (List.length elements)], state

let mkRange startPos endPos = { startPos = startPos; endPos = endPos }

let addIdentifierType (identifier, identifierType) state =
    { state with identifierTypes = state.identifierTypes.Add(identifier, identifierType) }

let addIdentifier identifier state =
    { state with identifiers = identifier::state.identifiers }
