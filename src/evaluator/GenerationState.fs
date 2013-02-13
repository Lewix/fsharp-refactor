module FSharpRefactor.Evaluator.GenerationState

open System

type Type =
    | Int
    | Fun of Type * Type

type Pos = int * int

type Range = {
    startPos : Pos;
    endPos : Pos
    }

type GenerationState = {
    identifierTypes : Map<string, Type>;
    randomNumbers : seq<int>;
    }

let chooseFrom (elements : list<'a>) (state : GenerationState) =
    let randomNumbers = state.randomNumbers
    let state = { state with randomNumbers = Seq.skip 1 randomNumbers }
    elements.[(Seq.head randomNumbers) % (List.length elements)], state

let addIdentifierType state (identifier, identifierType) =
    { state with identifierTypes = state.identifierTypes.Add(identifier, identifierType) }

let mkRange startPos endPos = { startPos = startPos; endPos = endPos }
