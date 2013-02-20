module FSharpRefactor.Evaluator.GenerationState

open System

type Type =
    | Int
    | Fun of Type * Type

type GenerationState = {
    identifierTypes : Map<string, Type>;
    randomNumbers : seq<int>;
    identThreshold : int
    }

let chooseFrom (elements : list<'a>) (state : GenerationState) =
    let randomNumbers = state.randomNumbers
    let state = { state with randomNumbers = Seq.skip 1 randomNumbers }
    elements.[(Seq.head randomNumbers) % (List.length elements)], state

let chooseFromWeighted (elements : list<'a * int>) (state : GenerationState) =
    let expandedElements = List.collect (fun (i,w) -> List.replicate w i) elements
    chooseFrom expandedElements state

let addIdentifierType state (identifier, identifierType) =
    { state with identifierTypes = state.identifierTypes.Add(identifier, identifierType) }
