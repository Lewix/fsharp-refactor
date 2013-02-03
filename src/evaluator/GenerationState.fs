module FSharpRefactor.Evaluator.GenerationState

open System

type Type =
    | Int
    | Fun of Type * Type
    | Generic of int

type GenerationState = {
    identifierTypes : Map<string, Type>;
    genericTypes : Map<int,Set<Type>>; // This is sort of a disjoint set
    randomNumbers : seq<int>
    }

let chooseFrom (elements : list<'a>) (state : GenerationState) =
    let randomNumbers = state.randomNumbers
    let state = { state with randomNumbers = Seq.skip 1 randomNumbers }
    elements.[(Seq.head randomNumbers) % (List.length elements)], state

let addIdentifierType state (identifier, identifierType) =
    { state with identifierTypes = state.identifierTypes.Add(identifier, identifierType) }

let isGeneric t =
    match t with | Generic _ -> true | _ -> false

let typesAreEquivalent state t1 t2 =
    let isGenericSet (genericTypes : Map<int,Set<Type>>) i =
        Set.fold (&&) true (Set.map isGeneric genericTypes.[i])
    let append genericTypes typeSet =
        if Map.exists (fun _ s -> Set.isSubset typeSet s) genericTypes then
            genericTypes
        else
            Map.add genericTypes.Count typeSet genericTypes
    let union (genericTypes : Map<int,Set<Type>>) key1 key2 =
        Set.union genericTypes.[key1] genericTypes.[key2]
        |> append genericTypes
        |> Map.remove key1
        |> Map.remove key2

    let findTypeIndex genericTypes t =
        Map.findKey (fun _ s -> Set.contains t s) genericTypes

    let genericTypes = append state.genericTypes (Set [t1])
    let genericTypes = append genericTypes (Set [t2])
    let t1Index = findTypeIndex genericTypes t1
    let t2Index = findTypeIndex genericTypes t2

    let equivalent, genericTypes =
        if t1Index = t2Index then
            true, genericTypes
        elif isGenericSet genericTypes t1Index || isGenericSet genericTypes t2Index then
            true, union genericTypes t1Index t2Index
        else
            false, genericTypes
    equivalent, { state with genericTypes = genericTypes }

let usedGenerics state =
    let listGenerics s =
        Set.toList s
        |> List.choose (fun t -> match t with | Generic i -> Some i | _ -> None)
    Map.toList state.genericTypes
    |> List.map snd
    |> List.collect listGenerics
