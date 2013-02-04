module FSharpRefactor.Evaluator.GenerationState

open System

type Type =
    | Int
    | Fun of Type * Type
    | Generic of int

type DisjointSet = Set<Set<Type>> // This is sort of a disjoint set

let add t disjointSet =
    if Set.exists (Set.contains t) disjointSet then
        disjointSet
    else
        Set.add (Set [t]) disjointSet

let union set1 set2 (disjointSet : DisjointSet) =
    let newSet = Set.union set1 set2
    Set.remove set1 disjointSet
    |> Set.remove set2
    |> Set.add newSet

let findSet t disjointSet =
    Seq.find (Set.contains t) (seq disjointSet)

let unionSets t1 t2 disjointSet =
    let disjointSet =
        add t1 disjointSet
        |> add t2
    let set1 = findSet t1 disjointSet
    let set2 = findSet t2 disjointSet
    union set1 set2 disjointSet


type GenerationState = {
    identifierTypes : Map<string, Type>;
    genericTypes : DisjointSet;
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

let rec occurs t1 t2 =
    match t2 with
        | Int -> false
        | Fun(ta,tb) -> occurs t1 ta || occurs t1 tb
        | _ -> t1 = t2

let unifyTypes t1 t2 : (DisjointSet option)=
    let rec unifyTypesWithConstraints t1 t2 (constraints : DisjointSet) =
        match t1,t2 with
            | Int,Int -> Some constraints
            | Int,Fun(_,_) -> None
            | Fun(_,_),Int -> None
            | Fun(ta1,tb1),Fun(ta2,tb2) ->
                let constraintsA = unifyTypesWithConstraints ta1 ta2 constraints
                if Option.isSome constraintsA then
                    unifyTypesWithConstraints tb1 tb2 constraintsA.Value
                else
                    None
            | (Generic _ as g),t | t,(Generic _ as g) ->
                if g = t then Some constraints
                elif not (occurs g t) then
                    Some (unionSets g t constraints)
                else None
    unifyTypesWithConstraints t1 t2 (Set[])

let typesAreEquivalent state t1 t2 =
    let genericTypes = add t1 state.genericTypes
    let genericTypes = add t2 genericTypes
    
    let t1Set = findSet t1 genericTypes
    let t2Set = findSet t2 genericTypes 

    let t2UnifiesWithT1Set = (Set.map (fun t -> Option.isSome (unifyTypes t2 t)) t1Set) = Set [true]
    let t1UnifiesWithT2Set = (Set.map (fun t -> Option.isSome (unifyTypes t1 t)) t2Set) = Set [true]

    if t2UnifiesWithT1Set && t1UnifiesWithT2Set then
        true, { state with genericTypes = union t1Set t2Set genericTypes }
    else
        false, state

let usedGenerics state =
    let listGenerics s =
        Set.toList s
        |> List.choose (fun t -> match t with | Generic i -> Some i | _ -> None)
    Set.toList state.genericTypes
    |> List.collect listGenerics
