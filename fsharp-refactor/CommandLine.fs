module FSharpRefactor.CommandLine

open System
open System.IO
open System.Text.RegularExpressions
open Mono.Options
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow
open FSharpRefactor.Refactorings.Rename
open FSharpRefactor.Refactorings.ExtractFunction
open FSharpRefactor.Refactorings.AddArgument

let getSource filename =
    match filename with
        | Some f -> File.ReadAllText f
//TODO: read code from stdout

let Rename filename position newName =
    let source = getSource filename
    let tree = (Ast.Parse source).Value
    let declarationIdentifier = FindDeclarationIdentifier source position
    DoRename source tree declarationIdentifier newName

let ExtractFunction filename (startPosition,endPosition) functionName =
    let source = getSource filename
    let tree = (Ast.Parse source).Value
    let expressionRange = mkRange "/home/lewis/test.fs" startPosition endPosition
    let inScopeTree = (DefaultInScopeTree source tree expressionRange).Value
    DoExtractFunction source tree inScopeTree expressionRange functionName

let AddArgument filename position argumentName defaultValue =
    let source = getSource filename
    let tree = (Ast.Parse source).Value
    let bindingRange = (DefaultBindingRange source tree position).Value
    AddArgument source tree bindingRange argumentName defaultValue

let printUsage () =
    printfn "Usage:"
    printfn "  rename <position> <new_name> [<filename>]"
    printfn "  extract-function  <expression_range> <function_name> [<filename>]"
    printfn "  add-argument <position> <argument_name> <default_value> [<filename>]"
    printfn ""
    printfn "Options:"
    printfn "  -h, --help                           Display this message and exit"
    printfn "  -i [SUFFIX], --in-place=[SUFFIX]     Modify the input file in-place (makes backup if extension supplied)"
    printfn "  -o FILENAME, --output-file=FILENAME  Write result to FILENAME"
    

exception ArgumentException of string

let parsePos (positionString : string) =
    let m = Regex.Match(positionString, "([0-9]+):([0-9]+)")
    if m.Success then
        let line = Int32.Parse m.Groups.[1].Value
        let column = Int32.Parse m.Groups.[2].Value
        mkPos line column
    else raise (ArgumentException (sprintf "%s is not a valid position" positionString))

let parseRenameArguments (args : string list) =
    match List.length args with
        | 0 | 1 -> raise (ArgumentException "Too few arguments")
        | 2 -> (parsePos args.[0], args.[1], None)
        | 3 -> (parsePos args.[0], args.[1], Some args.[2])
        | _ -> raise (ArgumentException "Too many arguments")

let parseExtractFunctionArguments (args : string list) =
    match List.length args with
        | 0 | 1 | 2 -> raise (ArgumentException "Too few arguments")
        | 3 -> ((parsePos args.[0], parsePos args.[1]), args.[2], None)
        | 4 -> ((parsePos args.[0], parsePos args.[1]), args.[2], Some args.[3])
        | _ -> raise (ArgumentException "Too many arguments")

let parseAddArgumentArguments (args : string list) =
    match List.length args with
        | 0 | 1 | 2 -> raise (ArgumentException "Too few arguments")
        | 3 -> (parsePos args.[0], args.[1], args.[2], None)
        | 4 -> (parsePos args.[0], args.[1], args.[2], Some args.[3])
        | _ -> raise (ArgumentException "Too many arguments")

let refactorWithArguments args =
    match args with
        | "rename"::rest ->
            let position, newName, filename =
                parseRenameArguments rest
            Rename filename position newName
        | "extract-function"::rest ->
            let expressionRange, functionName, filename =
                parseExtractFunctionArguments rest
            ExtractFunction filename expressionRange functionName
        | "add-argument"::rest ->
            let position, argumentName, defaultValue, filename =
                parseAddArgumentArguments rest
            AddArgument filename position argumentName defaultValue
        | name::_ -> raise (ArgumentException (sprintf "%s is not a valid refactoring name" name))
        | [] -> raise (ArgumentException "Too few arguments")

type Options() =
    member this.Help = ref false
    member this.InPlace = ref false
    member this.OutputFile = ref None
//TODO: Handle KeyNotFoundException sensible
[<EntryPoint>]
let main(args : string[]) =
    let options = new Options()
    let optionSet = new OptionSet()
    //TODO: get the argument passed in with -i or -o
    optionSet.Add("h|help", fun _ -> options.Help := true) |> ignore
    optionSet.Add("i|in-place", fun s -> options.InPlace := true) |> ignore
    optionSet.Add("o|output-file", fun f -> options.OutputFile := Some f) |> ignore
    let extra = Seq.toList (optionSet.Parse(args))

    if !options.Help then
        printUsage ()
        0
    else
        let resultCode =
            try Some (refactorWithArguments extra) with
                | ArgumentException message ->
                    printfn "%s" message
                    printfn ""
                    printUsage ()
                    None
                | RefactoringFailure message ->
                    printfn "%s" message
                    None

        if Option.isNone resultCode then
            1
        else
            if !options.InPlace then
                //TODO: write resultCode back to the file after backing it up
                ()
            if Option.isSome !options.OutputFile then
                //TODO: write resultCode to outputfile
                ()
            printfn "%s" resultCode.Value
            0
