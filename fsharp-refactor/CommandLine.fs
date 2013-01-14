module FSharpRefactor.CommandLine

open System
open System.IO
open System.Text.RegularExpressions
open Mono.Options
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Engine.RefactoringWorkflow
open FSharpRefactor.Refactorings.Rename
open FSharpRefactor.Refactorings.ExtractFunction
open FSharpRefactor.Refactorings.AddArgument

exception ArgumentException of string

//TODO: Don't block if no data in stdin
let readFromStdin () =
    let stdin = Console.OpenStandardInput()
    let buffer : byte array = Array.zeroCreate 10000
    stdin.Read(buffer, 0, 10000) |> ignore
    new String(Array.map char buffer)

let readFromFile filename =
    if File.Exists filename then
        File.ReadAllText filename
    else
        raise (ArgumentException "The file does not exist")

let getSource filename =
    match filename with
        | Some f -> readFromFile f
        | None -> readFromStdin ()

let Rename filename position newName =
    let source = getSource filename
    let tree = (Ast.Parse source).Value
    let identifierName = FindIdentifierName source position
    if Option.isSome identifierName then
        let declarationIdentifier = TryFindIdentifierWithName (makeScopeTrees tree) identifierName.Value
        if Option.isSome declarationIdentifier then
            DoRename source tree declarationIdentifier.Value newName
        else
            raise (ArgumentException "The specified identifier was not declared in the given source")
    else
        raise (ArgumentException "No identifier found at the given range")

let ExtractFunction filename (startPosition,endPosition) functionName =
    let source = getSource filename
    let tree = (Ast.Parse source).Value
    let expressionRange = mkRange "/home/lewis/test.fs" startPosition endPosition
    let inScopeTree = DefaultInScopeTree source tree expressionRange
    if Option.isNone (TryFindExpressionAroundRange expressionRange tree) then
        raise (ArgumentException "No expression found at the given range")
    elif Option.isNone inScopeTree then
        raise (ArgumentException "Could not find a suitable expression to use as the function's scope")
    else
        DoExtractFunction source tree (Ast.AstNode.Expression inScopeTree.Value) expressionRange functionName

let AddArgument filename position argumentName defaultValue =
    let source = getSource filename
    let tree = (Ast.Parse source).Value
    let bindingRange = DefaultBindingRange source tree position
    if Option.isSome bindingRange then
        AddArgument source tree bindingRange.Value argumentName defaultValue
    else
        raise (ArgumentException "No binding found around the given position")

let printUsage () =
    printfn "Usage:"
    printfn "  rename <position> <new_name> [<filename>]"
    printfn "  extract-function  <expression_range> <function_name> [<filename>]"
    printfn "  add-argument <position> <argument_name> <default_value> [<filename>]"
    printfn ""
    printfn "Options:"
    printfn "  -h, --help                          Display this message and exit"
    printfn "  -i[SUFFIX], --in-place=[SUFFIX]     Modify the input file in-place (makes backup if extension supplied)"
    printfn "  -oFILENAME, --output-file=FILENAME  Write result to FILENAME"
    


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

let filenameAndActionFromArguments args =
    match args with
        | "rename"::rest ->
            let position, newName, filename =
                parseRenameArguments rest
            (filename, fun () -> Rename filename position newName)
        | "extract-function"::rest ->
            let expressionRange, functionName, filename =
                parseExtractFunctionArguments rest
            (filename, fun () -> ExtractFunction filename expressionRange functionName)
        | "add-argument"::rest ->
            let position, argumentName, defaultValue, filename =
                parseAddArgumentArguments rest
            (filename, fun () -> AddArgument filename position argumentName defaultValue)
        | name::_ -> raise (ArgumentException (sprintf "%s is not a valid refactoring name" name))
        | [] -> raise (ArgumentException "Too few arguments")

let refactorWithArguments args =
    (snd (filenameAndActionFromArguments args)) ()

let getFilename args =
    let filename, _ = filenameAndActionFromArguments args
    if Option.isSome filename then filename.Value
    else raise (ArgumentException "No filename supplied")

type Options() =
    let mutable m_help = false
    let mutable m_inPlace = false
    let mutable m_inPlaceSuffix = None
    let mutable m_outputFile = None
    
    member this.Help with get() = m_help
                     and set newHelp = m_help <- newHelp
    member this.InPlace with get() = m_inPlace
                        and set newInPlace = m_inPlace <- newInPlace
    member this.InPlaceSuffix with get() = m_inPlaceSuffix
                              and set newInPlaceSuffix = m_inPlaceSuffix <- newInPlaceSuffix
    member this.OutputFile with get() = m_outputFile
                           and set newOutputFile = m_outputFile <- newOutputFile
                           
//TODO: Handle KeyNotFoundException sensibly
[<EntryPoint>]
let main(args : string[]) =
    let options = new Options()
    let optionSet = new OptionSet()
    optionSet.Add("h|help", fun _ -> options.Help <- true) |> ignore
    optionSet.Add("i:|in-place:",
                  fun s -> options.InPlace <- true;
                           options.InPlaceSuffix <- if s = null then None else Some s) |> ignore
    optionSet.Add("o=|output-file=", fun f -> options.OutputFile <- Some f) |> ignore
    let extra = Seq.toList (optionSet.Parse(args))

    if options.Help then
        printUsage ()
        0
    else
        try
            let resultCode = refactorWithArguments extra

            if options.InPlace then
                let filename = getFilename extra
                if Option.isSome options.InPlaceSuffix then
                    File.WriteAllText(filename + "." + (options.InPlaceSuffix).Value,
                                      File.ReadAllText(filename))
                File.WriteAllText(filename, resultCode)

            if Option.isSome options.OutputFile then
                File.WriteAllText((options.OutputFile).Value, resultCode)

            printfn "%s" resultCode
            0
        with
            | ArgumentException message ->
                printfn "%s" message
                printfn ""
                printUsage ()
                1
            | RefactoringFailure message ->
                printfn "%s" message
                1
