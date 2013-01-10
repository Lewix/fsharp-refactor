module FSharpRefactor.CommandLine

open System
open System.IO
open System.Text.RegularExpressions
open Mono.Options
open Microsoft.FSharp.Compiler.Range
open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Refactorings.Rename
open FSharpRefactor.Refactorings.ExtractFunction
open FSharpRefactor.Refactorings.AddArgument
    
let Rename filename position newName =
    let source = File.ReadAllText filename
    let tree = (Ast.Parse source).Value
    let declarationIdentifier = FindDeclarationIdentifier source position
    DoRename source tree declarationIdentifier newName

let ExtractFunction filename expressionRange functionName =
    let source = File.ReadAllText filename
    let tree = (Ast.Parse source).Value
    let inScopeTree = (DefaultInScopeTree source tree expressionRange).Value
    DoExtractFunction source tree inScopeTree expressionRange functionName

let AddArgument filename position argumentName defaultValue =
    let source = File.ReadAllText filename
    let tree = (Ast.Parse source).Value
    let bindingRange = (DefaultBindingRange source tree position).Value
    AddArgument source tree bindingRange argumentName defaultValue

let tryParsePos (positionString : string) =
    let m = Regex.Match(positionString, "([0-9]+):([0-9]+)")
    if m.Success then
        let line = Int32.Parse m.Groups.[1].Value
        let column = Int32.Parse m.Groups.[2].Value
        Some (mkPos line column)
    else None

[<EntryPoint>]
let main(args : string[]) =
    let optionSet = new OptionSet()
    let printUsage () =
        printfn "Usage:"
        printfn "  rename <filename> <position> <new_name>"
        printfn "  extract-function <filename> <expression_range> <function_name>"
        printfn "  add-argument <filename> <position> <argument_name> <default_value>"
        
    let extra = optionSet.Parse(args)

    match extra.[0] with
        | "rename" ->
            let position = tryParsePos extra.[2]
            if Option.isNone position then printUsage ()
            else
                printfn "%s" (Rename extra.[1] position.Value extra.[3])
        | "extract-function" ->
            let startPos = tryParsePos extra.[2]
            let endPos = tryParsePos extra.[3]
            if Option.isNone startPos && Option.isNone endPos then printUsage ()
            else
                let range = mkRange "/home/lewis/test.fs" startPos.Value endPos.Value
                printfn "%s" (ExtractFunction extra.[1] range extra.[4])
        | "add-argument" ->
            let position = tryParsePos extra.[2]
            if Option.isNone position then printUsage ()
            else
                printfn "%s" (AddArgument extra.[1] position.Value extra.[3] extra.[4])
        | _ -> printUsage ()

    0
