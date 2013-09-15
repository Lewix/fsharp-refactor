namespace FSharpRefactor.Engine

open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpRefactor.Engine.Ast

type ITypedInfo =
    abstract member GetDeclarationLocation : (int*int) * (seq<string>) -> ((int*int) * string) option

type UntypedInfo =
    { untypedInfo : UntypedParseInfo
      contents : string }
type TypedInfo =
    { typedInfo : ITypedInfo
      contents : string }

type ParseInfoCache(project:Project, initialLazyTypedInfos) as self =
    let checker, options = Ast.getCheckerAndOptions project.Files
    let lazyUntypedInfos = new Dictionary<string,Lazy<UntypedInfo>>()
    let lazyTypedInfos = new Dictionary<string,Lazy<TypedInfo>>()

    let getLazyUntypedInfo filename =
        lazy
            let contents = project.GetContents filename
            { untypedInfo = Ast.ParseSourceWithChecker checker options project.Files filename contents
              contents = contents }
        
    let getLazyTypedInfo filename =
        lazy
            let untypedInfo = self.UntypedInfoAndContents filename
            let typedInfo =
                Some (Ast.TryTypeCheckSourceWithChecker checker options untypedInfo.untypedInfo project.Files filename untypedInfo.contents)
            let iTypedInfo =
                { new ITypedInfo with member self.GetDeclarationLocation(position, names) = Ast.TryGetDeclarationLocation typedInfo filename untypedInfo.contents names position }
            { typedInfo = iTypedInfo
              contents = untypedInfo.contents }

    do
        checker.StartBackgroundCompile options
        
        Seq.map getLazyUntypedInfo project.Files
        |> Seq.zip project.Files
        |> Seq.iter (fun (k,v) -> lazyUntypedInfos.Add(k,v))
        
        Seq.map getLazyTypedInfo project.Files
        |> Seq.zip project.Files
        |> Seq.iter (fun (k,v) -> lazyTypedInfos.Add(k,v))
        
        Seq.zip project.Files initialLazyTypedInfos
        |> Seq.iter (fun (k,v) -> if Option.isSome v then lazyTypedInfos.[k] <- v.Value)
    
    member private self.UntypedInfoAndContents filename =
        let untypedInfo = (lazyUntypedInfos.[filename]).Force()
        if untypedInfo.contents = project.GetContents filename then untypedInfo
        else
            checker.StartBackgroundCompile options
            let newLazyUntypedInfo = getLazyUntypedInfo filename
            lazyUntypedInfos.[filename] <- newLazyUntypedInfo
            newLazyUntypedInfo.Force()

    member self.UntypedInfo filename =
        (self.UntypedInfoAndContents filename).untypedInfo
            
    member self.TypedInfo filename =
        let typedInfo = (lazyTypedInfos.[filename]).Force()
        if typedInfo.contents = project.GetContents filename then typedInfo.typedInfo
        else
            let newLazyTypedInfo = getLazyTypedInfo filename
            lazyTypedInfos.[filename] <- newLazyTypedInfo
            newLazyTypedInfo.Force().typedInfo
            
        
and Project(filesAndContents:(string * string option) array, updatedFiles:Set<string>, lazyTypedInfos:Lazy<TypedInfo> option array) as self =
    let getIndex filename = Seq.findIndex ((=) filename) self.Files
    let parseInfoCache = lazy (new ParseInfoCache(self, lazyTypedInfos))

    new(filesAndContents:(string * string option) array) =
        Project(filesAndContents, Set.empty, Array.map (fun _ -> None) filesAndContents)
    new(source:string, filename:string) =
        Project([|filename, Some source|])
        
    member self.Files with get() = Array.map (fst >> Path.GetFullPath) filesAndContents
    member self.FileContents = Array.map snd filesAndContents
    member self.UpdatedFiles = updatedFiles
    
    member self.FilesInScope filename =
        let filename = Path.GetFullPath filename
        Seq.skipWhile ((<>) filename) self.Files |> Seq.toList
    member self.GetParseTree filename =
        let filename = Path.GetFullPath filename
        (parseInfoCache.Force().UntypedInfo filename).ParseTree
        |> Ast.MakeAstNode
    member self.TryGetDeclarationLocation filename names position =
        let filename = Path.GetFullPath filename
        let typedInfo = parseInfoCache.Force().TypedInfo filename
        typedInfo.GetDeclarationLocation (position, names)
    member self.GetContents filename =
        let filename = Path.GetFullPath filename
        let index = getIndex filename
        if Option.isSome self.FileContents.[index] then self.FileContents.[index].Value
        else
            let contents = File.ReadAllText filename
            Array.set filesAndContents index (filename, Some contents)
            contents
    member self.UpdateContents filename contents =
        let filename = Path.GetFullPath filename
        let index = getIndex filename
        let filesAndContents = Array.copy filesAndContents
        Array.set filesAndContents index (filename, Some contents)
        new Project(filesAndContents, Set.add filename updatedFiles, Array.map (fun _ -> None) filesAndContents)
        
    member self.PopulateCache filename =
        let filename = Path.GetFullPath filename
        if Seq.exists ((=) filename) self.Files then
            ignore (parseInfoCache.Force().UntypedInfo filename)
            
    member self.StartBackgroundCompile () = ignore (parseInfoCache.Force())
        
        
module Projects =
    let GetParseTree (project:Project) filename =
        project.GetParseTree filename
    
    let TryGetDeclarationLocation (project:Project) filename names position =
        project.TryGetDeclarationLocation filename names position