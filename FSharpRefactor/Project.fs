namespace FSharpRefactor.Engine

open System
open System.IO
open FSharpRefactor.Engine.Ast

type Project(currentFile:string, filesAndContents:(string * string option) array, updatedFiles:Set<string>) as self =
    let currentFile = Path.GetFullPath currentFile
    let getIndex filename = Seq.findIndex ((=) filename) self.Files
    
    new(currentFile:string, filesAndContents:(string * string option) array) =
        Project(currentFile, filesAndContents, Set.empty)

    new(source:string, filename:string) =
        Project(filename, [|filename, Some source|])

    member self.Files with get() = Array.map (fst >> Path.GetFullPath) filesAndContents
    member self.CurrentFile with get() = currentFile
    member self.FileContents = Array.map snd filesAndContents
    member self.CurrentFileContents = self.GetContents self.CurrentFile
    member self.UpdatedFiles = updatedFiles
    
    member self.FilesInScope filename =
        let filename = Path.GetFullPath filename
        Seq.skipWhile ((<>) filename) self.Files |> Seq.toList
    member self.GetParseTree filename =
        Ast.Parse self.Files filename (self.GetContents filename)
        |> Option.get
    member self.TryGetDeclarationLocation filename names position =
        Ast.TryGetDeclarationLocation self.Files filename (self.GetContents filename) names position
    member self.GetContents filename = 
        let filename = Path.GetFullPath filename
        let index = getIndex filename
        if Option.isSome self.FileContents.[index] then self.FileContents.[index].Value
        else File.ReadAllText filename
    member self.UpdateContents filename contents =
        let filename = Path.GetFullPath filename
        let index = getIndex filename
        let filesAndContents = Array.copy filesAndContents
        Array.set filesAndContents index (filename, Some contents)
        new Project(currentFile, filesAndContents, Set.add filename updatedFiles)
    member self.UpdateCurrentFileContents contents =
        self.UpdateContents self.CurrentFile contents
        
module Projects =
    let GetParseTree (project:Project) filename =
        project.GetParseTree filename
    
    let TryGetDeclarationLocation (project:Project) filename names position =
        project.TryGetDeclarationLocation filename names position