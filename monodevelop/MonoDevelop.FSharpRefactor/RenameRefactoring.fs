namespace MonoDevelop.FSharpRefactor.Rename

open System
open MonoDevelop.Components.Commands
open MonoDevelop.Refactoring
open MonoDevelop.Ide
open MonoDevelop.Ide.Gui
open MonoDevelop.Ide.Gui.Content
open MonoDevelop.Projects.Text
open Mono.TextEditor
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Refactorings.Rename

type RenameRefactoring() as self =
    inherit RefactoringOperation()

    let rename (source:string) (position:pos) (newName:string) =
        let tree = (Ast.Parse source).Value
        let identifier = FindIdentifier source position
        if Option.isSome identifier then
            let declarationIdentifier =
                TryFindIdentifierDeclaration (makeScopeTrees tree) identifier.Value
            if Option.isSome declarationIdentifier then
                DoRename source tree declarationIdentifier.Value newName
            else
                source
        else
            source

    do
        self.Name <- "Rename (F#)"
    override self.IsValid(options) =
        let doc = options.Document
        let wholeFileChange = new TextReplaceChange()
        let fileName = options.Document.FileName.ToString()
        let source = doc.GetContent<ITextFile>().Text
        let position = options.Location.Line, options.Location.Column

        options.MimeType = "text/x-fsharp"
        |> (&&) (IsValid source fileName (Some position, None))
    override self.PerformChanges(options, properties) =
        let doc = options.Document
        let wholeFileChange = new TextReplaceChange()
        let fileName = options.Document.FileName.ToString()
        wholeFileChange.FileName <- fileName
        wholeFileChange.Offset <- 0
        let textFile = doc.GetContent<ITextFile>()
        wholeFileChange.RemovedChars <- textFile.Length
        let position = mkPos options.Location.Line (options.Location.Column-1)
        wholeFileChange.InsertedText <-
            rename textFile.Text position "testName" 

        Collections.Generic.List<Change>([wholeFileChange :> Change])
