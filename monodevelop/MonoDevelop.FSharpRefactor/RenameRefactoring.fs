namespace MonoDevelop.FSharpRefactor.Rename

open System
open MonoDevelop.Components.Commands
open MonoDevelop.Refactoring
open MonoDevelop.Refactoring.Rename
open MonoDevelop.Ide
open MonoDevelop.Ide.Gui
open MonoDevelop.Ide.Gui.Content
open MonoDevelop.Projects.Text
open MonoDevelop.Core.ProgressMonitoring
open Mono.TextEditor
open Microsoft.FSharp.Compiler.Range

open FSharpRefactor.Engine.Ast
open FSharpRefactor.Engine.CodeAnalysis.RangeAnalysis
open FSharpRefactor.Engine.CodeAnalysis.ScopeAnalysis
open FSharpRefactor.Refactorings.Rename

type FSharpRenameItemDialog(options:RefactoringOptions, rename:RenameRefactoring) as self =
    inherit MonoDevelop.Refactoring.Rename.RenameItemDialog(options, rename)
    do
        self.Title <- "Rename (F#)"

type RenameRefactoring() as self =
    inherit MonoDevelop.Refactoring.Rename.RenameRefactoring()

    let rename (source:string) (position:pos) (newName:string) =
        let tree = (Ast.Parse source).Value
        let identifier = FindIdentifier source position
        let declarationIdentifier =
            TryFindIdentifierDeclaration (makeScopeTrees tree) identifier.Value
        DoRename source tree declarationIdentifier.Value newName

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
        let renameProperties = properties :?> MonoDevelop.Refactoring.Rename.RenameRefactoring.RenameProperties
        let doc = options.Document
        let wholeFileChange = new TextReplaceChange()
        let fileName = options.Document.FileName.ToString()
        wholeFileChange.FileName <- fileName
        wholeFileChange.Offset <- 0
        let textFile = doc.GetContent<ITextFile>()
        wholeFileChange.RemovedChars <- textFile.Length
        let position = mkPos options.Location.Line (options.Location.Column-1)
        wholeFileChange.InsertedText <-
            rename textFile.Text position renameProperties.NewName

        Collections.Generic.List<Change>([wholeFileChange :> Change])

    override self.Run(options) =
        MessageService.ShowCustomDialog(new FSharpRenameItemDialog(options, self)) |> ignore
