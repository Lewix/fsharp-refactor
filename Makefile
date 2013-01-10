REFS=-r:/mnt/media/git/university/personalproj/fsharp-refactor/FSharp.Compiler.dll \
	-r:/usr/lib/nunit/nunit.framework.dll
OPTS=--target:library --nologo
SOURCES=fsharp-refactor/Ast.fs \
	fsharp-refactor/CodeTransforms.fs \
	fsharp-refactor/CodeAnalysis.fs \
	fsharp-refactor/RefactoringWorkflow.fs \
	fsharp-refactor/CodeTemplates.fs \
        fsharp-refactor/Rename.fs \
	fsharp-refactor/ExtractFunction.fs \
	fsharp-refactor/AddArgument.fs
TESTS=fsharp-refactor-tests/EngineTests.fs \
      fsharp-refactor-tests/RenameTests.fs \
      fsharp-refactor-tests/ExtractFunctionTests.fs \
      fsharp-refactor-tests/AddArgumentTests.fs

all: FSharp.Refactor.dll FSharp.Refactor.Tests.dll

FSharp.Refactor.dll: $(SOURCES)
	fsharpc $(OPTS) -o:FSharp.Refactor.dll $(REFS) $(SOURCES)

FSharp.Refactor.Tests.dll: FSharp.Refactor.dll $(TESTS)
	fsharpc $(OPTS) -o:FSharp.Refactor.Tests.dll -r:FSharp.Refactor.dll $(REFS) $(TESTS)

run-tests: FSharp.Refactor.Tests.dll
	mono /usr/lib/nunit/nunit-console.exe -nologo FSharp.Refactor.Tests.dll

tags: 
	ctags -e $(SOURCES) $(TESTS)

cli: Mono.Options/Options.dll FSharp.Refactor.dll
	fsharpc -r:Mono.Options/Options.dll -r:FSharp.Refactor.dll fsharp-refactor/CommandLine.fs
