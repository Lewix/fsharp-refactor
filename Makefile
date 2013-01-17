REFS=-r:/mnt/media/git/university/personalproj/fsharp-refactor/FSharp.Compiler.dll \
	-r:/usr/lib/nunit/nunit.framework.dll
OPTS=--target:library --nologo
SOURCES=src/Ast.fs \
	src/CodeTransforms.fs \
	src/CodeAnalysis.fs \
	src/RefactoringWorkflow.fs \
	src/CodeTemplates.fs \
    src/Rename.fs \
	src/ExtractFunction.fs \
	src/AddArgument.fs
TESTS=tests/EngineTests.fs \
      tests/RenameTests.fs \
      tests/ExtractFunctionTests.fs \
      tests/AddArgumentTests.fs

all: FSharp.Refactor.dll FSharp.Refactor.Tests.dll

FSharp.Refactor.dll: $(SOURCES)
	fsharpc $(OPTS) -o:FSharp.Refactor.dll $(REFS) $(SOURCES)

FSharp.Refactor.Tests.dll: FSharp.Refactor.dll $(TESTS)
	fsharpc $(OPTS) -o:FSharp.Refactor.Tests.dll -r:FSharp.Refactor.dll $(REFS) $(TESTS)

CommandLine.exe: Mono.Options/Options.dll FSharp.Refactor.dll src/CommandLine.fs
	fsharpc -r:Mono.Options/Options.dll -r:FSharp.Refactor.dll src/CommandLine.fs

run-tests: FSharp.Refactor.Tests.dll CommandLine.exe command-line-tests.sh
	mono /usr/lib/nunit/nunit-console.exe -nologo FSharp.Refactor.Tests.dll
	./command-line-tests.sh

tags: 
	ctags -e $(SOURCES) $(TESTS)

cli: CommandLine.exe
