REFS=-r:libs/FSharp.Compiler.dll \
	-r:libs/Options.dll \
	-r:libs/FSharp.Compiler.CodeDom.dll

OPTS=--target:library --nologo --lib:libs

SOURCES=src/Ast.fs \
		src/CodeAnalysis.fs \
		src/CodeTransforms.fs \
		src/Refactoring.fs \
		src/Rename.fs \
		src/ExtractFunction.fs \
		src/AddArgument.fs

TESTS=tests/EngineTests.fs \
      tests/RenameTests.fs \
      tests/ExtractFunctionTests.fs \
      tests/AddArgumentTests.fs

INTEGRATION_TESTS=tests/integration_tests/CompilerTests.fs

all: libs/FSharp.Refactor.dll bin/FSharpRefactor.exe

unit-tests: libs/FSharp.Refactor.Tests.dll
	mono /usr/lib/nunit/nunit-console.exe -noresult libs/FSharp.Refactor.Tests.dll

libs/FSharp.Refactor.dll: $(SOURCES)
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.dll $(REFS) $(SOURCES)

libs/FSharp.Refactor.Tests.dll: libs/FSharp.Refactor.dll $(TESTS)
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.Tests.dll -r:libs/FSharp.Refactor.dll $(REFS) -r:/usr/lib/nunit/nunit.framework.dll $(TESTS)

libs/FSharp.Refactor.IntegrationTests.dll:
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.IntegrationTests.dll $(REFS) -r:/usr/lib/nunit/nunit.framework.dll $(INTEGRATION_TESTS)

bin/FSharpRefactor.exe: libs/Options.dll libs/FSharp.Refactor.dll src/CommandLine.fs
	fsharpc $(OPTS) $(REFS) --target:exe -o:bin/FSharpRefactor.exe -r:libs/FSharp.Refactor.dll src/CommandLine.fs

tests: libs/FSharp.Refactor.Tests.dll libs/FSharp.Refactor.IntegrationTests.dll bin/FSharpRefactor.exe tests/command-line-tests.sh
	mono /usr/lib/nunit/nunit-console.exe -noresult libs/FSharp.Refactor.Tests.dll libs/FSharp.Refactor.IntegrationTests.dll
	export MONO_PATH=libs; tests/command-line-tests.sh

tags: 
	ctags -e $(SOURCES) $(TESTS)
