REFS=-r:lib/FSharp.Compiler.dll \
	-r:lib/Options.dll \
	-r:lib/FSharp.Compiler.CodeDom.dll

OPTS=--target:library --nologo --lib:lib

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


all: lib/FSharp.Refactor.dll bin/FSharpRefactor.exe

unit-tests: lib/FSharp.Refactor.Tests.dll
	mono /usr/lib/nunit/nunit-console.exe -noresult lib/FSharp.Refactor.Tests.dll

lib/FSharp.Refactor.dll: $(SOURCES)
	fsharpc $(OPTS) -o:lib/FSharp.Refactor.dll $(REFS) $(SOURCES)

lib/FSharp.Refactor.Tests.dll: lib/FSharp.Refactor.dll $(TESTS)
	fsharpc $(OPTS) -o:lib/FSharp.Refactor.Tests.dll -r:lib/FSharp.Refactor.dll $(REFS) -r:/usr/lib/nunit/nunit.framework.dll $(TESTS)

bin/FSharpRefactor.exe: lib/Options.dll lib/FSharp.Refactor.dll src/CommandLine.fs
	fsharpc $(OPTS) $(REFS) --target:exe -o:bin/FSharpRefactor.exe -r:lib/FSharp.Refactor.dll src/CommandLine.fs

tests: lib/FSharp.Refactor.Tests.dll bin/FSharpRefactor.exe tests/command-line-tests.sh
	mono /usr/lib/nunit/nunit-console.exe -noresult lib/FSharp.Refactor.Tests.dll
	export MONO_PATH=lib; tests/command-line-tests.sh

tags: 
	ctags -e $(SOURCES) $(TESTS)
