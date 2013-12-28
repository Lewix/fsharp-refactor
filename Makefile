DEPS=lib/FSharp.Compiler.Editor.dll \
     lib/Options.dll

CORE_SOURCES=FSharpRefactor/Ast.fs \
             FSharpRefactor/RangeAnalysis.fs \
             FSharpRefactor/ScopeTrees.fs \
             FSharpRefactor/Project.fs \
             FSharpRefactor/Scoping.fs \
             FSharpRefactor/CodeTransforms.fs \
             FSharpRefactor/ValidityChecking.fs \
             FSharpRefactor/Refactoring.fs \
             FSharpRefactor/Rename.fs \
             FSharpRefactor/ExtractFunction.fs \
             FSharpRefactor/AddArgument.fs \
             FSharpRefactor/AssemblyInfo.fs

CLI_SOURCES=FSharpRefactor.CommandLine/CommandLine.fs \
            FSharpRefactor.CommandLine/AssemblyInfo.fs

TESTS=FSharpRefactorTests/EngineTests.fs \
      FSharpRefactorTests/RenameTests.fs \
      FSharpRefactorTests/ExtractFunctionTests.fs \
      FSharpRefactorTests/AddArgumentTests.fs \
      FSharpRefactorTests/ScopingTests.fs \
      FSharpRefactorTests/ProjectTests.fs


all: bin/FSharpRefactor.dll bin/FSharpRefactor.CommandLine.exe

bin/FSharpRefactor.dll: $(CORE_SOURCES) lib/FSharp.Compiler.Editor.dll FSharpRefactor/FSharpRefactor.fsproj
	xbuild FSharpRefactor/FSharpRefactor.fsproj
	cp FSharpRefactor/bin/Debug/FSharpRefactor.dll bin

bin/FSharpRefactor.CommandLine.exe: $(CLI_SOURCES) lib/Options.dll FSharpRefactor.CommandLine/FSharpRefactor.CommandLine.fsproj
	xbuild FSharpRefactor.CommandLine/FSharpRefactor.CommandLine.fsproj
	cp FSharpRefactor.CommandLine/bin/Debug/FSharpRefactor.CommandLine.exe bin

bin/FSharpRefactorTests.dll: $(TESTS) FSharpRefactorTests/FSharpRefactorTests.fsproj
	xbuild FSharpRefactorTests/FSharpRefactorTests.fsproj
	cp FSharpRefactorTests/bin/Debug/FSharpRefactorTests.dll bin


unit-tests: bin/FSharpRefactorTests.dll
	mono /usr/lib/nunit/nunit-console.exe -noresult bin/FSharpRefactorTests.dll

cli-tests: bin/FSharpRefactor.dll bin/FSharpRefactor.CommandLine.exe FSharpRefactorTests/command-line-tests.sh
	export MONO_PATH=lib; FSharpRefactorTests/command-line-tests.sh

tests: unit-tests cli-tests

#TODO: install
#TODO: clean
