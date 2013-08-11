DEPS=lib/FSharp.Compiler.Editor.dll \
     lib/Options.dll

bin_SCRIPTS = bin/fsharp-refactor
fsharprefactordir=$(pkglibdir)
fsharprefactor_SCRIPTS = bin/FSharpRefactor.CommandLine.exe
fsharprefactor_DATA = $(DEPS) \
					  bin/FSharpRefactor.dll

REWRITE = \
	sed -e "s|\@prefix\@|$(prefix)|g"       \
	    -e "s|\@pkglibdir\@|$(pkglibdir)|g" \
	    -e "s|\@bindir\@|$(bindir)|g"		\
	    -e "s|\@MONO\@|$(MONO)|g" 

$(bin_SCRIPTS): bin/fsharp-refactor.in Makefile
	$(REWRITE) bin/fsharp-refactor.in > $@.tmp
	mv $@.tmp $@

CLEANFILES = \
	bin/fsharp-refactor \
	bin/FSharpRefactor.CommandLine.exe \
	bin/FSharpRefactor.dll \
	bin/FSharpRefactorTests.dll

DISTCLEANFILES = \
	configure \
	config.guess \
	config.sub \
	Makefile.in \
	install-sh \
	aclocal.m4 \
	INSTALL \
	missing

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

EXTRA_DIST = FSharpRefactor/FSharpRefactor.fsproj \
             FSharpRefactor.CommandLine/FSharpRefactor.CommandLine.fsproj \
             bin/fsharp-refactor.in \
			 $(CORE_SOURCES) \
             $(CLI_SOURCES) \
			 $(DEPS)


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
