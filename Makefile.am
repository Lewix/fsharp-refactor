bin_SCRIPTS = bin/fsharp-refactor

fsharprefactordir=$(pkglibdir)
fsharprefactor_SCRIPTS = bin/FSharpRefactor.exe

SOURCES=src/Ast.fs \
		src/CodeAnalysis.fs \
		src/CodeTransforms.fs \
		src/ValidityChecking.fs \
		src/Refactoring.fs \
		src/Rename.fs \
		src/ExtractFunction.fs \
		src/AddArgument.fs

TESTS=tests/EngineTests.fs \
      tests/RenameTests.fs \
      tests/ExtractFunctionTests.fs \
      tests/AddArgumentTests.fs

DEPS=lib/FSharp.Compiler.dll \
     lib/Options.dll

fsharprefactor_DATA = $(DEPS) \
					  lib/FSharp.Refactor.dll

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
	bin/FSharpRefactor.exe \
	lib/FSharp.Refactor.dll

DISTCLEANFILES = \
	configure \
	config.guess \
	config.sub \
	Makefile.in \
	install-sh \
	aclocal.m4 \
	INSTALL \
	COPYING \
	missing

EXTRA_DIST = bin/fsharp-refactor.in \
			 $(DEPS) \
			 $(SOURCES) \
			 src/CommandLine.fs

REFS=-r:lib/FSharp.Compiler.dll \
	-r:lib/Options.dll

OPTS=--target:library --nologo --lib:lib


all: lib/FSharp.Refactor.dll bin/FSharpRefactor.exe

lib/FSharp.Refactor.dll: $(SOURCES)
	fsharpc $(OPTS) -o:lib/FSharp.Refactor.dll $(REFS) $(SOURCES)

bin/FSharpRefactor.exe: lib/Options.dll lib/FSharp.Refactor.dll src/CommandLine.fs
	fsharpc $(OPTS) $(REFS) --target:exe -o:bin/FSharpRefactor.exe -r:lib/FSharp.Refactor.dll src/CommandLine.fs



lib/FSharp.Refactor.Tests.dll: lib/FSharp.Refactor.dll $(TESTS)
	fsharpc $(OPTS) -o:lib/FSharp.Refactor.Tests.dll -r:lib/FSharp.Refactor.dll $(REFS) -r:/usr/lib/nunit/nunit.framework.dll $(TESTS)

unit-tests: lib/FSharp.Refactor.Tests.dll
	mono /usr/lib/nunit/nunit-console.exe -noresult lib/FSharp.Refactor.Tests.dll

tests: lib/FSharp.Refactor.Tests.dll bin/FSharpRefactor.exe tests/command-line-tests.sh
	mono /usr/lib/nunit/nunit-console.exe -noresult lib/FSharp.Refactor.Tests.dll
	export MONO_PATH=lib; tests/command-line-tests.sh
