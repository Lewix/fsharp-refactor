bin_SCRIPTS = fsharp-refactor

fsharprefactordir=$(pkglibdir)
fsharprefactor_SCRIPTS = FSharpRefactor.exe

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

DEPS=lib/FSharp.Compiler.dll \
     lib/FSharp.Core.dll \
     lib/FSharp.Core.optdata \
     lib/FSharp.Core.sigdata \
     lib/Options.dll

fsharprefactor_DATA = $(DEPS) \
					  lib/FSharp.Refactor.dll

REWRITE = \
	sed -e "s|\@prefix\@|$(prefix)|g"       \
	    -e "s|\@pkglibdir\@|$(pkglibdir)|g" \
	    -e "s|\@bindir\@|$(bindir)|g"		\
	    -e "s|\@MONO\@|$(MONO)|g" 

$(bin_SCRIPTS): $(srcdir)/fsharp-refactor.in Makefile
	$(REWRITE) $(srcdir)/fsharp-refactor.in > $@.tmp
	mv $@.tmp $@

CLEANFILES = \
	fsharp-refactor \
	FSharpRefactor.exe \
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

EXTRA_DIST = fsharp-refactor.in \
			 $(DEPS) \
			 $(SOURCES) \
			 src/CommandLine.fs

REFS=-r:lib/FSharp.Compiler.dll \
	-r:lib/Options.dll

OPTS=--target:library --nologo --lib:lib


all: lib/FSharp.Refactor.dll FSharpRefactor.exe

lib/FSharp.Refactor.dll: $(SOURCES)
	fsharpc $(OPTS) -o:lib/FSharp.Refactor.dll $(REFS) $(SOURCES)

FSharpRefactor.exe: lib/Options.dll lib/FSharp.Refactor.dll src/CommandLine.fs
	fsharpc $(OPTS) $(REFS) --target:exe -o:FSharpRefactor.exe -r:lib/FSharp.Refactor.dll src/CommandLine.fs



lib/FSharp.Refactor.Tests.dll: lib/FSharp.Refactor.dll $(TESTS)
	fsharpc $(OPTS) -o:lib/FSharp.Refactor.Tests.dll -r:lib/FSharp.Refactor.dll $(REFS) -r:/usr/lib/nunit/nunit.framework.dll $(TESTS)

unit-tests: lib/FSharp.Refactor.Tests.dll
	mono /usr/lib/nunit/nunit-console.exe -noresult lib/FSharp.Refactor.Tests.dll

tests: lib/FSharp.Refactor.Tests.dll bin/FSharpRefactor.exe tests/command-line-tests.sh
	mono /usr/lib/nunit/nunit-console.exe -noresult lib/FSharp.Refactor.Tests.dll
	export MONO_PATH=lib; tests/command-line-tests.sh
