REFS=-r:/mnt/media/git/university/personalproj/fsharp-refactor/FSharp.Compiler.dll \
	-r:nunit.framework.dll
OPTS=--target:library --nologo
SOURCES=fsharp-refactor/astfetch.fs \
	fsharp-refactor/Ast.fs \
	fsharp-refactor/TreeTransforms.fs
TESTS=fsharp-refactor-tests/EngineTests.fs

all: FSharp.Refactor.dll FSharp.Refactor.Tests.dll

FSharp.Refactor.dll: $(SOURCES)
	fsharpc $(OPTS) -o:FSharp.Refactor.dll $(REFS) $(SOURCES)

FSharp.Refactor.Tests.dll: FSharp.Refactor.dll $(TESTS)
	fsharpc $(OPTS) -o:FSharp.Refactor.Tests.dll -r:FSharp.Refactor.dll $(REFS) $(TESTS)

run-tests: FSharp.Refactor.Tests.dll
	mono /usr/lib/mono/4.5/nunit-console.exe -nologo -labels FSharp.Refactor.Tests.dll

tags: 
	ctags -e $(SOURCES) $(TESTS)
