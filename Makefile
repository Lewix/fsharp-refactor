REFS=-r:libs/FSharp.Compiler.dll \
	-r:/usr/lib/nunit/nunit.framework.dll \
	-r:libs/Options.dll \
	-r:libs/FSharp.Compiler.CodeDom.dll

OPTS=--target:library --nologo --lib:libs

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
      tests/AddArgumentTests.fs \
      tests/EvaluatorTests.fs

all: libs/FSharp.Refactor.dll libs/FSharp.Refactor.Tests.dll bin/FSharpRefactor.exe libs/FSharp.Refactor.Evaluator.dll

libs/FSharp.Refactor.Evaluator.dll: src/evaluator/BehaviourChecker.fs
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.Evaluator.dll $(REFS) src/evaluator/BehaviourChecker.fs

libs/FSharp.Refactor.dll: $(SOURCES)
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.dll $(REFS) $(SOURCES)

libs/FSharp.Refactor.Tests.dll: libs/FSharp.Refactor.Evaluator.dll libs/FSharp.Refactor.dll $(TESTS)
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.Tests.dll -r:libs/FSharp.Refactor.dll -r:libs/FSharp.Refactor.Evaluator.dll $(REFS) $(TESTS)

bin/FSharpRefactor.exe: libs/Options.dll libs/FSharp.Refactor.dll src/CommandLine.fs
	fsharpc $(OPTS) $(REFS) --target:exe -o:bin/FSharpRefactor.exe -r:libs/FSharp.Refactor.dll src/CommandLine.fs

run-tests: libs/FSharp.Refactor.Tests.dll bin/FSharpRefactor.exe tests/command-line-tests.sh
	mono /usr/lib/nunit/nunit-console.exe -noresult libs/FSharp.Refactor.Tests.dll
	export MONO_PATH=libs; tests/command-line-tests.sh

tags: 
	ctags -e $(SOURCES) $(TESTS)
