REFS=-r:libs/FSharp.Compiler.dll \
	-r:/usr/lib/nunit/nunit.framework.dll \
	-r:libs/Options.dll \
	-r:libs/FSharp.Compiler.CodeDom.dll

OPTS=--target:library --nologo --lib:libs

SOURCES=src/Ast.fs \
	src/CodeAnalysis.fs \
	src/CodeTransforms.fs \
	src/RefactoringWorkflow.fs \
	src/CodeTemplates.fs \
    src/Rename.fs \
	src/ExtractFunction.fs \
	src/AddArgument.fs

EVALUATOR_SOURCES=src/evaluator/BehaviourChecker.fs \
				  src/evaluator/CodeGenerator.fs

TESTS=tests/EngineTests.fs \
      tests/RenameTests.fs \
      tests/ExtractFunctionTests.fs \
      tests/AddArgumentTests.fs \
      tests/EvaluatorTests.fs

INTEGRATION_TESTS=tests/integration_tests/CompilerTests.fs

all: libs/FSharp.Refactor.dll libs/FSharp.Refactor.Tests.dll bin/FSharpRefactor.exe libs/FSharp.Refactor.Evaluator.dll

libs/FSharp.Refactor.Evaluator.dll: $(EVALUATOR_SOURCES)
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.Evaluator.dll $(REFS) $(EVALUATOR_SOURCES)

libs/FSharp.Refactor.dll: $(SOURCES)
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.dll $(REFS) $(SOURCES)

libs/FSharp.Refactor.Tests.dll: libs/FSharp.Refactor.Evaluator.dll libs/FSharp.Refactor.dll $(TESTS)
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.Tests.dll -r:libs/FSharp.Refactor.dll -r:libs/FSharp.Refactor.Evaluator.dll $(REFS) $(TESTS)

libs/FSharp.Refactor.IntegrationTests.dll: libs/FSharp.Refactor.Evaluator.dll
	fsharpc $(OPTS) -o:libs/FSharp.Refactor.IntegrationTests.dll -r:libs/FSharp.Refactor.Evaluator.dll $(REFS) $(INTEGRATION_TESTS)

bin/FSharpRefactor.exe: libs/Options.dll libs/FSharp.Refactor.dll src/CommandLine.fs
	fsharpc $(OPTS) $(REFS) --target:exe -o:bin/FSharpRefactor.exe -r:libs/FSharp.Refactor.dll src/CommandLine.fs

unit-tests: libs/FSharp.Refactor.Tests.dll
	mono /usr/lib/nunit/nunit-console.exe -noresult libs/FSharp.Refactor.Tests.dll

tests: libs/FSharp.Refactor.Tests.dll libs/FSharp.Refactor.IntegrationTests.dll bin/FSharpRefactor.exe tests/command-line-tests.sh
	mono /usr/lib/nunit/nunit-console.exe -noresult libs/FSharp.Refactor.Tests.dll libs/FSharp.Refactor.IntegrationTests.dll
	export MONO_PATH=libs; tests/command-line-tests.sh

tags: 
	ctags -e $(SOURCES) $(TESTS)
