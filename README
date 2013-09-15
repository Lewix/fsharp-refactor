F# Refactor
===========

This is a refactoring library for F#. It currently provides the
following refactorings:

  * Rename
  * Extract an expression into a function
  * Add an argument to a function

It can be used either via a command-line interface or using the Vim
bindings. The `Rename` refactoring is also included in the
[`fsharpbinding` add-in for
MonoDevelop](https://github.com/mono-soc-2013/fsharpbinding/tree/fsharp-refactor).

Calling it from the command-line is done like so:

    Usage:
      rename <position> <new_name> [<filename>]
      extract-function <expression_range> <function_name> [<filename>]
      add-argument <position> <argument_name> <default_value> [<filename>]
  
    Options:
      -h, --help                          Display this message and exit
      -i[SUFFIX], --in-place=[SUFFIX]     Modify the input file in-place (makes backup if extension supplied)
      -oFILENAME, --output-file=FILENAME  Write result to FILENAME

Positions should be written as `line:column`. Alternatively the Vim bindings
supply the functions:

  * `:FSharpRename`
  * `:FSharpExtractFunction`
  * `:FSharpAddArgument`

`rename`
--------

`fsharp-refactor` currently allows renaming of identifiers bound in any of the following code constructs:

  * Let statements
  * Lambda abstractions
  * Patterns (for example in match statements or `for ... in` loops)
  * `for ... to` loops

It does not yet allow renaming of types, namespaces or modules. If
the renaming is done via the MonoDevelop add-in, the identifier will
be renamed throughout the containing project (but, as of yet, not the
entire solution).

`extract-function` and `add-argument`
-------------------------------------

These two refactorings are still under development, but you can try
them out using the command-line utility (or, if you're adventurous,
in the MonoDevelop plugin by compiling it yourself after removing the
appropriate comment in the .addin.xml file).

Installation
============

Command-line utility
--------------------

1. Clone the repository
  
       git clone https://github.com/Lewix/fsharp-refactor.git

2. Run `autogen.sh` to generate and run the `configure` script and generate the makefile

       ./autogen.sh

3. Make and install the program

       make
       make install

The program can then be run from the command-line as
`fsharp-refactor`.

MonoDevelop add-in
------------------

Clone, and build the appropriate branch of
[`fsharpbinding`](https://github.com/mono-soc-2013/fsharpbinding/tree/fsharp-refactor)
and install the resulting .mpack file.
