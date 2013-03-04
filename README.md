F# Refactor
===========

This is a refactoring library for F#. It currently provides the following
refactorings:

  * Rename
  * Extract an expression into a function
  * Add an argument to a function

It can be used either via a command-line interface or using the Vim bindings
(hopefully support for more editors will follow).

Calling it from the command-line is done like so:

  Usage:
    rename <position> <new_name> [<filename>]
    extract-function <expression_range> <function_name> [<filename>]
    add-argument <position> <argument_name> <default_value> [<filename>]

  Options:
    -h, --help                          Display this message and exit
    -i[SUFFIX], --in-place=[SUFFIX]     Modify the input file in-place (makes backup if extension supplied)
    -oFILENAME, --output-file=FILENAME  Write result to FILENAME

Positions should be written as "line:column". Alternatively the Vim bindings
supply the functions:

  * `:FSharpRename`
  * `:FSharpExtractFunction`
  * `:FSharpAddArgument`

Installation
============

In order to use the tool you will need to compile a modified version of the F#
compiler (using the `compiler.patch` patch), and place it in `libs`. You can
then build the library using `make`.

(I haven't tried this on any computer other than my own, running Mono on
ArchLinux)

Future Work
===========

This is by no means a finished product, work remaining to be done includes:

  * Testing on different machines
  * Support for other editors
  * Some fixes to the refactorings
  * Make it run faster on Vim, possibly by not starting everything up every time
  * ...
