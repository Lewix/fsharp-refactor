" Vim filetype plugin for refactoring F# programs
" Last Change:	2013 Jan 14
" Maintainer:	Lewis Brown <lab61@cam.ac.uk>

"if exists("g:loaded_fsharp_refactor")
"    finish
"endif
"let g:loaded_fsharp_refactor = 1

command! -nargs=? FSharpRename call <SID>FSharpRename(<q-args>)
command! -nargs=? -range FSharpExtractFunction call <SID>FSharpExtractFunction(<q-args>)
command! -nargs=* FSharpAddArgument call <SID>FSharpAddArgument(<q-args>)

function! SetBufferContents(new_contents)
    silent %delete
    call append(0, split(a:new_contents, '\v\n'))
    silent $delete
endfunction

function! GetBufferContents()
    let old_a = @a
    silent %yank a
    let contents = @a
    let @a = old_a
    return contents
endfunction

function! RunWithErrors(command, input)
    let old_shellredir = &shellredir
    let error_file = tempname()
    let &shellredir = "2>".error_file." >"
    let stdout = system(a:command, a:input)
    let stderr = join(readfile(error_file), "\n")
    if stderr == ""
        let result = {'stdout': stdout, 'has_errors': 0}
    else
        let result = {'stdout': stdout, 'has_errors': 1, 'stderr': stderr}
    endif
    let &shellredir = old_shellredir
    return result
endfunction

function! SaveCursorPosition()
    return {'line': line('.'), 'col': col('.')}
endfunction

function! RestoreCursorPosition(position)
   execute "normal! gg".a:position.line."jk".a:position.col."|"
endfunction

function! Refactor(command)
    let position = SaveCursorPosition()
    let result = RunWithErrors(a:command, GetBufferContents())
    if result.has_errors
        redraw
        echom result.stderr
    else
        call SetBufferContents(result.stdout)
    endif
    call RestoreCursorPosition(position)
endfunction


function! s:FSharpRename(name)
    if a:name == ""
        let new_name = input("New name:")
    else
        let new_name = a:name
    endif
    let position = line(".").":".col(".")
    let command = "mono /mnt/media/bookmarks/p/FSharpRefactor.exe rename ".position." ".new_name
    call Refactor(command)
endfunction

function! s:FSharpExtractFunction(name)
    if a:name == ""
        let function_name = input("Function name:")
    else
        let function_name = a:name
    endif
    let start_position = line("'<").":".col("'<")
    let end_position = line("'>").":".col("'>")
    let command = "mono /mnt/media/bookmarks/p/FSharpRefactor.exe extract-function ".start_position." ".end_position." ".function_name
    call Refactor(command)
endfunction

function! s:FSharpAddArgument(args)
    let args = split(a:args)
    let nargs = len(args)
    if nargs == 0
        let argument_name = input("Argument name:")
        let default_value = input("Default value:")
    elseif nargs == 1
        let argument_name = args
        let default_value = input("Default value:")
    elseif nargs == 2
        let argument_name = args[0]
        let default_value = args[1]
    endif

    let position = line(".").":".col(".")
    let command = "mono /mnt/media/bookmarks/p/FSharpRefactor.exe add-argument ".position." ".argument_name." ".default_value
    call Refactor(command)
endfunction
