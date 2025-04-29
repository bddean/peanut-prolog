:- use_module(lib/comp).
:- use_module(lib/backend/js).

% Main predicate for compiling Prolog files to JavaScript
main :-
    current_prolog_flag(argv, Args),
    (append(_, [InputFile, OutputFile], Args) ->
        ( process_file(InputFile, OutputFile) -> true
				; writeln("Failed to compile."), halt(1) )
    ;
        usage_error
    ).

% Process a Prolog file and compile it to JavaScript
process_file(InputFile, OutputFile) :-
    % Use comp:compile_file to read file and compile terms
    comp:compile_file(js:ir_to_js, InputFile, JSCode),

    % Write to output file
    write_file(OutputFile, JSCode).

% Write content to a file
write_file(File, Content) :-
    open(File, write, Stream),
    write(Stream, Content),
    close(Stream).

% Display usage error
usage_error :-
    writeln('PLTS Compiler v0.1'),
    writeln('Usage: swipl -q -f compile.pl -- input.pl output.js').

:- main.
:- halt.
