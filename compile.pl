:- use_module(lib/comp).
:- use_module(lib/backend/js).

% Main predicate for compiling Prolog files to JavaScript
main :-
    current_prolog_flag(argv, Args),
    (append(_, [InputFile, OutputFile], Args) ->
        format('Compiling ~w to ~w...~n', [InputFile, OutputFile]),
        process_file(InputFile, OutputFile)
    ;
        usage_error
    ).

% Process a Prolog file and compile it to JavaScript
process_file(InputFile, OutputFile) :-
    read_file_to_clauses(InputFile, Clauses),
    
    % Group clauses by predicate
    group_clauses_by_predicate(Clauses, GroupedClauses),
    
    % Compile each group of clauses
    maplist(compile_predicate_group, GroupedClauses, IRList),
    
    % Generate JavaScript code
    generate_js_code(IRList, OutputFile).

% Read a Prolog file and extract its clauses
read_file_to_clauses(File, Clauses) :-
    open(File, read, Stream),
    read_clauses(Stream, Clauses),
    close(Stream),
    format('Read clauses: ~w~n', [Clauses]).

% Read all clauses from a stream
read_clauses(Stream, Clauses) :-
    read_term(Stream, Term, []),
    (Term == end_of_file ->
        Clauses = []
    ;
        Clauses = [Term|Rest],
        read_clauses(Stream, Rest)
    ).

% Group clauses by predicate name/arity
group_clauses_by_predicate(Clauses, GroupedClauses) :-
    group_by_predicate(Clauses, [], GroupedClauses).

% Helper predicate for grouping clauses
group_by_predicate([], Groups, Groups).
group_by_predicate([Clause|Rest], Acc, Groups) :-
    (Clause = (Head :- _) -> true ; Head = Clause),
    functor(Head, Name, Arity),
    Pred = Name/Arity,
    (select(group(Pred, GroupClauses), Acc, NewAcc) ->
        append(GroupClauses, [Clause], UpdatedGroupClauses),
        NewGroups = [group(Pred, UpdatedGroupClauses)|NewAcc]
    ;
        NewGroups = [group(Pred, [Clause])|Acc]
    ),
    group_by_predicate(Rest, NewGroups, Groups).

% Compile a group of clauses for the same predicate
compile_predicate_group(group(Pred, Clauses), IR) :-
    length(Clauses, ClauseCount),
    format('Compiling predicate ~w with ~d clauses~n', [Pred, ClauseCount]),
    comp:compile_clauses(Clauses, IR).

% Generate JavaScript code from IR
generate_js_code(IRList, OutputFile) :-
    % Combine all IR into a single JavaScript module
    combine_ir(IRList, CombinedIR),
    
    % Generate JavaScript code
    catch(
        js:ir_to_js(CombinedIR, JSCode),
        Error,
        (format('Error generating JavaScript: ~w~n', [Error]), fail)
    ),
    
    % Write to output file
    write_file(OutputFile, JSCode).

% Combine multiple IR statements into a single module
combine_ir([], nothing).
combine_ir([IR], IR).
combine_ir([IR1, IR2|Rest], Combined) :-
    combine_ir([IR2|Rest], TailCombined),
    Combined = (IR1 ; TailCombined).

% Write content to a file
write_file(File, Content) :-
    open(File, write, Stream),
    write(Stream, Content),
    close(Stream).

% Display usage error
usage_error :-
    writeln('PLTS Compiler v0.1'),
    writeln('Usage: swipl -q -f compile.pl -- input.pl output.js').

% Initialize the compiler
:- initialization(main).
