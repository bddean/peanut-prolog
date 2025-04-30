:- module(integration_test, [run_tests/0, run_test/1]).

:- use_module(library(process)).

% Main predicate to run all integration tests
run_tests :-
	% Get all test files from the test/in directory
	directory_files('test/in', Files),
	% Filter out non-Prolog files and directories
	include(prolog_file, Files, TestFiles),
	% Run tests for each file
	maplist(run_test, TestFiles),
	writeln('All integration tests completed.').

% Run a single integration test
run_test(TestFile) :-
	% Construct full path
	atomic_list_concat(['test/in/', TestFile], TestPath),
	% Run test in SWI-Prolog to get expected output
	run_prolog(TestPath, PrologOutput, PrologError),
	% Run test with our compiler to get actual output
	run_compiled(TestPath, CompiledOutput, CompiledError),
	% Compare outputs
	compare_outputs(TestFile, PrologOutput, CompiledOutput, PrologError, CompiledError).

% Check if a file is a Prolog file
prolog_file(File) :-
	atom(File),
	file_name_extension(_, 'pl', File),
	\+ atom_concat('.', _, File).  % Filter out hidden files

% Run a Prolog file with SWI-Prolog
run_prolog(TestPath, Output, Error) :-
	process_create(path(swipl), ['-q', '-f', TestPath, '-g', 'main', '-g', 'halt'],
		[stdout(pipe(POut)), stderr(pipe(PErr))]),
	read_stream_to_codes(POut, OutCodes),
	read_stream_to_codes(PErr, ErrCodes),
	close(POut),
	close(PErr),
	atom_codes(Output, OutCodes),
	atom_codes(Error, ErrCodes).

% Run a Prolog file with our compiler and node
run_compiled(TestPath, Output, Error) :-
	% Create temporary file for JS output
	tmp_file(js, TmpJS),
	% Compile the Prolog file to JavaScript
	atomic_list_concat(['@bundle ', TestPath, ' > ', TmpJS], CompileCmd),
	shell(CompileCmd, _),
	% Run the compiled JavaScript with Node.js
	%
	% TODO: When this fails, print JOut and JErr before throwing
	% an error. (Rewriting in Bash may make this easier).
	process_create(path(node), [TmpJS],
		[stdout(pipe(JOut)), stderr(pipe(JErr))]),
	read_stream_to_codes(JOut, OutCodes),
	read_stream_to_codes(JErr, ErrCodes),
	close(JOut),
	close(JErr),
	atom_codes(Output, OutCodes),
	atom_codes(Error, ErrCodes),
	% Clean up temporary file
	delete_file(TmpJS).

% Compare outputs and report differences
compare_outputs(TestFile, PrologOutput, CompiledOutput, PrologError, CompiledError) :-
	(PrologOutput = CompiledOutput, PrologError = CompiledError ->
		format('✓ ~w: Test passed~n', [TestFile])
	;
		format('✗ ~w: Test failed~n', [TestFile]),
		format('  Expected output: ~w~n', [PrologOutput]),
		format('  Actual output: ~w~n', [CompiledOutput]),
		format('  Expected error: ~w~n', [PrologError]),
		format('  Actual error: ~w~n', [CompiledError])
	).
