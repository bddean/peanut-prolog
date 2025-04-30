# Project

This is a Prolog-to-JS compiler written in SWI-Prolog. Very early stages.

Source is in ./lib. A JS runtime is compiled from ./runtime.ts.

TODO list in in ./TODO.md. Mark items as IN_PROGRESS and DONE as you go
through the list. Feel free to add tasks.


# Style

Commit very often. Commit messages MUST with 'CODEX:'.

You MUST prioritize elegance and completeness. Existing code may not be
exemplary :-)

Very incremental TDD. Use simple inline plunit tests to verify
implementation details as you go, own-file unit tests for more complex
things, but especially integration tests that run end-to-end.

# Useful commands / patterns

swipl -g 'apropos(...)' -g halt

swipl -g 'help(...)' -g halt

swipl -l ... -g 'run_tests' -g halt

swipl -l ... -g 'run_tests(Spec)' -g halt

# Future Project goals

* JS integration and webdev utility 
* Efficient
* Self-hosted
