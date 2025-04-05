:- use_module(library(readutil)).
:- use_module(library(http/json)).

pre -->
	"const{add_fact,run_goal,Var,Term}=require('./pl.js');",
	"((",
	"_v,_,V=n=>_v[n]??(_v[n]=new Var()),T=(a,r)=>new Term(a,r),",
	"A"
	")=>{".

post --> "})([])".

fact_args(A, A, 0) :- atomic(A), !.
fact_args((H :- _), T, N) :- !, fact_args(H, T, N).
fact_args(T, Name, Arity) :- functor(T, Name, Arity).


out(Terms) --> pre, defs(Terms), post.
defs([]) --> "".
defs([H|T]) --> def(H), defs(T).
def(:-(G)) --> !, "run_goal(", expr(G), ");".
def(T) -->
  "add_fact(",
 {fact_args(T, Tag, Arity)},
 expr(Tag), ",",
 expr(Arity), ",",
 expr(T),
 ");".

expr('$VAR'(N)) --> !, {number_string(N, S)}, "V(", S, ")".
expr(N) --> {number(N), number_string(N, S)}, !, S. %% TODO...
expr(At) --> {atomic(At)}, !, {
  atom_json_term(Esc, At, []),
	atom_string(Esc, Escaped)
}, Escaped.
expr(T) -->
	{T =.. [Tag|Args]},
	"T(", expr(Tag),	",",js_list(Args), ")".
js_list(X) --> "[", js_list_(X), "]".
js_list_([]) --> "[]".
js_list_([X]) --> expr(X).
js_list_([X|Xs]) --> expr(X), ",", js_list_(Xs).

:-
	read_file_to_terms(
		"./test.pl",
		Ts,
		[]
	),
	maplist(expand_term, Ts, Ts1),
	writeln(Ts1), nl, nl,
	fail,


	numbervars(Ts, 0, _),
	once(phrase(out(Ts), W)),
	atom_codes(A, W),
	write(A), fail ; halt.
