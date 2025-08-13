:- module(js_identifier,
          [ js_escape_ident/2                 % ?PlainAtom ?JsIdentifier
          ]).

/*======================================================================
  Public relation
  ======================================================================*/

js_escape_ident(PlainAtom, EscapedAtom) :-
        when( (ground(PlainAtom) ; ground(EscapedAtom)),
              transduce(PlainAtom, EscapedAtom) ).

transduce(PlainAtom, EscAtom) :-
        (   ground(PlainAtom)                       % ENCODE  (+,-)
        ->  atom_codes(PlainAtom, PlainCodes),
            phrase(pair(PlainCodes), EscCodes),
            atom_codes(EscAtom, EscCodes)
        ;                                           % DECODE  (-,+)
            atom_codes(EscAtom, EscCodes),
            phrase(pair(PlainCodes), EscCodes),
            atom_codes(PlainAtom, PlainCodes)
        ).

/*======================================================================
   A single DCG that *relates* the two streams
  ======================================================================*/

pair([])     --> [].
pair([C|Cs]) --> pair_char(first, C),   pair_tail(Cs).

pair_tail([])     --> [].

pair_tail([C|Cs]) --> pair_char(rest,  C), pair_tail(Cs).

/*------------------------------------------------------------------*
 *  one character (literal or escaped)                               *
 *------------------------------------------------------------------*/

pair_char(Pos, C) -->                  % keep literally …
        [C], { keep(Pos, C), C =\= 0'$ }, !.

pair_char(_,  C)  -->                  % … otherwise escape
        "$", hex4(H1,H2,H3,H4),
        { bidir_code([H1,H2,H3,H4], C) }.

/* bidirectional bridge between code-point and the four hex digits ---- */

bidir_code(Digits, Code) :-
        (   ground(Code)
        ->  to_hex4(Code, Digits)
        ;   ground_list(Digits)
        ->  hex_value(Digits, Code)
        ;   when(   ground(Code),      to_hex4(Code, Digits)),
            when(   ground_list(Digits), hex_value(Digits, Code))
        ).

ground_list(L) :- maplist(ground, L).

/*------------------------------------------------------------------*
 *  characters that may be kept literally                            *
 *------------------------------------------------------------------*/

keep(first,C) :- char_type(C, alpha) ; C == 0'_.        % A-Z a-z _
keep(rest ,C) :- keep(first,C) ; char_type(C, digit).

/*======================================================================
   Low-level hex helpers
  ======================================================================*/

hex4(A,B,C,D) --> [A,B,C,D].

to_hex4(Code, [H1,H2,H3,H4]) :-
        nibble(Code >> 12 /\ 0xF, H1),
        nibble(Code >>  8 /\ 0xF, H2),
        nibble(Code >>  4 /\ 0xF, H3),
        nibble(Code        /\ 0xF, H4).

nibble(N, D) :-                         % 0..15  -->  ASCII hex digit
        ( N < 10 ->
            D is 0'0 + N
        ;   D is 0'A + N - 10            % upper-case
        ).

hex_value([H1,H2,H3,H4], Code) :-
        hex_digit_val(H1,N1),
        hex_digit_val(H2,N2),
        hex_digit_val(H3,N3),
        hex_digit_val(H4,N4),
        Code is (((N1<<4)+N2)<<8)+((N3<<4)+N4).

hex_digit_val(D, N) :- between(0'0,0'9, D), N is D-0'0.
hex_digit_val(D, N) :- between(0'A,0'F, D), N is D-0'A+10.

/*======================================================================
   Unit tests (plunit)
  ======================================================================*/
