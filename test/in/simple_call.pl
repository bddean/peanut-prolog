:- module(simple_call, []).

hi :- writeln("hi").

top :- call(simple_call:hi).%, call(writeln("bye")).
