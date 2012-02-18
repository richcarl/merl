%% ---------------------------------------------------------------------
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2012 Richard Carlsson
%% @doc Unit tests for merl.

-module(merl_tests).

-include("../include/merl.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, merl}).

%% utilities

f(Ts) when is_list(Ts) ->
    lists:flatmap(fun erl_prettypr:format/1, Ts);
f(T) ->
    erl_prettypr:format(T).

g() ->
    {ok, ?Q("foo:bar(42)")}.

%%
%% tests
%%

parse_error_test_() ->
    [?_assertThrow({error, "1: syntax error before: '{'" ++ _},
                   f(merl:quote("{")))
    ].

term_test_() ->
    [?_assertEqual(tuple, erl_syntax:type(merl:term({}))),
     ?_assertEqual("{foo, 42}", f(merl:term({foo, 42})))
    ].

quote_form_test_() ->
    [?_assertEqual("f(X) -> {ok, X}.",
                   f(?Q("f(X) -> {ok, X}."))),
     ?_assertEqual("-module(foo).",
                   f(?Q("-module(foo)."))),
     ?_assertEqual("-import(bar, [f/1, g/2]).",
                   f(?Q("-import(bar, [f/1, g/2])."))),
     ?_assertEqual(("-module(foo)."
                    "-export([f/1])."
                    "f(X) -> {ok, X}."),
                   f(?Q(["-module(foo).\n",
                         "-export([f/1]).\n",
                         "f(X) -> {ok, X}."])))
    ].

quote_term_test_() ->
    [?_assertEqual("foo",
                   f(?Q("foo"))),
     ?_assertEqual("42",
                   f(?Q("42"))),
     ?_assertEqual("{foo, 42}",
                   f(?Q("{foo, 42}"))),
     ?_assertEqual(("1" ++ "2" ++ "3"),
                   f(?Q("1, 2, 3"))),
     ?_assertEqual(("foo" "42" "{}" "true"),
                   f(?Q("foo, 42, {}, (true)")))
    ].

quote_expr_test_() ->
    [?_assertEqual("2 + 2",
                   f(?Q("2 + 2"))),
     ?_assertEqual("f(foo, 42)",
                   f(?Q("f(foo, 42)"))),
     ?_assertEqual("case X of\n  a -> 1;\n  b -> 2\nend",
                   f(?Q("case X of a -> 1; b -> 2 end"))),
     ?_assertEqual(("2 + 2" ++ "f(42)" ++ "catch 22"),
                   f(?Q("2 + 2, f(42), catch 22")))
    ].

quote_try_clause_test_() ->
    [?_assertEqual("(error:R) when R =/= foo -> ok",
                   f(?Q("error:R when R =/= foo -> ok"))),
     %% note that without any context, clauses are printed as fun-clauses
     ?_assertEqual(("(error:badarg) -> badarg"
                    "(exit:normal) -> normal"
                    "(_) -> other"),
                   f(?Q(["error:badarg -> badarg;",
                         "exit:normal -> normal;"
                         "_ -> other"])))
    ].

quote_fun_clause_test_() ->
    [?_assertEqual("(X, Y) when X < Y -> {ok, X}",
                   f(?Q("(X, Y) when X < Y -> {ok, X}"))),
     ?_assertEqual(("(X, Y) when X < Y -> less"
                    "(X, Y) when X > Y -> greater"
                    "(_, _) -> equal"),
                   f(?Q(["(X, Y) when X < Y -> less;",
                         "(X, Y) when X > Y -> greater;"
                         "(_, _) -> equal"])))].

quote_case_clause_test_() ->
    [?_assertEqual("({X, Y}) when X < Y -> X",
                   f(?Q("{X, Y} when X < Y -> X"))),
     ?_assertEqual(("({X, Y}) when X < Y -> -1"
                    "({X, Y}) when X > Y -> 1"
                    "(_) -> 0"),
                   f(?Q(["{X, Y} when X < Y -> -1;",
                         "{X, Y} when X > Y -> 1;"
                         "_ -> 0"])))].

subst_test_() ->
    [?_assertEqual("42", f(merl:subst(merl:template(?Q("_@foo")),
                                      [{foo, merl:term(42)}]))),
     ?_assertEqual("{42}",
                   f(merl:subst(merl:template(?Q("{_@foo}")),
                                [{foo, merl:term(42)}]))),
     ?_assertEqual("{42}",
                   f(merl:subst(merl:template(?Q("{_@foo}")),
                                [{foo, merl:term(42)}]))),
     ?_assertEqual("fun bar/0",
                   f(merl:subst(merl:template(?Q("fun '@foo'/0")),
                                [{foo, merl:term(bar)}]))),
     ?_assertEqual("fun foo/3",
                   f(merl:subst(merl:template(?Q("fun foo/9901")),
                                [{1, merl:term(3)}]))),
     ?_assertEqual("[42]",
                   f(merl:subst(merl:template(?Q("[_@foo]")),
                                [{foo, merl:term(42)}]))),
     ?_assertEqual("[foo, bar]",
                   f(merl:subst(merl:template(?Q("[_@@foo]")),
                                [{foo, [merl:term(foo),merl:term(bar)]}]))),
     ?_assertEqual("foo",
                   f(merl:subst(merl:template(?Q("[_@_foo]")),
                                [{foo, merl:term(foo)}]))),
     ?_assertEqual("-export([foo/1, bar/2]).",
                   f(merl:subst(merl:template(?Q("-export(['@_@foo'/0]).")),
                                [{foo, [erl_syntax:arity_qualifier(
                                          merl:term(foo),
                                          merl:term(1)),
                                        erl_syntax:arity_qualifier(
                                          merl:term(bar),
                                          merl:term(2))
                                       ]}
                                ])))
     ].
