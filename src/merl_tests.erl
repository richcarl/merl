%% ---------------------------------------------------------------------
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2012 Richard Carlsson
%% @doc Unit tests for merl.

-module(merl_tests).

-include_lib("eunit/include/eunit.hrl").

%% utilities

f(Ts) ->
    lists:flatmap(fun erl_prettypr:format/1, Ts).

%%
%% tests
%%

parse_error_test_() ->
    [?_assertThrow(<<"parse error: {error,{0,erl_parse,", _/bytes>>,
                       f(merl:quote(["{"])))
     ].

term_test() ->
    ?assertEqual(tuple, erl_syntax:type(merl:term({}))).

quote_form_test_() ->
    [?_assertEqual("f(X) -> {ok, X}.",
                   f(merl:quote(["f(X) -> {ok, X}."]))),
     ?_assertEqual("-module(foo).",
                   f(merl:quote(["-module(foo)."]))),
     ?_assertEqual("-import(bar, [f/1, g/2]).",
                   f(merl:quote(["-import(bar, [f/1, g/2])."]))),
     ?_assertEqual(("-module(foo)."
                    "-export([f/1])."
                    "f(X) -> {ok, X}."),
                   f(merl:quote(["-module(foo).\n",
                                 "-export([f/1]).\n",
                                 "f(X) -> {ok, X}."])))
    ].

quote_term_test_() ->
    [?_assertEqual("foo",
                   f(merl:quote(["foo"]))),
     ?_assertEqual("{foo, 42}",
                   f(merl:quote(["{foo, 42}"]))),
     ?_assertEqual(("1" "2" "3"),
                   f(merl:quote(["1, 2, 3"]))),
     ?_assertEqual(("foo" "42" "{}" "true"),
                   f(merl:quote(["foo, 42, {}, (true)"])))
    ].

quote_expr_test_() ->
    [?_assertEqual("2 + 2",
                   f(merl:quote(["2 + 2"]))),
     ?_assertEqual("f(foo, 42)",
                   f(merl:quote(["f(foo, 42)"]))),
     ?_assertEqual("case X of\n  a -> 1;\n  b -> 2\nend",
                   f(merl:quote(["case X of a -> 1; b -> 2 end"]))),
     ?_assertEqual(("2 + 2" "f(42)" "catch 22"),
                   f(merl:quote(["2 + 2, f(42), catch 22"])))
    ].

quote_try_clause_test_() ->
    [?_assertEqual("(error:R) when R =/= foo -> ok",
                   f(merl:quote(["error:R when R =/= foo -> ok"]))),
     %% note that without any context, clauses are printed as fun-clauses
     ?_assertEqual(("(error:badarg) -> badarg"
                    "(exit:normal) -> normal"
                    "(_) -> other"),
                   f(merl:quote(["error:badarg -> badarg;",
                                 "exit:normal -> normal;"
                                 "_ -> other"])))
    ].

quote_fun_clause_test_() ->
    [?_assertEqual("(X, Y) when X < Y -> {ok, X}",
                   f(merl:quote(["(X, Y) when X < Y -> {ok, X}"]))),
     ?_assertEqual(("(X, Y) when X < Y -> less"
                    "(X, Y) when X > Y -> greater"
                    "(_, _) -> equal"),
                   f(merl:quote(["(X, Y) when X < Y -> less;",
                                 "(X, Y) when X > Y -> greater;"
                                 "(_, _) -> equal"])))].

quote_case_clause_test_() ->
    [?_assertEqual("({X, Y}) when X < Y -> X",
                   f(merl:quote(["{X, Y} when X < Y -> X"]))),
     ?_assertEqual(("({X, Y}) when X < Y -> -1"
                    "({X, Y}) when X > Y -> 1"
                    "(_) -> 0"),
                   f(merl:quote(["{X, Y} when X < Y -> -1;",
                                 "{X, Y} when X > Y -> 1;"
                                 "_ -> 0"])))].
