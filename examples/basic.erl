%% ---------------------------------------------------------------------
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2012 Richard Carlsson
%% @doc Trivial Basic interpreter in Erlang

-module(basic).

-export([run/2]).

-include_lib("eunit/include/eunit.hrl").

-define(INTERPRETED, true).
-include("basic_test.erl").

run(N, Prog) ->
    ets:new(var, [private, named_table]),
    ets:new(line, [private, named_table, ordered_set]),
    lists:foreach(fun (T) -> ets:insert(line, T) end, Prog),
    goto(N).

stop(N) ->
    ets:delete(var),
    ets:delete(line),
    N.

goto('$end_of_table') -> stop(0);
goto(L) ->
    L1 = ets:next(line, L),
    %% user-supplied line numbers might not exist
    case ets:lookup(line, L) of
        [{_, X}] ->
            stmt(X, L1);
        _ ->
            goto(L1)
    end.

stmt({print, S, As}, L) -> io:format(S, [expr(A) || A <- As]), goto(L);
stmt({set, V, X}, L) -> ets:insert(var, {V, expr(X)}), goto(L);
stmt({goto, X}, _L) -> goto(expr(X));
stmt({stop, X}, _L) -> stop(expr(X));
stmt({iff, X, A, B}, _L) ->
    case expr(X) of
        0 -> goto(B);
        _ -> goto(A)
    end.

expr(X) when is_number(X) ; is_list(X) ->
    X;
expr(X) when is_atom(X) ->
    case ets:lookup(var, X) of
        [] -> 0;
        [{_,V}] -> V
    end;
expr({plus, X, Y}) ->
    expr(X) + expr(Y);
expr({equal, X, Y}) ->
    bool(expr(X) == expr(Y));
expr({gt, X, Y}) ->
    bool(expr(X) > expr(Y));
expr({knot, X}) ->
    case expr(X) of
        0 -> 1;
        _ -> 0
    end.

bool(true) -> 1;
bool(false) -> 0.
