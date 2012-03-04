%% ---------------------------------------------------------------------
%% @author Richard Carlsson <richardc@klarna.com>
%% @copyright 2012 Richard Carlsson
%% @doc Parse transform for merl. Evaluates calls to functions in `merl',
%% turning strings to templates, etc., at compile-time.

-module(merl_transform).

-export([parse_transform/2]).

-include("../include/merl.hrl").


%% TODO: unroll calls to switch? it will probably get messy

parse_transform(Forms, _Options) ->
    erl_syntax:revert_forms(
      erl_syntax_lib:map(fun (T) -> transform(T) end,
                         erl_syntax:form_list(Forms))).

transform(T) ->
    merl:switch(
      [T],
      [{?Q("merl:_@function(_@@args)"),
        [{fun ([{args, As}, {function, F}]) ->
                  lists:all(fun erl_syntax:is_literal/1, [F|As])
          end,
          fun ([{args, As}, {function, F}]) ->
                  [F1|As1] = lists:map(fun erl_syntax:concrete/1, [F|As]),
                  eval_call(merl, F1, As1, T)
          end},
         fun ([{args, As}, {function, F}]) ->
                 merl:switch(
                   [F],
                   [{?Q("qquote"), fun ([]) -> expand_quote(As, T, 1) end},
                    {?Q("subst"), fun ([]) -> expand_template(F, As, T) end},
                    {?Q("match"), fun ([]) -> expand_template(F, As, T) end},
                    fun () -> T end
                   ])
         end]},
       fun () -> T end]).

expand_quote([StartPos, Text, Env], T, _) ->
    case erl_syntax:is_literal(StartPos) of
        true ->
            expand_quote([Text, Env], T, erl_syntax:concrete(StartPos));
        false ->
            T
    end;
expand_quote([Text, Env], T, StartPos) ->
    case erl_syntax:is_literal(Text) of
        true ->
            As = [StartPos, erl_syntax:concrete(Text)],
            [T1] = ?Q("merl:subst(_@tree, _@env)",
                      [{tree, eval_call(merl, quote, As, T)},
                       {env, Env}]),
            transform(T1);  % keep expanding if possible
        false ->
            T
    end.

expand_template(F, [Pattern | Args], T) ->
    case erl_syntax:is_literal(Pattern) of
        true ->
            As = [erl_syntax:concrete(Pattern)],
            [T1] = ?Q("merl:_@function(_@pattern, _@args)",
                      [{function, F},
                       {pattern, eval_call(merl, template, As, T)},
                       {args, Args}]),
            T1;
        false ->
            T
    end.

eval_call(M, F, As, T) ->
    try apply(M, F, As) of
        T1 -> merl:term(T1)
    catch
        throw:_Reason -> T
    end.
