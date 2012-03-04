%% ---------------------------------------------------------------------
%% @author Richard Carlsson <richardc@klarna.com>
%% @copyright 2012 Richard Carlsson
%% @doc Parse transform for merl. Evaluates calls to functions in `merl',
%% turning strings to templates, etc., at compile-time.

-module(merl_transform).

-export([parse_transform/2]).

-include("../include/merl.hrl").


parse_transform(Forms, _Options) ->
    erl_syntax:revert_forms(
      erl_syntax_lib:map(fun (T) -> transform(T) end,
                         erl_syntax:form_list(Forms))).

transform(T) ->
    case merl:match(?Q("merl:_@function(_@@args)"), [T]) of
        {ok, [{args, As}, {function, F}]} ->
            case lists:all(fun erl_syntax:is_literal/1, [F|As]) of
                true ->
                    [F1|As1] = lists:map(fun erl_syntax:concrete/1, [F|As]),
                    eval_call(merl, F1, As1, T);
                false ->
                    case merl:match(?Q("qquote"), [F]) of
                        {ok, _} ->
                            expand_qquote(As, T, 1);
                        error ->
                            case merl:match(?Q("subst"), [F]) of
                                {ok, _} ->
                                    expand_subst(As, T);
                                error ->
                                    case merl:match(?Q("match"), [F]) of
                                        {ok, _} ->
                                            expand_match(As, T);
                                        error ->
                                            T
                                    end
                            end
                    end
            end;
        error ->
            T
    end.

expand_qquote([StartPos, Text, Env], T, _) ->
    case erl_syntax:is_literal(StartPos) of
        true ->
            expand_qquote([Text, Env], T, erl_syntax:concrete(StartPos));
        false ->
            T
    end;
expand_qquote([Text, Env], T, StartPos) ->
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

expand_subst([Pattern, Env], T) ->
    case erl_syntax:is_literal(Pattern) of
        true ->
            As = [erl_syntax:concrete(Pattern)],
            [T1] = ?Q("merl:subst(_@pattern, _@env)",
                      [{pattern, eval_call(merl, template, As, T)},
                       {env, Env}]),
            T1;
        false ->
            T
    end.

expand_match([Pattern, Tree], T) ->
    case erl_syntax:is_literal(Pattern) of
        true ->
            As = [erl_syntax:concrete(Pattern)],
            [T1] = ?Q("merl:match(_@pattern, _@tree)",
                      [{pattern, eval_call(merl, template, As, T)},
                       {tree, Tree}]),
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
