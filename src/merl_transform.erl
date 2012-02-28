%% ---------------------------------------------------------------------
%% @author Richard Carlsson <richardc@klarna.com>
%% @copyright 2012 Richard Carlsson
%% @doc Parse transform for merl. Turns strings to templates at
%% compile-time.

-module(merl_transform).

-export([parse_transform/2]).

-include("../include/merl.hrl").


%% TODO: apply to self at compile time, one way or another

%% FIXME: handle partially constant calls; only the text needs to be constant

parse_transform(Forms, _Options) ->
    P = merl:template(hd(?Q("merl:_@function(_@@args)"))),
    erl_syntax:revert_forms(
      erl_syntax_lib:map(fun (T) -> transform(T, P) end,
                         erl_syntax:form_list(Forms))).

transform(T, P) ->
    case merl:match(P, T) of
        {ok, [{args, As}, {function, {atom,_,A}=F}]}
          when A =:= quote ; A =:= template ->
            case lists:all(fun erl_syntax:is_literal/1, [F|As]) of
                true ->
                    [F1|As1] = lists:map(fun erl_syntax:concrete/1, [F|As]),
                    try apply(merl, F1, As1) of
                        T1 -> merl:term(T1)
                    catch
                        throw:_Reason -> T
                    end;
                false -> T
            end;
        _ -> T
    end.
