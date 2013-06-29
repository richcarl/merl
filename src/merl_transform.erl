%% ---------------------------------------------------------------------
%% @author Richard Carlsson <richardc@klarna.com>
%% @copyright 2012 Richard Carlsson
%% @doc Parse transform for merl. Evaluates calls to functions in `merl',
%% turning strings to templates, etc., at compile-time.

-module(merl_transform).

-export([parse_transform/2]).

%% NOTE: We cannot use inline metavariables or any other parse transform
%% features in this module, because it must be possible to compile it with
%% the parse transform disabled!
-include("../include/merl.hrl").

%% TODO: unroll calls to switch? it will probably get messy

parse_transform(Forms, _Options) ->
    erl_syntax:revert_forms(map(erl_syntax:form_list(Forms))).

map(Tree0) ->
    Tree = pre(Tree0),
    post(case erl_syntax:subtrees(Tree) of
             [] ->
                 Tree;
             Gs ->
                 erl_syntax:update_tree(Tree, [[map(T) || T <- G] || G <- Gs])
         end).

pre(T) ->
    merl:switch(
      T,
      [{?Q("merl:quote(_@line, _@text) = _@expr"),
        fun ([{expr, _}, {line, _}, {text,Text}]) ->
                erl_syntax:is_literal(Text)
        end,
        fun ([{expr, _}, {line, _}, {text, Text}]=Env) ->
                expand_match(Text, Env, T)
        end},
       fun () -> T end
      ]).

post(T) ->
    merl:switch(
      T,
      [{?Q("merl:_@function(_@@args)"),
        [{fun ([{args, As}, {function, F}]) ->
                  lists:all(fun erl_syntax:is_literal/1, [F|As])
          end,
          fun ([{args, As}, {function, F}]) ->
                  [F1|As1] = lists:map(fun erl_syntax:concrete/1, [F|As]),
                  eval_call(F1, As1, T)
          end},
         fun ([{args, As}, {function, F}]) ->
                 merl:switch(
                   F,
                   [{?Q("qquote"), fun ([]) -> expand_quote(As, T, 1) end},
                    {?Q("subst"), fun ([]) -> expand_template(F, As, T) end},
                    {?Q("match"), fun ([]) -> expand_template(F, As, T) end},
                    fun () -> T end
                   ])
         end]},
       fun () -> T end]).

expand_quote([StartPos, Text, Env], T, _) ->
    %% TODO: use switch guard instead of is_literal test here
    case erl_syntax:is_literal(StartPos) of
        true ->
            expand_quote([Text, Env], T, erl_syntax:concrete(StartPos));
        false ->
            T
    end;
expand_quote([Text, Env], T, StartPos) ->
    %% TODO: use switch guard instead of is_literal test here
    case erl_syntax:is_literal(Text) of
        true ->
            As = [StartPos, erl_syntax:concrete(Text)],
            % keep expanding if possible
            map(?Q("merl:subst(_@tree, _@env)",
                   [{tree, eval_call(quote, As, T)},
                    {env, Env}]));
        false ->
            T
    end.

expand_template(F, [Pattern | Args], T) ->
    %% TODO: use switch guard instead of is_literal test here
    case erl_syntax:is_literal(Pattern) of
        true ->
            As = [erl_syntax:concrete(Pattern)],
            ?Q("merl:_@function(_@pattern, _@args)",
               [{function, F},
                {pattern, eval_call(template, As, T)},
                {args, Args}]);
        false ->
            T
    end.

eval_call(F, As, T) ->
    try apply(merl, F, As) of
        T1 when F =:= quote ->
            %% lift metavariables in a template to Erlang variables
            Template = merl:template(T1),
            Vars = merl:template_vars(Template),
            case lists:any(fun is_inline_metavar/1, Vars) of
                true when is_list(T1) ->
                    ?Q("merl:tree([_@template])",
                       [{template, merl:meta_template(Template)}]);
                true ->
                    ?Q("merl:tree(_@template)",
                       [{template, merl:meta_template(Template)}]);
                false ->
                    merl:term(T1)
            end;
        T1 ->
            merl:term(T1)
    catch
        throw:_Reason -> T
    end.

expand_match(Text, Env, _T) ->
    _Text1 = erl_syntax:concrete(Text),
    %% TODO: must get variables from Text and match on output env
    map(?Q("{ok, 42} = merl:match(merl:quote(_@line, _@text), _@expr)", Env)).

is_inline_metavar(Var) when is_atom(Var) ->
    is_erlang_var(atom_to_list(Var));
is_inline_metavar(_) -> false.

is_erlang_var([C|_]) when C >= $A, C =< $Z ; C >= $À, C =< $Þ, C /= $× ->
    true;
is_erlang_var(_) ->
    false.
