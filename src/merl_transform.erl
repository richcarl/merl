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
        fun ([{expr, _}, {line, Line}, {text,Text}]) ->
                erl_syntax:is_literal(Text) andalso erl_syntax:is_literal(Line)
        end,
        fun ([{expr, Expr}, {line, _}, {text, Text}]) ->
                expand_match(Expr, Text)
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
    end;
expand_quote(_As, T, _StartPos) ->
    T.

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
    end;
expand_template(_F, _As, T) ->
    T.

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

expand_match(Expr, Text) ->
    %% we must rewrite the metavariables in the pattern to use lowercase,
    %% and then use real matching to bind the Erlang-level variables
    T0 = merl:template(merl:quote(erl_syntax:concrete(Text))),
    Vars = [V || V <- merl:template_vars(T0), is_inline_metavar(V)],
    T1 = merl:tsubst(T0, [{V, {var_to_tag(V)}} || V <- Vars]),
    Out = erl_syntax:list([erl_syntax:tuple([erl_syntax:atom(var_to_tag(V)),
                                             erl_syntax:variable(V)])
                           || V <- Vars]),
    map(?Q("{ok, _@out} = merl:match(_@template, _@expr)",
           [{expr, Expr}, {out, Out}, {template, erl_syntax:abstract(T1)}])).

var_to_tag(V) ->
    list_to_atom(string:to_lower(atom_to_list(V))).

is_inline_metavar(Var) when is_atom(Var) ->
    is_erlang_var(atom_to_list(Var));
is_inline_metavar(_) -> false.

is_erlang_var([C|_]) when C >= $A, C =< $Z ; C >= $À, C =< $Þ, C /= $× ->
    true;
is_erlang_var(_) ->
    false.
