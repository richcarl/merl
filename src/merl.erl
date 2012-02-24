%% ---------------------------------------------------------------------
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2010-2012 Richard Carlsson
%% @doc Metaprogramming in Erlang.

-module(merl).

-export([term/1, var/1, is_metavar/1]).

-export([quote/1, quote/2, qquote/2, qquote/3]).

-export([template/1, subst/2, match/2]).

-export([init_module/1, module_forms/1, add_function/4, add_record/3,
         add_import/3, add_attribute/3, compile/1, compile/2,
         compile_and_load/1, compile_and_load/2]).

-include("../include/merl.hrl").

-export([parse_transform/2]).

%% TODO: simple text visualization of syntax trees, for debugging etc.
%% TODO: work in ideas from smerl to make an almost-drop-in replacement
%% TODO: lifting function that creates a fun that interprets the code

-type tree() :: erl_syntax:syntaxTree().

-type env() :: [{Key::atom(), tree()}].

-type text() :: string() | [string()].

-type location() :: erl_scan:location().


%% ------------------------------------------------------------------------
%% Parse transform for turning strings to templates at compile-time

%% TODO: move out of merl module, apply to self at compile time

%% FIXME: handle partially constant calls; only the text needs to be constant

parse_transform(Forms, _Options) ->
    P = template(hd(?Q("merl:_@function(_@@args)"))),
    erl_syntax:revert_forms(
      erl_syntax_lib:map(fun (T) -> transform(T, P) end,
                         erl_syntax:form_list(Forms))).

transform(T, P) ->
    case match(P, T) of
        {ok, [{args, As}, {function, {atom,_,A}=F}]}
          when A =:= quote ; A =:= template ->
            case lists:all(fun erl_syntax:is_literal/1, [F|As]) of
                true ->
                    [F1|As1] = lists:map(fun erl_syntax:concrete/1, [F|As]),
                    try apply(merl, F1, As1) of
                        T1 -> term(T1)
                    catch
                        throw:_Reason -> T
                    end;
                false -> T
            end;
        _ -> T
    end.

%% ------------------------------------------------------------------------
%% Primitives and utility functions

%% TODO: setting line numbers

%% @doc Create a variable.
var(Name) ->
    erl_syntax:variable(Name).

%% @doc Create a syntax tree for a constant term.
term(Term) ->
    erl_syntax:abstract(Term).

-spec is_metavar(tree()) -> {true,string()} | false.

%% TODO: anonymous metavariables, not included in match result
%% TODO: document that multiple occurrences of metavariables are not checked

%% @doc Check if a tree represents a metavariable. Metavariables are atoms
%% starting with `@', variables starting with `_@', or integers starting
%% with `909'. Following the prefix, one or more `_' or `0' characters may
%% be used to indicate "lifting" of the variable one or more levels, and
%% after that, a `@' or `9' character indicates a group metavariable rather
%% than a node metavariable.
is_metavar(Tree) ->
    case erl_syntax:type(Tree) of
        atom ->
            case erl_syntax:atom_name(Tree) of
                "@" ++ Cs when Cs =/= [] -> {true,Cs};
                _ -> false
            end;
        variable ->
            case erl_syntax:variable_literal(Tree) of
                "_@" ++ Cs when Cs =/= [] -> {true,Cs};
                _ -> false
            end;
        integer ->
            case erl_syntax:integer_value(Tree) of
                N when N > 9090 ->
                    case integer_to_list(N) of
                        "909" ++ Cs -> {true,Cs};
                        _ -> false
                    end;
                _ -> false
            end;
        _ -> false
    end.


%% ------------------------------------------------------------------------
%% Parsing and instantiating code fragments

%% The quoting functions always return a list of one or more elements.

%% TODO: setting source line statically vs. dynamically (Erlang vs. DSL source)
%% TODO: only take lists of lines, or plain lines as well? splitting?


-spec qquote(Text::text(), Env::[{Key::atom(),term()}]) -> [term()].

%% @doc Parse text and substitute meta-variables from environment.

qquote(Text, Env) ->
    qquote(1, Text, Env).


-spec qquote(StartPos::location(), Text::text(), Env::env()) -> [tree()].

%% @see quote/2

qquote(StartPos, Text, Env) ->
    lists:flatmap(fun (T) -> subst(T, Env) end, quote(StartPos, Text)).


-spec quote(Text::text()) -> [tree()].

%% @doc Parse text.

quote(Text) ->
    quote(1, Text).


-spec quote(StartPos::location(), Text::text()) -> [tree()].

%% @see quote/1

quote({Line, Col}, Text)
  when is_integer(Line), is_integer(Col), Line > 0, Col > 0 ->
    quote_1(Line, Col, Text);
quote(StartPos, Text) when is_integer(StartPos), StartPos > 0 ->
    quote_1(StartPos, undefined, Text).

quote_1(StartLine, StartCol, Text) ->
    %% be backwards compatible as far as R12, ignoring any starting column
    StartPos = case erlang:system_info(version) of
                   "5.6" ++ _ -> StartLine;
                   "5.7" ++ _ -> StartLine;
                   "5.8" ++ _ -> StartLine;
                   _ when StartCol =:= undefined -> StartLine;
                   _ -> {StartLine, StartCol}
               end,
    {ok, Ts, _} = erl_scan:string(flatten_text(Text), StartPos),
    parse_1(Ts).

flatten_text([L | _]=Lines) when is_list(L) ->
    lists:foldr(fun(S, T) -> S ++ [$\n | T] end, "", Lines);
flatten_text(Text) ->
    Text.

parse_1(Ts) ->
    %% if dot tokens are present, it is assumed that the text represents
    %% complete forms, not dot-terminated expressions or similar
    case split_forms(Ts) of
        {ok, Fs} -> parse_forms(Fs);
        error ->
            parse_2(Ts)
    end.

split_forms(Ts) ->
    split_forms(Ts, [], []).

split_forms([{dot,_}=T|Ts], Fs, As) ->
    split_forms(Ts, [lists:reverse(As, [T]) | Fs], []);
split_forms([T|Ts], Fs, As) ->
    split_forms(Ts, Fs, [T|As]);
split_forms([], Fs, []) ->
    {ok, lists:reverse(Fs)};
split_forms([], [], _) ->
    error;  % no dot tokens found - not representing form(s)
split_forms([], _, [T|_]) ->
    fail("incomplete form after ~p", [T]).

parse_forms([Ts | Tss]) ->
    case erl_parse:parse_form(Ts) of
        {ok, Form} -> [Form | parse_forms(Tss)];
        {error, {_L,M,Reason}} ->
            fail(M:format_error(Reason))
    end;
parse_forms([]) ->
    [].

parse_2(Ts) ->
    %% one or more comma-separated expressions?
    %% (recall that Ts has no dot tokens if we get to this stage)
    case erl_parse:parse_exprs(Ts ++ [{dot,0}]) of
        {ok, Exprs} -> Exprs;
        {error, E} ->
            parse_3(Ts ++ [{'end',0}, {dot,0}], [E])
    end.

parse_3(Ts, Es) ->
    %% try-clause or clauses?
    case erl_parse:parse_exprs([{'try',0}, {atom,0,true}, {'catch',0} | Ts]) of
        {ok, [{'try',_,_,_,_,_}=X]} ->
            %% get the right kind of qualifiers in the clause patterns
            erl_syntax:try_expr_handlers(X);
        {error, E} ->
            parse_4(Ts, [E|Es])
    end.

parse_4(Ts, Es) ->
    %% fun-clause or clauses? (`(a)' is also a pattern, but `(a,b)' isn't,
    %% so fun-clauses must be tried before normal case-clauses
    case erl_parse:parse_exprs([{'fun',0} | Ts]) of
        {ok, [{'fun',_,{clauses,Cs}}]} -> Cs;
        {error, E} ->
            parse_5(Ts, [E|Es])
    end.

parse_5(Ts, Es) ->
    %% case-clause or clauses?
    case erl_parse:parse_exprs([{'case',0}, {atom,0,true}, {'of',0} | Ts]) of
        {ok, [{'case',_,_,Cs}]} -> Cs;
        {error, E} ->
            case lists:last(lists:sort([E|Es])) of
                {L, M, R} when is_atom(M), is_integer(L), L > 0 ->
                    fail("~w: ~s", [L, M:format_error(R)]);
                {{L,C}, M, R} when is_atom(M), is_integer(L), is_integer(C),
                                   L > 0, C > 0 ->
                    fail("~w:~w: ~s", [L,C,M:format_error(R)]);
                {_, M, R} when is_atom(M) ->
                    fail(M:format_error(R));
                R ->
                    fail("unknown parse error: ~p", [R])
            end
    end.


%% ------------------------------------------------------------------------
%% Templates, substitution and matching

%% @doc Turn a syntax tree into a template. Templates can be instantiated or
%% matched against.
%% @see subst/2
%% @see match/2

%% TODO: more optimized template representation; keep ground subtrees intact

%% Leaves are normal syntax trees (generally atomic), and inner nodes are
%% tuples {template,Type,Attrs,Groups} where Groups are lists of lists of nodes.
%% Metavariables are 1-tuples {VarName}, where VarName is an atom or an
%% integer, and can exist both on the group level and the node level.
template(Trees) when is_list(Trees) ->
    [template_0(T) || T <- Trees];
template(Tree) ->
    template_0(Tree).

template_0(Tree) ->
    case template_1(Tree) of
        {Kind,Name} when Kind =:= lift ; Kind =:= group ->
            fail("bad metavariable: '~s'", [Name]);
        Other -> Other
    end.

template_1(Tree) ->
    case erl_syntax:subtrees(Tree) of
        [] ->
            case is_metavar(Tree) of
                {true,"_"++Cs} when Cs =/= [] -> {lift,Cs};
                {true,"0"++Cs} when Cs =/= [] -> {lift,Cs};
                {true,"@"++Cs} when Cs =/= [] -> {group,Cs};
                {true,"9"++Cs} when Cs =/= [] -> {group,Cs};
                {true,Cs} -> {tag(Cs)};
                false -> Tree
            end;
        Gs ->
            Gs1 = [case [template_1(T) || T <- G] of
                       [{group,Name}] -> {tag(Name)};
                       G1 -> check_group(G1), G1
                   end
                   || G <- Gs],
            case lift(Gs1) of
                {true,"_"++Cs} when Cs =/= [] -> {lift,Cs};
                {true,"0"++Cs} when Cs =/= [] -> {lift,Cs};
                {true,"@"++Cs} when Cs =/= [] -> {group,Cs};
                {true,"9"++Cs} when Cs =/= [] -> {group,Cs};
                {true,Cs} -> {tag(Cs)};
                _ ->
                    {template, erl_syntax:type(Tree),
                     erl_syntax:get_attrs(Tree), Gs1}
            end
    end.

%% TODO: should it be allowed to mix group metavars with other elements?

%% group metavariables are only allowed as the only member of their group,
%% so as to not quietly discard the other members

%% FIXME: is this broken? only checks for multiple group metavars in group!

check_group(G) ->
    case [Name || {group,Name} <- G] of
        [] -> ok;
        Names ->
            fail("misplaced group metavariable: ~w", [Names])
    end.

%% convert the remains of the name string back to an integer or atom
tag(Name) ->
    try list_to_integer(Name)
    catch
        error:badarg ->
            list_to_atom(Name)
    end.

%% allow a lifted metavariable in a subgroup to replace the entire node
lift(Gs) ->
    case [Name || {lift,Name} <- lists:concat([G || G <- Gs, is_list(G)])] of
        [] ->
            false;
        [Name] ->
            {true, Name};
        Names ->
            fail("clashing metavariables: ~w", [Names])
    end.


%% @doc Revert a template tree to a normal syntax tree. Any remaining
%% metavariables are turned into @-prefixed atoms or 909-prefixed integers.
tree({template, Type, Attrs, Groups}) ->
    Gs = [case G of
              {Var} when is_atom(Var) ->
                  [erl_syntax:atom(tag("@@"++atom_to_list(Var)))];
              {Var} when is_integer(Var) ->
                  [erl_syntax:integer(tag("9099"++integer_to_list(Var)))];
              _ ->
                  [tree(T) || T <- G]
          end
          || G <- Groups],
    erl_syntax:set_attrs(erl_syntax:make_tree(Type, Gs), Attrs);
tree({Var}) when is_atom(Var) ->
    erl_syntax:atom("@"++atom_to_list(Var));
tree({Var}) when is_integer(Var) ->
    erl_syntax:integer(tag("909"++integer_to_list(Var)));
tree(Leaf) ->
    Leaf.  % any syntax tree, not necessarily atomic (due to substitutions)


%% @doc Substitute metavariables, both on group and node level.
subst(Trees, Env) when is_list(Trees) ->
    [subst_0(T, Env) || T <- Trees];
subst(Tree, Env) ->
    subst_0(Tree, Env).

subst_0(Tree, Env) ->
    %% TODO: can we do this faster instead of going via the template form?
    tree(subst_1(ensure_template(Tree), Env)).

%% handle both trees and templates as input
ensure_template({template, _, _, _}=Template) -> Template;
ensure_template({_}=Template) -> Template;
ensure_template(Tree) -> template(Tree).

subst_1({template, Type, Attrs, Groups}, Env) ->
    Gs1 = [case G of
               {Var} ->
                   case lists:keyfind(Var, 1, Env) of
                       {Var, G1} when is_list(G1) ->
                           G1;
                       {Var, _} ->
                           fail("value of group metavariable "
                                "must be a list: '~s'", [Var]);
                       false -> {Var}
                   end;
               _ ->
                   lists:flatten([subst_1(T, Env) || T <- G])
           end
           || G <- Groups],
    {template, Type, Attrs, Gs1};
subst_1({Var}, Env) ->
    case lists:keyfind(Var, 1, Env) of
        {Var, TreeOrTrees} -> TreeOrTrees;
        false -> {Var}
    end;
subst_1(Leaf, _Env) ->
    Leaf.

%% Matches a pattern tree against a ground tree (or patterns against ground
%% trees) returning an environment mapping variable names to subtrees;
%% the environment is always sorted on keys

%% TODO: instead of sorting at the end, use an orddict accumulator

match(Patterns, Trees) when is_list(Patterns), is_list(Trees) ->
    try {ok, sort(lists:foldr(fun ({P, T}, Env) -> match_0(P, T) ++ Env end,
                              [], lists:zip(Patterns, Trees)))}
    catch
        error -> error
    end;
match(Pattern, Tree) ->
    try {ok, sort(match_0(Pattern, Tree))}
    catch
        error -> error
    end.

sort(Env) ->
    lists:keysort(1, Env).

match_0(Pattern, Tree) ->
    match_template(ensure_template(Pattern), Tree).

%% match a template against a syntax tree
match_template({template, Type, _, Gs}, Tree) ->
    case erl_syntax:type(Tree) of
        Type ->
            match_template_1(Gs, erl_syntax:subtrees(Tree));
        _ ->
            throw(error)  % type mismatch
    end;
match_template({Var}, Tree) ->
    [{Var, Tree}];
match_template({template,_,_,_}, _Tree) ->
    throw(error);  % non-leaf vs leaf
match_template(Tree1, Tree2) ->
    %% if Tree1 is not a template, Tree1 and Tree2 are both syntax trees
    match_trees(Tree1, Tree2).

match_template_1([{Var} | Gs1], [Group | Gs2]) ->
    [{Var, Group}] ++ match_template_1(Gs1, Gs2);
match_template_1([G1 | Gs1], [G2 | Gs2]) ->
    match_template_2(G1, G2) ++ match_template_1(Gs1, Gs2);
match_template_1([], []) ->
    [];
match_template_1(_, _) ->
    throw(error).  % shape mismatch

match_template_2([{Var} | Ts1], [Tree | Ts2]) ->
    [{Var, Tree}] ++ match_template_2(Ts1, Ts2);
match_template_2([T1 | Ts1], [T2 | Ts2]) ->
    match_template(T1, T2) ++ match_template_2(Ts1, Ts2);
match_template_2([], []) ->
    [];
match_template_2(_, _) ->
    throw(error).  % shape mismatch

%% match two syntax trees, ignoring metavariables on either side
match_trees(T1, T2) ->
    Type1 = erl_syntax:type(T1),
    case erl_syntax:type(T2) of
        Type1 ->
            case erl_syntax:subtrees(T1) of
                [] ->
                    case erl_syntax:subtrees(T2) of
                        [] ->
                            case compare_leaves(Type1, T1, T2) of
                                true -> [];
                                false -> throw(error)  % different leaves
                            end;
                        Gs2 -> throw(error)  % shape mismatch
                    end;
                Gs1 ->
                    case erl_syntax:subtrees(T2) of
                        [] -> throw(error);  % shape mismatch
                        Gs2 -> match_trees_1(Gs1, Gs2)
                    end
            end;
        _Type2 ->
            throw(error)  % different tree types
    end.

match_trees_1([G1 | Gs1], [G2 | Gs2]) ->
    match_trees_2(G1, G2) ++ match_trees_1(Gs1, Gs2);
match_trees_1([], []) ->
    [];
match_trees_1(_, _) ->
    throw(error).  % shape mismatch

match_trees_2([T1 | Ts1], [T2 | Ts2]) ->
    match_trees(T1, T2) ++ match_trees_2(Ts1, Ts2);
match_trees_2([], []) ->
    [];
match_trees_2(_, _) ->
    throw(error).  % shape mismatch

compare_leaves(Type, T1, T2) ->
    case Type of
        atom ->
            erl_syntax:atom_value(T1)
                =:= erl_syntax:atom_value(T2);
        char ->
            erl_syntax:char_value(T1)
                =:= erl_syntax:char_value(T2);
        float ->
            erl_syntax:float_value(T1)
                =:= erl_syntax:float_value(T2);
        integer ->
            erl_syntax:integer_value(T1)
                =:= erl_syntax:integer_value(T2);
        string ->
            erl_syntax:string_value(T1)
                =:= erl_syntax:string_value(T2);
        operator ->
            erl_syntax:operator_name(T1)
                =:= erl_syntax:operator_name(T2);
        text ->
            erl_syntax:text_string(T1)
                =:= erl_syntax:text_string(T2);
        variable ->
            erl_syntax:variable_name(T1)
                =:= erl_syntax:variable_name(T2);
        _ ->
            true  % trivially equal nodes
    end.


%% ------------------------------------------------------------------------
%% Compiling and loading code directly to memory

%% @equiv compile(Code, [])
compile(Code) ->
    compile(Code, []).

%% @doc Compile a syntax tree or list of syntax trees representing a module
%% into a binary BEAM object.
%% @see compile_and_load/2
%% @see compile/1
compile(Code, Options) when not is_list(Code)->
    case erl_syntax:type(Code) of
        form_list -> compile(erl_syntax:form_list_elements(Code));
        _ -> compile([Code], Options)
    end;
compile(Code, Options0) when is_list(Options0) ->
    Forms = [erl_syntax:revert(F) || F <- Code],
    Options = [verbose, report_errors, report_warnings, binary | Options0],
    %% Note: modules compiled from forms will have a '.' as the last character
    %% in the string given by proplists:get_value(source,
    %% erlang:get_module_info(ModuleName, compile)).
    compile:noenv_forms(Forms, Options).


%% @equiv compile_and_load(Code, [])
compile_and_load(Code) ->
    compile_and_load(Code, []).

%% @doc Compile a syntax tree or list of syntax trees representing a module
%% and load the resulting module into memory.
%% @see compile/2
%% @see compile_and_load/1
compile_and_load(Code, Options) ->
    case compile(Code, Options) of
        {ok, ModuleName, Binary} ->
            code:load_binary(ModuleName, "", Binary),
            {ok, Binary};
        Other -> Other
    end.


%% ------------------------------------------------------------------------
%% Making it simple to build a module

%% TODO: put in separate module, apply transform

-record(module, { name          :: atom()
                , exports=[]    :: [{atom(), integer()}]
                , imports=[]    :: [{atom(), [{atom(), integer()}]}]
                , records=[]    :: [{atom(), [{atom(), term()}]}]
                , attributes=[] :: [{atom(), [term()]}]
                , functions=[]  :: [{atom(), {[term()],[term()],[term()]}}]
                }).

%% TODO: init module from a list of forms (from various sources)

%% @doc Create a new module representation, using the given module name.
init_module(Name) when is_atom(Name) ->
    #module{name=Name}.

%% TODO: setting current file (between forms)

%% @doc Get the list of syntax tree forms for a module representation. This can
%% be passed to compile/2.
module_forms(#module{name=Name,
                     exports=Xs,
                     imports=Is,
                     records=Rs,
                     attributes=As,
                     functions=Fs})
  when is_atom(Name), Name =/= undefined ->
    [Module] = ?Q("-module('@name').", [{name,term(Name)}]),
    [Export] = ?Q("-export(['@_@name'/1]).",
                  [{name, [erl_syntax:arity_qualifier(term(N), term(A))
                           || {N,A} <- ordsets:from_list(Xs)]}]),
    Imports = lists:concat([?Q("-import('@module', ['@_@name'/1]).",
                               [{module,term(M)},
                                {name,[erl_syntax:arity_qualifier(term(N),
                                                                  term(A))
                                       || {N,A} <- ordsets:from_list(Ns)]}])
                            || {M, Ns} <- Is]),
    Records = lists:concat([?Q("-record('@name',{'@_@fields'}).",
                               [{name,term(N)},
                                {fields,[erl_syntax:record_field(term(F),
                                                                 term(V))
                                         || {F,V} <- Es]}])
                            || {N,Es} <- lists:reverse(Rs)]),
    Attrs = lists:concat([?Q("-'@name'('@term').",
                             [{name,term(N)}, {term,term(T)}])
                          || {N,T} <- lists:reverse(As)]),
    [Module, Export | Imports ++ Records ++ Attrs ++ lists:reverse(Fs)].

%% @doc Add a function to a module representation.
add_function(Exported, Name, Clauses, #module{exports=Xs, functions=Fs}=M)
  when is_boolean(Exported), is_atom(Name), Clauses =/= [] ->
    Arity = length(erl_syntax:clause_patterns(hd(Clauses))),
    Xs1 = case Exported of
              true -> [{Name,Arity} | Xs];
              false -> Xs
          end,
    M#module{exports=Xs1,
             functions=[erl_syntax:function(term(Name), Clauses) | Fs]}.

%% @doc Add an import declaration to a module representation.
add_import(From, Names, #module{imports=Is}=M)
  when is_atom(From), is_list(Names) ->
    M#module{imports=[{From, Names} | Is]}.

%% @doc Add a record declaration to a module representation.
add_record(Name, Fs, #module{records=Rs}=M) when is_atom(Name) ->
    M#module{records=[{Name, Fs} | Rs]}.

%% @doc Add a "wild" attribute, such as `-compile(Opts)' to a module
%% representation. Note that such attributes can only have a single argument.
add_attribute(Name, Term, #module{attributes=As}=M) when is_atom(Name) ->
    M#module{attributes=[{Name, Term} | As]}.


%% ------------------------------------------------------------------------
%% Internal utility functions

fail(Text) ->
    fail(Text, []).

fail(Fs, As) ->
    throw({error, lists:flatten(io_lib:format(Fs, As))}).
