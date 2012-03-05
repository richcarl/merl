%% ---------------------------------------------------------------------
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2010-2012 Richard Carlsson
%% @doc Metaprogramming in Erlang.

-module(merl).

-export([term/1, var/1]).

-export([quote/1, quote/2, qquote/2, qquote/3]).

-export([template/1, tree/1, subst/2, tsubst/2, match/2, switch/2]).

-export([template_vars/1, meta_template/1]).

-export([init_module/1, module_forms/1, add_function/4, add_record/3,
         add_import/3, add_attribute/3]).

-export([compile/1, compile/2, compile_and_load/1, compile_and_load/2]).

%% NOTE: this module should not include merl.hrl!

%% TODO: simple text visualization of syntax trees, for debugging etc.?
%% TODO: utility function to get free/bound variables in expr?
%% TODO: work in ideas from smerl to make an almost-drop-in replacement
%% TODO: add a lifting function that creates a fun that interprets code?

-type tree() :: erl_syntax:syntaxTree().

-type pattern() :: tree() | template().

-type env() :: [{Key::id(), pattern() | [pattern()]}].

-type id() :: atom() | integer().

%% A list of strings or binaries is assumed to represent individual lines,
%% while a flat string or binary represents source code containing newlines.
-type text() :: string() | binary() | [string()] | [binary()].

-type location() :: erl_scan:location().


%% ------------------------------------------------------------------------
%% Call indirections

init_module(Name) ->
    merl_build:init_module(Name).

module_forms(Module) ->
    merl_build:module_forms(Module).

add_function(Exported, Name, Clauses, Module) ->
    merl_build:add_function(Exported, Name, Clauses, Module).

add_import(From, Names, Module) ->
    merl_build:add_import(From, Names, Module).

add_record(Name, Fs, Module) ->
    merl_build:add_record(Name, Fs, Module).

add_attribute(Name, Term, Module) ->
    merl_build:add_attribute(Name, Term, Module).


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
%% Utility functions

%% TODO: setting line numbers


-spec var(atom()) -> tree().

%% @doc Create a variable.

var(Name) ->
    erl_syntax:variable(Name).


-spec term(term()) -> tree().

%% @doc Create a syntax tree for a constant term.

term(Term) ->
    erl_syntax:abstract(Term).


%% ------------------------------------------------------------------------
%% Parsing and instantiating code fragments

%% TODO: setting source line statically vs. dynamically (Erlang vs. DSL source)


-spec qquote(Text::text(), Env::env()) -> tree() | [tree()].

%% @doc Parse text and substitute meta-variables.
%%
%% @equiv qquote(1, Text, Env)

qquote(Text, Env) ->
    qquote(1, Text, Env).


-spec qquote(StartPos::location(), Text::text(), Env::env()) ->
                    tree() | [tree()].

%% @doc Parse text and substitute meta-variables. Takes an initial scanner
%% starting position as first argument.
%%
%% @see quote/2

qquote(StartPos, Text, Env) ->
    subst(quote(StartPos, Text), Env).


-spec quote(Text::text()) -> [tree()].

%% @doc Parse text.
%%
%% @equiv quote(1, Text)

quote(Text) ->
    quote(1, Text).


-spec quote(StartPos::location(), Text::text()) -> tree() | [tree()].

%% @doc Parse text. Takes an initial scanner starting position as first
%% argument.
%%
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
    case parse_1(Ts) of
        [T] -> T;
        Other -> Other
    end.

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
            %% select the best error to report
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

-spec template(pattern() | [pattern()]) -> template() | [template()].

%% @doc Turn a syntax tree or list of trees into a template or templates.
%% Templates can be instantiated or matched against, and reverted back to
%% normal syntax trees using {@link tree/1}. If the input is already a
%% template, it is not modified further.
%%
%% @see subst/2
%% @see match/2
%% @see tree/1

%% Leaves are normal syntax trees, and inner nodes are tuples
%% {template,Type,Attrs,Groups} where Groups are lists of lists of nodes.
%% Metavariables are 1-tuples {VarName}, where VarName is an atom or an
%% integer. {'_'} and {0} work as anonymous variables in matching. Glob
%% metavariables are tuples {'*',VarName}, and {'*','_'} and {'*',0} are
%% anonymous globs.

%% Note that although template() :: tree() | ..., it is implied that these
%% syntax trees are free from metavariables, so pattern() :: tree() |
%% template() is in fact a wider type than template().

-opaque template() :: tree()
                    | {id()}
                    | {'*',id()}
                    | {template, atom(), term(), [[template()]]}.

template(Trees) when is_list(Trees) ->
    [template_0(T) || T <- Trees];
template(Tree) ->
    template_0(Tree).

template_0({template, _, _, _}=Template) -> Template;
template_0({'*',_}=Template) -> Template;
template_0({_}=Template) -> Template;
template_0(Tree) ->
    case template_1(Tree) of
        false -> Tree;
        {Name} when is_list(Name) ->
            fail("bad metavariable: '~s'", [Name]);
        Template -> Template
    end.

template_1(Tree) ->
    case erl_syntax:subtrees(Tree) of
        [] ->
            case check_meta(Tree) of
                {"_"++Cs}=V when Cs =/= [] -> V;
                {"0"++Cs}=V when Cs =/= [] -> V;
                {"@"++Name} when Name =/= [] -> {'*',tag(Name)};
                {"9"++Name} when Name =/= [] -> {'*',tag(Name)};
                {Name} -> {tag(Name)};
                false -> false
            end;
        Gs ->
            case template_2(Gs, [], false) of
                Gs1 when is_list(Gs1) ->
                    {template, erl_syntax:type(Tree),
                     erl_syntax:get_attrs(Tree), Gs1};
                Other ->
                    Other
            end
    end.

template_2([G | Gs], As, Bool) ->
    case template_3(G, [], false) of
        {"_"++Cs}=V when Cs =/= [] -> V;  % lift further
        {"0"++Cs}=V when Cs =/= [] -> V;  % lift further
        {"@"++Name} when Name =/= [] -> {'*',tag(Name)};  % lifted to here
        {"9"++Name} when Name =/= [] -> {'*',tag(Name)};  % lifted to here
        {Name} when is_list(Name) -> {tag(Name)};  % lifted to here
        false -> template_2(Gs, [G | As], Bool);
        G1 -> template_2(Gs, [G1 | As], true)
    end;
template_2([], _As, false) -> false;
template_2([], As, true) -> lists:reverse(As).

template_3([T | Ts], As, Bool) ->
    case template_1(T) of
        {"_"++Cs} when Cs =/= [] -> {Cs};  % lift
        {"0"++Cs} when Cs =/= [] -> {Cs};  % lift
        false -> template_3(Ts, [T | As], Bool);
        T1 -> template_3(Ts, [T1 | As], true)
    end;
template_3([], _As, false) -> false;
template_3([], As, true) -> lists:reverse(As).


%% @doc Turn a template into a syntax tree representing the template.
%% Meta-variables in the template are turned into normal Erlang variables if
%% their names (after the metavariable prefix characters) begin with an
%% uppercase character. E.g., `_@Foo' in the template becomes the variable
%% `Foo' in the meta-template.

meta_template(Templates) when is_list(Templates) ->
    [meta_template_1(T) || T <- Templates];
meta_template(Template) ->
    meta_template_1(Template).

meta_template_1({template, Type, Attrs, Groups}) ->
    erl_syntax:tuple(
      [erl_syntax:atom(template),
       erl_syntax:atom(Type),
       erl_syntax:abstract(Attrs),
       erl_syntax:list([erl_syntax:list([meta_template_1(T) || T <- G])
                        || G <- Groups])]);
meta_template_1({Var}=V) when is_atom(Var) ->
    meta_template_2(Var, V);
meta_template_1({'*',Var}=V) when is_atom(Var) ->
    meta_template_2(Var, V);
meta_template_1(Leaf) ->
    erl_syntax:abstract(Leaf).

meta_template_2(Var, V) ->
    case atom_to_list(Var) of
        [C|_]=Name when C >= $A, C =< $Z ; C >= $À, C =< $Ş, C /= $× ->
            erl_syntax:variable(Name);
        _ ->
            erl_syntax:abstract(V)
    end.


%% @doc Return an ordered list of the metavariables in the template.

template_vars(Template) ->
    template_vars(Template, []).

template_vars(Templates, Vars) when is_list(Templates) ->
    lists:foldl(fun template_vars_1/2, Vars, Templates);
template_vars(Template, Vars) ->
    template_vars_1(Template, Vars).

template_vars_1({template, _, _, Groups}, Vars) ->
    lists:foldl(fun (G, V) -> lists:foldl(fun template_vars_1/2, V, G) end,
                Vars, Groups);
template_vars_1({Var}, Vars) ->
    ordsets:add_element(Var, Vars);
template_vars_1({'*',Var}, Vars) ->
    ordsets:add_element(Var, Vars);
template_vars_1(_, Vars) ->
    Vars.


-spec tree(template() | [template()]) -> tree() | [tree()].

%% @doc Revert a template to a normal syntax tree. Any remaining
%% metavariables are turned into `@'-prefixed atoms or `909'-prefixed
%% integers.
%% @see template/1

tree(Templates) when is_list(Templates) ->
    [tree_1(T) || T <- Templates];
tree(Template) ->
    tree_1(Template).

tree_1({template, Type, Attrs, Groups}) ->
    %% flattening here is needed for templates created via source transforms
    Gs = [lists:flatten([tree_1(T) || T <- G]) || G <- Groups],
    erl_syntax:set_attrs(erl_syntax:make_tree(Type, Gs), Attrs);
tree_1({Var}) when is_atom(Var) ->
    erl_syntax:atom(list_to_atom("@"++atom_to_list(Var)));
tree_1({Var}) when is_integer(Var) ->
    erl_syntax:integer(list_to_integer("909"++integer_to_list(Var)));
tree_1({'*',Var}) when is_atom(Var) ->
    erl_syntax:atom(list_to_atom("@@"++atom_to_list(Var)));
tree_1({'*',Var}) when is_integer(Var) ->
    erl_syntax:integer(list_to_integer("9099"++integer_to_list(Var)));
tree_1(Leaf) ->
    Leaf.  % any syntax tree, not necessarily atomic (due to substitutions)


-spec subst(pattern() | [pattern()], env()) -> tree() | [tree()].

%% @doc Substitute metavariables in a pattern or list of patterns, yielding
%% a syntax tree or list of trees as result. Both for normal metavariables
%% and glob metavariables, the substituted value may be a single element or
%% a list of elements. For example, if a list representing `1, 2, 3' is
%% substituted for `var' in either of `[foo, _@var, bar]' or `[foo, _@@var,
%% bar]', the result represents `[foo, 1, 2, 3, bar]'.

subst(Trees, Env) when is_list(Trees) ->
    [subst_0(T, Env) || T <- Trees];
subst(Tree, Env) ->
    subst_0(Tree, Env).

subst_0(Tree, Env) ->
    tree_1(subst_1(template(Tree), Env)).


-spec tsubst(pattern() | [pattern()], env()) -> template() | [template()].

%% @doc Like subst/2, but does not convert the result from a template back
%% to a tree. Useful if you want to do multiple separate substitutions.
%% @see subst/2
%% @see tree/2

tsubst(Trees, Env) when is_list(Trees) ->
    [subst_1(template(T), Env) || T <- Trees];
tsubst(Tree, Env) ->
    subst_1(template(Tree), Env).

subst_1({template, Type, Attrs, Groups}, Env) ->
    Gs1 = [lists:flatten([subst_1(T, Env) || T <- G]) || G <- Groups],
    {template, Type, Attrs, Gs1};
subst_1({Var}=V, Env) ->
    case lists:keyfind(Var, 1, Env) of
        {Var, TreeOrTrees} -> TreeOrTrees;
        false -> V
    end;
subst_1({'*',Var}=V, Env) ->
    case lists:keyfind(Var, 1, Env) of
        {Var, TreeOrTrees} -> TreeOrTrees;
        false -> V
    end;
subst_1(Leaf, _Env) ->
    Leaf.


-spec match(pattern() | [pattern()], tree() | [tree()]) ->
                   {ok, env()} | error.

%% @doc Match a pattern against a syntax tree (or patterns against syntax
%% trees) returning an environment mapping variable names to subtrees; the
%% environment is always sorted on keys. Note that multiple occurrences of
%% metavariables in the pattern is not allowed, but is not checked.
%%
%% @see template/1
%% @see switch/2

match(Patterns, Trees) when is_list(Patterns), is_list(Trees) ->
    try {ok, match_1(Patterns, Trees, [])}
    catch
        error -> error
    end;
match(Pattern, Tree) ->
    try {ok, match_template(template(Pattern), Tree, [])}
    catch
        error -> error
    end.

match_1([P|Ps], [T | Ts], Dict) ->
    match_1(Ps, Ts, match_template(template(P), T, Dict));
match_1([], [], Dict) ->
    Dict.

%% match a template against a syntax tree
match_template({template, Type, _, Gs}, Tree, Dict) ->
    case erl_syntax:type(Tree) of
        Type -> match_template_1(Gs, erl_syntax:subtrees(Tree), Dict);
        _ -> throw(error)  % type mismatch
    end;
match_template({Var}, _Tree, Dict)
  when Var =:= '_' ; Var =:= 0 ->
    Dict;  % anonymous variable
match_template({Var}, Tree, Dict) ->
    orddict:store(Var, Tree, Dict);
match_template(Tree1, Tree2, Dict) ->
    %% if Tree1 is not a template, Tree1 and Tree2 are both syntax trees
    case compare_trees(Tree1, Tree2) of
        true -> Dict;
        false -> throw(error)  % different trees
    end.

match_template_1([G1 | Gs1], [G2 | Gs2], Dict) ->
    match_template_2(G1, G2, match_template_1(Gs1, Gs2, Dict));
match_template_1([], [], Dict) ->
    Dict;
match_template_1(_, _, _Dict) ->
    throw(error).  % shape mismatch

match_template_2([{Var} | Ts1], [_ | Ts2], Dict)
  when Var =:= '_' ; Var =:= 0 ->
    match_template_2(Ts1, Ts2, Dict);  % anonymous variable
match_template_2([{Var} | Ts1], [Tree | Ts2], Dict) ->
    match_template_2(Ts1, Ts2, orddict:store(Var, Tree, Dict));
match_template_2([{'*',Var} | Ts1], Ts2, Dict) ->
    match_glob(lists:reverse(Ts1), lists:reverse(Ts2), Var, Dict);
match_template_2([T1 | Ts1], [T2 | Ts2], Dict) ->
    match_template_2(Ts1, Ts2, match_template(T1, T2, Dict));
match_template_2([], [], Dict) ->
    Dict;
match_template_2(_, _, _Dict) ->
    throw(error).  % shape mismatch

%% match the tails in reverse order; no further globs allowed
match_glob([{Var} | Ts1], [_ | Ts2], Var, Dict)
  when Var =:= '_' ; Var =:= 0 ->
    match_glob(Ts1, Ts2, Var, Dict);  % anonymous variable
match_glob([{Var} | Ts1], [Tree | Ts2], Var, Dict) ->
    match_glob(Ts1, Ts2, Var, orddict:store(Var, Tree, Dict));
match_glob([{'*',Var} | _], _, _, _) ->
    fail("multiple glob variables in same match group: ~w", [Var]);
match_glob([T1 | Ts1], [T2 | Ts2], Var, Dict) ->
    match_glob(Ts1, Ts2, Var, match_template(T1, T2, Dict));
match_glob([], _Group, Var, Dict) when Var =:= '_' ; Var =:= 0 ->
    Dict;  % anonymous glob variable
match_glob([], Group, Var, Dict) ->
    orddict:store(Var, lists:reverse(Group), Dict);
match_glob(_, _, _, _Dict) ->
    throw(error).  % shape mismatch


%% compare two syntax trees for equivalence
compare_trees(T1, T2) ->
    Type1 = erl_syntax:type(T1),
    case erl_syntax:type(T2) of
        Type1 ->
            case erl_syntax:subtrees(T1) of
                [] ->
                    case erl_syntax:subtrees(T2) of
                        [] -> compare_leaves(Type1, T1, T2);
                        _Gs2 -> false  % shape mismatch
                    end;
                Gs1 ->
                    case erl_syntax:subtrees(T2) of
                        [] -> false;  % shape mismatch
                        Gs2 -> compare_trees_1(Gs1, Gs2)
                    end
            end;
        _Type2 ->
            false  % different tree types
    end.

compare_trees_1([G1 | Gs1], [G2 | Gs2]) ->
    compare_trees_2(G1, G2) andalso compare_trees_1(Gs1, Gs2);
compare_trees_1([], []) ->
    true;
compare_trees_1(_, _) ->
    false.  % shape mismatch

compare_trees_2([T1 | Ts1], [T2 | Ts2]) ->
    compare_trees(T1, T2) andalso compare_trees_2(Ts1, Ts2);
compare_trees_2([], []) ->
    true;
compare_trees_2(_, _) ->
    false.  % shape mismatch

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


%% @doc Match against multiple guarded clauses.
%% @see match/2

-type guard() :: fun( (env()) -> boolean() ).

-type body() :: fun( (env()) -> any() ).

-type guarded_body() :: {guard(), body()}.

-type default() :: fun( () -> any() ).

-type clause() :: {pattern() | [pattern()], body()}
                | {pattern() | [pattern()], guarded_body()}
                | {pattern() | [pattern()], [guarded_body() | body()]}
                | {pattern() | [pattern()], guard(), body()}
                | default().

-spec switch(tree() | [tree()], [clause()]) -> any().

switch(Tree, [{Pattern, Body} | Cs]) ->
    switch_1(Tree, Pattern, Body, Cs);
switch(Tree, [{Pattern, Guard, Body} | Cs]) ->
    switch_1(Tree, Pattern, {Guard, Body}, Cs);
switch(_Tree, [Body]) when is_function(Body, 0) ->
    Body();
switch(_Tree, []) ->
    erlang:error(merl_switch_clause);
switch(_Tree, _) ->
    erlang:error(merl_switch_badarg).

switch_1(Tree, Pattern, Body, Cs) ->
    case match(Pattern, Tree) of
        {ok, Env} ->
            case Body of
                List when is_list(List) ->
                    switch_2(Env, List);
                {Guard, Body1}
                  when is_function(Guard, 1), is_function(Body1, 1) ->
                    case Guard(Env) of
                        true -> Body1(Env);
                        false -> switch(Tree, Cs)
                    end;
                _ when is_function(Body, 1) ->
                    Body(Env);
                _ ->
                    erlang:error(merl_switch_badarg)
            end;
        error ->
            switch(Tree, Cs)
    end.

switch_2(Env, [{Guard, Body} | Cs])
  when is_function(Guard, 1), is_function(Body, 1) ->
    case Guard(Env) of
        true -> Body(Env);
        false -> switch_2(Env, Cs)
    end;
switch_2(Env, [Body]) when is_function(Body, 1) ->
    Body(Env);
switch_2(_Env, _) ->
    erlang:error(merl_switch_badarg).


%% ------------------------------------------------------------------------
%% Internal utility functions

fail(Text) ->
    fail(Text, []).

fail(Fs, As) ->
    throw({error, lists:flatten(io_lib:format(Fs, As))}).

flatten_text([L | _]=Lines) when is_list(L) ->
    lists:foldr(fun(S, T) -> S ++ [$\n | T] end, "", Lines);
flatten_text([B | _]=Lines) when is_binary(B) ->
    lists:foldr(fun(S, T) -> binary_to_list(S) ++ [$\n | T] end, "", Lines);
flatten_text(Text) when is_binary(Text) ->
    binary_to_list(Text);
flatten_text(Text) ->
    Text.

%% convert a metavariable name string back to an integer or atom
tag(Name) ->
    try list_to_integer(Name)
    catch
        error:badarg ->
            list_to_atom(Name)
    end.

-spec check_meta(tree()) -> {string()} | false.

%% Check if a syntax tree represents a metavariable. Metavariables are atoms
%% starting with `@', variables starting with `_@', or integers starting
%% with `909'. Following the prefix, one or more `_' or `0' characters may
%% be used to indicate "lifting" of the variable one or more levels, and
%% after that, a `@' or `9' character indicates a glob metavariable rather
%% than a normal metavariable. If the name after the prefix is `_' or `0',
%% the variable is treated as an anonymous catch-all pattern in matches.

check_meta(Tree) ->
    case erl_syntax:type(Tree) of
        atom ->
            case erl_syntax:atom_name(Tree) of
                "@" ++ Cs when Cs =/= [] -> {Cs};
                _ -> false
            end;
        variable ->
            case erl_syntax:variable_literal(Tree) of
                "_@" ++ Cs when Cs =/= [] -> {Cs};
                _ -> false
            end;
        integer ->
            case erl_syntax:integer_value(Tree) of
                N when N >= 9090 ->
                    case integer_to_list(N) of
                        "909" ++ Cs -> {Cs};
                        _ -> false
                    end;
                _ -> false
            end;
        _ -> false
    end.
