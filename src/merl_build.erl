%% ---------------------------------------------------------------------
%% @author Richard Carlsson <richardc@klarna.com>
%% @copyright 2012 Richard Carlsson
%% @doc Making it simple to build a module with merl

-module(merl_build).

-export([init_module/1, module_forms/1, add_function/4, add_record/3,
         add_import/3, add_attribute/3]).

-import(merl, [term/1]).

-include("../include/merl.hrl").


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
    Module = ?Q("-module('@Name@')."),
    Exported = [erl_syntax:arity_qualifier(term(N), term(A))
                || {N,A} <- ordsets:from_list(Xs)],
    Export = ?Q("-export(['@_Exported'/1])."),
    Imports = [?Q("-import('@M@', ['@_NAs'/1]).")
               || {M, Ns} <- Is,
                  NAs <- [[erl_syntax:arity_qualifier(term(N), term(A))
                           || {N,A} <- ordsets:from_list(Ns)]]
              ],
    Records = [?Q("-record('@N@',{'@_RFs'=[]}).")
               || {N,Es} <- lists:reverse(Rs),
                  RFs <- [[erl_syntax:record_field(term(F), term(V))
                           || {F,V} <- Es]]
              ],
    Attrs = [?Q("-'@N@'('@T@').") || {N,T} <- lists:reverse(As)],
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
