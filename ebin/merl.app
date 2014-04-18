%% -*- mode: erlang -*-
{application,merl,
 [{description,"Metaprogramming in Erlang"},
  {vsn,"0.9.0"},
  {modules, [merl, merl_transform, merl_tests]},
  {applications, [kernel, stdlib, compiler, syntax_tools]},
  {registered,[]}
 ]}.
