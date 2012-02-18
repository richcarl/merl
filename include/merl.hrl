%% ---------------------------------------------------------------------
%% Header file for merl

%% Quoting a piece of code
-define(Q(Text), merl:quote(?LINE, Text)).

%% Quasi-quoting code, substituting metavariables listed in Env
-define(Q(Text, Env), merl:qquote(?LINE, Text, Env)).
