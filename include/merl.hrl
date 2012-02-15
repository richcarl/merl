%% ---------------------------------------------------------------------
%% Header file for merl

%% Quoting a piece of code
-define(Q(Text), merl:quote_at(?LINE, Text)).

%% Quasi-quoting code, substituting metavariables listed in Env
-define(Q(Text, Env), merl:quote_at(?LINE, Text, Env)).
