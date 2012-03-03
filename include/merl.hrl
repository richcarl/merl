%% ---------------------------------------------------------------------
%% Header file for merl

-ifndef(MERL_HRL).


%% Quoting a piece of code
-define(Q(Text), merl:quote(?LINE, Text)).

%% Quasi-quoting code, substituting metavariables listed in Env
-define(Q(Text, Env), merl:qquote(?LINE, Text, Env)).


-ifndef(MERL_NO_TRANSFORM).
-compile({parse_transform, merl_transform}).
-endif.


-endif.
