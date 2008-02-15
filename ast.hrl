%% every term is represented in the form of:
%% term() = {type(),line(),form()}
%% type() = atom()
%% line() = integer()

%% form() = expr() | term()

%% AST datatypes

%% -define(serl_term,'__serl_term').
%% -define(serl_float,'__serl_float').
%% -define(serl_integer,'__serl_integer').
%% -define(serl_string,'__serl_string').
%% -define(serl_atom,'__serl_atom').
%% -define(serl_special_atom,'__serl_special_atom').
%% -define(serl_binding,'__serl_binding').

-define(serl_float,'float').
-define(serl_integer,'integer').
-define(serl_string,'string').
-define(serl_atom,'atom').
-define(serl_special_atom,'special_atom').
-define(serl_binding,'binding').
