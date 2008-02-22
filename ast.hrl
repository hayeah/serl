
%% AST datatypes

-define(ast_float(L,F),['__float',L,F]).
-define(ast_integer(L,I),['__integer',L,I]).
-define(ast_string(L,S),['__string',L,S]).
-define(ast_atom(L,Name),['__atom',L,Name]).
-define(ast_var(L,Name),['__var',L,Name]).

-define(ast_brace(Body),['__brace'|Body]).
-define(ast_block(Body),['__block'|Body]).

-define(ast_satom(Name),{'__special_atom',Name}). 

