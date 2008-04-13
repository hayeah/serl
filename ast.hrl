
%% AST datatypes
%% I wish I have /real/ macro to use!

%% An ast is a 4-tuple: {Type,Line,Module,Payload}

%% Erlang macro is half-baked. It doesn't even have stringnifying for tokens!

%% -record(ast_float,{line,
%% 		   mod,
%% 		   visible, %% if the ast appears in source
%% 		   payload}).



-define(ast_float3(L,M,E),{'__float',L,M,E}). 
-define(ast_integer3(L,M,E),{'__integer',L,M,E}). 
-define(ast_string3(L,M,E),{'__string',L,M,E}).

-define(ast_atom3(L,M,E),{'__atom',L,M,E}).
-define(ast_satom3(L,M,E),{'__satom',L,M,E}). 
-define(ast_var3(L,M,E),{'__var',L,M,E}).

-define(ast_paren3(L,M,E),{'__paren',L,M,E}).
-define(ast_brace3(L,M,E),{'__brace',L,M,E}).
-define(ast_block3(L,M,E),{'__block',L,M,E}).

-define(ast_paren2(L,E),{'__paren',L,curmod(),E}).
-define(ast_brace2(L,E),{'__brace',L,curmod(),E}).
-define(ast_block2(L,E),{'__block',L,curmod(),E}).

%% ast accessors, used for pattern matching

-define(ast_float(E),?ast_float3(_,_,E)). 
-define(ast_integer(E),?ast_integer3(_,_,E)). 
-define(ast_string(E),?ast_string3(_,_,E)).

-define(ast_atom(E),?ast_atom3(_,_,E)).
-define(ast_satom(E),?ast_satom3(_,_,EE). 
-define(ast_var(E),?ast_var3(_,_,E)).

-define(ast_paren(E),?ast_paren3(_,_,E)).
-define(ast_brace(E),?ast_brace3(_,_,E)).
-define(ast_block(E),?ast_block3(_,_,E)).

%% ast constructors. The modules that use these should define lineno() and curmod()
-define(cast_float(E),?ast_float3(lineno(),curmod(),E)). 
-define(cast_integer(E),?ast_integer3(lineno(),curmod(),E)). 
-define(cast_string(E),?ast_string3(lineno(),curmod(),E)).

-define(cast_atom(E),?ast_atom3(lineno(),curmod(),E)).
-define(cast_satom(E),?ast_satom3(lineno(),curmod(),E)). 
-define(cast_var(E),?ast_var3(lineno(),curmod(),E)).

-define(cast_paren(E),?ast_paren3(lineno(),curmod(),E)).
-define(cast_brace(E),?ast_brace3(lineno(),curmod(),E)).
-define(cast_block(E),?ast_block3(lineno(),curmod(),E)).


-define(ast_quote3(L,M,E),{'__quote',L,M,E}).
-define(ast_bquote3(L,M,E),{'__bquote',L,M,E}).
-define(ast_unquote3(L,M,E),{'__unquote',L,M,E}).
-define(ast_sunquote3(L,M,E),{'__sunquote',L,M,E}).

-define(ast_quote(E),?ast_quote3(_,_,E)).
-define(ast_bquote(E),?ast_bquote3(_,_,E)).
-define(ast_unquote(E),?ast_unquote3(_,_,E)).
-define(ast_sunquote(E),?ast_sunquote3(_,_,E)).

-define(cast_quote(E),?ast_quote3(lineno(),curmod(),E)).
-define(cast_bquote(E),?ast_bquote3(lineno(),curmod(),E)).
-define(cast_unquote(E),?ast_unquote3(lineno(),curmod(),E)).
-define(cast_sunquote(E),?ast_sunquote3(lineno(),curmod(),E)).


