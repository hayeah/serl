
%% AST datatypes


-define(ast_float3(L,Mod,F),{'__float',L,Mod,F}). 
-define(ast_integer3(L,Mod,I),{'__integer',L,Mod,I}). 
-define(ast_string3(L,Mod,S),{'__string',L,Mod,S}).

-define(ast_atom3(L,Mod,Name),{'__atom',L,Mod,Name}).
-define(ast_satom3(L,Mod,Name),{'__satom',L,Mod,Name}). 
-define(ast_var3(L,Mod,Name),{'__var',L,Mod,Name}).

-define(ast_paren3(L,Mod,Body),{'__paren',L,Mod,Body}).
-define(ast_brace3(L,Mod,Body),{'__brace',L,Mod,Body}).
-define(ast_block3(L,Mod,Body),{'__block',L,Mod,Body}).

%% ast accessors, used for pattern matching

-define(ast_float(F),?ast_float3(_,_,F)). 
-define(ast_integer(I),?ast_integer3(_,_,I)). 
-define(ast_string(S),?ast_string3(_,_,S)).

-define(ast_atom(Name),?ast_atom3(_,_,Name)).
-define(ast_satom(Name),?ast_satom3(_,_,FName). 
-define(ast_var(Name),?ast_var3(_,_,Name)).

-define(ast_paren(Body),?ast_paren3(_,_,Body)).
-define(ast_brace(Body),?ast_brace3(_,_,Body)).
-define(ast_block(Body),?ast_block3(_,_,Body)).

%% ast constructors. The modules that use these should define lineno() and curmod()
-define(cast_float(F),?ast_float3(lineno(),curmod(),F)). 
-define(cast_integer(I),?ast_integer3(lineno(),curmod(),I)). 
-define(cast_string(S),?ast_string3(lineno(),curmod(),S)).

-define(cast_atom(Name),?ast_atom3(lineno(),curmod(),Name)).
-define(cast_satom(Name),?ast_satom3(lineno(),curmod(),Name)). 
-define(cast_var(Name),?ast_var3(lineno(),curmod(),Name)).

-define(cast_paren(Body),?ast_paren3(lineno(),curmod(),Body)).
-define(cast_brace(Body),?ast_brace3(lineno(),curmod(),Body)).
-define(cast_block(Body),?ast_block3(lineno(),curmod(),Body)).


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


