-module(printer).
-export([p/1]).
-include("ast.hrl").

p(Arg) ->
    p_(Arg),
    io:nl().


%% p_(?ast_paren([H|T]=L)) ->
%%     NotAst=
%% 	case H of
%% 	    ?ast_atom('__block') -> p_block(T);
%% 	    ?ast_atom('__brace') -> p_brace(T);
%% 	    ?ast_atom('__quote') -> p_quote(T);
%% 	    ?ast_atom('__bquote') -> p_bquote(T);
%% 	    ?ast_atom('__unquote') -> p_unquote(T);
%% 	    ?ast_atom('__sunquote') -> p_sunquote(T);
%% 	    ?ast_atom('__float') -> p_float(T);
%% 	    ?ast_atom('__integer') -> p_integer(T);
%% 	    ?ast_atom('__string') -> p_string(T);
%% 	    ?ast_atom('__atom') -> p_atom(T);
%% 	    ?ast_atom('__var') -> p_var(T);
%% 	    _ -> p_paren(L)
%% 	end,
%%     case H of
%% 	    ?ast_atom('__block') -> p_block(T);
%% 	    ?ast_atom('__brace') -> p_brace(T);
%% 	    ?ast_atom('__quote') -> p_quote(T);
%% 	    ?ast_atom('__bquote') -> p_bquote(T);
%% 	    ?ast_atom('__unquote') -> p_unquote(T);
%% 	    ?ast_atom('__sunquote') -> p_sunquote(T);
%% 	    ?ast_atom('__float') -> p_float(T);
%% 	    ?ast_atom('__integer') -> p_integer(T);
%% 	    ?ast_atom('__string') -> p_string(T);
%% 	    ?ast_atom('__atom') -> p_atom(T);
%% 	    ?ast_atom('__var') -> p_var(T);
%% 	    _ -> p_paren(L)
%%     end;

p_(?ast_paren(L)) -> p_paren(L);
p_(?ast_block(L)) -> p_block(L);
p_(?ast_brace(L)) -> p_brace(L);
p_(?ast_quote(E)) -> p_quote(E);
p_(?ast_bquote(E)) -> p_bquote(E);
p_(?ast_unquote(E)) -> p_unquote(E);
p_(?ast_sunquote(E)) -> p_sunquote(E); 
p_(?ast_float(F)) -> p_float(F);
p_(?ast_integer(I)) -> p_integer(I);
p_(?ast_string(S)) -> p_string(S);
p_(?ast_atom(A)) -> p_atom(A);
p_(?ast_var(V)) -> p_var(V);
p_(E) -> io:write(E).

p_float(F) ->
    io:write(F).

p_integer(I) ->
    io:write(I).

p_string(S) ->
    io:format("~p",[S]).

p_atom(A) ->
    io:put_chars(atom_to_list(A)).

p_var(V) ->
    io:put_chars(atom_to_list(V)).

p_paren(L) ->
    p_glist("()",L).

p_block([]) ->
    io:put_chars(":");
p_block([H|T]) ->
    io:put_chars(":"),
    p_(H),
    lists:foreach(
      fun (I) -> io:put_chars(" "),p_(I) end,
      T). 

p_brace(L) ->
    p_glist("{}",L).
		   
p_quote(E) ->
    io:put_chars("'"),
    p_(E).

p_bquote(E) ->
    io:put_chars("`"),
    p_(E).

p_unquote(E) ->
    io:put_chars(","),
    p_(E).

p_sunquote(E) ->
    io:put_chars(";"),
    p_(E). 

p_glist(Paren,[]) ->
    io:put_chars(Paren);
p_glist([Open,Close],[H|T]) ->
    io:put_chars([Open]),
    p_(H),
    lists:foreach(
      fun (I) -> io:put_chars(" "),p_(I) end,
      T),
    io:put_chars([Close]). 
