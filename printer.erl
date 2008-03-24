-module(printer).
-export([p/1]).
-include("ast.hrl").

p(Arg) ->
    p_(Arg),
    io:nl().
    

p_(?ast_quote(E)) ->
    io:put_chars("'"),
    p_(E);
p_(?ast_bquote(E)) ->
    io:put_chars("`"),
    p_(E);
p_(?ast_unquote(E)) ->
    io:put_chars(","),
    p_(E);
p_(?ast_sunquote(E)) ->
    io:put_chars(";"),
    p_(E); 
p_(?ast_paren(L)) ->
    p_glist("()",L);
p_(?ast_block(L)) ->
    p_glist("[]",L); 
p_(?ast_brace(L)) ->
    p_glist("{}",L); 
p_(?ast_float(F)) ->
    io:write(F);
p_(?ast_integer(I)) ->
    io:write(I);
p_(?ast_string(S)) ->
    io:format("~p",[S]);
p_(?ast_atom(A)) ->
    io:write(A);
p_(?ast_var(V)) ->
    io:put_chars(atom_to_list(V)). 

p_glist(Paren,[]) ->
    io:put_chars(Paren);
p_glist([Open,Close],[H|T]) ->
    io:put_chars([Open]),
    p_(H),
    lists:foreach(
      fun (I) -> io:put_chars(" "),p_(I) end,
      T),
    io:put_chars([Close]). 
