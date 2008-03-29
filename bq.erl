-module(bq).
-include("ast.hrl").

-import(scompile,[error/1,
		  error/2]
	).

-export([qq/1,qqp/1,
	 simplify/1,
	 completely_expand/1
 	]).

%% -compile(export_all).

%% Quotation rules:
%% <glist> := {<x1> <x2> ... <xn>} | (<x1> <x2> ... <xn>) | [<x1> <x2> ... <xn>]

%% ## if <basic> is not a <glist>
%% `<basic> => '<basic> => (quote basic)
%% `,form => form ## form is not (sunquote form2)

%% `<glist> => (append t(<x1>) t(<x2>) ... t(<xn>))
%%  for each <xj>
%%  t(form) => (list `form)  ## recurse on `form
%%  t(,form) => (list form)
%%  t(;form) => form  ## form evals to an erlang list

curmod() ->
    verl.

lineno() ->
    1.

qq(In) ->
    {_Env,?ast_bquote(X)}=scompile:read(In),
    completely_expand(X).

qqp(In) ->
    printer:p(qq(In)).

completely_expand(Ast) ->
    gen_code(simplify(expand(Ast))).

% list,list*,quote,data,append,block,paren,brace

gen_code({quote,Q}) ->
    ?cast_quote(Q);
gen_code({data,D}) ->
    D;
gen_code({ls,_}=L) ->
    gen_glist(L);
gen_code({'ls*',_,_}=L) ->
    gen_glist(L); 
gen_code({block,GL}) -> 
    %% should provide macros block,paren,brace to build syntax objects.
    %% (block a) => [a]
    %% (block a ;X) => [a|X]
    ?cast_paren([?cast_atom('block'),gen_glist(GL)]);
gen_code({paren,GL}) ->
    ?cast_paren([?cast_atom('paren'),gen_glist(GL)]);
gen_code({brace,GL}) ->
    ?cast_paren([?cast_atom('brace'),gen_glist(GL)]). 


gen_glist({ls,L}) ->
    ?cast_paren([?cast_atom('ls'),?cast_block([gen_code(I) || I <- L])]);
gen_glist({'ls*',Conses,T}) ->
    ?cast_paren([?cast_atom('ls*'),
		 ?cast_block([gen_code(I) || I <- Conses]),
		 gen_code(T)]);
gen_glist({cat,L}) ->
    ?cast_paren([?cast_atom('cat'),?cast_block([gen_code(I) || I <- L])]).


expand(Exp) ->
    case Exp of
	?ast_unquote(X) -> {data,X};
	?ast_sunquote(_) -> error("Illegal splicing.");
	?ast_bquote(X) -> expand(completely_expand(X)); 
	?ast_block(X) -> {block,glist(X)};
	?ast_paren(X) -> {paren,glist(X)};
	?ast_brace(X) -> {brace,glist(X)};
	_ -> {quote,Exp} %% `<basic> => (quote <basic>)
    end.

%% only a glist would introduce new appends
glist(L) ->
    {cat,glist(L,[])}.

glist([],Acc) -> lists:reverse(Acc);
glist([H|T],Acc) ->
    glist(T,[glist_item(H)|Acc]).

glist_item(Exp) ->
    case Exp of
	?ast_unquote(X) -> {ls,[{data,X}]};
	?ast_sunquote(X) -> {data,X};
	_ -> {ls,[expand(Exp)]}
%	[?ast_atom(bquote),X] -> {ls,expand(Exp)}; %% what? 
	%_ -> {quote,Exp}
    end.

simplify(Exp) ->
    case Exp of 
	{ls,L} -> {ls,[simplify(O) || O <- L]};
	{block,L} -> {block,simplify(L)};
	{paren,L} -> {paren,simplify(L)};
	{brace,L} -> {brace,simplify(L)}; 
	{cat,Args} ->
	    Args2=[simplify(A) || A <- Args],
	    simplify_append(Args2); 
	{quote,_} -> Exp;
	_ -> Exp
    end.

splicing_frob(X) ->
    case X of
	?ast_sunquote(_) -> true; 
	_ -> false
    end.
    
simplify_append(Args) ->
    simplify_append(lists:reverse(Args),[]).

simplify_append([],Result) ->
    Result; 
simplify_append([Arg|RArgs],Result) ->
    case Arg of
	{ls,L} ->
	    T1=lists:all(fun (X) -> not splicing_frob(X) end,L),
	    if T1 -> simplify_append(RArgs,attach_conses(L,Result));
	       true -> simplify_append(RArgs,attach_append(L,Result))
	    end;
	{'ls*',Conses,T}->
	    T1=lists:all(fun (X) -> not splicing_frob(X) end,[Conses|T]),
	    if T1 -> Simplified=attach_conses(Conses,attach_append(T,Result)),
		     simplify_append(RArgs,Simplified);
	       true -> simplify_append(RArgs,attach_append([Conses|T],Result))
	    end; 
	_ -> simplify_append(RArgs,attach_append(Arg,Result))
    end.

attach_append(Item,Result) ->
    case Result of
	[] -> NotS=case Item of
		       {data,Data} -> not splicing_frob(Data);
		       _ -> false
		   end,
	      if NotS -> Item;
		 true -> {cat,Item}
	      end;
	{cat,Args} -> {cat,[Item|Args]};
	_ -> {cat,[Item|Result]}
    end.

attach_conses(Items,Result) ->
    case Result of
	[] -> {ls,Items};
	{ls,R} -> {ls,lists:append(Items,R)};
	{'ls*',Conses,R} -> {'ls*',Items++Conses,R}; 
	_ -> {'ls*',Items,Result}
    end.
