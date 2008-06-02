%% language file for vanilla erlang.
-module(verl).
-include("ast.hrl").
-include("verl.hrl").
%-include_lib("eunit/include/eunit.hrl").


-import(env,[assoc/2,assoc_put/3]).
-import(scompile,[error/1,
		  error/2,
		  curmod/0,is_curmod/1,
		  lineno/0,
		  lookup/3,
		  transform/2,
		  transform_each/2
		 ]).
-import(lists,[map/2,member/2]).

-compile(export_all).

%% -define(defsp(Name,Args),Name(Args,Env)).

-define(defsp(Name,Args),Name(?ast_paren3(_,_Mod,[_|Args]),Env)).
-define(defm(Name,Args),Name(?ast_paren3(_,_Mod,[_|Args]))).


%% reader macros
%% char
'__rm_c'([],[C]) ->
    ?cast_integer(C).
%% atom
'__rm_a'([],Str) ->
    ?cast_atom(list_to_atom(Str)).
%% string literal
'__rm_s'([],Str) ->
    ?cast_string(Str).


%% verl's compiler options
-record(opt,
	{bin, %% if true, do not output .beam
	 dry,
	 load, %% load the compiled .beam
	 expand_only, %% expansion only
	 ast, %% the erlang ast
	 def, %% pick out the definitions of defs
	 env, %% the final compile environment
	 meta, %% produce the meta module
	 mod,  %% the outputting module
	 options
	 %% unknown options handled by erlang compiler.
	 }).

set_opts(Options) ->
    #opt{bin=lookup_opt(Options,bin),
	 dry=lookup_opt(Options,dry),
	 load=lookup_opt(Options,load),
	 expand_only=lookup_opt(Options,expand_only),
	 ast=lookup_opt(Options,ast),
	 def=lookup_opt(Options,def),
	 env=lookup_opt(Options,env),
	 meta=lookup_opt(Options,meta),
	 mod=case lookup_opt(Options,mod) of
		 undefined -> curmod();
		 Mod -> Mod
	     end, 
	 options=Options 
	}.

lookup_opt(Opts,Key) ->
    case lists:keysearch(Key,1,Opts) of
	{_,{_Key,Val}} -> Val;
	_ -> case lists:member(Key,Opts) of
		 true -> true; 
		 _ -> undefined
	     end
    end.

emit(Forms,Env,CO) ->
    Emits
	=[if CO#opt.expand_only -> {expand_only};
	     true -> emit_bin(Forms,CO)
	  end,
	  emit_ast(Forms,CO),
	  emit_def(Env,CO),
	  emit_env(Env,CO) 
	 ],
    lists:filter(fun (Emit) -> not (Emit=={}) end,
		 Emits). 

emit_bin(Forms,CO) ->
    Mod=CO#opt.mod,
    {Status,Bin,Warnings,Errors}=
	compile_forms(Forms,CO#opt.options),
    case Status of
	error -> {error,Errors,Warnings};
	ok -> if CO#opt.bin==true -> {bin,[Mod,Bin,Warnings]};
		 CO#opt.dry==true -> {dry,[Warnings,Errors]};
		 true -> write_beam(Mod,Bin),
			 if CO#opt.load==true ->
				 code:purge(Mod),
				 code:load_file(Mod);
			    true -> ok
			 end,
			 {beam,Mod}
	      end 
    end.

compile_forms(Forms,Options) ->
    case compile:forms(Forms,Options) of
	{ok,_Mod} -> {ok,[],[],[]};
	{ok,_,Bin} ->
	    {ok,Bin,[],[]};
	{ok,_,Bin,Warnings} ->
	    {ok,Bin,Warnings,[]};
	{error,Errors,Warnings} ->
	    {error,[],Warnings,Errors};
	error -> {error,[],[],[]}
    end.

write_beam(Mod,Bin) when is_binary(Bin) ->
    BeamFileName=atom_to_list(Mod)++".beam",
    file:write_file(BeamFileName,Bin);
write_beam(_,_) ->
    error("Beam is empty.").
    
    
emit_ast(Forms,CO) ->
    case CO#opt.ast of
	true -> [io:format("~s",[erl_pp:form(Form)]) || Form <- Forms],{ast,Forms};
	undefined -> {}
    end.

emit_env(Env,CO) ->
    case CO#opt.env of
	true -> {env,Env};
	undefined -> {}
    end.

emit_def(Env,CO) ->
    case CO#opt.def of
	undefined ->
	    {};
	Fs when is_list(Fs) ->
	    [begin Ast=def_info(Env,functions,F,ast),
		   io:format("Function: ~p ::\n~s\n",[F,erl_pp:form(Ast)])
	     end
	     || F <- Fs, is_atom(F)],
	    {};
	F when is_atom(F) ->
	    emit_def(Env,CO#opt{def=[F]}) 
    end.

def_info(Env,NSType,Name,Prop) ->
    case env:assoc(Env,[definitions,NSType,Name]) of
       {ok,Def} ->
           case env:assoc(element(2,Def),Prop) of
               {ok,Val} -> Val;
               _ -> error("Unknown property ~p of function ~p",[Prop,Name])
           end;
       _ -> error("Function undefined: ~p",[Name])
    end. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.3 Pattern
%%
%% If Ps is a sequence of patterns P_1, ..., P_k, then Rep(Ps) = [Rep(P_1), ..., Rep(P_k)]. Such sequences occur as the list of arguments to a function or fun.
%%
%% Individual patterns are represented as follows: 
%%  If P is an atomic literal L, then Rep(P) = Rep(L).

%% code here is ugly as sin.

patterns(Patterns,Env) -> 
    %% first pass generates the bindings 
    Env2=binds(Patterns,Env),
    %% second pass generates the pattern matching code
    {Env2,gen_pats(Patterns,Env2)}.

pattern(P,Env) ->
    {Env2,[RP]}=patterns([P],Env),
    {Env2,RP}.
    
binds([],Env) ->
    Env;
binds([P|Ps],Env) ->
    binds(Ps,bindmac(P,Env)).


%% Pattern binding first does macro expand, then deal only with the special forms it recognizes.
%%%% all recursive calls should first macroexpand the subexpressions.

%% this function should be the entry point of call, and recursive calls.
bindmac(P,Env) ->
    bind(scompile:macroexpand(P,Env),Env).

bind(?ast_quote(_E),Env) ->
    Env;
bind(?ast_bquote(E),Env) ->
    bindmac(bq:completely_expand(E), Env);
bind(?ast_var('_'),Env) -> Env;
bind(?ast_float(_),Env) -> Env;
bind(?ast_integer(_),Env) -> Env;
bind(?ast_string(_),Env) -> Env;
bind(?ast_atom(_),Env) -> Env;
bind(?ast_var3(_L,M,V),Env) ->
    scompile:lexical_extend(Env,vars,[{{M,V},scompile:gensym()}]); 
bind(?ast_block(Es),Env) ->
    binds(Es,Env);
bind(?ast_brace(Es),Env) ->
    binds(Es,Env);
bind(?ast_paren([?ast_atom(Car)|Es])=P,Env) ->
    case Car of
	nil -> Env;
	cons -> case Es of
		    [H,T] -> bindmac(H,bindmac(T,Env));
		    [] -> Env
		end;
	tuple -> binds(Es,Env);
	'==' -> binds(Es,Env);
	_  -> error("Invalid pattern\n~.4p.",[P])
    end;
bind(P,_) ->
    error("Invalid pattern\n~.4p.",[P]).


gen_pats(Es,Env) ->
    [gen_pat(E,Env) || E <- Es].

%% walk the pattern so syntax objects match the "appearance" (ignoring line and module).
gen_pat(?ast_quote(E),Env) ->
    gen_pat_quote(E,Env);
gen_pat(?ast_bquote(E),Env) ->
    gen_pat(bq:completely_expand(E),Env);
gen_pat(?ast_var(_)=E,Env) -> transform(E,Env);
gen_pat(?ast_float(_)=E,Env) -> transform(E,Env);
gen_pat(?ast_integer(_)=E,Env) -> transform(E,Env);
gen_pat(?ast_string(_)=E,Env) -> transform(E,Env);
gen_pat(?ast_atom(_)=E,Env) -> transform(E,Env); 
gen_pat(?ast_block(Es),Env) ->
    gen_pat(?cast_paren([?cast_atom(ls),?cast_block(Es)]),Env);
gen_pat(?ast_brace(Es),Env) ->
    gen_pat(?cast_paren([?cast_atom(tuple),?cast_block(Es)]),Env);
gen_pat(?ast_paren([?ast_atom(Car)|Es])=P,Env) ->
    case Car of
	nil -> transform(P,Env);
	cons -> case Es of
		    [H,T] ->
			RH=gen_pat(H,Env),
			RT=gen_pat(T,Env),
			{cons,lineno(),RH,RT};
		    [] -> {nil,lineno()}
		end;
	tuple ->
	    [?ast_block(Elements)]=Es,
	    Rs=gen_pats(Elements,Env),
	    {tuple,lineno(),Rs};
	'==' ->
	    [Last|Rest]=lists:reverse(Es),
	    lists:foldl(
	      fun (E,Acc) ->
		      L=element(2,E),
		      {match,L,
		       gen_pat(E,Env),
		       Acc}
	      end,
	      gen_pat(Last,Env),
	      Rest);
	%% should maybe make the ast macros special forms so they work better with pattern matching.
	paren -> gen_pat_glist('__paren',Es,Env);
	block -> gen_pat_glist('__block',Es,Env);
	brace -> gen_pat_glist('__brace',Es,Env);
	float -> [Data]=Es,
		 gen_pat(mk_ast_pat('__float',Data),Env);
	integer -> [Data]=Es,
		   gen_pat(mk_ast_pat('__integer',Data),Env);
	string -> [Data]=Es,
		  gen_pat(mk_ast_pat('__string',Data),Env);
	atom -> [Data]=Es,
		gen_pat(mk_ast_pat('__atom',Data),Env);
	var -> [Data]=Es,
	       gen_pat(mk_ast_pat('__var',Data),Env);
	_ -> gen_pat(scompile:expand1_(P,Env),Env) %% can't use macroexpand here.
    end;
gen_pat(P,Env) -> 
    transform(P,Env).

gen_pat_quote(E,Env) ->
    Type=element(1,E),
    Data=element(4,E),
    case Type of
	'__paren' -> gen_pat_quote_glist(paren,Data,Env);
	'__brace' -> gen_pat_quote_glist(brace,Data,Env);
	'__block' -> gen_pat_quote_glist(block,Data,Env);
	'__float' -> gen_pat(mk_ast_pat(Type,?cast_float(Data)),Env);
	'__integer' -> gen_pat(mk_ast_pat(Type,?cast_integer(Data)),Env);
	'__string' -> gen_pat(mk_ast_pat(Type,?cast_string(Data)),Env);
	'__atom' -> gen_pat(mk_ast_pat(Type,?cast_atom(Data)),Env);
	'__var' -> gen_pat(mk_ast_pat(Type,?cast_atom(Data)),Env)
    end.

gen_pat_quote_glist(Type,Es,Env) ->
    gen_pat_glist(Type,
		  [[?cast_quote(E) || E <- Es]],
		  Env).


gen_pat_glist(Type,[Es],Env) ->
    gen_pat_glist(Type,
		  [Es,?cast_var('_'), ?cast_var('_')],
		  Env); 
gen_pat_glist(Type,[Es,L,M],Env) ->
    gen_pat(mk_ast(Type,Es,L,M), Env).


%% 4.5 Clauses

%% erlang's guarded clauses take guard-sequences

%% bindings made in serl's clauses are not visible outside.

%% There are function clauses, if clauses, case clauses and catch clauses. 
%% A clause C is one of the following alternatives:

clause(Ps,G,Es,Line,Env) ->
    %% A <guard> is a list of <guard-test>
    {Env2,RPs}=patterns(Ps,scompile:lexical_shadow(Env,vars,[])),
    REs=transform_each(Es,Env2), 
    GuardSequence=
	case G of
	    [] -> [];
	    _ -> [[guard(G,Env2)]]
	end,
    {clause,Line, RPs, GuardSequence, REs}.

%% 4.6 Guards


%% The set of valid guard expressions (sometimes called guard tests) is a subset of the set of valid Erlang expressions. The reason for restricting the set of valid expressions is that evaluation of a guard expression must be guaranteed to be free of side effects. Valid guard expressions are:

%%  the atom true, 
%%  other constants (terms and bound variables), all regarded as false, 
%%  calls to the BIFs specified below, 
%%  term comparisons, 
%%  arithmetic expressions, 
%%  boolean expressions, and 
%%  short-circuit boolean expressions.

%% Type Test BIFs.
%% is_atom/1 
%%   is_binary/1 
%%   is_constant/1 
%%   is_float/1 
%%   is_function/1 
%%   is_function/2 
%%   is_integer/1 
%%   is_list/1 
%%   is_number/1 
%%   is_pid/1 
%%   is_port/1 
%%   is_reference/1 
%%   is_tuple/1 
%%   is_record/2 
%%   is_record/3

%% Other BIFs Allowed in Guard Expressions.
%% abs(Number) 
%%   element(N, Tuple) 
%%   float(Term) 
%%   hd(List) 
%%   length(List) 
%%   node() 
%%   node(Pid|Ref|Port) 
%%   round(Number) 
%%   self() 
%%   size(Tuple|Binary) 
%%   tl(List) 
%%   trunc(Number)

guard(E,Env) ->
    %% fingers crossed that they are valid Guards.
    %% these are the allowable tests in erlang's if
    ErlAst=transform(E,Env),
    Test=erl_lint:is_guard_test(ErlAst),
    %Literal=erl_syntax:is_literal(ErlAst),
    %io:format("Guard: ~p\nTest: ~p\n",[E,Test]),
    if Test -> ErlAst;
       true -> error("Invalid guard: ~p",[E])
    end.


-define(defast_mac(Name,Type),
	?defm(Name,[E]) ->
	       mk_ast(Type,E,
		      ?cast_integer(0), %% generated syntax object has no line.
		      ?cast_atom(curmod())); 
	?defm(Name,[E,L,M]) ->
	       mk_ast(Type,E,L,M)).

-define(defasts_mac(Name,Type),
	?defm(Name,[?ast_block(Es)]) ->
	       %% `[;(gen-ls (E Es): (Type E))]
	       ?cast_block([?cast_paren([?cast_atom(Type),E]) || E <- Es]);
	?defm(Name,[?ast_block(Es),?ast_block([L,M])]) ->
	       ?cast_block([?cast_paren([?cast_atom(Type),E,L,M]) || E <- Es])).

mk_ast(Type,E,L,M) ->
    ?cast_brace([?cast_atom(Type), L ,M ,E]).

mk_ast_pat(Type) ->
    mk_ast_pat(Type,?cast_var('_')). 
mk_ast_pat(Type,E) ->
    ?cast_brace([?cast_atom(Type),
		 ?cast_var('_'),
		 ?cast_var('_'),
		 E]).

%% (atom: a) => 'a
?defast_mac('__mac_float','__float'). 
?defast_mac('__mac_integer','__integer'). 
?defast_mac('__mac_string','__string'). 
?defast_mac('__mac_atom','__atom'). 
?defast_mac('__mac_var','__var'). 
?defast_mac('__mac_paren','__paren'). 
?defast_mac('__mac_brace','__brace').
?defast_mac('__mac_block','__block').

%% (atoms: a b c) => ['a 'b 'c]
%% (atoms: a b c: L M)
?defasts_mac('__mac_floats','__float'). 
?defasts_mac('__mac_integers','__integer'). 
?defasts_mac('__mac_strings','__string'). 
?defasts_mac('__mac_atoms','__atom'). 
?defasts_mac('__mac_vars','__var'). 
?defasts_mac('__mac_parens','__paren'). 
?defasts_mac('__mac_braces','__brace').
?defasts_mac('__mac_blocks','__block').
