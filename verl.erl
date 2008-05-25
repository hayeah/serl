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
-define(output,{?MODULE,output}).
-define(moutput,{?MODULE,moutput}).

%% -define(atomic_literals,[integer,float,string,atom]).
%% -define(patterns,[match,var,tuple,nil,cons,op,record,record_index]++?atomic_literals).
%% -define(guards,[var,tuple,nil,cons,bin,op,record,record_index,record_field,call]++?atomic_literals).

%% Serl Extension

%% char
'__rm_c'([],[C]) ->
    ?cast_integer(C).
%% atom
'__rm_a'([],Str) ->
    ?cast_atom(list_to_atom(Str)).
%% string literal
'__rm_s'([],Str) ->
    ?cast_string(Str).



%% Translate [e0 e1 ...] to a list if not handled by some other macro
?defsp('__sp_block',[Es]) ->
    transform(?cast_paren([?cast_atom(ls),?cast_block(Es)]),Env).

%% Translate {e0 e1 ...} to a tuple if not handled by some other macro
?defsp('__sp_brace',[Es]) -> 
    transform(?cast_paren([?cast_atom(tuple),?cast_block(Es)]),Env).

%% (eval-binding <var>) gets the value in eval-bindings named by <var>
%% %% this is strictly a value expression.
%% %% (eval-binding <var>) simply compiles to <var>, bypassing binding check.
?defsp('__sp_eval-binding',[?ast_atom(V)]) ->
    Env,
    Line=lineno(),
    ?erl_var(Line,V);
?defsp('__sp_eval-binding',[?ast_var(V)]) ->
    Env,
    Line=lineno(),
    ?erl_var(Line,V).


?defsp('__sp_bof',[]) ->
    put(?output,[]),
    %% the meta environment starts out the same as environment.
    {0,Env}.

get_meta_env(Env) ->
    env:assoc(Env,[meta_env]).
put_meta_env(Env,MEnv) ->
    env:assoc_put(Env,[meta_env],MEnv).

?defsp('__sp_eof',['after']) ->
    Env,
    cleanup();
?defsp('__sp_eof',[normal]) ->
    Forms=output_forms(Env), %% erlang compilable forms
    Options=scompile:options(),
    CO=set_opts(Options), 
    emit(Forms,Env,CO).

output_forms(Env) ->
    {Header,Body}=
	transform(?cast_paren([?cast_atom('functions-output')]),Env),
    [{attribute,0,module,curmod()}|Header]++Body.

cleanup() ->
    put(?output,[]),
    put(?moutput,[]),
    ok.

%% output_forms(Env) ->
%%     %% lazy output waits till the very end to output.
%%     %% it receives the final compilation environment.
%%     %% no evaluation order is gauranteed.
    
%%     try 
%% 	{Header,Body}=transform(?cast_paren([?cast_atom('functions-output')]),Env),
%% 	io:format("~p\n~p",[Header,Body])
%%     catch
%% 	error:R ->
%% 	    io:format("Error ~p\n",[R])

%%     end, 
%%     lists:reverse(
%%       [case Form of
%% 	   Fn when is_function(Fn) ->
%% 	       Fn(Env);
%% 	   Ast when is_tuple(Ast) -> Ast 
%%        end || Form <- get_output()]).



-define(lazyout(Env,Body), output(fun (Env) -> Body end)).
-define(lazymout(Env,Body), moutput(fun (Env) -> Body end)).


get_output() ->
    get(?output).
get_moutput() ->
    get(?moutput).

output(Ast) ->
    put(?output,[Ast|get_output()]).
moutput(Ast) ->
    put(?moutput,[Ast|get_moutput()]).


%% verl's compiler options
-record(opt,
	{bin, %% if true, do not output .beam
	 dry,
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
	  emit_env(Env,CO),
	  emit_meta(Env,CO)],
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


emit_meta(Env,CO) ->
    %% oh what the heck. Just do a quick hack to get it to work.
    case CO#opt.meta of
	true ->
	    MetaMod=list_to_atom(atom_to_list(CO#opt.mod)++"__meta"),
	    Exports=
		case env:assoc(Env,[exports]) of
		    {ok,NSs} ->
			%% [{attribute,0,serl_exports,
%% 			  erl_syntax:revert(erl_syntax:abstract(NSs))}];
			[{attribute,0,serl_exports,NSs}]; 
		    false -> []
		end, 
	    Forms=[{attribute,0,module,MetaMod}]++Exports,
	    {meta,emit(Forms,Env,CO#opt{meta=undefined,mod=MetaMod})}; 
	undefined -> {}
    end.

%% (def name (A1 A2 ...): E1 E2 ...)

?defsp('__sp_def',[?ast_atom(Name),?ast_paren(Args),?ast_block(Es)]) ->
    Line=lineno(),
    Arity=length(Args),
    Doc="Function: " ++ atom_to_list(Name),
    case env:assoc(Env,[definitions,functions,Name]) of
	{ok,_} -> error("Function already defined: ~p", [Name]); 
	_ -> ok
    end,
    FunClause=clause(Args,[],Es,Line,Env), 
%%     case env:assoc(Env2,[lexical_unbound,vars]) of 
%% 	{ok,UnboundVars} -> error("Unbound vars: ~w",[UnboundVars]);
%% 	_ -> ok
%%     end,
    Ast={function,Line, Name, Arity, [FunClause]},
    NewEnv=env:assoc_put(Env,[definitions,functions,Name],
			 {{curmod(),Name},
			  [{ast,Ast},
			   {doc,Doc},
			   {arities,[Arity]}]
			 }),
    output(Ast),
    {?def_sec,NewEnv}.

def_info(Env,NSType,Name,Prop) ->
    case env:assoc(Env,[definitions,NSType,Name]) of
	{ok,Def} ->
	    case env:assoc(element(2,Def),Prop) of
		{ok,Val} -> Val;
		_ -> error("Unknown property ~p of function ~p",[Prop,Name])
	    end;
	_ -> error("Function undefined: ~p",[Name])
    end. 

%% (compile-for expand run: <form>*) 
%%%% the forms must be toplevel forms for the same section

%% ?defsp('__sp_compile_for',Es) ->
%%     [?ast_block(As),?ast_block(Forms)] = to_blocks(Es),
%%     Times=ordset:from_list([A || ?ast_atom(A) <- As]),
%%     %% if compiling for both expand and run times, expand-time forms are compiled first.
%%     %% This order is an implementation detail.
%%     compile_for(Times,Forms,Env).

%% compile_for([run],Forms,Env) ->
%%     compile_for(run,Forms,Env);
%% compile_for([expand],Forms,Env) ->
%%     compile_for(expand,Forms,Env);
%% compile_for([expand,run],Forms,Env) ->
%%     %% forms for expand and run times must agree to the section they are in.
%%     {Env2,S1}=compile_for(expand,Forms,Env),
%%     {Env3,S2}=compile_for(run,Forms,Env2), 
%%     if S1==S2 -> {Env3,S1}; 
%%        true -> error("compile-for: expand and runtime forms belong to different compile sections")
%%     end;
%% compile_for(run,Forms,Env) ->
%%     {Env2,Rs}=transform_each(Forms,Env),
%%     Section=hd(Rs),
%%     lists:foreach(
%%       fun (I) ->
%% 	      if I==Section -> true;
%% 		 true -> error("compile-for: not all forms are toplevel from the same section")
%% 	      end
%%       end,
%%       Rs),
%%     {Env2,Section};
%% compile_for(expand,Forms,Env) ->
%%     MEnv=get_meta_env(Env),
%%     {MEnv2,Rs}=transform_each(Forms,MEnv),
%%     Section=hd(Rs),
%%     lists:foreach(
%%       fun (I) ->
%% 	      if I==Section -> true;
%% 		 true -> error("compile-for: not all forms are toplevel from the same section")
%% 	      end
%%       end,
%%       Rs),
%%     {put_meta_env(Env,MEnv2),Section}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.1 Module declarations and forms
%
%% A module declaration consists of a sequence of forms that are either function declarations or attributes.
%

%%  If D is a module declaration consisting of the forms F_1, ..., F_k, then Rep(D) = [Rep(F_1), ..., Rep(F_k)].

%%  If F is an attribute -module(Mod), then Rep(F) = {attribute,LINE,module,Mod}.

?defsp('__sp_module',[?ast_atom(Mod)]) ->
    Line=lineno(),
    T=is_curmod(Mod),
    if T -> ok;
       true -> error("Module name mismatch: ~p",[Mod])
    end,
    output({attribute,Line,module,Mod}),
    {?module_sec,Env}.
    
%% %%  If F is an attribute -export([Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) = {attribute,LINE,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}.

%% (export a b)
?defsp('__sp_export',Atoms) ->
    Line=lineno(),
    Mod=curmod(),
    Names=[Name || ?ast_atom3(_,_,Name) <- Atoms],
    ?lazyout(Env2,export_attribute(Names,Line,Env2)),
    {?header_sec,
     env:assoc_append(
       Env,
       [exports,functions],
       [{Name,{Mod,Name}} || Name <- Names])}.

%% (export a b c)
export_attribute(Names,Line,Env) ->
    Exports=
	[begin
	     {Name,def_info(Env,functions,Name,arities)}
	 end 
	 || Name <- Names],
    {attribute,Line,export,
     [{Name,Arity} ||
	 {Name,Arities} <- Exports,
	 Arity <- Arities]}.


%% %%  If F is an attribute -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) = {attribute,LINE,import,{Mod,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}}.
%% (import mod a b (c alias-of-c))
%% (import :all) %% not supported for verl
%% only import functions
%% compile-time error if the same name from different imports.

%% erl-spec pg120:
%% for an imported foo, all calls in the module calls the imported foo.
%% is foo is exported, it is the defined foo that is exported.

%% (import mod a b c)
?defsp('__sp_import',[?ast_atom(Mod)|Fs]) ->
    Imports=ordsets:from_list([A || ?ast_atom(A) <- Fs]),
    %% check for conflicts
    lists:foreach(
      fun (F) ->
	      case scompile:toplevel_lookup(Env,functions,F) of
		  {ok,{Mod2,F2}} -> error("Conflicting import ~p:~p with ~p:~p",
					  [Mod,F,Mod2,F2]);
		  _ -> ok
	      end
      end,
      Imports),
    Env2=try env:import(Env,Mod,functions,Imports)
	 catch no_imports ->
		 env:assoc_put(
		   Env,
		   [imports,Mod,functions],
		   [{F,{Mod,F}} || F <- Imports]) 
	 end,
    {?header_sec,Env2}.
    
    

%% %% %%  If F is a wild attribute -A(T), then Rep(F) = {attribute,LINE,A,T}. 
%% %% '__mac_wild'([wild|_]) ->
%% %%     error("wild attribute not supported").

%% %%  If F is a function declaration Name Fc_1 ; ... ; Name Fc_k, where each Fc_i is a function clause with a pattern sequence of the same length Arity, then Rep(F) = {function,LINE,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% 4.2 Atomic literals
%% %
%% %% There are five kinds of atomic literals, which are represented in the same way in patterns, expressions and guards:
%% %%%% HY: Hmmm... I count only four kinds...

%% %%  If L is an integer or character literal, then Rep(L) = {integer,LINE,L}.

?defsp('__sp_integer',[I]) ->
    Env,
    Line=lineno(),
    ?erl_integer(Line,I).

%% %%  If L is a float literal, then Rep(L) = {float,LINE,L}. 
%% '__sp_float'([L,F]) ->
%%     ?erl_float(L,F).

%%  If L is a string literal consisting of the characters C_1, ..., C_k, then Rep(L) = {string,LINE,[C_1, ..., C_k]}.
?defsp('__sp_string',[S]) ->
    Env,
    Line=lineno(),
    ?erl_string(Line,S).

%%  If L is an atom literal, then Rep(L) = {atom,LINE,L}. 
?defsp('__sp_atom',[A]) ->
    Env,
    Line=lineno(),
    ?erl_atom(Line,A).

%% %% Note that negative integer and float literals do not occur as such; they are parsed as an application of the unary negation operator.

%% %% %%atomic_literal({i_string,_Line,_S}) -> error("Interpolated string not supported.").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% 4.3 Pattern
%% %%
%% %% If Ps is a sequence of patterns P_1, ..., P_k, then Rep(Ps) = [Rep(P_1), ..., Rep(P_k)]. Such sequences occur as the list of arguments to a function or fun.
%% %%
%% %% Individual patterns are represented as follows: 
%% %%  If P is an atomic literal L, then Rep(P) = Rep(L).

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
    scompile:lexical_extend(Env,vars,[{M,V}]); 
bind(?ast_block(Es),Env) ->
    binds(Es,Env);
bind(?ast_brace(Es),Env) ->
    binds(Es,Env);
bind(?ast_paren([?ast_atom(Car)|Es])=P,Env) ->
    case Car of
	nil -> Env;
	cons -> [H,T]=Es,
		bindmac(H,bindmac(T,Env));
	tuple -> binds(Es,Env);
	'=' -> binds(Es,Env);
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
	cons -> [H,T]=Es,
		RH=gen_pat(H,Env),
		RT=gen_pat(T,Env),
		{cons,lineno(),RH,RT};
	tuple ->
	    [?ast_block(Elements)]=Es,
	    Rs=gen_pats(Elements,Env),
	    {tuple,lineno(),Rs};
	'=' ->
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


%% %% %%  If P is a variable pattern V, then Rep(P) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V. 
%% %% %%  If P is a universal pattern _, then Rep(P) = {var,LINE,'_'}.


%% %%  If E is P = E_0, then Rep(E) = {match,LINE,Rep(P),Rep(E_0)}.
%% ?defsp('__sp_=',[P,E]) ->
%%     Line=lineno(),
%%     %% erlang spec 6.10 pg74
%%     {Env1,RE} = transform(E,Env),
%%     %% '=' never declares bindings
%%     {Env2,RP} = gen_pat(P,Env1),
%%     {Env2,{match,Line,RP,RE}}.

?defsp('__sp_let',Es) ->
    Line=lineno(),
    [?ast_block(Bindings),?ast_block(Body)]=to_blocks(Es), 
    {Patterns,Assignments}=let_bindings(Bindings,[],[]),
    Env2=binds(Patterns,scompile:lexical_shadow(Env,vars,[])), %% create new scope
    RAssignments=
	[begin
	     L=element(2,P), 
	     RP=gen_pat(P,Env2), %% use new bindings
	     RV=transform(V,Env),  %% value evaluated in original scope
	     {match,L,RP,RV}
	 end
	 || {P,V} <- Assignments],
    RBody=transform_each(Body,Env2), %% body in new scope
    {block,Line,RAssignments++RBody}. 

let_bindings([],Patterns,Assignments) ->
    {lists:reverse(Patterns),lists:reverse(Assignments)};
let_bindings([Pattern,Value|Bindings],Patterns,Assignments) ->
    let_bindings(Bindings,[Pattern|Patterns],[{Pattern,Value}|Assignments]). 

?defm('__mac_>>',[V,P|Es]) ->
    ?cast_paren([?cast_atom('let'),
		 P,V,
		 ?cast_block(Es)]).

%%  If E is a variable V, then Rep(E) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V.
%%HY: It's silly to call variables variables when they don't vary... I call them bindings.

'__sp_var'(?ast_paren([?ast_atom3(Line,M,_),V]),Env) ->
    case V of
	'_' -> ?erl_var(Line,'_');
	_ -> case lookup(Env,vars,{M,V}) of
		 {ok,Alias} -> ?erl_var(Line,Alias); 
		 false -> {unbound_var,Line,M,V}
			    %{env:assoc_cons(Env,[lexical_unbound,vars],V),}
	     end
    end.


%% %%  If E is a tuple skeleton {E_1, ..., E_k}, then Rep(E) = {tuple,LINE,[Rep(E_1), ..., Rep(E_k)]}.

?defsp('__sp_tuple',[?ast_block(Es)]) ->
    Line=lineno(),
    %% doesn't respect "eval in some order". See transform_each in scompile.erl
    {tuple,Line,transform_each(Es,Env)}.

%% %%  If E is [], then Rep(E) = {nil,LINE}.

?defsp('__sp_nil',[]) ->
    Env,
    Line=lineno(),
    {nil,Line}.

?defm('__mac_ls',[]) ->
    ?cast_paren([?cast_atom(nil)]); 
?defm('__mac_ls',[?ast_block(Conses)]) ->
    'mk_list*'(Conses,?cast_paren([?cast_atom(nil)]));
?defm('__mac_ls',[?ast_block(Conses),?ast_block([Tail])]) ->
    'mk_list*'(Conses,Tail). 

'mk_list*'(Conses,Tail) ->
    lists:foldr(
      fun (H,T) ->
	      ?cast_paren([?cast_atom(cons),H,T])
      end,
      Tail,
      Conses).

%% %%  If E is a cons skeleton [E_h | E_t], then Rep(E) = {cons,LINE,Rep(E_h),Rep(E_t)}.

?defsp('__sp_cons',[Eh,Et]) ->
    Line=lineno(),
    REh=transform(Eh,Env),
    REt=transform(Et,Env),
    {cons,Line,REh,REt}.

%% %%  If E is case E_0 of Cc_1 ; ... ; Cc_k end, where E_0 is an expression and each Cc_i is a case clause then Rep(E) = {'case',LINE,Rep(E_0),[Rep(Cc_1), ..., Rep(Cc_k)]}.
%% <case-expr> := (case <exp> <case-clause>+)
%% <case-clause> := (<pattern>: <form>+) | (<pattern> when <guard>: <form>+)

?defsp('__sp_case',[E|Cs]) ->
    Line=lineno(),
    {'case',Line, transform(E,Env),
     [case_clause(C,Env) || C <- Cs]}.

%% If E is begin B end, where B is a body, then Rep(E) = {block,LINE,Rep(B)}.
?defsp('__sp_begin',Es) ->
    Line=lineno(),
    REs=transform_each(Es,Env),
    {block,Line,REs}.

%%  If E is E_m:E_0(E_1, ..., E_k), then Rep(E) = {call,LINE,{remote,LINE,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}. 
?defsp('__sp_call',[?ast_brace([M,F])|Es]) ->
    Line=lineno(),
    RM=transform(M,Env),
    RF=transform(F,Env),
    REs=transform_each(Es,Env),
    {call,Line,{remote,Line,RM,RF},REs};

%%  If E is E_0(E_1, ..., E_k), then Rep(E) = {call,LINE,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}.
?defsp('__sp_call',[?ast_atom3(_L,M,F)|Es]=E) ->
    Line=lineno(),
    %% For hygiene, test to see if the symbol came from another module.
    IsRemote=(M/=curmod()),
    if IsRemote ->
	    transform(?cast_paren([?cast_atom('__call')|
				   [?cast_brace([?cast_atom(M),?cast_atom(F)])|Es]]),
		      Env);
       true ->
	    case lookup(Env,functions,{M,F}) of 
		{ok,{{M2,Fn}}} ->
		    %% call to imported function
		    transform(?cast_paren([?cast_atom('__call')|
					   [?cast_brace([?cast_atom(M2),?cast_atom(Fn)])|Es]]),
			      Env); 
		{ok,_Fn} ->
		    %% call to lexical or defined module function
		    REs=transform_each(Es,Env),
		    {call,Line,?erl_atom(Line,F),REs}; 
		_ -> %% call to (not yet defined) module function 
		    make_call(E,Env) 
	    end
    end;
?defsp('__sp_call',E) ->
    make_call(E,Env).

make_call([E0|Es],Env) ->
    L=lineno(),
    RE0=transform(E0,Env),
    REs=transform_each(Es,Env),
    {call,L,RE0,REs}.

?defsp('__sp_fn',[?ast_paren(Ps)|Es]) ->
    L=lineno(),
    {'fun',L,
     {'clauses', [clause(Ps,[],Es,L,Env)]}}.

%% 4.5 Clauses

%% erlang's guarded clauses take guard-sequences

%% serl's guarded clauses just take one guard (a list of guard-tests):
%% (Pat when <guard-test>+: <form>+)
%% use (or <guard-test>+) to express guard-sequence

%% bindings made in serl's clauses are not visible outside.

%% There are function clauses, if clauses, case clauses and catch clauses. 
%% A clause C is one of the following alternatives:

case_clause(?ast_paren3(L,_,[P|Es]),Env) ->
    clause([P],[],Es,L,Env). 

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Quotation

?defsp('__sp_quote',[E]) ->
    Env,
    %% TODO syntax objects built by quote should have 0 as the lineno.
    erl_syntax:revert(erl_syntax:set_pos(erl_syntax:abstract(E),lineno())).

?defsp('__sp_bquote',[E]) ->
    transform(bq:completely_expand(E),Env).


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

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility Functions
  
    
%% a common idiom is a list of blocks seperated by ':' (foo a b c d: e f g h: i j k l)
%% this functions return a list of blocks.

%% this is somewhat problematic
%% no [] is allowed in the first list of items
%% (foo [a b] c d: e f g) would fail, perhaps surprisingly.
%%%% at least it surprised me.
%%

to_blocks(Es) ->
    {FirstBlock,Blocks}=
	lists:splitwith(fun (E) ->
			 case E of
			     ?ast_block(_) -> false;
			     _ -> true
			 end
		    end,
		    Es),
    [?cast_block(FirstBlock)|Blocks].


upto_block(Es) ->
    {UpToFirstBlock,Rest}=
	lists:splitwith(fun (E) ->
			 case E of
			     ?ast_block(_) -> false;
			     _ -> true
			 end
		    end,
		    Es),
    {UpToFirstBlock,Rest}.
