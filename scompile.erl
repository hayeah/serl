-module(scompile).
-include("ast.hrl").

-export([lineno/0,
	 curmod/0,curmod/1,
	 gensym/0, gensym/1, genvar/0, genvar/1,
	 reset_gensym/0, %% for debug purposes 
	 options/0,set_options/1,
	 
	 read/2,read_/2,
	 mexpand/2,mexpand_/2,
	 expand1/2,expand1_/2,
	 expand/2,expand_/2,
	 eval/3,eval_/3,eval_erl/3,
	 compile/2,compile/3,
	 transform1/2, transform/2, transform_each/2, 
	 macroexpand/2,
	 map_env0/3,
	 
	 lexical_shadow/3,lexical_extend/3,
	 get_def/3,new_def/4,
	 lookup/3,lookup_expander/2,toplevel_lookup/3,
	 warn/1,warn/2,error/1,error/2
	 ]).
-import(lists,[map/2,keysearch/3]).
-import(env,[assoc/2,
	     assoc_put/3,assoc_cons/3,assoc_append/3]).

warn(Message) ->
    warn(Message,[]).
warn(Message,Args) ->
    io:format(Message,Args).

error(Message) ->
    error(Message,[]).
error(Message,Args) ->
    Reason=lists:flatten(io_lib:format(Message,Args)),
    erlang:error({serl_error,Reason}).

-define(serl_toplevel,serl_eval).

-record(state,
	{curmod=?serl_toplevel,
	 lineno=1,
	 gensym_counter=0,
	 options=[]
	}).

%% process dictionary keys to store the state variables
-define(lineno,{?MODULE,'lineno'}).
-define(curmod,{?MODULE,'curmod'}).
-define(gensym,{?MODULE,'gensym_counter'}).
-define(options,{?MODULE,'options'}).

init_state() ->
    %% reset compiler state.
    init_state(#state{}).
init_state(S) when is_record(S,state) ->
    put(?lineno,S#state.lineno),
    put(?curmod,S#state.curmod),
    put(?gensym,S#state.gensym_counter),
    put(?options,S#state.options).

%% not sure if get_state is ever useful
%% get_state() ->
%%     #state{lineno=lineno(),curmod=curmod(),gensym_counter=gensym_counter()}.


%% state accessors

lineno() -> get(?lineno).
set_lineno(L) -> put(?lineno,L). 

is_curmod(Mod) when is_atom(Mod) -> Mod==curmod().
curmod() -> get(?curmod). 
%% check to see if a syntax object is contained within the current compiling module.
curmod(Ast) when is_tuple(Ast)->
    AstMod=element(3,Ast),
    curmod()==AstMod. 

reset_gensym() -> put(?gensym,0).
gensym_counter() -> get(?gensym).

gensym() ->
    [S]=gensym(1),
    S.
gensym(N) ->
    GC=gensym_counter(),
    put(?gensym,GC+N), 
    [list_to_atom("#"++io_lib:print(I)) || I <- lists:seq(GC,GC+N-1)].

genvar() ->
    [V]=genvar(1),
    V.
genvar(N) ->
    [?cast_var(S) || S <- gensym(N)].

options() ->
    get(?options).
    
set_options(Opts) ->
    put(?options,Opts).



read(In,Env) -> 
    new_process(fun read_/2,[In,Env]). 

read_(In,Env) -> 
    reader:exp(In,Env,lineno()) %% {Env2,In2,Line,Ast}
    .

mexpand(Ast,Env) ->
    new_process(fun mexpand_/2,
		[Ast,Env]).
    
mexpand_(Ast,Env) ->
    macroexpand(Ast,Env).

expand1(Ast,Env) ->
    new_process(fun expand1_/2,
		[Ast,Env]). 
expand1_(Ast,Env) ->
    transform1(Ast,Env).

expand(Ast,Env) ->
    new_process(fun expand_/2,
		[Ast,Env]).
    
expand_(Ast,Env) ->
    transform(Ast,Env).


eval(Ast,Env,Bindings) ->
    new_process(fun eval_/3,[Ast,Env,Bindings]).

eval_(Ast,Env,Bindings) ->
    ErlAst=transform(Ast,Env),
    eval_erl(ErlAst,Bindings,Env). 
    
eval_erl(ErlAst,Bindings,Env) ->
    {value,Val,NewBindings}=erl_eval:expr(
      ErlAst,
      lists:foldl(
	fun ({Key,Val},Acc) ->
		erl_eval:add_binding(Key,Val,Acc)
	end,
	erl_eval:new_bindings(),
	Bindings), 
      {value, fun (Name,Arg) ->
		      local_funcall_handler(Name,Arg,Env)
	      end},
      {value, fun remote_funcall_handler/2}),
    {Val,erl_eval:bindings(NewBindings)}.
    
local_funcall_handler(Name,Args,Env) ->
    case lookup(Env,functions,{curmod(),Name}) of
	{ok,Def} when is_tuple(Def) -> 
	    %%io:format("Applying ~s:~s/~p",[Mod,F,length(Args)]),
	    case element(1,Def) of
		%% import function
		{M,A} -> apply(M,A,Args);
		%% defined function made available at compile time
		%% %% Note that function call protocol is different
		F when is_function(F) ->
		    F(Args)
	    end;
	%% TODO should throw undef exception.
	_ -> error("undefined function: ~p\n",[Name])
    end.

remote_funcall_handler({M,F},Args) ->
    apply(M,F,Args).


compile(Mod,Env) when is_atom(Mod) ->
    compile(Mod,Env,[]).

compile(Mod,Env,Options) when is_atom(Mod) ->
    new_process(fun compile_/2,[Mod,Env],
		#state{curmod=Mod,options=Options}).

%% transforms expressions for side effect on the environment.
compile_(Mod,Env) ->
    %% TODO modify streamer to parse binary.
    In=case file:read_file(atom_to_list(Mod)++".serl") of
	{ok,Bin} -> binary_to_list(Bin);
	_ -> error("Cannot find source module ~p\n",[Mod])
    end,
    try {0,Env2}=transform(?cast_paren([?cast_atom('__bof')]),Env),
	{eof,Env3}=compile_loop(In,Env2,0),
	%% at end of file, transforms the pseudo special form (__eof)
	%% what happens is language dependent.
	%% maybe compile to erlang, maybe compile to javascript, whatever.
	transform(?cast_paren([?cast_atom('__eof'),normal]),Env3)
    after
	transform(?cast_paren([?cast_atom('__eof'),'after']),Env)
    end.

%% I want a way to restrict toplevel forms...
%% one easy way is for toplevel forms to return false as the transformed ast!
%% I also want toplevel forms to come in a sequence...
%% one way is for them to return integers counting up!
%% haha. This is so kludgey, but works well.
%%%% so if the returned N2 >= N1, then proceed.
%%%% toplevels that returns the atom infinity can occur anywhere.
compile_loop(In,Env,Section) -> 
    {In2,ReaderLine,Ast}=read_(In,Env), 
    case Ast of
	eof -> {eof,Env};
	_ -> %% toplevel forms return a tuple of section-number and environment.
	    {Section2,Env2}=transform(Ast,Env),
	     if not(is_integer(Section2)) -> error("Not toplevel form: ~p\n",[Section2]);
		Section2<Section -> error("Toplevel form out of sequence: ~p\n",[Section2]);
		true -> ok
	     end,
	     set_lineno(ReaderLine), %% after transform, set lineno to where the reader left off.
	     compile_loop(In2,Env2,Section2)
    end.

new_process(Fun,Args) ->
    new_process(Fun,Args,#state{}).

new_process(Fun,Args,State) ->
    init_state(State),
    %%process_flag(trap_exit, true),
    try apply(Fun,Args) of
	R -> R
    after
	%% reset compiler state.
	init_state()
    end. 


%% new_process(Fun,Args,State) ->
%%     %%process_flag(trap_exit, true),
%%     Return=self(),
%%     Sync=spawn_link(
%% 	   fun () -> init_state(State), 
%% 		     R=apply(Fun,Args),
%% 		     Return ! {self(),R}
%% 	   end),
%%     wait_result(Sync).


%% wait_exit(Sync) ->
%%     receive
%% 	{'EXIT',Sync,normal} -> wait_result(Sync);
%% 	{'EXIT',Sync,{{serl_error,Reason,Trace},_}} ->
%% 	    io:format("Error: ~p\n~p\n",[Reason,Trace]),
%% 	    {error,Reason};
%% 	{'EXIT',Sync,Reason} ->
%% 	    io:format("Error: ~p\n",[Reason]),
%% 	    erlang:error(Reason) 
%%     after 120000 ->
%% 	    %% time out after 2 minutes
%% 	    erlang:error(timeout)
%%     end.

%% wait_result(Sync) ->
%%     receive
%% 	{Sync,Result} -> Result
%%     after 120000 ->
%% 	    %% time out after 2 minutes
%% 	    erlang:error(timeout)
%%     end.

macroexpand(?ast_paren([Car|_])=Exp,Env) when is_tuple(Exp) ->
    try case lookup_expander(Env,Car) of
	{macro,F} -> macroexpand(F(Exp),Env);
	_ -> Exp
	end
    catch
	error:{serl_error,Reason,Trace} ->
	    erlang:error({serl_error,Reason,[Car|Trace]});
	error:Reason ->
	    erlang:error({serl_error,Reason,
			  [Car,erlang:get_stacktrace()]})
    end; 
macroexpand(Exp,_Env) when is_tuple(Exp) ->
    Exp.

transform1(?ast_paren(_)=Exp,Env) ->
    case do_transform(Exp,Env) of
	{special,Result} -> Result;
	{macro,Result} -> Result
    end;
transform1(Exp,Env) when is_tuple(Exp) ->
    [Car,L,M|Es]=tuple_to_list(Exp),
    transform1(?ast_paren2(L,[?ast_atom3(L,M,Car)|Es]),Env).

transform(?ast_paren(_)=Exp,Env) ->
    case do_transform(Exp,Env) of
	{special,Result} -> Result;
	{macro,Result} -> transform(Result,Env)
    end;
transform(Exp,Env) when is_tuple(Exp) ->
    [Car,L,M,E]=tuple_to_list(Exp),
    transform(?ast_paren2(L,[?ast_atom3(L,M,Car),E]),Env).

%% do one expansion
%% not tail-recursive. I'd rather have a stack for backtrace.
do_transform(?ast_paren3(L,_M,[Car|Body])=Exp,Env) ->
    %%io:format("T: ~p ~p\n",[L,Car]),
    %% line tracking
    %% %% lineno() always give the line of the closest open paren visible in source.
    if L > 0 -> set_lineno(L);
       true -> ok %% non-source syntax objects have lineno==0.
    end, 
    try case lookup_expander(Env,Car) of
	    {special,F} -> {special,F(Exp,Env)};
	    {macro,F} -> {macro,F(Exp)} ;
	    _ -> case lookup_expander(Env,?cast_atom('__call')) of
		     %% make sure to raise error, otherwise go into loop.
		     {Type,_F} when Type==special;Type==macro ->
			 R=transform(?cast_paren([?cast_atom('__call'),Car|Body]),
				     Env),
			 {Type,R};
		     _ -> error("No expander for function call.")
		 end
	end
    catch
	error:{serl_error,Reason,Trace} ->
	    erlang:error({serl_error,Reason,[Car|Trace]});
	error:Reason ->
	    erlang:error({serl_error,Reason,
			  [Car,erlang:get_stacktrace()]})
    end.


%% doesn't really conform to "eval in some order"
%% for a series of expressions, transform_each extends environment from left to right.
%%%% this doesn't catch error like:  X=1+X.
%%%% but the erlang compiler would catch it, so let's not worry about it.
transform_each(Es,Env) ->
    [transform(E,Env) || E <- Es].

%% apply transformation function over expressions, start with Env.
map_env0(F,Es,Env) ->
    map_env0(F,Es,[],Env). 
map_env0(_F,[],Acc,Env) ->
    {Env,lists:reverse(Acc)};
map_env0(F,[E|Es],Acc,Env) ->
    {Env2,R}=F(E,Env),
    map_env0(F,Es,[R|Acc],Env2).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Environment

lookup_expander(Env,Car) ->
    %% macros shadow special forms
    Key=case Car of
	?ast_atom3(_L,M,A) ->
	    {M,A};
	?ast_brace([?ast_atom(M),?ast_atom(A)]) ->
	    {M,A};
	A when is_atom(A) ->
		{curmod(),A}; 
	T when is_tuple(T) -> T
    end,
    case lookup(Env,macros,Key) of
	{ok,Def} ->
	    case element(1,Def) of
		F when is_function(F) -> {macro,F};
	        MA when is_tuple(MA) -> {macro,MA}
	    end;
	_ -> case lookup(Env,specials,Key) of
		 {ok,Def} ->
		     case element(1,Def) of
			 F when is_function(F) -> {special,F};
			 MA when is_tuple(MA) -> {special,MA}
		     end; 
		 _ -> false
	     end
    end.

lookup(Env,NSType,{M,_F}=Key) ->
    T=is_curmod(M),
    if T -> local_lookup(Env,NSType,Key);
       true -> remote_lookup(Env,NSType,Key)
    end.



%% -for a symbol a$home
%% -look up the lexical scope for {a home} or {a true}
%% -if not, look up the top-level of home for a

local_lookup(Env,NSType,{M,A}) ->
    case lexical_lookup(Env,NSType,{M,A}) of
	false -> toplevel_lookup(Env,NSType,A);
	Val -> Val
    end.

remote_lookup(Env,NSType,{M,A}) ->
    case lexical_lookup(Env,NSType,{M,A}) of
	%% TODO cache toplevels
	false ->
	    case get({remote_lookup_topenv,M}) of
		undefined -> TopEnv=env:toplevel_of(M),
			     put({remote_lookup_topenv,M},TopEnv); 
		TopEnv -> TopEnv
	    end,
	    toplevel_lookup(TopEnv,NSType,A);
	Val -> Val
    end.

toplevel_lookup(Env,NSType,A) ->
    case lookup_definitions(Env,NSType,A) of 
	false -> lookup_imports(Env,NSType,A); 
	Val -> Val
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lexical Scoping

%% map a list of symbols to gensyms
lexical_shadow(Env,NSType,Bindings) ->
    %% Bindings is a list of {M,A}
    %% M and A are atoms
    Bs=case Bindings of
	   [] -> [];
	   _ ->lists:zip(Bindings,
			 gensym(length(Bindings)))
       end,
    assoc_cons(Env,[lexical,NSType],Bs).

lexical_extend(Env,NSType,NewKeys) ->
    %% Bindings is a list of {M,A}
    %% M and A are atoms
    {Scope,Scopes}=
	case assoc(Env,[lexical,NSType]) of
	    {ok,[H|T]} -> {H,T};
	    _ -> {[],[]}
	end, 
    OldKeys=ordsets:from_list([Key || {Key,_} <- Scope]),
    case ordsets:intersection(OldKeys,ordsets:from_list(NewKeys)) of
	[] -> NewBs=lists:zip(NewKeys,
			      gensym(length(NewKeys))), 
	      assoc_put(Env,[lexical,NSType],[NewBs++Scope|Scopes]);
	Conflicts -> error("Lexical binding conflicts: ~p",[Conflicts])
    end.
    
get_def(Env,NSType,Key) ->
    assoc(Env,[definitions,NSType,Key]).
new_def(Env,NSType,Key,Def) ->
    assoc_put(Env,[definitions,NSType,Key],Def).

%% %% assign once bindings. Error if already existing.
%% %% probably only used for variables. I can't imagine using it for any other purpose.
%% lexical_extend(Env,NSType,Bs) ->
%%     NewEnv=assoc_append(Env,[lexical_base,NSType],Bs),
%%     %% check for duplicate element.
%%     {ok,NewBs}=assoc(NewEnv,[lexical_base,NSType]),
%%     T=(length(NewBs)==length(ordsets:from_list(NewBs))),
%%     if T -> NewEnv;
%%        true -> error("Conflicting bindings. Extending with \n~p\n\tto:\n~p\n",[NewBs,Env])
%%     end.

lexical_lookup(Env,NSType,Key) ->
    case assoc(Env,[lexical,NSType]) of
	{ok,Scopes} -> lookup_scopes(Key,Scopes);
	_ -> false
    end.

lookup_scopes(_Key,[]) ->
    false;
lookup_scopes(Key,[Scope|Ss]) ->
    case lookup_scope(Key,Scope) of
	false -> lookup_scopes(Key,Ss);
	Val -> Val 
    end.

lookup_scope(_,[]) -> false;
lookup_scope({Mod,Key}=K,[{{BMod,BKey},Val}|Bs]) ->
    if ((Mod==true) or (Mod==BMod)) and (Key==BKey) -> {ok,Val};
       true -> lookup_scope(K,Bs)
    end.
    
lookup_definitions(Env,NSType,Key) ->
    assoc(Env,[definitions,NSType,Key]).

lookup_imports(Env,NSType,Key) ->
    case assoc(Env,[imports]) of
	{ok,Imports} -> lookup_imports_(Imports,NSType,Key);
	_ -> false
    end.

lookup_imports_([],_,_) -> false;
lookup_imports_([{_Mod,NSs}|Imports],NSType,Key) ->
    case assoc(NSs,[NSType,Key]) of
	false -> lookup_imports_(Imports,NSType,Key);
	V -> V
    end.
