-module(scompile).
-include("ast.hrl").

-export([lineno/0,
	 curmod/0,is_curmod/1,
	 env/0,set_env/1,
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

	 lexical_current_scope/2,
	 lexical_lookup/3,lexical_shadow/2,lexical_shadow/3,lexical_extend/3,
	 get_def/3,new_def/4,
	 lookup/3,lookup_meta_fun/3,lookup_expander/2,
	 toplevel_lookup/3,external_lookup/3,
	 lookup_definitions/3,
	 lookup_imports/3,
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
    erlang:error(Reason).

-define(serl_toplevel,serl_eval).

-record(state,
	{curmod=?serl_toplevel,
	 lineno=1,
	 gensym_counter=0,
	 bootstrap=false,
	 env,
	 options=[]
	}).

%% process dictionary keys to store the state variables
-define(lineno,{?MODULE,'lineno'}).
-define(curmod,{?MODULE,'curmod'}).
-define(gensym,{?MODULE,'gensym_counter'}).
-define(options,{?MODULE,'options'}).
-define(bootstrap,{?MODULE,bootstrap}).
-define(env,{?MODULE,env}).
-define(toplevel_cache(M),{?MODULE,{toplevel_env_cache,M}}).

%% init_state() ->
%%     %% reset compiler state.
%%     init_state(#state{}).

init_state(S) when is_record(S,state) ->
    erase(),
    put(?lineno,S#state.lineno),
    put(?curmod,S#state.curmod),
    put(?gensym,S#state.gensym_counter),
    put(?bootstrap,S#state.bootstrap),
    put(?env,S#state.env),
    put(?options,S#state.options).

%% not sure if get_state is ever useful
%% get_state() ->
%%     #state{lineno=lineno(),curmod=curmod(),gensym_counter=gensym_counter()}.


%% state accessors

lineno() -> get(?lineno).
set_lineno(L) -> put(?lineno,L). 

is_curmod(Mod) when is_atom(Mod) ->
    %% whenever the module of syntax object is the pseudo module true,
    %% it behaves as though it came from source.
    (Mod==curmod()) or (Mod==true);
%% check to see if a syntax object is appears in the current compiling module.
is_curmod(Ast) when is_tuple(Ast)->
    AstMod=element(3,Ast),
    is_curmod(AstMod). 

curmod() ->
    case get(?curmod) of
	undefined -> ?serl_toplevel;
	M -> M
    end.


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


bootstrap() -> get(?bootstrap).
%set_bootstrap(V) -> put(?bootstrap,V).

env() ->
    get(?env).
set_env(Env) ->
    put(?env,Env).

toplevel_of(M) ->
    case get(?toplevel_cache(M)) of
	undefined -> TopEnv=env:toplevel_of(M),
		     put(?toplevel_cache(M),TopEnv),
		     TopEnv; 
	TopEnv -> TopEnv
    end,
    TopEnv.

read(In,Env) -> 
    new_process(fun read_/2,[In,Env]). 

read_(In,Env) ->
    %% returns {Env2,In2,Line,Ast}
    %% Line is where the expression ended.
    %% Ast is the expression read.
    reader:exp(In,Env,lineno()).

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
    %io:format("funcall: ~p\n",[Env]),
    case lookup(Env,functions,{curmod(),Name}) of
	{ok,Def} when is_tuple(Def) -> 
	    %%io:format("local apply: ~p\n",[Name]),
	    case element(1,Def) of
		%% import function
		{M,A} -> apply(M,A,Args);
		%% defined function made available at compile time
		%% Note that the function at compile time expects the
		%% its arguments in a list, as well as the environment
		%% to lookup functions it calls. 
		F when is_function(F) ->
		    F(Args,Env)
	    end;
	%% TODO should throw undef exception.
	_ ->
	    %error("undefined function: ~p\n",[Name])
	    erlang:error({undef,Name})
    end.

remote_funcall_handler({M,F},Args) ->
    apply(M,F,Args).



compile(Mod,Env) when is_atom(Mod) ->
    compile(Mod,Env,[]).

compile(Mod,Env,Options) when is_atom(Mod) ->
    %% FIX Ugly kludge for bootstrapping.
    %% It makes definitions unavailable at compile time.
    new_process(fun compile_/2,[Mod,Env],
		#state{bootstrap=lists:member(bootstrap,Options),
		       curmod=Mod,
		       options=Options}).


%% __bof and  __eof are pseudo forms for a language to do its language specific things.
compile_(Mod,Env) ->
    %% TODO modify streamer to parse binary.
    In=case file:read_file(atom_to_list(Mod)++".serl") of
	{ok,Bin} -> binary_to_list(Bin);
	_ -> error("Cannot find source: ~p\n",[Mod])
    end,
    try {0,Env2}=transform(?cast_paren([?cast_atom('__bof')]),Env),
	{eof,Env3}=compile_loop(In,0,Env2),
	%% at end of file, transforms the pseudo special form (eof)
	%% what happens is language dependent.
	%% maybe compile to erlang, maybe compile to javascript, whatever.
	transform(?cast_paren([?cast_atom('__eof'),normal]),Env3)
    after
	transform(?cast_paren([?cast_atom('__eof'),'after']),Env)
    end.

%% the compiler loop transforms a sequence of toplevel forms found in a module.
%% the toplevel forms in a module are divided into numbered sections.
%% The numbered sections must follow each other in order.
%% The toplevel forms are transformed purely for side-effects.
compile_loop(In,Section,Env) ->
    %io:format("cloop: ~p\n",[Env]),
    {In2,ReaderLine,Ast}=read_(In,Env),
    case Ast of
	eof -> {eof,Env};
	_ -> 
	    %% TODO should give better error when compile loop protocol is violated
	    {Section2,Env2}=transform(Ast,Env),
	     if not(is_integer(Section2)) -> error("Not toplevel form: ~p\n",[Section2]);
		Section2<Section -> error("Toplevel form out of sequence: ~p\n",[Section2]);
		true -> ok
	     end,
	     set_lineno(ReaderLine), %% after transform, set lineno to where the reader left off.
	     compile_loop(In2,Section2,Env2)
    end.

new_process(Fun,Args) ->
    new_process(Fun,Args,#state{}).

new_process(Fun,Args,State) ->
    process_flag(trap_exit, true),
    Sync=spawn_link(
	   fun () -> init_state(State),
		     %% maybe the thrown error keeps
		     R=apply(Fun,Args),
		     exit({result,R})
	   end),
    wait_result(Sync).


wait_result(Sync) ->
    receive
	{'EXIT',Sync,{result,R}} -> R;
%% do custom serl error reporting, then rethrow the error.
%% 	{'EXIT',Sync,{{serl_error,Reason,Trace},_}} ->
%% 	    io:format("Error: ~p\n~p\n",[Reason,Trace]),
%% 	    erlang:error(Reason);
	
	{'EXIT',Sync,Reason} ->
	    io:format("Error: ~p\n",[Reason]),
	    erlang:error(Reason) 
    after 10000 ->
	    %% time out after 10 seconds
	    erlang:error(timeout)
    end.

%% macroexpand(?ast_paren([Car|_])=Exp,Env) when is_tuple(Exp) ->
%%     try case lookup_expander(Env,Car) of
%% 	{macro,F} -> macroexpand(F(Exp),Env);
%% 	_ -> Exp
%% 	end
%%     catch
%% 	error:{serl_error,Reason,Trace} ->
%% 	    erlang:error({serl_error,Reason,[Car|Trace]});
%% 	error:Reason ->
%% 	    erlang:error({serl_error,Reason,
%% 			  [Car,erlang:get_stacktrace()]})
%%     end;

macroexpand(?ast_paren([Car|_])=Exp,Env) when is_tuple(Exp) ->
    case lookup_expander(Env,Car) of
	{macro,F} -> case F of
			 {M,A} -> R=apply(M,A,[Exp]);
			 _ when is_function(F) -> R=F(Exp,Env) 
		     end,
		     macroexpand(R,Env);
	_ -> Exp
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
    transform_ast(Exp,Env). 

transform_ast(Exp,Env) when is_tuple(Exp) ->
    {Tag,Line,Mod,Data}=Exp,
    % if an ast comes from another module,
    %% (1) An expander is defined (use it to override curmod)
    %% (2) No expander is defined (use curmod's expander)
    % if an ast comes from the compiling module
    %% (1) An expander is defined (use it)
    %% (2) No expander is defined (error)
    CurMod=curmod(),
    case lookup_expander(Env,{Mod,Tag}) of
	{special,F} -> F(?cast_paren([?cast_atom(Tag),Exp]),Env);
	%% guard
	_ when CurMod /= Mod -> transform_ast({Tag,Line,curmod(),Data},Env); 
	_ -> error("No special defined for ast: ~s~n",[Tag])
    end.

%% do one expansion
%% not tail-recursive, to keep the stack for backtrace.
do_transform(?ast_paren3(L,_M,[Car|_Body])=Exp,Env) ->
    %%io:format("~p: ~p\n",[L,Car]),
    %% line tracking
    %% lineno() always give the line of the closest open paren visible in source.
    if L > 0 -> set_lineno(L);
       true -> ok %% non-source syntax objects have lineno < 0.
    end, 
    try case lookup_expander(Env,Car) of
	    {special,F} -> {special,F(Exp,Env)};
	    {macro,F} -> case F of
			     {M,A} -> R=apply(M,A,[Exp]);
			     _ when is_function(F) -> R=F(Exp,Env)
			 end,
			 {macro,R};
	    %% transform paren as ast if no expander is defined for its head.
	    %% asts always use special expander
	    _ -> {special,transform_ast(Exp,Env)}
	end
    catch
	error:{serl_error,Reason,Stack,Trace} ->
	    erlang:error({serl_error,Reason,Stack,[Car|Trace]});
	error:Reason ->
	    erlang:error({serl_error,Reason,erlang:get_stacktrace(),[Car,{env,Env}]})
    end.



transform_each(Es,Env) ->
    [transform(E,Env) || E <- Es].

%% apply transformation function over expressions, start with Env.
map_env0(F,Es,Env) ->
    map_env0(F,Es,[],Env). 
map_env0(_F,[],Acc,Env) ->
    {lists:reverse(Acc),Env};
map_env0(F,[E|Es],Acc,Env) ->
    {R,Env2}=F(E,Env),
    map_env0(F,Es,[R|Acc],Env2).
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Environment Lookup

%% Lookup function objects in the environment.
%% meta_funs are macros, specials, and reader-macros
lookup_meta_fun(Env,Type,Key) ->
    case lookup(Env,Type,Key) of
	{ok,Def} ->
	    case element(1,Def) of
		F when is_function(F) -> {ok,F};
	        MA when is_tuple(MA) -> {ok,MA}
	    end;
	_ -> false
    end.

%% This function is used to lookup the appropriate expander for
%% the car of an expression.
lookup_expander(Env,Car) ->
    %% macros shadow special forms
    %io:format("Lookup Expander: ~p\n",[Car]),
    Key=case Car of
	?ast_atom3(_L,M,A) ->
	    {M,A};
	?ast_brace([?ast_atom(M),?ast_atom(A)]) ->
	    {M,A};
	A when is_atom(A) ->
		{curmod(),A}; 
	T when is_tuple(T) -> T
	end,
    case lookup_meta_fun(Env,macros,Key) of
	{ok,F} -> {macro,F}; 
	false -> case lookup_meta_fun(Env,specials,Key) of
		     {ok,F} -> {special,F};
		     false -> false
		 end
    end.


lookup(Env,NSType,{M,_F}=Key) ->
    %% note that if M is true, it is local lookup
    T=is_curmod(M),
    %io:format("Lookup: ~p Curmod: ~p\n",[Key,T]),
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
	false -> toplevel_lookup(toplevel_of(M),NSType,A);
	Val -> Val
    end.

toplevel_lookup(Env,NSType,A) ->
    case lookup_definitions(Env,NSType,A) of 
	false -> external_lookup(Env,NSType,A);
	Val -> Val
    end.

external_lookup(Env,NSType,A) ->
    case lookup_imports(Env,NSType,A) of
	false -> lookup_base(Env,NSType,A);
	Val -> Val
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lexical Scoping

%% map a list of symbols to gensyms
lexical_shadow(Env,NSType) -> 
    assoc_cons(Env,[lexical,NSType],[]).

lexical_shadow(Env,NSType,Bindings) -> 
    lexical_extend(lexical_shadow(Env,NSType),
		   NSType,Bindings).

lexical_extend(Env,NSType,Bindings) ->
    %% Bindings is a list of {{Module,Name},Value}
    {Scope,Scopes}=
	case assoc(Env,[lexical,NSType]) of
	    {ok,[H|T]} -> {H,T};
	    _ -> {[],[]}
	end,
    NewKeys=ordsets:from_list([Key || {Key,_} <- Bindings]),
    OldKeys=ordsets:from_list([Key || {Key,_} <- Scope]),
    case ordsets:intersection(OldKeys,NewKeys) of
	[] -> assoc_put(Env,[lexical,NSType],
			[Bindings++Scope|Scopes]);
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

lexical_current_scope(Env,NSType) ->
    case assoc(Env,[lexical,NSType]) of
	{ok,[S|_Scopes]} -> S;
	_ -> []
    end.

lexical_lookup(Env,NSType,Key) ->
    %io:format("lex-lk: ~p\n",[Env]),
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
    %% captures binding if either of Key or Binding belongs to the pseudo module true.
    if ((Mod==true) or (BMod==true) or (Mod==BMod)) and (Key==BKey) -> {ok,Val};
       true -> lookup_scope(K,Bs)
    end.
    
lookup_definitions(Env,NSType,Key) ->
    %io:format("bootstrap: ~p\n",[bootstrap()]),
    case bootstrap() of
	true -> false;
	_ -> assoc(Env,[definitions,NSType,Key])
    end.

lookup_imports(Env,NSType,Key) ->
    case assoc(Env,[imports]) of
	{ok,Imports} -> lookup_envs(Imports,NSType,Key);
	_ -> false
    end.

lookup_base(Env,NSType,Key) ->
    case assoc(Env,[base]) of
	{ok,Base} -> lookup_envs(Base,NSType,Key);
	_ -> false
    end.
    
lookup_envs([],_,_) -> false;
lookup_envs([{_Mod,NSs}|Envs],NSType,Key) ->
    case assoc(NSs,[NSType,Key]) of
	false -> lookup_envs(Envs,NSType,Key);
	V -> V
    end.
