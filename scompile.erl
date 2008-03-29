-module(scompile).
-include("ast.hrl").
-include("state.hrl").

-export([curmod/0, lineno/0,
	 read/1,read/2,
	 expand1/1,expand1/2,expand/1,expand/2,
	 eval/1,eval/2,eval/3,eval_erl/3,
	 compile/1,compile/2,
	 transform1/2, transform/2, transform_each/2, map_env0/3,
	 
	 gensym/1,reset_gensym/0,
	 lexical_shadow/3,lexical_extend/3,
	 get_def/3,new_def/4,
	 lookup/3,toplevel_lookup/3,
	 warn/1,warn/2,error/1,error/2
	 ]).
-import(lists,[map/2,keysearch/3]).
-import(env,[assoc/2,
	     assoc_put/3,assoc_cons/3,assoc_append/3]).


%% global process vars
-define(lineno,{?MODULE,'lineno'}).
-define(curmod,{?MODULE,'curmod'}).
-define(gensym,{?MODULE,'gensym_counter'}).
-define(state,{?MODULE,'state'}).

lineno() -> get(?lineno).
lineno(N) -> put(?lineno,N).

curmod() -> get(?curmod).
curmod(M) -> put(?curmod,M).

reset_gensym() ->
    put(?gensym,0),ok.

gensym(N) ->
    OldC=
	case get(?gensym) of
	    undefined -> put(?gensym,N),0;
	    C when is_integer(C)-> put(?gensym,C+N)
				   
	end, 
    [list_to_atom("$"++io_lib:print(I)) || I <- lists:seq(OldC,OldC+N-1)].
    


%% erlang record sucks! Why can't the compiler work a little harder?
%% Now I am doomed to adhoc inefficiency.
scompile_S_pos(Field) ->
    case Field of
	lineno -> #scompile_S.lineno;
	curmod -> #scompile_S.curmod;
	input -> #scompile_S.input;
	reader_lineno -> #scompile_S.reader_lineno;
	gensym_counter -> #scompile_S.gensym_counter
%	namespaces -> #scompile_S.namespaces;
%	namespace_safety -> #scompile_S.namespace_safety 
    end.

%% all state information is encapsulated in the functions set_state and get_state.
set_state(S) when is_record(S,scompile_S) ->
    put(?state,S),
    put(?gensym,S#scompile_S.gensym_counter),
    lineno(S#scompile_S.lineno),
    curmod(S#scompile_S.curmod).
set_state(Field,Val) ->
    set_state(setelement(scompile_S_pos(Field),get_state(),Val)).
   
get_state() ->
    S=get(?state),
    S#scompile_S{lineno=lineno(),
		 curmod=curmod(),
		 gensym_counter=get(?gensym)
		}.
get_state(Field) ->
    element(scompile_S_pos(Field),get_state()).


warn(Message) ->
    warn(Message,[]).
warn(Message,Args) ->
   io:format(Message,Args).

error(Message) ->
    error(Message,[]).
error(Message,Args) ->
    Reason=io_lib:format(Message,Args),
    throw({serl_error,Reason}).


read(In) ->
    read(In,verl).
read(In,TLM) ->
    new_process(fun read_/1,[env:new(TLM)],
		#scompile_S{input=In}). 

read_(Env) ->
    {S,Env2,Ast}=reader:exp(Env,get_state()),
    set_state(S),
    {Env2,Ast}.

%% expands(In) ->
%%     expands(In,verl).
%% expands(In,TLM) ->
%%     expand(read(In,TLM),TLM). 

expand1(Ast) ->
    expand1(Ast,env:new(verl)).
expand1(Ast,Env) ->
    new_process(fun expand1_/2,
		[Ast,Env]). 
expand1_(Ast,Env) ->
    transform1(Ast,Env).

expand(Ast) ->
    expand(Ast,env:new(verl)).

expand(Ast,Env) ->
    new_process(fun expand_/2,
		[Ast,Env]).
    
expand_(Ast,Env) ->
    transform(Ast,Env).



%% evals(In) ->
%%     evals(In,verl).
%% evals(In,TopLevelMod) ->
%%     evals(In,TopLevelMod,erl_eval:new_bindings()).
%% evals(In,TLM,Bindings) ->
%%     Ast=read(In,TLM),
%%     eval(Ast,TLM,Bindings).

eval(Ast) ->
    %% by default eval with verl as the initial import.
    eval(Ast,env:new(verl)).
eval(Ast,Env) ->
    eval(Ast,Env,[]).
eval(Ast,Env,Bindings) ->
    new_process(fun eval_/3,
		[Ast,Env,Bindings]).

eval_(Ast,Env,Bindings) ->
    {Env2,ErlAst}=transform(Ast,Env),
    {Val,NewBindings}=eval_erl(ErlAst,Bindings,Env2),
    {Env2,Val,NewBindings}.
    
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
    case env:lookup(Env,functions,Name) of
	{value,{Mod,F}} -> apply(Mod,F,Args);
	{value,F} -> apply(F,Args);
	%% TODO should throw undef exception.
	_ -> error("undefined function: ~p\n",[Name])
    end.

remote_funcall_handler(F,Args) ->
    F(Args).


%% TODO maybe allow parameterization of symbol macros.
compile(Mod) ->
    compile(Mod,verl).
compile(Mod,TLM) when is_atom(Mod) ->
    %% TODO modify streamer to parse binary.
    In=case file:read_file(atom_to_list(Mod)++".serl") of
	{ok,Bin} -> binary_to_list(Bin);
	_ -> error("Cannot source moduel ~p\n",[Mod])
    end,
    new_process(fun compile_/1,
		[env:new(TLM)],
		#scompile_S{curmod=Mod,input=In}).

%% transforms expressions for side effect on the environment.
compile_(Env) ->
    {Env2,0}=transform(?cast_paren([?cast_atom('__bof')]),Env),
    compile_loop(Env2,0).

%% I want a way to restrict toplevel forms...
%% one easy way is for toplevel forms to return false as the transformed ast!
%% I also want toplevel forms to come in a sequence...
%% one way is for them to return integers counting up!
%% haha. This is so kludgey, but works well.
%%%% so if the returned N2 >= N1, then proceed.
%%%% toplevels that returns the atom infinity can occur anywhere.
compile_loop(Env,Count) -> 
    {Env2,Ast}=read_(Env), 
    case Ast of
	eof ->
	    %% at end of file, transforms the pseudo special form (__eof)
	    %% what happens is language dependent.
	    %% maybe compile to erlang, maybe compile to javascript, whatever.
	    transform(?cast_paren([?cast_atom('__eof')]),Env2);
	_ -> {Env3,N}=transform(Ast,Env2),
	     if not(is_integer(N)) -> error("Not toplevel form: ~p\n",[N]);
		N<Count -> error("Toplevel form out of sequence: ~p\n",[N]);
		true -> ok
	     end,
	     compile_loop(Env3,N)
    end.


     
new_process(Fun,Args) ->
    new_process(Fun,Args,#scompile_S{}).
new_process(Fun,Args,S) ->
    Return=self(),
    Sync=spawn_link(
	   fun () -> set_state(S),
		     R=apply(Fun,Args),
		     Return ! {self(),R}
	   end),
    receive
	{Sync,R} -> R 
    after 120000 ->
	    %% time out after 2 minutes
	    error("Compiler Timeout")
    end. 

transform1(?ast_paren3(L,_M,_)=Exp,Env) ->
    %% source tracking
    lineno(L),
    case do_transform(Exp,Env) of
	{special,Result} -> Result;
	{macro,Result} -> Result
    end;
transform1(Exp,Env) when is_tuple(Exp) ->
    [Car,L,M|Es]=tuple_to_list(Exp),
    %% source tracking
    lineno(L), 
    transform1(?cast_paren([?ast_atom3(L,M,Car)|Es]),Env).

transform(?ast_paren3(L,_M,_)=Exp,Env) ->
    %% source tracking
    lineno(L),
    case do_transform(Exp,Env) of
	{special,{_Env2,_Result}=Result} -> Result;
	{macro,{_,Exp2}} -> transform(Exp2,Env)
    end;
transform(Exp,Env) when is_tuple(Exp) ->
    [Car,L,M,E]=tuple_to_list(Exp),
    %% source tracking
    lineno(L), 
    transform(?cast_paren([?ast_atom3(L,M,Car),E]),Env).

%% do one expansion
do_transform(?ast_paren([Car|Body])=Exp,Env) ->
    case lookup_expander(Env,Car) of
	{special,F} ->
	    try {special,F(Exp,Env)}
	    catch
		{serl_error,Reason} ->
			io:format("~s:~w: ~s\n",
				  [curmod(),lineno(),Reason]),
		    %% raise the exception as error class so it's not caught again by previous transform calls.
		    erlang:error({serl_error,Reason}) 
	    end;
	{macro,F} ->
	    try {macro,{Env,F(Exp)}}
	    catch
		{serl_error,Reason} ->
		    io:format("~s:~w: ~s\n",
			      [curmod(),lineno(),Reason]),
		    printer:p(Exp),
		    %% raise the exception as error class so it's not caught again by previous transform calls.
		    erlang:error({serl_error,lists:flatten(Reason)})
	    end;
	_ -> case lookup_expander(Env,?cast_atom('__call')) of
		 %% make sure to raise error, otherwise go into loop.
		 false -> error("No expander for function call.");
		 _ -> transform(?cast_paren([?cast_atom('__call'),Car|Body]),
				Env) 
	     end
    end.


%% doesn't really conform to "eval in some order"
%% for a series of expressions, transform_each extends environment from left to right.
%%%% this doesn't catch error like:  X=1+X.
%%%% but the erlang compiler would catch it, so let's not worry about it.
transform_each(Es,Env) ->
    map_env0(fun transform/2, Es,Env).

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
	    {curmod(),A}
    end,
    case lookup(Env,macros,Key) of
	{ok,F} -> {macro,F};
	_ -> case lookup(Env,specials,Key) of
		 {ok,F} -> {special,F}; 
		 _ -> false
	     end
    end.

lookup(Env,NSType,{M,_A}=Key) ->
    CurMod=curmod(),
    if M==CurMod -> local_lookup(Env,NSType,Key);
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
	false -> toplevel_lookup(env:new(M),NSType,A);
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
    Bs=lists:zip(Bindings,
		 gensym(length(Bindings))),
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

%% it's annoying. A lot of these recursion helpers would be better
%% expressed as lists:foreach and a return
lookup_imports_([],_,_) -> false;
lookup_imports_([{_Mod,NSs}|Imports],NSType,Key) ->
    case assoc(NSs,[NSType,Key]) of
	false -> lookup_imports_(Imports,NSType,Key);
	V -> V
    end.
