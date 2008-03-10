-module(scompile).
-include("ast.hrl").
-include("state.hrl").

-export([
	 read/1,read/2,
	 expand/1,expand/2, 
	 eval/1,eval/2,eval/3,
	 compile/1,compile/2,
	 transform/2, transform_each/2,
	 map_env0/3,
	 %compile1/1,
	 curmod/0,
	 lineno/0,
	 gensym/1,
	 lookup/3,
	 warn/1,warn/2,error/1,error/2
	 ]).
-import(lists,[map/2,keysearch/3]).


%% global process vars
-define(lineno,{?MODULE,'lineno'}).
-define(curmod,{?MODULE,'curmod'}).
-define(gensym,{?MODULE,'gensym_counter'}).
-define(state,{?MODULE,'state'}).

lineno() -> get(?lineno).
lineno(N) -> put(?lineno,N).

curmod() -> get(?curmod).
curmod(M) -> put(?curmod,M).

gensym(N) ->
    OldC=
	case get(?gensym) of
	    undefined -> put(?gensym,N),0;
	    C when is_integer(C)-> C
	end,
    put(?gensym,OldC+N),
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
    io:format(Message,Args),
    erlang:error(serl_error).


read(In) ->
    read(In,verl).
read(In,TLM) ->
    new_process(fun read_/1,[env:new(TLM)],
		#scompile_S{input=In}).
    

read_(Env) ->
    {S,Env2,Ast}=reader:exp(Env,get_state()),
    set_state(S),
    {Env2,Ast}.

expand(In) ->
    expand(In,verl).
expand(In,TLM) ->
    new_process(fun expand_/1,[env:new(TLM)],
		#scompile_S{input=In}).
    
expand_(Env) ->
    {Env2,Ast}=read_(Env),
    transform(Ast,Env2).

eval(In) ->
    eval(In,verl).
eval(In,TopLevelMod) ->
    eval(In,TopLevelMod,erl_eval:new_bindings()).
eval(In,TLM,Bindings) ->
    new_process(fun eval_/2,
		[env:new(TLM),Bindings],
		#scompile_S{input=In}).

eval_(Env,Bindings) ->
    {Env2,Ast}=read_(Env),
    {Env3,ErlAst}=transform(Ast,Env2),
    erl_eval:expr(ErlAst,Bindings,
		  {value, fun (Name,Arg) ->
				  local_funcall_handler(Name,Arg,Env3)
			  end},
		  {value, fun remote_funcall_handler/2}).

%% TODO maybe allow parameterization of symbol macros.
compile(In) ->
    compile(In,verl).
compile(Mod,TLM) when is_atom(Mod) ->
    %% TODO modify streamer to parse binary.
    In=case file:read_file(atom_to_list(Mod)++".serl") of
	{ok,Bin} -> binary_to_list(Bin);
	_ -> error("Cannot source moduel ~p\n",[Mod])
    end,
    new_process(fun compile_/1,
		[env:new(TLM)],
		#scompile_S{curmod=Mod,input=In}).

%% transforms expressions for side effect.
compile_(Env) ->
    compile_loop(Env,0).

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


     

new_process(Fun,Args,S) ->
    Return=self(),
    Sync=make_ref(),
    spawn_link(
      fun () -> set_state(S),
		R=apply(Fun,Args),
		Return ! {Sync,R}
      end),
    receive
	{Sync,R} -> R
    after 120000 ->
	    error("Compiler Timeout")
    end. 
    
local_funcall_handler(Name,Args,Env) ->
    case env:lookup(functions,Name,Env) of
	{value,{Mod,F}} -> apply(Mod,F,Args);
	{value,F} -> apply(F,Args);
	%% TODO should throw undef exception.
	_ -> error("undefined function: ~p\n",[Name])
    end.

remote_funcall_handler(F,Args) ->
    F(Args).


%% compile(Exp) ->
%%     foo.
%%     %%transform().

transform(Exp,Env) ->
    %DExp=desugar:renest(Exp),
    DExp=Exp,
    case DExp of 
	?ast_paren([Car|Body]) ->
	    do_transform(Car,Body,Env) ;
	_ when is_tuple(DExp) -> % an ast element
	    do_transform(element(1,DExp),DExp,Env) 
    end.

do_transform(Car,Body,Env) ->
    case lookup_expander(Env,Car) of
	{special,F} -> F(Body,Env);
	{macro,F} -> transform(F(Body),Env);
	_ -> case lookup_expander(Env,?cast_atom('call')) of
		 %% this is probably correct. A function call should follow the convention of the active module.
		 %% it could be a special form or a macro.
		 %% %% TODO Hmmmm... should I inspect the function header so I know how 'call' should actually be used?
		 {special,F} -> F([Car|Body]);
		 {macro,F} -> transform(F(Body),Env);
		 _ -> error("cannot find an expander for functional call. ")
	     end
    end. 

%% doesn't really conform to "eval in some order"
%% for a series of expressions, transform_each extends environment from left to right.
%%%% this doesn't catch error like:  X=1+X.
%%%% but the erlang compiler would catch it, so let's not worry about it.
transform_each(Es,Env) ->
    map_env0(Env,fun transform/2, Es).

%% apply transformation function over expressions, start with Env.
map_env0(Env,F,Es) ->
    map_env0(Env,F,Es,[]). 
map_env0(Env,_F,[],Acc) ->
    {Env,lists:reverse(Acc)};
map_env0(Env,F,[E|Es],Acc) ->
    {Env2,R}=F(E,Env),
    map_env0(Env2,F,Es,[R|Acc]).
    

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
    case lookup_macro(Env,Key) of
	{ok,F} -> {macro,F};
	_ -> case lookup_special(Env,Key) of
		 {ok,F} -> {special,F}; 
		 _ -> false
	     end
    end.

lookup_special(Env,Key) ->
    lookup(Env,specials,Key).

lookup_macro(Env,Key) ->
    lookup(Env,macros,Key).

lookup(Env,NSType,{M,_A}=Key) ->
    CurMod=curmod(),
    if M==CurMod -> env:local_lookup(Env,NSType,Key);
       true -> env:remote_lookup(Env,NSType,Key)
    end.
