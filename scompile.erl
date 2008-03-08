-module(scompile).
-include("ast.hrl").
-include("state.hrl").

-export([
	 read1/1,read1/2,
	 eval/2,eval/3,
	 expand/2,
	 transform/2, transform_each/2,
	 %compile1/1,
	 curmod/0,
	 lineno/0,
	 lookup/3,
	 warn/1,warn/2,error/1,error/2
	 ]).
-import(lists,[map/2,keysearch/3]).


%% global process vars
-define(lineno,{?MODULE,'lineno'}).
-define(curmod,{?MODULE,'curmod'}).
-define(state,{?MODULE,'state'}).

lineno() -> get(?lineno).
lineno(N) -> put(?lineno,N).

curmod() -> get(?curmod).
curmod(M) -> put(?curmod,M). 


%% erlang record sucks! Why can't the compiler work a little harder?
%% Now I am doomed to adhoc inefficiency.
scompile_S_pos(Field) ->
    case Field of
	lineno -> #scompile_S.lineno;
	curmod -> #scompile_S.curmod;
	namespaces -> #scompile_S.namespaces;
	namespace_safety -> #scompile_S.namespace_safety 
    end.

%% all state information is encapsulated in the functions set_state and get_state.
set_state(S) when is_record(S,scompile_S) ->
    put(?state,S),
    lineno(S#scompile_S.lineno),
    curmod(S#scompile_S.curmod).
set_state(Field,Val) ->
    set_state(setelement(scompile_S_pos(Field),get_state(),Val)).
   
get_state() ->
    S=get(?state),
    S#scompile_S{lineno=lineno(),
		 curmod=curmod()}.
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
    throw({serl_error,Message}).


read1(In) ->
    reader:exp(#reader_S{input=In,curmod=serl}).

read1(In,Mod) ->
    reader:exp(#reader_S{input=In,curmod=Mod}).

expand(In,TopLevelMod) ->
    Ast=read1(In),
    transform(Ast,env:new(TopLevelMod)). 

eval(In,TopLevelMod) ->
    eval(In,TopLevelMod,erl_eval:new_bindings()).
eval(In,TopLevelMod,Bindings) ->
    eval(In,TopLevelMod,#scompile_S{},Bindings).
eval(In,TopLevelMod,S,Bindings) ->
    set_state(S),
    Ast=read1(In,TopLevelMod),
    {NewEnv,ErlAst}=transform(Ast,env:new(TopLevelMod)),
    erl_eval:expr(ErlAst,Bindings,
		  {value, fun (Name,Arg) ->
				  local_funcall_handler(Name,Arg,NewEnv)
			  end},
		  {value, fun remote_funcall_handler/2}). 

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
    transform_each(Es,Env,[]).
transform_each([],Env,Acc) ->
    {Env,lists:reverse(Acc)};
transform_each([E|Es],Env,Acc) ->
    {Env2,R}=transform(E,Env),
    transform_each(Es,Env2,[R|Acc]).

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


%%     case Key of
%% 	_ when ?ast_atom3(_L,M,A)=Key;
%% 	       ?ast_var3(_L,M,A)=Key ->
%% 	    CurMod=curmod(),
%% 	    case M of 
%% 		CurMod -> env:local_lookup(Env,NSType,{M,A});
%% 		true -> env:local_lookup(Env,NSType,{M,A});
%% 		_ -> env:remote_lookup(Env,NSType,{M,A})
%% 	    end; 
%% 	?ast_brace([?ast_atom(M),?ast_atom(A)]) ->
%% 	    env:remote_lookup(Env,NSType,{M,A});
%% 	A when is_atom(A) -> env:local_lookup(Env,NSType,{CurMod,A})
%%     end.

