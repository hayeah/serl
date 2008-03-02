-module(scompile).
-include("ast.hrl").
-include("state.hrl").

-export([eval/1,eval/3,
	 read1/1,read1/2,
	 transform/1, transform_each/1,
	 %compile1/1,
	 module_meta_info/2,
	 lookup_namespace/2,
	 curmod/0,
	 lineno/0,
	 warn/1,warn/2,error/1,error/2
	 ]).
-import(lists,[keysearch/3]).


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


assoc(AList,Key) when is_atom(Key) ->
    assoc(AList,[Key]);
assoc(Val,[]) ->
    {value,Val};
assoc(AList,[Key|Keys]) ->
    case keysearch(Key,1,AList) of
	{value,{_Key,Val}} -> assoc(Val,Keys);
	_ -> false
    end.
    
module_meta_info(Mod,Fs) when is_list(Fs) ->
    case meta_module_of(Mod) of
	{ok,MetaMod} -> module_meta_info_(MetaMod,Fs);
	_ -> false
    end;
module_meta_info(Mod,F) when is_atom(F) ->
    module_meta_info(Mod,[F]).
    
module_meta_info_(MetaMod,Fs) ->
    assoc(MetaMod:module_info(attributes),Fs). 
    
meta_module_of(Mod) when is_atom(Mod) ->
    MetaMod=list_to_atom(atom_to_list(Mod)++"__meta"),
    case code:which(MetaMod) of
	not_existing -> false;
	_ -> {ok,MetaMod}
    end.


lookup_namespace(NSName,Key) ->
    case Key of
	?ast_atom3(_L,M,A) ->
	    CurMod=curmod(),
	    if M==CurMod ->
		    %% local
		    lookup_namespace_local(NSName,A);
	       true ->
		    %% module hygiene sees that the ast_atom is from
		    %% another module, so it is implicitly a remote lookup.
		    lookup_namespace_remote(NSName,M,A)
	    end;
	?ast_brace([?ast_atom(M),?ast_atom(A)]) ->
	    lookup_namespace_remote(NSName,M,A);
	_  when is_atom(Key) -> lookup_namespace_local(NSName,Key)
    end.

lookup_namespace_local(NSName,Key) when is_atom(Key) ->
    %% lookup the current dynamic module namespace.
    case assoc(get_state(namespaces),NSName) of
	{value,NS} ->
	    case dict:find(Key,NS) of
		{ok,V} -> {value,V}; 
		_ -> false
	    end;
	_ -> false
    end.

lookup_namespace_remote(NSName,Mod,Key) ->
    %% lookup the static information from a remote/external module.
    module_meta_info(Mod,[serl_namespaces,NSName,Key]).


merge_namespace(NSName,Defs) ->
    NSs=get_state(namespaces),
    NewNS=dict:from_list(Defs),
    MergedNSs=case lists:keysearch(NSName,1,NSs) of
	{value,NS} ->
	    MergedNS=dict:merge(fun (_K,V1,V2) ->
			  case get_state(namespace_safety) of
			      0 -> V2;
			      1 -> warn("Shadowed definition: \n\t~p\n\t ~p\n",[V1,V2]), V2;
			      2 -> error("Shadowed definition: \n\t~p\n\t ~p\n",[V1,V2])
			  end
		  end,
		  NS,
		  NewNS),
	    lists:keyreplace(NSName,1,NSs,{NSName,MergedNS});
	_ -> [{NSName,dict:from_list(Defs)}|NSs]
    end,
    set_state(namespaces,MergedNSs).

read1(In) ->
    reader:exp(#reader_S{input=In,curmod=serl}).

read1(In,Mod) ->
    reader:exp(#reader_S{input=In,curmod=Mod}).



eval(In) ->
    eval(In,#scompile_S{},erl_eval:new_bindings()).
eval(In,S,Bindings) ->
    init_state(S),
    Ast=read1(In,curmod()),
    ErlAst=transform(Ast),
    erl_eval:expr(ErlAst,Bindings,
		  {value, fun local_funcall_handler/2},
		  {value, fun remote_funcall_handler/2}). 

local_funcall_handler(Name,Args) ->
    case lookup_namespace_local(functions,Name) of
	{value,{Mod,F}} -> apply(Mod,F,Args);
	{value,F} -> apply(F,Args);
	_ -> error("undefined function: ~p\n",[Name])
    end.

remote_funcall_handler(F,Args) ->
    F(Args).

%% init the transformer state
init_state(S) ->
    %%init namespaces from top-level module.
    set_state(S),
    case module_meta_info(curmod(),serl_namespaces) of
	{value,NSs} -> 
	    lists:foreach(fun ({NSName,Defs}) ->
				  merge_namespace(NSName,Defs)
			  end, 
			  NSs);
	_ -> nil
    end
    %%io:format("Initialized State:\n~p\n",[get_state()])
    .
    

%% compile(Exp) ->
%%     foo.
%%     %%transform().

transform(Exp) ->
    %DExp=desugar:renest(Exp),
    DExp=Exp,
    case DExp of 
	?ast_paren([Car|Body]) -> 
	    case lookup_expander(Car) of
		{special,F} -> F(Body);
		{macro,F} -> transform(F(Body)); 
		_ -> case lookup_expander(?cast_atom('call')) of
			 %% this is probably correct. A function call should follow the convention of the active module.
			 %% it could be a special form or a macro.
			 %% %% TODO Hmmmm... should I inspect the function header so I know how 'call' should actually be used?
			 {_,F} ->
			     F([Car|Body]);
			 _ -> error("cannot find an expander for functional call. ")
		     end 
	    end;
	_ when is_tuple(DExp) ->
	    [AstType,L,Mod|Body]=tuple_to_list(DExp),
	    transform(?cast_paren([?ast_atom3(L,Mod,AstType)|Body])) 
    end.

transform_each(Es) ->
    lists:map(fun transform/1,Es).

lookup_expander(Car) ->
    %% macros shadow special forms
    case lookup_macro(Car) of
	{value,F} -> {macro,F};
	_ -> case lookup_special(Car) of
		 {value,F} -> {special,F}; 
		 _ -> false
	     end
    end.

lookup_special(Car) ->
    lookup_namespace(specials,Car).

lookup_macro(Car) ->
    lookup_namespace(macros,Car).
