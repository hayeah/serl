-module(scompile).
-include("ast.hrl").

-export([compile1/1,error/1,error/2,lineno/0,module_meta_info/2]).


-define(lineno,'__line_of_head').
lineno() -> get(?lineno).
lineno(N) -> put(?lineno,N).

-record(c_state,
	{namespaces=[], 
	 namespace_safety=1 %% none, whiny (gives warning when shadowed), anal (error when shadowed)
	 }).


-define(c_state,'__serl_compiler_state').

%% erlang record sucks! Why can't the compiler work a little harder?
%% Now I am doomed to adhoc inefficiency.
c_state_pos(Field) ->
    case Field of
	namespaces -> #c_state.namespaces;
	namespace_safety -> #c_state.namespace_safety
    end.

set_state(S) when is_record(S,c_state) ->
    put(?c_state,S).
set_state(Field,Val) ->
    set_state(setelement(c_state_pos(Field),get_state(),Val)).
   
get_state() ->
    get(?c_state).
get_state(Field) ->
    element(c_state_pos(Field),get_state()).


warn(Message) ->
    warn(Message,[]).
warn(Message,Args) ->
    io:format(Message,Args).

error(Message) ->
    error(Message,[]).
error(Message,Args) ->
    io:format(Message,Args),
    throw({serl_error,Message}).

parse(In) ->
    read:exps(In,?MODULE).


module_meta_info(Mod,Field) ->
    case meta_module_of(Mod) of
	{ok,MetaMod} ->
	    case lists:keysearch(serl,1,MetaMod:module_info(attributes)) of
		{value,_} ->
		    case lists:keysearch(Field,1,MetaMod:module_info(attributes)) of
			{value,{_,Val}} -> {ok,Val};
			_ -> false
		    end;
		_ -> false
	    end; 
	_ -> false
    end.

meta_module_of(Mod) when is_atom(Mod) ->
    MetaMod=list_to_atom(atom_to_list(Mod)++"__meta"),
    case code:which(MetaMod) of
	not_existing -> false;
	_ -> {ok,MetaMod}
    end.


compile1(_In) ->
    set_state(#c_state{}),
    case module_meta_info(serl,serl_namespaces) of
	{ok,NSs} -> 
	    lists:foreach(fun ({NSName,Defs}) ->
				  merge_namespace(NSName,Defs)
			  end, 
			  NSs);
	_ -> nil
    end,
    io:format("Special forms:~n~p~n",[dict:to_list(lookup_namespace(special_forms))]).
    %%transform().

lookup_namespace(NSName) ->
    case lists:keysearch(NSName,1,get_state(namespaces)) of
	{value,{_,NS}} -> NS;
	_ -> false
    end.
lookup_namespace(NSName,Name) ->
    case lookup_namespace(NSName) of
	{value,{_,NS}} -> dict:find(Name,NS);
	_ -> false
    end.

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


transform(Exp) -> 
    case DExp=desugar:renest(Exp) of 
	?ast_block(Es) ->
	    transform([?ast_atom(lineno(),list),?ast_block(Es)]);
	?ast_brace(Es) ->
	    transform([?ast_atom(lineno(),tuple),?ast_block(Es)]);
	[Car|Body] -> 
	    case lookup_expander(Car) of
		{special,F} -> F(Body);
		{macro,F} -> transform(F(Body));
		{ast,F} -> F(DExp); 
		_ -> transform([?ast_atom(lineno(),'call')|DExp])
	    end;
	_ -> DExp 
    end.

transform_each(Es) ->
    lists:map(fun (E) -> transform(E) end,Es).

lookup_expander(_Car) -> foo.
    
%% lookup_expander(Car) ->
%%     %% set the line number whenever a macro is expanded.
%%     case Car of
%% 	?ast_brace([?ast_atom(L,Mod),?ast_atom(_,Name)]) ->
%% 	    foo;
%% 	    %% lineno(L),
%% %% 	    ExternalMod=macro_module_of(Mod),
%% %% 	    case code:which(MacMod) of
%% %% 		non_existing -> false;
%% %% 		_ -> lookup_external_env(Name,ExternalMod)
%% %% 	    end;
	
%% 	?ast_atom(L,Name) ->
%% 	    lineno(L),
%% 	    %% macro definitions can shadow special forms.
%% 	    %% function definitions can't shadow special forms.
%% 	    lookup_current_env(Name);
%% 	_ when is_atom(Car) ->
%% 	    %% this is only possible for ast leaf nodes.
%% 	    %% TODO For hygiene, I probably will have to keep track the module of the asts...
%% 	    lookup_special(Car); 
%% 	_  -> false
%%     end.
    
%% lookup_remote_env(Name,Mod) ->
%%     case Mac=lookup_macro(Name,Mod) of
%% 	false -> lookup_special(Name,Mod);
%% 	_ -> Mac
%%     end.


    
%% %% find the name by looking up the _static_ module information.
%% %%%% TODO support language inheritance. treat an atom a module for recursive lookup.

%% lookup_macro(Name) -> 
%%     %% local macros are either imported, or not yet compiled.
%%     %% Their names are known in the compiler state. 
%%     %%look for macro by name
%%     get_state(macros).

%% lookup_macro(Name,Mod) ->
%%     %% external macro is compiled, so can be found in the module info.
%%     Mod:module_info(macros).

%% lookup_special(Name) when is_atom(Name) ->
%%     case lists:keysearch(Name,1,?module_specials) of
%% 	{value,{_,F}} -> {special,F};
%% 	_ -> false
%%     end.
