-module(env).
-import(lists,[keysearch/3,
	       map/2
	      ]).
-export([new/1,
	 lookup/3,
	 exports_of/1,exports_of/2,exports_of/3,
	 imports_of/1,
	 definitions_of/1,
	 toplevel_of/1,
	 module_meta_info/2
	]).

%% creates a new environment.
%% The argument has to be a serl module.
error(Msg) ->
    error(Msg,[]).

error(Msg,Args) ->
    erlang:error({serl_env_error,lists:flatten(io_lib:format(Msg,Args))}).

new(TopLevelMod) ->
    {env,[],toplevel_of(TopLevelMod)}. 

lookup(NSType,Key,Env) ->
    lookup_lexical(NSType,Key,Env).

lookup_lexical(NSType,Key,{env,_Lex,TopLevel}) ->
    %% lexical lookup failed:
    lookup_toplevel(NSType,Key,TopLevel).

%% pattern matching on "false" is kinda ugly. Maybe change it later.
lookup_toplevel(NSType,Key,{toplevel,_Mod,{definitions,false},{imports,Imports}}) ->
    lookup_imports(NSType,Key,Imports);
lookup_toplevel(NSType,Key,{toplevel,_Mod,{definitions,Defs},{imports,Imports}}) ->
    case assoc(Defs,[NSType,Key]) of
	false ->
	    lookup_imports(NSType,Key,Imports);
	Val -> Val
    end.


lookup_imports(_NSType,_Key,false) ->
    false;
lookup_imports(_NSType,_Key,[]) ->
    false;
lookup_imports(NSType,Key,[{_Mod,NSs}|Is]) ->
    case assoc(NSs,[NSType,Key]) of
	false ->
	    lookup_imports(NSType,Key,Is); 
	Val -> Val
    end.
    
    
    


%% import(Env,functions,Mod,Defs).

%% import(Env,NSType,Mod,Defs).


%% %% establishes a new lexical scope.
%% shadow(Env,NSType,Bindings).

%% %% assign once bindings. Error if already existing.
%% extend(Env,NSType,Bindings).


exports_of(Mod) ->
    case meta_module_of(Mod) of
	%% treat as serl
	{ok,MetaMod} ->
	    {ok,Exports}=module_meta_info__(MetaMod,[serl_exports]),
	    [exports_of__(Mod,MetaMod,NSType,Defs) || {NSType,Defs} <- Exports]
	    ;
	%% treat as compiled and loaded normal erlang modules
	_ -> case code:which(Mod) of
		 not_existing -> false;
		 _ -> [{functions,Mod:module_info(exports)}]
	     end
    end.


exports_of(Mod,NSType) ->
    case meta_module_of(Mod) of
	{ok,MetaMod} ->
	    case module_meta_info(Mod,[serl_exports,NSType]) of
		{ok,Keys} -> exports_of__(Mod,MetaMod,NSType,Keys);
		_ -> false
	    end; 
	_ -> false
    end.

exports_of(Mod,NSType,Keys) ->
    case meta_module_of(Mod) of
	{ok,MetaMod} ->
	    exports_of__(Mod,MetaMod,NSType,Keys);
	_ -> false
    end.
    
exports_of__(Mod,MetaMod,NSType,Keys) ->
    case module_meta_info__(MetaMod,[serl_definitions,NSType]) of
	{ok,Bindings} ->
	    case Keys of
		all -> {NSType,Bindings};
		_ -> map(
		       fun (Key) ->
			 case keysearch(Key,1,Bindings) of
			    {value,B} -> B;
			     _ -> error("No definition for ~p in namespace ~p in module ~p", [Key,NSType,Mod])
			 end
		       end,
		       Keys)
	    end;
	__ -> error("No definitions for namespace ~p in module ~p",[NSType,Mod])
    end.


imports_of(Mod) ->
    case module_meta_info(Mod,[serl_imports]) of
	{ok,Imports} ->
	    map(fun ({ImportMod,NSs}) -> 
		 {ImportMod,[imports_from(ImportMod,NSType,Keys) || {NSType,Keys} <- NSs]}
		end,
		Imports);
	_ -> false
    end.

imports_from(Mod,NSType,Keys) ->
    exports_of(Mod,NSType,Keys). 

definitions_of(Mod) ->
    case module_meta_info(Mod,[serl_definitions]) of
	{ok,AllDefs} -> AllDefs;
	_ -> false
    end.

toplevel_of(Mod) ->
    {toplevel,Mod,{definitions,definitions_of(Mod)},{imports,imports_of(Mod)}}.



%% utilities

assoc(AList,Key) when is_atom(Key) ->
    assoc(AList,[Key]);
assoc(Val,[]) ->
    {ok,Val};
assoc(AList,[Key|Keys]) ->
    case keysearch(Key,1,AList) of
	{value,{_Key,Val}} -> assoc(Val,Keys);
	_ -> false
    end.
    
module_meta_info(Mod,Fs) when is_list(Fs) ->
    case meta_module_of(Mod) of
	{ok,MetaMod} -> module_meta_info__(MetaMod,Fs);
	_ -> false
    end;
module_meta_info(Mod,F) when is_atom(F) ->
    module_meta_info(Mod,[F]).
    
module_meta_info__(MetaMod,Fs) ->
    assoc(MetaMod:module_info(attributes),Fs). 
    
meta_module_of(Mod) when is_atom(Mod) ->
    MetaMod=list_to_atom(atom_to_list(Mod)++"__meta"),
    case code:which(MetaMod) of
	not_existing -> false;
	_ -> {ok,MetaMod}
    end.
