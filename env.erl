-module(env).
-import(lists,[keysearch/3,
	       map/2
	      ]).
-export([new/1,
	 lookup/3,
	 flatten/2,
	 exports_of/1,exports_of/2,exports_of/3,
	 imports_of/1,
	 definitions_of/1,
	 toplevel_of/1,
	 
	 import/4,
	 shadow/4,
	 extend/3, 
	 assoc/2,assoc_put/3,
	 module_meta_info/2
	]).

error(Msg) ->
    error(Msg,[]).

error(Msg,Args) ->
    erlang:error({serl_env_error,lists:flatten(io_lib:format(Msg,Args))}).

%% creates a new environment.
%% The argument has to be a serl module.
new(TopLevelMod) ->
    [{lexical,[]},{top_level_mod,TopLevelMod}|toplevel_of(TopLevelMod)]. 

%% collapse namespace
flatten(Env,NSType) ->
    %% fuck. This is totaly crap.
    {ok,Imports}=assoc(Env,imports),
    ImportBs=map(fun ({_Mod,NSs}) -> assoc(NSs,NSType) end,
		 Imports),
    DefBs=assoc(Env,[definitions,NSType]),
    LexicalBase=assoc(Env,[lexical_base,NSType]),
    LexicalScopes=
	case assoc(Env,[lexical,NSType]) of
	    false -> []; 
	    {ok,Scopes} ->  Scopes
	end,
    %% collapse according to lookup order.
    Bs=lists:foldl(
	 fun (Bs,AccDict) ->
		 if Bs/=false ->
			 case Bs of
			     {ok,Val} -> Defs=Val;
			     _ -> Defs=Bs
			 end,
			 dict:merge(
			   fun (_K,Old,_New) -> Old end,
			   AccDict,
			   dict:from_list(Defs));
		    true -> AccDict
		 end
	 end,
	 dict:new(),
	 LexicalScopes++[LexicalBase,DefBs|ImportBs]),
    dict:to_list(Bs).


lookup(Env,NSType,Key) -> 
    case lookup_lexical(Env,NSType,Key) of
	false -> case lookup_definitions(Env,NSType,Key) of 
		     false -> lookup_imports(Env,NSType,Key); 
		     Val -> Val
		 end;
	Val -> Val
    end.
				   
								 
lookup_lexical(Env,NSType,Key) ->
    %% fugly!
    case assoc(Env,[lexical,NSType]) of
	{ok,Scopes} ->
	    case lookup_scopes(Key,Scopes) of
		false -> assoc(Env,[lexical_base,NSType,Key]);
		Val -> Val
	    end;
	false -> assoc(Env,[lexical_base,NSType,Key])
    end.

lookup_scopes(_Key,[]) ->
    false;
lookup_scopes(Key,[Scope|Ss]) ->
    case assoc(Scope,Key) of
	{ok,Val} -> Val; 
	false -> lookup_scopes(Key,Ss)
    end.
    

lookup_definitions(Env,NSType,Key) ->
    assoc(Env,[definitions,NSType,Key]).

lookup_imports(Env,NSType,Key) ->
    assoc(Env,[imports,NSType,Key]).


%% doesn't check for shadowing.
import(Env,NSType,Mod,Keys) ->
    {NSType,ImportDefs}=exports_of(Mod,NSType,Keys),
    case assoc(Env,[imports,Mod,NSType]) of
	{ok,Val} -> Imports=Val;
	_ -> Imports=[]
    end,
    assoc_put(Env,[imports,Mod,NSType],ImportDefs++Imports). 

%% establishes a new lexical scope.
%% map a list of symbols to gensyms
shadow(Env,NSType,Prefix,Names) ->
    ExistingAliases=[Alias || {_Name,Alias} <- flatten(Env,NSType)],
    Gensyms=
	erl_syntax_lib:new_variable_names(
	  length(Names),
	  fun (I) -> list_to_atom(Prefix++io_lib:print(I)) end,
	  sets:from_list(ExistingAliases)),
    Bindings=
	lists:zipwith(
	 fun (Name,Alias) -> {Name,Alias} end,
	 Names,
	 Gensyms),
    assoc_cons(Env,[lexical,NSType],Bindings). 

%% assign once bindings. Error if already existing.
%% probably only used for variables. I can't imagine using it for any other purpose.
extend(Env,NSType,Bs) ->
    NewEnv=assoc_append(Env,[lexical_Base,NSType],Bs),
    %% check for duplicate element.
    {ok,NewBs}=assoc(NewEnv,[lexical_Base,NSType]),
    T=(length(NewBs)==length(ordsets:from_list(NewBs))), %ordsets are implemented as alists.
    if T -> NewEnv;
       true -> error("Conflicting bindings. Extending with \n~p\n\tto:\n~p\n",[NewBs,Env])
    end.

%% constructors:

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
		_ -> Bs=map(
		       fun (Key) ->
			 case keysearch(Key,1,Bindings) of
			    {value,B} -> B;
			     _ -> error("No definition for ~p in namespace ~p in module ~p", [Key,NSType,Mod])
			 end
		       end,
		       Keys),
		     {NSType,Bs}
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
	_ -> []
    end.

imports_from(Mod,NSType,Keys) ->
    exports_of(Mod,NSType,Keys). 

definitions_of(Mod) ->
    case module_meta_info(Mod,[serl_definitions]) of
	{ok,AllDefs} -> AllDefs;
	_ -> []
    end.

toplevel_of(Mod) ->
    [{definitions,definitions_of(Mod)},{imports,imports_of(Mod)}] .



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

assoc_put(AList,[Key],Val) ->
    lists:keystore(Key,1,AList,{Key,Val});
assoc_put(AList,[Key|Keys],Val) -> 
    case keysearch(Key,1,AList) of
	{value,{_Key,AList2}} when is_list(AList2) ->
	    lists:keystore(Key,1,AList,{Key,assoc_put(AList2,Keys,Val)});
	_ -> lists:keystore(Key,1,AList,{Key,assoc_put([],Keys,Val)})
    end.


assoc_cons(AList,Keys,Item) ->
    assoc_append(AList,Keys,[Item]).

assoc_append(AList,Keys,List) ->
    case assoc(AList,Keys) of
	{ok,OldList} when is_list(OldList) ->
	    assoc_put(AList,Keys,List++OldList); 
	false -> assoc_put(AList,Keys,List)
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
