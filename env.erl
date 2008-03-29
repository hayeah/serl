-module(env).
-include("ast.hrl").

-import(lists,[keysearch/3,
	       map/2
	      ]).

-import(scompile,[warn/1,warn/2,error/1,error/2]).

-export([
	 new/0,new/1,
	 import/2,import/4,
	 
%% 	 local_lookup/3,
%% 	 remote_lookup/3,
%% 	 toplevel_lookup/3,
	 
	 flatten/2,
	 exports_of/1,exports_of/2,exports_of/3,
	 imports_of/1,
	 definitions_of/1,
 	 toplevel_of/1,
	 
	 assoc/2,
	 assoc_put/3,assoc_cons/3,assoc_append/3,
	 module_meta_info/2
	]).

%% empty environment
new() ->
    [].

%% an environment with initial import.
new(ImportMod) ->
    Env=try import(new(),ImportMod)
	catch no_imports -> []
	end,
    assoc_put(Env,[lexical],[]).


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


import(Env,Mod) ->
    case exports_of(Mod) of
	{ok,NS} -> assoc_cons(Env,[imports],{Mod,NS});
	_ -> throw(no_imports)
    end.

%% doesn't check for shadowing.
import(Env,Mod,NSType,Keys) ->
    case exports_of(Mod,NSType,Keys) of
	{ok,{NSType,ImportDefs}} ->
	    %% case assoc(Env,[imports,Mod,NSType]) of
%%  		{ok,Val} -> Imports=Val;
%%  		_ -> Imports=[]
%%  	    end,
	    assoc_append(Env,[imports,Mod,NSType],ImportDefs);
	_ -> throw(no_imports)
    end. 


exports_of(Mod) ->
    case module_meta_info(Mod,[serl_exports]) of
	%% treat as serl
	{ok,NSs} -> {ok,NSs};
	%% treat as compiled and loaded normal erlang modules
	%% use M:module_info(exports) to get exported functions.
	_ -> case code:which(Mod) of
		 non_existing -> false;
		 _ -> {ok,NS}=exports_of(Mod,functions,all),
		      {ok,[NS]}
	     end
    end.


exports_of(Mod,NSType) ->
    case exports_of(Mod,NSType,all) of
	{ok,NSs} -> {ok,NSs};
	_ when NSType==functions -> exports_of(Mod,functions,all);
	_ -> false
    end.

exports_of(Mod,NSType,Keys) ->
    case module_meta_info(Mod,[serl_exports,NSType]) of
	{ok,NS} ->
	    case Keys of
		all -> {ok,{NSType,NS}};
		_ -> Bs=[case keysearch(Key,1,NS) of
			     {value,B} -> B;
			     _ -> error("No definition for ~p in namespace ~p in module ~p", [Key,NSType,Mod])
			 end
			 || Key <- Keys],
		     {ok,{NSType,Bs}}
	    end;
	_ when NSType==functions-> 
	    %% import compatiblity with .erl modules. If compiled & available, check its module_info for exported functions.
	    case code:ensure_loaded(Mod) of 
		 {error,nofile} -> false; 
		 _ ->
		    Exports=ordsets:from_list([F || {F,_Arity} <- Mod:module_info(exports)]),
		    case Keys of
			all -> {ok,{functions,[{B,{Mod,B}} || B <- Exports]}};
			_ -> Names=ordsets:from_list(Keys),
			     Bs=ordsets:intersection(Names,Exports),
			     if length(Bs)==length(Keys) -> {ok,{functions,[{B,{Mod,B}} || B <- Bs]}};
				true -> error("Undefined functions ~p in module ~p",
					      [ordsets:subtract(Names,Exports),Mod])
			     end 
		    end 
	     end;
	_ -> false
    end.

imports_of(Mod) ->
    case module_meta_info(Mod,[serl_imports]) of
	{ok,Imports} ->
	    {ok,map(fun ({ImportMod,NSs}) -> 
		 {ImportMod,[imports_from(ImportMod,NSType,Keys) || {NSType,Keys} <- NSs]}
		end,
		Imports)};
	_ -> false
    end.

imports_from(Mod,NSType,Keys) ->
    case exports_of(Mod,NSType,Keys) of
	{ok,Bs} -> Bs;
	_ -> false
    end.

definitions_of(Mod) ->
    case module_meta_info(Mod,[serl_definitions]) of
	{ok,AllDefs} -> AllDefs;
	_ -> []
    end.

toplevel_of(Mod) ->
    Imports=
	case imports_of(Mod) of
	    {ok,V} -> V;
	    _ -> []
	end,
    %% THINK: What is the toplevel?
    %% "Toplevel" is probably used for the shell.
    [{definitions,definitions_of(Mod)},
     {imports,Imports}].



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

%% make sure that a new element is placed in front of the alist.
assoc_put(_AList,[],Val) ->
    Val;
assoc_put(AList,[Key|Keys],Val) -> 
    case keysearch(Key,1,AList) of
	{value,{_Key,AList2}} when is_list(AList2) ->
	    lists:keystore(Key,1,AList,{Key,assoc_put(AList2,Keys,Val)});
	_ -> [{Key,assoc_put([],Keys,Val)}|AList] 
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
	non_existing -> false;
	_ -> {ok,MetaMod}
    end.

