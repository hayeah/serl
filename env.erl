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
	 
%%	 flatten/2,
	 exports_of/1,exports_of/2,exports_of/3,
	 imports_of/1,imports_of/3,
	 base_of/1,
	 definitions_of/1,
 	 toplevel_of/1,
	 
	 assoc/2,
	 assoc_put/3,assoc_cons/3,assoc_append/3,
	 serl_info/2,serl_info/1
	]).

%% empty environment
new() ->
    [].

%% an environment with base definitions.
%% modules that come first shadow those that come after.
new(Mod) ->
    {ok,Base}=base_of(Mod),
    [{base,Base},
     {lexical,[]},
     {imports,[]}]. 

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
    case serl_info(Mod,[exports]) of
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
    case serl_info(Mod,[exports,NSType]) of
	{ok,NS} ->
	    case Keys of
		all -> {ok,{NSType,NS}};
		_ -> Bs=[case keysearch(Key,1,NS) of
			     {value,B} -> B;
			     _ -> error("undef: ~p\\~p in namespace: ~p", [Mod,Key,NSType])
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
			all -> {ok,{functions,[{B,{{Mod,B}}} || B <- Exports]}};
			_ -> Names=ordsets:from_list(Keys),
			     Bs=ordsets:intersection(Names,Exports),
			     if length(Bs)==length(Keys) ->
				     {ok,{functions,[{B,{{Mod,B}}} || B <- Bs]}};
				true -> error("Undefined functions ~p in module ~p",
					      [ordsets:subtract(Names,Exports),Mod])
			     end 
		    end 
	     end;
	_ -> false
    end.

imports_of(Mod) ->
    serl_info(Mod,[imports]).



imports_of(Mod,NSType,Keys) ->
    case exports_of(Mod,NSType,Keys) of
	false -> false;
	Val -> Val
    end.

definitions_of(Mod) ->
    case serl_info(Mod,[definitions]) of
	false -> false; 
	Val -> Val
    end.

base_of(Mod) ->
    case serl_info(Mod,[base]) of
	{ok,Mods} -> {ok,[{M,case exports_of(M) of
				 {ok,Defs} -> Defs;
				 _ -> error("None existing base module.")
			     end}
			  || M <- Mods]}; 
	_ -> false
    end.
	    
    
toplevel_of(Mod) ->
    Base=
	case base_of(Mod) of
	    {ok,V0} -> V0;
	    _ -> []
	end,
    Imports=
	case imports_of(Mod) of
	    {ok,V1} -> V1;
	    _ -> []
	end,
    Exports=
	case exports_of(Mod) of
	    {ok,V2} -> V2;
	    _ -> []
	end,
    %% THINK: What is the toplevel?
    %% ANSWER: the toplevel is the imports of a module shadowed by its exported definitions
    [{base,Base},{imports,Imports},{definitions,Exports}].



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


serl_info(Mod,Fs) when is_list(Fs) ->
    %% THINK: what happens of MOD is not a serl module?
    %% Or when it is not loaded?
    %% Or when the module does not exist? 
    case serl_info(Mod) of
	{ok,Env} -> assoc(Env,Fs);
	_ -> false
    end;
serl_info(Mod,F) when is_atom(F) ->
    serl_info(Mod,[F]).


serl_info(Mod)  -> 
    case code:ensure_loaded(Mod) of
	{module,_} ->
	    case assoc(Mod:module_info(attributes),[serl]) of
		{ok,_} -> {ok,Mod:'serl-info'()};
		_ -> false
	    end;
	_ -> false
    end.

