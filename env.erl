-module(env).
-include("ast.hrl").

-import(lists,[keysearch/3,
	       map/2
	      ]).

-import(scompile,[warn/1,warn/2,error/1,error/2]).

-export([
	 new/0,new/1,
	 local_lookup/3,
	 remote_lookup/3,
	 toplevel_lookup/3,
	 flatten/2,
	 exports_of/1,exports_of/2,exports_of/3,
	 imports_of/1,
	 definitions_of/1,
	 toplevel_of/1,
	 
	 import/2,import/4,
	 shadow/3,
	 extend/3, 
	 assoc/2,
	 assoc_put/3,assoc_cons/3,assoc_append/3,
	 module_meta_info/2
	]).

-compile(export_all).
%% empty environment
new() ->
    [].

%% an environment with initial import.
new(ImportMod) ->
    Env=import(new(),ImportMod),
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


%% -for a symbol a$home
%% -look up the lexical scope for {a home} or {a true}
%% -if not, look up the top-level of home for a

local_lookup(Env,NSType,{M,A}) ->
    case lookup_lexical(Env,NSType,{M,A}) of
	false -> toplevel_lookup(Env,NSType,A);
	Val -> Val
    end.

remote_lookup(Env,NSType,{M,A}) ->
    case lookup_lexical(Env,NSType,{M,A}) of
	%% TODO cache toplevels
	false -> toplevel_lookup(env:new(M),NSType,A);
	Val -> Val
    end.

toplevel_lookup(Env,NSType,A) ->
    case lookup_definitions(Env,NSType,A) of 
	false -> lookup_imports(Env,NSType,A); 
	Val -> Val
    end.

%% lookup(Env,NSType,Key) -> 
%%     case lookup_lexical(Env,NSType,Key) of
%% 	false -> case lookup_definitions(Env,NSType,Key) of 
%% 		     false -> lookup_imports(Env,NSType,Key); 
%% 		     Val -> Val
%% 		 end;
%% 	Val -> Val
%%     end.
				   
								 
lookup_lexical(Env,NSType,Key) ->
    %% fugly!
    LexVal=
	case assoc(Env,[lexical,NSType]) of
	    {ok,Scopes} -> lookup_scopes(Key,Scopes);
	    _ -> false
	end,
    case LexVal of
	false -> case assoc(Env,[lexical_base,NSType]) of
		     {ok,Scope} -> lookup_scope(Key,Scope); 
		     _ -> false
		 end;
	_ -> LexVal
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
    
%% establishes a new lexical scope.
%% map a list of symbols to gensyms
shadow(Env,NSType,Bindings) -> 
    assoc_cons(Env,[lexical,NSType],Bindings).

%% assign once bindings. Error if already existing.
%% probably only used for variables. I can't imagine using it for any other purpose.
extend(Env,NSType,Bs) ->
    NewEnv=assoc_append(Env,[lexical_base,NSType],Bs),
    %% check for duplicate element.
    {ok,NewBs}=assoc(NewEnv,[lexical_base,NSType]),
    T=(length(NewBs)==length(ordsets:from_list(NewBs))), %ordsets are implemented as alists.
    if T -> NewEnv;
       true -> error("Conflicting bindings. Extending with \n~p\n\tto:\n~p\n",[NewBs,Env])
    end.


exports_of(Mod) ->
    case meta_module_of(Mod) of
	%% treat as serl
	{ok,MetaMod} ->
	    {ok,Exports}=module_meta_info__(MetaMod,[serl_exports]),
	    {ok,[exports_of__(Mod,MetaMod,NSType,Defs) || {NSType,Defs} <- Exports]}
	    ;
	%% treat as compiled and loaded normal erlang modules
	_ -> case code:which(Mod) of
		 non_existing -> false;
		 _ -> {ok,NS}=exports_of(Mod,functions,all),
		      {ok,[NS]}
	     end
    end.



exports_of(Mod,NSType) ->
    case meta_module_of(Mod) of
	{ok,MetaMod} ->
	    case module_meta_info(Mod,[serl_exports,NSType]) of
		{ok,Keys} -> {ok,exports_of__(Mod,MetaMod,NSType,Keys)};
		_ -> false
	    end; 
	_ -> case NSType of
		 functions -> exports_of(Mod,functions,all); 
		 _ -> false
	     end
    end.

exports_of(Mod,NSType,Keys) ->
    case meta_module_of(Mod) of
	{ok,MetaMod} ->
	    {ok,exports_of__(Mod,MetaMod,NSType,Keys)};
	_ -> %% import compatiblity with .erl modules. If compiled & available, check its module_info for exported functions.
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
	     end
    end.

%% TODO eleganify.
%% TODO this should probably be named 'definitions_of'
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
    [{definitions,definitions_of(Mod)},
     {imports,Imports}] .



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

