-module(erl_lang).
-export([lookup_macro/1,compile/1]).
-export([module/1,call/1]).
-import(serl,[error/1,error/2]).
%% "lookup_macro" is an generated meta function.
%% there are symbol and expression macros.


compile(In) ->
    serl:compile(In,?MODULE).

transform(Exp) ->
    serl:transform(Exp,?MODULE).

transform_each(Exp) ->
    serl:transform_each(Exp,?MODULE).
    
lookup_macro(Name) ->    
    R=lists:keysearch(Name,1,
		      [{module,module},
		       {call,call}]),
    case R of
	{value,{_,F}} -> {value,F};
	_ -> false
    end. 

module([module,[atom,L,A]]) ->
    {attribute,L,module,A}.
call([call,Car|Body]) ->
    {call,Car,Body}.

%% %% 4.1 Module declarations and atoms

%% builtin([module,[atom,L,A]]) ->
%%     {attribute,L,module,A};
%% builtin([export|Fs]) ->
%%     {attribute,0,export, lists:map(fun ([F,A]) -> {F,A} end,Fs)};
%% builtin([import,Mod|Fs]) ->
%%     {attribute,0,import,{Mod,lists:map(fun ([F,A]) -> {F,A} end,Fs)}};
%% builtin([compile,Options]) ->
%%     {attribute,0,compile,Options};
%% builtin([file,File,Line]) ->
%%     {attribute,0,file,{File,Line}};
%% builtin([record,Name|Fs]) ->
%%     {attribute,0,record,{Name,lists:map(fun (F) -> record_field(F) end,Fs)}};
%% builtin([wild|_]) ->
%%     error("wild attribute not supported");
%% builtin([def,Name,{block,[C|_]=Clauses}]) ->
%%     [ArgsList|_]=C,
%%     Arity=list_size(ArgsList), 
%%     build_function_clauses(Name,Arity,Clauses);
%% builtin([function,Name,Arity|Clauses]) ->
%%     {function,0,Name,Arity,
%%      lists:map(fun (C) -> clause(C) end),Clauses}.

%% build_function_clauses(Name,Arity,Clauses) ->
%%     lists:map(
%%       fun (Clause) ->
%% 	      [Args|_] = Clause,
%% 	      GoodArity=list_size(Args)==Arity,
%% 	      if not GoodArity  -> error("Arity mismatch in function declaration:\n\t ~p/~p\n",[Name,Arity]);
%% 		 true -> nil
%% 	      end,
%% 	      case Clause of
%% 		  [Args,{block,Guards},{block,Body}] -> Gs=Guards,B=Body; 
%% 		  [Args,{block,Body}] -> Gs=[],B=Body
%% 	      end,
%% 	      clause([function,Args,Gs,B])
%%       end,
%%       Clauses). 

%% list_size(Ls) ->
%%     lists:foldl(fun (_,C) -> C+1 end,0,Ls).

%% %% 4.1.1 Record Fields
%% record_field([F]) -> {record_field,0,transform(F)};
%% record_field([F,V]) -> {record_field,0,transform(F),transform(V)}.
    

%% %% 4.2 Atomic Literals
%% atomic_literal({int,I}) -> {integer,0,I};
%% atomic_literal({float,F}) -> {float,0,F};
%% atomic_literal({atom,A}) -> {atom,0,list_to_atom(A)};
%% atomic_literal({string,S}) -> {string,0,S}.
%% %%atomic_literal({i_string,_Line,_S}) -> error("Interpolated string not supported.").

%% %% 4.3 Pattern

%% pattern(A) ->
%%     A.

%% %% 4.5 Clauses

%% clause([function,Patterns,Guards,Body]) ->
%%     Ps=lists:map(fun (P) -> pattern(P) end,Patterns),
%%     B=transform_each(Body),
%%     Gs=lists:map(fun (G) -> guard(G) end,Guards),
%%     {clause,0,Ps,Gs,B}.

%% %% 4.6 Guards

%% guard(A) ->
%%     A.
