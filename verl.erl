%% language file for vanilla erlang.
-module(verl).
-include("ast.hrl").
-include("verl.hrl").
-include_lib("eunit/include/eunit.hrl").


-import(env,[assoc/2,assoc_put/3]).
-import(scompile,[error/1,
		  error/2,
		  curmod/0,
		  lineno/0,
		  transform/2,
		  transform_each/2
		 ]).
-import(lists,[map/2,member/2]).

-compile(export_all).

%% -define(atomic_literals,[integer,float,string,atom]).
%% -define(patterns,[match,var,tuple,nil,cons,op,record,record_index]++?atomic_literals).
%% -define(guards,[var,tuple,nil,cons,bin,op,record,record_index,record_field,call]++?atomic_literals).

%% Serl Extension

'__rm_lit'([],Here) ->
    ?cast_string(Here).


%% the quotation forms might as well be macros.
%% but it's easier to implement them as special forms (direct translation to erlang ast).

'__sp_quote'(?ast_quote3(L,_,E),Env) -> 
    {Env,erl_syntax:revert(erl_syntax:set_pos(erl_syntax:abstract(E),L))}.


'__sp_bquote'(?ast_bquote(E),Env) ->
    transform(bq:completely_expand(E),Env).

'__mac_block'([Es]) ->
    ?cast_brace([?cast_atom('__block'),
		 ?cast_integer(lineno()),
		 ?cast_atom(curmod()),
		 Es
		]);
'__mac_block'([Es,L,M]) ->
    ?cast_brace([?cast_atom('__block'),L ,M ,Es]).

'__mac_paren'([Es]) ->
    ?cast_brace([?cast_atom('__paren'),
		 ?cast_integer(lineno()),
		 ?cast_atom(curmod()),
		 Es
		]);
'__mac_paren'([Es,L,M]) ->
    ?cast_brace([?cast_atom('__paren'),L ,M ,Es]).

'__mac_brace'([Es]) ->
    ?cast_brace([?cast_atom('__brace'),
		 ?cast_integer(lineno()),
		 ?cast_atom(curmod()),
		 Es
		]);
'__mac_brace'([Es,L,M]) ->
    ?cast_brace([?cast_atom('__brace'),L ,M ,Es]).

%% a common idiom is a list of blocks seperated by ':' (foo a b c d: e f g h: i j k l)
%% this functions return a list of blocks.

to_blocks(Es) ->
    {FirstBlock,Blocks}=
	lists:splitwith(fun (E) ->
			 case E of
			     ?ast_block(_) -> false;
			     _ -> true
			 end
		    end,
		    Es),
    [?cast_block(FirstBlock)|Blocks].
    

%% %% (defm foo "a macro":
%% %%  ((a): 'foo)
%% %%  ((A B): '(,A ,B))
%% %%  )

%% '__sp_defm'(Es,Env) ->
%%     [?ast_block(Header),?ast_block(Clauses)]=to_blocks(Es),
%%     %% massage a clause ((A ...): ...) to (([A ...]): ...)
%%     %%%% convuluted...
%%     Clauses2=
%% 	[begin Arg=?cast_paren(?cast_block(Param)),
%% 	       ?cast_paren([Arg|GuardedBody])
%% 	 end
%% 	 || ?ast_paren([?ast_paren(Param)|GuardedBody]) <- Clauses],
%%     {Name,Doc}=
%% 	case Header of
%% 	    [?ast_atom(A)] -> {A,""};
%% 	    [?ast_atom(A),?ast_string(S)] -> {A,S}
%% 	end,
    
%%     %% macro is compiled using the meta-environment 
%%     MEnv=env:assoc(Env,[macro_env]),
%%     Ast={function,lineno(), Name, 1,
%% 	 [clause_function(C,MEnv) || C <- Clauses2]},
%%     Val=erl_eval:expr(FAst,erl_eval:new_bindings()),
%%     %% augument environment with the new macro
%%     {defm(Env,FName,Val,Ast,Doc),?def_sec}.

%% defm(Env,Name,Val,Ast,Doc) ->
%%     case env:assoc(Env,[definitions,macros,Name]) of
%% 	{ok,_} -> error("Macro already defined: ~p/~p\n", [Name]); 
%% 	_ -> ok
%%     end,
%%     Env2=env:assoc_put(Env,[definitions,macros,Name],Val),
%%     Env3=env:assoc_put(Env2,[definitions,macros_info,ast,Name],Ast),
%%     Env4=env:assoc_put(Env3,[definitions,macros_info,doc,Name],Doc),
%%     Env4.


%% (def foo
%%  "a function foo": 
%%   ((a b c) when guards: a b c)
%%   ((c d e): a b c) 
%%   )

'__sp_def'(Es,Env) ->
    {FName,Arity,_Doc,Ast}=parse_def(Es,Env),
    case env:assoc(Env,[definitions,functions,FName,Arity]) of
	{ok,_} -> error("Function redefined: ~p/~p\n", [FName,Arity]); 
	_ -> ok
    end,
    Env2=env:assoc_put(Env,[definitions,functions,FName,Arity], Ast),
    {Env2,?def_sec}.

get_def(Env,Name) ->
    case env:assoc(Env,[definitions,functions,Name]) of
	{ok,Fs} -> Fs;
	_ -> []
    end.

parse_def(Es,Env) ->
    [?ast_block(Header),?ast_block(Clauses)]=to_blocks(Es),
    %% get the parameter list of the first functional clause.
    Arities=[length(Params) || ?ast_paren([?ast_paren(Params)|_]) <- Clauses],
    %% check arities are the same for all clauses
    T=(length(ordsets:from_list(Arities))==1),
    if T -> ok;
       true -> error("Different arities for function clauses.")
    end,
    Arity=hd(Arities),
    {?ast_atom(FName),Doc}=
	case Header of
	    [A] -> {A,""};
	    [A,?ast_string(S)] -> {A,S}
	end,
    %% transforming the body should only affect the lexical environment. So we discard the returned environment.
    Ast={function,lineno(),FName, Arity,
	 [clause_function(Clause,Env) || Clause <- Clauses]},
    {FName,Arity,Doc,Ast}.
    
%% (compile-for expand run: <form>*) 
%%%% the forms must be toplevel forms for the same section

'__sp_compile_for'(Es,Env) -> 
    [?ast_block(As),?ast_block(Forms)] = to_blocks(Es),
    Times=ordset:from_list([A || ?ast_atom(A) <- As]),
    %% if compiling for both expand and run times, expand-time forms are compiled first.
    %% This order is an implementation detail.
    compile_for(Times,Forms,Env).

compile_for([run],Forms,Env) ->
    compile_for(run,Forms,Env);
compile_for([expand],Forms,Env) ->
    compile_for(expand,Forms,Env);
compile_for([expand,run],Forms,Env) ->
    %% forms for expand and run times must agree to the section they are in.
    {Env2,S1}=compile_for(expand,Forms,Env),
    {Env3,S2}=compile_for(run,Forms,Env2), 
    if S1==S2 -> {Env3,S1}; 
       true -> error("compile-for: expand and runtime forms belong to different compile sections")
    end;
compile_for(run,Forms,Env) ->
    {Env2,Rs}=transform_each(Forms,Env),
    Section=hd(Rs),
    lists:foreach(
      fun (I) ->
	      if I==Section -> true;
		 true -> error("compile-for: not all forms are toplevel from the same section")
	      end
      end,
      Rs),
    {Env2,Section};
compile_for(expand,Forms,Env) ->
    MEnv=env:assoc(Env,[compile_env]),
    {MEnv2,Rs}=transform_each(Forms,MEnv),
    Section=hd(Rs),
    lists:foreach(
      fun (I) ->
	      if I==Section -> true;
		 true -> error("compile-for: not all forms are toplevel from the same section")
	      end
      end,
      Rs),
    {env:assoc_put(Env,[compile_env],MEnv2),Section}.



'__sp_bof'([],Env) ->
    {env:assoc_put(Env,[compile_env,env:new(mverl)]),0}.

'__sp_eof'([],Env) ->
    emit(Env). 

emit(Env) ->
    Module=gen_module(Env),
    Exports=gen_exports(Env),
    Defs=case env:assoc(Env,[definitions,functions]) of
	     {ok,Defuns} ->
		 [Def || {_FName,FDef} <- Defuns, {_Arity,Def} <- FDef]
	 end, 
    Forms=[Module]++Exports++Defs,
    compile_forms(atom_to_list(curmod())++".beam",Forms),
    Env.

compile_forms(FileName,Forms) ->
    Bin=case compile:forms(Forms) of
	    {ok,_Mod,B} -> B;
	    {ok,_,B,_Warnings} -> B;
	    {error,Errors,Warnings} -> error("Compile Error: \n~p\n~p\n",[Errors,Warnings]);
	    error -> error("Compile Error.")
	end,
    file:write_file(FileName,Bin).

gen_module(Env) ->
    case assoc(Env,[module]) of
	{ok,{Mod,L}} -> {attribute,L,module,Mod}
    end.

%% (export a b c)
gen_exports(Env) ->
    Exports=
	case env:assoc(Env,[exports,functions]) of
	    {ok,Names} ->
		[{Name,L,Arity} ||
		    ?ast_atom3(L,_Mod,Name) <- Names,
		    {Arity,_Ast} <- get_def(Env,Name)]; 
	    _ -> []
	end, 
    %% just let the erlang compiler check if exported functions are defined.
    [{attribute,L,export,[{F,Arity}]} || {F,L,Arity} <- Exports].

%% emit_meta(Env) -> 
%% %%     Bs=[{F,[{Arity,{curmod(),F}} || Arity <- Arities]}
%% %% 	|| {F,Arities} <- Keys]
%%     foo.

%% '__mac_defm'([Name,Body]) ->
%%     {def,foo,Name,Body}.

%% Translate [e0 e1 ...] to a list if not handled by some other macro

'__sp_block'(?ast_block(Exps),Env) ->
    transform(?cast_paren([?cast_atom(list),?cast_block(Exps)]),Env).

%% Translate {e0 e1 ...} to a tuple if not handled by some other macro
'__sp_brace'(?ast_brace(Exps),Env) ->
    transform(?cast_paren([?cast_atom(tuple),?cast_block(Exps)]),Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.1 Module declarations and forms
%
%% A module declaration consists of a sequence of forms that are either function declarations or attributes.
%

%%  If D is a module declaration consisting of the forms F_1, ..., F_k, then Rep(D) = [Rep(F_1), ..., Rep(F_k)].

%%  If F is an attribute -module(Mod), then Rep(F) = {attribute,LINE,module,Mod}.

'__sp_module'([?ast_atom3(L,_,Name)],Env) ->
    CM=curmod(),
    if CM==Name -> ok;
       true -> error("incorrect module name: ~p\n",[Name])
    end,
    {assoc_put(Env,[module],{Name,L}),?module_sec}.


%% %%  If F is an attribute -export([Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) = {attribute,LINE,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}.
%% (export a b)
'__sp_export'(Fs,Env) -> 
    {env:assoc_append(Env,[exports,functions],Fs),?header_sec}.

%%     RFs=map(fun (?ast_paren([?ast_atom(A)|Arities])) ->
%% 		    [{A,Arity} || ?ast_integer(Arity) <- Arities]
%% 	    end,
%% 	    Fs),
%%     {env:assoc_cons(Env,Fs),?header_sec}.

%% %%  If F is an attribute -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) = {attribute,LINE,import,{Mod,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}}.
%% (import mod a b (c alias-of-c))
%% (import :all) %% not supported for verl
%% only import functions
%% compile-time error if the same name from different imports.

%% erl-spec pg120:
%% for an imported foo, all calls in the module calls the imported foo.
%% is foo is exported, it is the defined foo that is exported.

%% (import mod a b c)
'__sp_import'([?ast_atom(Mod)|Fs],Env) ->
    Imports=ordsets:from_list([A || ?ast_atom(A) <- Fs]),
    %% check for conflicts
    lists:foreach(
      fun (F) ->
	      case env:toplevel_lookup(Env,functions,[F]) of
		  {ok,{Mod2,F2}} -> error("Conflicting import ~p:~p with ~p:~p",
					  [Mod,F,Mod2,F2]);
		  _ -> ok
	      end
      end,
      Imports),
    Env2=try env:import(Env,Mod,functions,Imports)
	 catch no_imports ->
		 env:assoc_put(
		   Env,
		   [imports,Mod,functions],
		   [{F,{Mod,F}} || F <- Imports]) 
	 end,
    {Env2,?header_sec}.
    
    

%% %% %%  If F is a wild attribute -A(T), then Rep(F) = {attribute,LINE,A,T}. 
%% %% '__mac_wild'([wild|_]) ->
%% %%     error("wild attribute not supported").

%% %%  If F is a function declaration Name Fc_1 ; ... ; Name Fc_k, where each Fc_i is a function clause with a pattern sequence of the same length Arity, then Rep(F) = {function,LINE,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% 4.2 Atomic literals
%% %
%% %% There are five kinds of atomic literals, which are represented in the same way in patterns, expressions and guards:
%% %%%% HY: Hmmm... I count only four kinds...

%% %%  If L is an integer or character literal, then Rep(L) = {integer,LINE,L}.

'__sp_integer'(?ast_integer(I),Env) ->
    {Env,?erl_integer(lineno(),I)}.

%% %%  If L is a float literal, then Rep(L) = {float,LINE,L}. 
%% '__sp_float'([L,F]) ->
%%     ?erl_float(L,F).

%%  If L is a string literal consisting of the characters C_1, ..., C_k, then Rep(L) = {string,LINE,[C_1, ..., C_k]}.
'__sp_string'(?ast_string(S),Env) ->
    {Env,?erl_string(lineno(),S)}.

%%  If L is an atom literal, then Rep(L) = {atom,LINE,L}. 
'__sp_atom'(?ast_atom(A),Env) ->
    {Env,?erl_atom(lineno(),A)}.

%% %% Note that negative integer and float literals do not occur as such; they are parsed as an application of the unary negation operator.

%% %% %%atomic_literal({i_string,_Line,_S}) -> error("Interpolated string not supported.").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% 4.3 Pattern
%% %%
%% %% If Ps is a sequence of patterns P_1, ..., P_k, then Rep(Ps) = [Rep(P_1), ..., Rep(P_k)]. Such sequences occur as the list of arguments to a function or fun.
%% %%
%% %% Individual patterns are represented as follows: 
%% %%  If P is an atomic literal L, then Rep(P) = Rep(L).

patterns(Patterns,Env) ->
    scompile:map_env0(Env,fun pattern/2,Patterns).

%% %%  If P is a variable pattern V, then Rep(P) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V. 
%% %%  If P is a universal pattern _, then Rep(P) = {var,LINE,'_'}. 
pattern(?ast_var('_'),Env) ->
    {Env,?erl_var(lineno(),'_')};
pattern(?ast_var3(L,M,A),Env) ->
    case scompile:lookup(Env,vars,{M,A}) of
	%% this is the function scope binding occurence
	false -> {env:extend(Env,vars,[{{M,A},A}]),?erl_var(L,A)};
	%% this is the lexical scope
	_ -> transform(?cast_var(A),Env)
    end;
pattern(P,Env) ->
    %% is a macro, should expand to one of the above.
    %% should expand once, then try to generate pattern, if fails, expand once more.
    error("macro not supported in pattern"),
    {Env2,RP}=transform(P,Env),
    pattern(RP,Env2).


%%  If E is P = E_0, then Rep(E) = {match,LINE,Rep(P),Rep(E_0)}.
'__sp_='([P,E],Env) ->
    %% erlang spec 6.10 pg74
    {Env1,RE} = transform(E,Env),
    {Env2,RP} = pattern(P,Env1),
    {Env2,{match,lineno(),RP,RE}}.

%%  If E is a variable V, then Rep(E) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V.
%%HY: It's silly to call variables variables when they don't vary... I call them bindings.

'__sp_var'(?ast_var3(L,M,A),Env) ->
    case scompile:lookup(Env,vars,{M,A}) of
	{ok,Alias} -> {Env,?erl_var(lineno(),Alias)}; 
	false -> error("~p:~p Variable not declared: ~p",[M,L,A])
    end.


%% %%  If E is a tuple skeleton {E_1, ..., E_k}, then Rep(E) = {tuple,LINE,[Rep(E_1), ..., Rep(E_k)]}.

'__sp_tuple'([?ast_block(Es)],Env) ->
    %% doesn't respect "eval in some order". See transform_each in scompile.erl
    {Env2,Rs}=transform_each(Es,Env),
    {Env2,{tuple,lineno(),Rs}}.

%% %%  If E is [], then Rep(E) = {nil,LINE}.

'__sp_list'([],Env) ->
    {Env,{nil,lineno()}};
'__sp_list'([?ast_block([])],Env) ->
    {Env,{nil,lineno()}};
'__sp_list'([?ast_block([H|T])],Env) ->
    transform(?cast_paren([?cast_atom(cons),H,
			   ?cast_paren([?cast_atom(list),?cast_block(T)])]),
	     Env).

%% %%  If E is a cons skeleton [E_h | E_t], then Rep(E) = {cons,LINE,Rep(E_h),Rep(E_t)}.

'__sp_cons'([Car,Cdr],Env) ->
    {Env1,A}=transform(Car,Env),
    {Env2,Z}=transform(Cdr,Env1),
    {Env2,{cons,lineno(),A,Z}}.

'__sp_do'(Es,Env) -> 
    {Env2,REs}=transform_each(Es,Env),
    {Env2,{block,lineno(),REs}}.

%%  If E is E_m:E_0(E_1, ..., E_k), then Rep(E) = {call,LINE,{remote,LINE,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}. 
'__sp_call'([?ast_brace([M,F])|Es],Env) ->
    {Env2,Mod}=transform(M,Env),
    {Env3,Fn}=transform(F,Env2),
    {Env4,Body}=transform_each(Es,Env3),
    {Env4,{call,lineno(),{remote,lineno(),Mod,Fn},Body}};

%%  If E is E_0(E_1, ..., E_k), then Rep(E) = {call,LINE,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}.
'__sp_call'([?ast_atom(Name)=E0|Es],Env) ->
    case env:toplevel_lookup(Env,functions,Name) of
	{ok,{M,F}} -> '__sp_call'([?cast_brace([?cast_atom(M),?cast_atom(F)])|Es],Env);
	_ -> 
	    {Env2,F}=transform(E0,Env),
	    {Env3,Body}=transform_each(Es,Env2),
	    {Env3,{call,lineno(),F,Body}}
    end;
'__sp_call'([E0|Es],Env) ->
    {Env2,F}=transform(E0,Env),
    {Env3,Body}=transform_each(Es,Env2),
    {Env3,{call,lineno(),F,Body}}.


%% 4.5 Clauses
%% There are function clauses, if clauses, case clauses and catch clauses. 
%% A clause C is one of the following alternatives:

%%  If C is a function clause ( Ps ) -> B where Ps is a pattern sequence and B is a body, then Rep(C) = {clause,LINE,Rep(Ps),[],Rep(B)}.

%%  If C is a function clause ( Ps ) when Gs -> B where Ps is a pattern sequence, Gs is a guard sequence and B is a body, then Rep(C) = {clause,LINE,Rep(Ps),Rep(Gs),Rep(B)}.

clause_function(?ast_paren(Es),Env) ->
    [?ast_block(MatchHead),?ast_block(Body)]=to_blocks(Es),
    %% TODO handle guards
    [?ast_paren(Patterns)]=MatchHead,
    clause([Patterns,[],Body],Env).

%% clause_function([Patterns,Guards,?ast_block(Body)]) ->
%%     clause([Patterns,when_guards(Guards),Body]).


%% when_guards(?ast_block([?ast_atom(_,'when')|Guards])) ->
%%     GS=map(fun (?ast_block(_Tests)=Guard) -> Guard;
%% 	       (Test) -> ?ast_block([Test])
%% 	end,
%% 	Guards),
%%     ?ast_brace(GS).

%% clause([Patterns,[],Body]) ->
%%     clause([Patterns,?ast_brace([]),Body]);

clause([Patterns,[],Body],Env) ->
    %% TODO this doesn't respect erlang semantics where the bindings in case clauses are visible outside.
    {Env2,Ps}=patterns(Patterns,Env),
    {_,Es}=transform_each(Body,Env2),
    {clause,lineno(),
     Ps,
     [],%guards(Guards),
     Es}.



    
erl_parse_f(In) ->
    case erl_scan:string(In) of
	{ok,Tks,_} ->
	    case erl_parse:parse_form(Tks) of
		{ok,R} -> R
	    end
    end.

erl_parse_e(In) ->
    case erl_scan:string(In) of
	{ok,Tks,_} ->
	    case erl_parse:parse_exprs(Tks) of
		{ok,[R]} -> R
	    end
    end.
