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
		  lookup/3,
		  transform/2,
		  transform_each/2
		 ]).
-import(lists,[map/2,member/2]).

-compile(export_all).

%% -define(defsp(Name,Args),Name(Args,Env)).

-define(defsp(Name,Args),Name(?ast_paren3(Line,_Mod,[_|Args]),Env)).

%% -define(atomic_literals,[integer,float,string,atom]).
%% -define(patterns,[match,var,tuple,nil,cons,op,record,record_index]++?atomic_literals).
%% -define(guards,[var,tuple,nil,cons,bin,op,record,record_index,record_field,call]++?atomic_literals).

%% Serl Extension

%% '__rm_lit'([],Here) ->
%%     ?cast_string(Here).

%% Translate [e0 e1 ...] to a list if not handled by some other macro
?defsp('__sp_block',[Es]) ->
    Line,
    transform(?cast_paren([?cast_atom(list),?cast_block(Es)]),Env).

%% Translate {e0 e1 ...} to a tuple if not handled by some other macro
?defsp('__sp_brace',[Es]) ->
    Line,
    transform(?cast_paren([?cast_atom(tuple),?cast_block(Es)]),Env).

%% (eval-binding <var>) gets the value in eval-bindings named by <var>
%% %% this is strictly a value expression.
%% %% (eval-binding <var>) simply compiles to <var>, bypassing binding check.
?defsp('__sp_eval-binding',[?ast_atom(V)]) ->
    {Env,?erl_var(Line,V)};
?defsp('__sp_eval-binding',[?ast_var(V)]) ->
    {Env,?erl_var(Line,V)}.


?defsp('__sp_eof',[]) ->
    Line,
    emit(Env). 

emit(Env) ->
    Module=gen_module(Env),
    Exports=gen_exports(Env),
    Defs=case env:assoc(Env,[definitions,functions]) of
	     {ok,Defuns} ->
		 [Def || {_FName,FDef} <- Defuns, {_Arity,Def} <- FDef]
	 end, 
    Forms=[Module]++Exports++Defs,
    %%compile_forms(atom_to_list(curmod())++".beam",Forms),
    {Env,Forms}.

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
	{ok,Ast} -> Ast
    end.

%% (export a b c)
gen_exports(Env) ->
    Exports=
	case env:assoc(Env,[exports,functions]) of
	    {ok,Names} ->
		[{Name,L,Arity} ||
		    ?ast_atom3(L,_Mod,Name) <- Names,
		    {Arity,_Ast} <- begin {ok,Defs}=scompile:get_def(Env,functions,Name),Defs end]; 
	    _ -> []
	end, 
    %% just let the erlang compiler check if exported functions are defined.
    [{attribute,L,export,[{F,Arity}]} || {F,L,Arity} <- Exports].

%% emit_meta(Env) -> 
%% %%     Bs=[{F,[{Arity,{curmod(),F}} || Arity <- Arities]}
%% %% 	|| {F,Arities} <- Keys]
%%     foo.


?defsp('__sp_bof',[]) ->
    Line,
    {put_meta_env(Env,env:toplevel_of(mverl)),0}.

get_meta_env(Env) ->
    env:assoc(Env,[compile_env]).
put_meta_env(Env,MEnv) ->
    env:assoc_put(Env,[compile_env],MEnv).

%% (defm foo "a macro":
%%  (A: 'foo)
%%  ([A B]: 'bar)
%%  )

%% macros are interpreted when used within the compiling module.
?defsp('__sp_defm',Es) ->
    [?ast_block(Header),?ast_block(Clauses)]=to_blocks(Es), 
    {Name,Doc}=
	case Header of
	    [?ast_atom(A)] -> {A,""};
	    [?ast_atom(A),?ast_string(S)] -> {A,S}
	end,
    case env:assoc(Env,[definitions,macros,Name]) of
	{ok,_} -> error("Macro already defined: ~p/~p\n", [Name]); 
	_ -> ok
    end,
    %% macro is compiled using the meta-environment
    [GSym]=scompile:gensym(1), 
    {ok,MEnv}=get_meta_env(Env),
    %% the ast to be evaled when macro is used.
    %% (case $V (A: 'foo) ([A B]: 'bar))
    {_,Ast}=transform(
	      ?cast_paren(
		 [?cast_atom('case'),?cast_paren([?cast_atom('eval-binding'),
						  ?cast_var(GSym)])
		  |Clauses]),
	      MEnv),
    %% the ast to be compiled in the meta-module
    %% %% (def $mac$<macro-name>: (($V): (case $V (A: 'foo) ([A B]: 'bar))) )
    MacName=list_to_atom("$mac$"++atom_to_list(Name)),
    FnAst={function,Line, MacName, 1,
	   function_clause(
	     ?cast_paren([?cast_paren([?cast_var(GSym)]),
			  ?cast_block(
			     [?cast_paren(
				 [?cast_atom('case'),?cast_var(GSym)
				  |Clauses])])]),
	     MEnv)}, 
    %% the macro function interprets the erl-ast of the macro definition
    %% the macro function closes over the current  meta-environment.
    %% the macro is not visible to its own definition.
    %% %% but the macro /can/ be used recursively.
    %% functions defined after the macro are not visible.
    %% %% I think this is sensible... 
    FnVal=fun (MacData) ->
		  {_,Val,_}=erl_eval:expr(Ast,[{GSym,MacData}],MEnv),
		  Val
	  end, 
    %% augument environment with the new macro 
    Env2=scompile:new_def(Env,macros,Name,FnVal),
    Env3=scompile:new_def(Env2,macros_info,Name,[{ast,FnAst},{doc,Doc}]),
    %% do the same for MEnv, so macro is usable for macro definitions that come later.
    MEnv2=scompile:new_def(MEnv,macros,Name,FnVal),
    Env4=put_meta_env(Env3,MEnv2),
    {Env4,?def_sec}.

%% (def foo
%%  "a function foo": 
%%   ((a b c) when guards: a b c)
%%   ((c d e): a b c) 
%%   )

?defsp('__sp_def',Es) ->
    %% {FName,Arity,_Doc,Ast}=parse_def(Es,Env),
    [?ast_block(Header),?ast_block(Clauses)]=to_blocks(Es),
    %% get the parameter list of the first functional clause.
    Arities=[length(Params) || ?ast_paren([?ast_paren(Params)|_]) <- Clauses],
    %% check arities are the same for all clauses
    T=lists:all(fun (Arity) -> Arity==hd(Arities) end,
		Arities),
    if T -> ok;
       true -> error("Different arities for function clauses.")
    end,
    Arity=hd(Arities),
    {?ast_atom(FName),_Doc}=
	case Header of
	    [A] -> {A,""};
	    [A,?ast_string(S)] -> {A,S}
	end, 
    case env:assoc(Env,[definitions,functions,FName,Arity]) of
	{ok,_} -> error("Function redefined: ~p/~p\n", [FName,Arity]); 
	_ -> ok
    end,
    %% transforming the body should only affect the lexical environment. So we discard the returned environment.
    Ast={function,Line,FName, Arity,
	 [function_clause(C,Env) || C <- Clauses]},
    Env2=env:assoc_put(Env,[definitions,functions,FName,Arity], Ast),
    {Env2,?def_sec}.

%% (compile-for expand run: <form>*) 
%%%% the forms must be toplevel forms for the same section

?defsp('__sp_compile_for',Es) ->
    Line,
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
    MEnv=get_meta_env(Env),
    {MEnv2,Rs}=transform_each(Forms,MEnv),
    Section=hd(Rs),
    lists:foreach(
      fun (I) ->
	      if I==Section -> true;
		 true -> error("compile-for: not all forms are toplevel from the same section")
	      end
      end,
      Rs),
    {put_meta_env(Env,MEnv2),Section}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.1 Module declarations and forms
%
%% A module declaration consists of a sequence of forms that are either function declarations or attributes.
%

%%  If D is a module declaration consisting of the forms F_1, ..., F_k, then Rep(D) = [Rep(F_1), ..., Rep(F_k)].

%%  If F is an attribute -module(Mod), then Rep(F) = {attribute,LINE,module,Mod}.

?defsp('__sp_module',[?ast_atom(Mod)]) ->
    CM=curmod(),
    if CM==Mod -> ok;
       true -> error("Module name mismatch: ~p",[Mod])
    end,
    {assoc_put(Env,[module],{attribute,Line,module,Mod}),
     ?module_sec}.


%% %%  If F is an attribute -export([Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) = {attribute,LINE,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}.
%% (export a b)
?defsp('__sp_export',Fs) ->
    Line,
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
?defsp('__sp_import',[?ast_atom(Mod)|Fs]) ->
    Line,
    Imports=ordsets:from_list([A || ?ast_atom(A) <- Fs]),
    %% check for conflicts
    lists:foreach(
      fun (F) ->
	      case scompile:toplevel_lookup(Env,functions,F) of
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

?defsp('__sp_integer',[I]) ->
    {Env,?erl_integer(Line,I)}.

%% %%  If L is a float literal, then Rep(L) = {float,LINE,L}. 
%% '__sp_float'([L,F]) ->
%%     ?erl_float(L,F).

%%  If L is a string literal consisting of the characters C_1, ..., C_k, then Rep(L) = {string,LINE,[C_1, ..., C_k]}.
?defsp('__sp_string',[S]) ->
    {Env,?erl_string(Line,S)}.

%%  If L is an atom literal, then Rep(L) = {atom,LINE,L}. 
?defsp('__sp_atom',[A]) ->
    {Env,?erl_atom(Line,A)}.

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
    scompile:map_env0(fun pattern/2,Patterns,Env).

pattern(?ast_quote(_Q),_Env) ->
    error("quote not supported in pattern");
pattern(?ast_var3(L,_,'_'),Env) ->
    {Env,?erl_var(L,'_')};
pattern(?ast_var3(_L,M,V)=Var,Env) ->
    case lookup(Env,vars,{M,V}) of
	%% the use occurence
	{ok,_Alias} -> transform(Var,Env);
	%% the binding occurence
	false -> transform(Var,scompile:lexical_extend(Env,vars,[{M,V}])) 
    end;
pattern(?ast_block(Es)=List,Env) ->
    %% first pass generates the bindings
    {Env2,_}=patterns(Es,Env),
    %% second pass generates the code
    transform(List,Env2);
pattern(?ast_brace(Es)=List,Env) ->
    %% first pass generates the bindings
    {Env2,_}=patterns(Es,Env),
    %% second pass generates the code
    transform(List,Env2); 
pattern(P,Env) ->
    error("Invalid pattern."),
    {Env2,P2}=scompile:expand1(P,Env),
    pattern(P2,Env2).

%% %% %%  If P is a variable pattern V, then Rep(P) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V. 
%% %% %%  If P is a universal pattern _, then Rep(P) = {var,LINE,'_'}.


%%  If E is P = E_0, then Rep(E) = {match,LINE,Rep(P),Rep(E_0)}.
?defsp('__sp_=',[P,E]) ->
    %% erlang spec 6.10 pg74
    {Env1,RE} = transform(E,Env),
    {Env2,RP} = pattern(P,Env1),
    {Env2,{match,Line,RP,RE}}.

%%  If E is a variable V, then Rep(E) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V.
%%HY: It's silly to call variables variables when they don't vary... I call them bindings.

'__sp_var'(?ast_paren([?ast_atom3(Line,M,_),V]),Env) ->
    case lookup(Env,vars,{M,V}) of
	{ok,Alias} -> {Env,?erl_var(Line,Alias)}; 
	false -> error("Variable not declared: ~s",[V])
    end.


%% %%  If E is a tuple skeleton {E_1, ..., E_k}, then Rep(E) = {tuple,LINE,[Rep(E_1), ..., Rep(E_k)]}.

?defsp('__sp_tuple',[?ast_block(Es)]) ->
    %% doesn't respect "eval in some order". See transform_each in scompile.erl
    {Env2,Rs}=transform_each(Es,Env),
    {Env2,{tuple,Line,Rs}}.

%% %%  If E is [], then Rep(E) = {nil,LINE}.

?defsp('__sp_list',[]) ->
    {Env,{nil,Line}};
?defsp('__sp_list',[?ast_block([])]) ->
    {Env,{nil,Line}};
?defsp('__sp_list',[?ast_block([H|T])]) ->
    Line,
    transform(?cast_paren([?cast_atom(cons),H,
			   ?cast_paren([?cast_atom(list),?cast_block(T)])]),
	     Env).

%% %%  If E is a cons skeleton [E_h | E_t], then Rep(E) = {cons,LINE,Rep(E_h),Rep(E_t)}.

?defsp('__sp_cons',[Eh,Et]) ->
    {Env1,REh}=transform(Eh,Env),
    {Env2,REt}=transform(Et,Env1),
    {Env2,{cons,Line,REh,REt}}.


%% %%  If E is case E_0 of Cc_1 ; ... ; Cc_k end, where E_0 is an expression and each Cc_i is a case clause then Rep(E) = {'case',LINE,Rep(E_0),[Rep(Cc_1), ..., Rep(Cc_k)]}.
%% <case-expr> := (case <exp> <case-clause>+)
%% <case-clause> := (<pattern>: <form>+) | (<pattern> when <guard>: <form>+)
?defsp('__sp_case',[E|Clauses]) ->
    %% this doesn't respect erlang semantics where the bindings in case clauses are visible outside.
    {Env2,RE0}=transform(E,Env),
    {Env2,
     {'case',Line,
      RE0,
      [case_clause(C,Env2)|| C <- Clauses ]}}.

%% If E is begin B end, where B is a body, then Rep(E) = {block,LINE,Rep(B)}.
?defsp('__sp_begin',Es) -> 
    {Env2,REs}=transform_each(Es,Env),
    {Env2,{block,Line,REs}}.

%%  If E is E_m:E_0(E_1, ..., E_k), then Rep(E) = {call,LINE,{remote,LINE,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}. 
?defsp('__sp_call',[?ast_brace([M,F])|Es]) ->
    {Env2,RM}=transform(M,Env),
    {Env3,RF}=transform(F,Env2),
    {Env4,REs}=transform_each(Es,Env3),
    {Env4,{call,Line,{remote,Line,RM,RF},REs}};

%%  If E is E_0(E_1, ..., E_k), then Rep(E) = {call,LINE,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}.
?defsp('__sp_call',[?ast_atom3(_L,M,F)|Es]=E) ->
    %% For hygiene, test to see if the symbol came from another module.
    IsRemote=(M==curmod()), 
    if IsRemote ->
	    transform(?cast_paren([?cast_atom('__call')|
				   [?cast_brace([?cast_atom(M),?cast_atom(F)])|Es]]),
		      Env);
       true ->
	    case lookup(Env,functions,{M,F}) of 
		{ok,{M2,F}} ->
		    %% call to imported function
		    transform(?cast_paren([?cast_atom('__call')|
					   [?cast_brace([?cast_atom(M2),?cast_atom(F)])|Es]]),
			      Env); 
		{ok,F} ->
		    %% call to lexical or module function (defined)
		    {Env2,REs}=transform_each(Es,Env),
		    {Env2,{call,Line,?erl_atom(Line,F),REs}}; 
		_ -> %% call to module function (not yet defined)
		    make_call(E,Env) 
	    end
    end;
?defsp('__sp_call',E) ->
    Line,
    make_call(E,Env).

make_call([E0|Es],Env) ->
    L=lineno(),
    {Env2,RE0}=transform(E0,Env),
    {Env3,REs}=transform_each(Es,Env2),
    {Env3,{call,L,RE0,REs}}.


%% 4.5 Clauses

%% erlang's guarded clauses take guard-sequences

%% serl's guarded clauses just take one guard (a list of guard-tests):
%% (Pat when <guard-test>+: <form>+)
%% use (or <guard-test>+) to express guard-sequence

%% bindings made in serl's clauses are not visible outside.

%% There are function clauses, if clauses, case clauses and catch clauses. 
%% A clause C is one of the following alternatives:

function_clause(?ast_paren3(L,_,Clause),Env) ->
    [?ast_block(Match),?ast_block(Forms)]=to_blocks(Clause),
    case Match of
	%%  If C is a function clause ( Ps ) -> B where Ps is a pattern sequence and B is a body, then Rep(C) = {clause,LINE,Rep(Ps),[],Rep(B)}.
	%% ((<pattern>*): <form>+)
	[?ast_paren(Patterns)] ->
	    clause(Patterns,[],Forms,L,Env);
	%%  If C is a function clause ( Ps ) when Gs -> B where Ps is a pattern sequence, Gs is a guard sequence and B is a body, then Rep(C) = {clause,LINE,Rep(Ps),Rep(Gs),Rep(B)}.
	%% ((<pattern>*) when <guard-test>+: <form>+)
	[?ast_paren(Patterns),?ast_atom('when'),GuardTests] ->
	    clause(Patterns,GuardTests,Forms,L,Env)
    end.


%% <case-clause> := (<pattern>: <exp>+) | (<pattern> when <guard>: <exp>+)
case_clause(?ast_paren3(L,_,[Pattern|GuardedBody]),Env) ->
    case GuardedBody of
	%% If C is a case clause P -> B where P is a pattern and B is a body, then Rep(C) = {clause,LINE,[Rep(P)],[],Rep(B)}.
	%% (<pattern>: <exp>+)
	[?ast_block(Forms)] ->
	    clause([Pattern],[],Forms,L,Env);
	%% If C is a case clause P when Gs -> B where P is a pattern, Gs is a guard sequence and B is a body, then Rep(C) = {clause,LINE,[Rep(P)],Rep(Gs),Rep(B)}.
	%% (<pattern> when <guard>: <exp>+)
	[?ast_atom('when'),Guard,?ast_block(Forms)] ->
	    clause([Pattern],Guard,Forms,L,Env)
    end.

clause(Patterns,_Guard,Body,Line,Env) ->
    %% A <guard> is a list of <guard-test>
    {Env2,Ps}=patterns(Patterns,Env),
    {_,Es}=transform_each(Body,Env2), 
    %{clause,lineno(), Ps, guard(Guard), Es}
    {clause,Line, Ps, [], Es}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Quotation

?defsp('__sp_quote',?ast_quote(E)) ->
    %% the quotation forms might as well be macros.
    %% but it's easier to implement them as special forms (direct translation to erlang ast).
    {Env,erl_syntax:revert(erl_syntax:set_pos(erl_syntax:abstract(E),Line))}.


?defsp('__sp_bquote',?ast_bquote(E)) ->
    Line,
    transform(bq:completely_expand(E),Env).


%% '__mac_block'([Es]) ->
%%     ?cast_brace([?cast_atom('__block'),
%% 		 ?cast_integer(lineno()),
%% 		 ?cast_atom(curmod()),
%% 		 Es
%% 		]);
%% '__mac_block'([Es,L,M]) ->
%%     ?cast_brace([?cast_atom('__block'),L ,M ,Es]).

%% '__mac_paren'([Es]) ->
%%     ?cast_brace([?cast_atom('__paren'),
%% 		 ?cast_integer(lineno()),
%% 		 ?cast_atom(curmod()),
%% 		 Es
%% 		]);
%% '__mac_paren'([Es,L,M]) ->
%%     ?cast_brace([?cast_atom('__paren'),L ,M ,Es]).

%% '__mac_brace'([Es]) ->
%%     ?cast_brace([?cast_atom('__brace'),
%% 		 ?cast_integer(lineno()),
%% 		 ?cast_atom(curmod()),
%% 		 Es
%% 		]);
%% '__mac_brace'([Es,L,M]) ->
%%     ?cast_brace([?cast_atom('__brace'),L ,M ,Es]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility Functions

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

read(In) -> scompile:read(In,?MODULE).

    
sexpand(In) ->
    {Env,Ast}=read(In),
    scompile:expand(Ast,Env).

seval(In) ->
    {Env,Ast}=read(In),
    scompile:eval(Ast,Env).
seval(In,Bindings) ->
    {Env,Ast}=read(In),
    scompile:eval(Ast,Env,Bindings).
    
    
%% a common idiom is a list of blocks seperated by ':' (foo a b c d: e f g h: i j k l)
%% this functions return a list of blocks.

%% this is somewhat problematic
%% no [] is allowed in the first list of items
%% (foo [a b] c d: e f g) would fail, perhaps surprisingly.
%%%% at least it surprised me.
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
