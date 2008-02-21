%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% These are generated from language spec.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(erl_lang).
-include_lib("eunit/include/eunit.hrl").
-import(serl,[error/1,error/2,lineno/0,lineno/1,erl_parse_f/1,erl_parse_e/1]).
-import(lists,[map/2,member/2]).
-export([lookup_macro/1,compile/1]).
-define(atomic_literals,[integer,float,string,atom]).

%% -define(patterns,[match,var,tuple,nil,cons,op,record,record_index]++?atomic_literals).
%% -define(guards,[var,tuple,nil,cons,bin,op,record,record_index,record_field,call]++?atomic_literals).

-include_lib("erl_lang.hrl").

%% "lookup_macro" should be an generated meta function.
lookup_macro([atom,L,Name]) ->
    %% set the line number whenever a macro is expanded. 
    lineno(L),
    lookup_macro(Name); 
lookup_macro(Name) when is_atom(Name) ->
    case lists:keysearch(Name,1,?module_macros) of
	{value,{_,F}} -> {macro,F};
	_ -> false
    end;
lookup_macro([brace,_Module,_Atom]) ->
    %% TODO, this is the mechanism for inheriting macros from another language module.
    %% since erl_lang is the base language, it has no where to look.
    %%{module,Module,Atom}
    false;
lookup_macro(_) -> false.

compile(In) ->
    serl:compile(In,?MODULE).

transform(Exp) ->
    serl:transform(Exp,?MODULE).

transform_each(Exp) ->
    serl:transform_each(Exp,?MODULE).
    

%% Every defined language must have a top-level macro to wrap all the top-level forms.
top_level(Exps) ->
    Exps.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Those above are generated
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Serl Extension

%% def([def,Name,{block,[C|_]=Clauses}]) ->
%%     [ArgsList|_]=C,
%%     Arity=length(ArgsList), 
%%     build_function_clauses(Name,Arity,Clauses).

%% build_function_clauses(Name,Arity,Clauses) ->
%%     lists:map(
%%       fun (Clause) ->
%% 	      [Args|_] = Clause,
%% 	      GoodArity=length(Args)==Arity,
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

'__mac_defm'([Name,Body]) ->
    {def,foo,Name,Body}.

%% Translate [e0 e1 ...] to a list if not handled by some other macro
'__mac_block'(Exps) ->
    [list,[block|Exps]].

%% Translate {e0 e1 ...} to a tuple if not handled by some other macro
'__mac_brace'(Exps) ->
    [tuple,[block|Exps]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.1 Module declarations and forms
%
%% A module declaration consists of a sequence of forms that are either function declarations or attributes.
%

%%  If D is a module declaration consisting of the forms F_1, ..., F_k, then Rep(D) = [Rep(F_1), ..., Rep(F_k)].

%%  If F is an attribute -module(Mod), then Rep(F) = {attribute,LINE,module,Mod}.
'__mac_module'([A]) ->
    {atom,_,Name}=transform(A),
    {attribute,lineno(),module,Name}.

%%  If F is an attribute -export([Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) = {attribute,LINE,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}.
'__mac_export'([[block|Fs]]) ->
    {attribute,lineno(),export,
     lists:map(fun ([F,A]) ->
		       {atom,_,Name}=transform(F),
		       {integer,_,Arity}=transform(A),
		       {Name,Arity}
	       end,Fs)}.

%%  If F is an attribute -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) = {attribute,LINE,import,{Mod,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}}.
'__mac_import'([Atom,[block|Fs]]) ->
    {atom,L,Mod}=transform(Atom),
    {attribute,L,import,
     {Mod,lists:map(fun ([F,A]) ->
			    {atom,_,Name}=transform(F),
			    {integer,_,Arity}=transform(A),
			    {Name,Arity}
		    end,Fs)}}.

%% %%  If F is an attribute -compile(Options), then Rep(F) = {attribute,LINE,compile,Options}.
%% '__mac_compile'([compile,Options]) ->
%%     {attribute,0,compile,Options}.

%% %%  If F is an attribute -file(File,Line), then Rep(F) = {attribute,LINE,file,{File,Line}}. 
%% '__mac_file'([file,File,Line]) ->
%%     {attribute,0,file,{File,Line}}.

%%  If F is a record declaration -record(Name,{V_1, ..., V_k}), then Rep(F) = {attribute,LINE,record,{Name,[Rep(V_1), ..., Rep(V_k)]}}. For Rep(V), see below. 
'__mac_record'([Atom,[block|Vs]]) ->
    {atom,_,Name}=transform(Atom),
    {attribute,lineno(),record,
     {Name,lists:map(fun (V) ->
			     case V of
				 %% err. Leaking abstraction here.
				 %% I really shouldn't have to peek into the argument to see that it's an atom...
				 [atom|_] ->
				     record_field(V);
				 [A,E] ->
				     record_field(A,E)
			     end
		     end,Vs)}}.

%% %%  If F is a wild attribute -A(T), then Rep(F) = {attribute,LINE,A,T}. 
%% '__mac_wild'([wild|_]) ->
%%     error("wild attribute not supported").

%%  If F is a function declaration Name Fc_1 ; ... ; Name Fc_k, where each Fc_i is a function clause with a pattern sequence of the same length Arity, then Rep(F) = {function,LINE,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}.

%% '__mac_function'([function,Name,Arity|Clauses]) ->
%%     {function,0,Name,Arity,
%%      lists:map(fun (C) -> clause(C) end),Clauses}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.1.1 Record Fields
%%
%% Each field in a record declaration may have an optional explicit default initializer expression 

%%  If V is A, then Rep(V) = {record_field,LINE,Rep(A)}.
record_field(A) ->
    RA={atom,L,_}=transform(A),
    {record_field,L,RA}.

%%  If V is A = E, then Rep(V) = {record_field,LINE,Rep(A),Rep(E)}.
record_field(A,E) ->
    RA={atom,L,_}=transform(A),
    {record_field,L,RA,transform(E)}.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.2 Atomic literals
%
%% There are five kinds of atomic literals, which are represented in the same way in patterns, expressions and guards:
%%%% HY: Hmmm... I count only four kinds...

%%  If L is an integer or character literal, then Rep(L) = {integer,LINE,L}. 
'__mac_integer'([L,I]) ->
    {integer,L,I}.

%%  If L is a float literal, then Rep(L) = {float,LINE,L}. 
'__mac_float'([L,F]) ->
    {float,L,F}.

%%  If L is a string literal consisting of the characters C_1, ..., C_k, then Rep(L) = {string,LINE,[C_1, ..., C_k]}.
'__mac_string'([L,S]) ->
    {string,L,S}.

%%  If L is an atom literal, then Rep(L) = {atom,LINE,L}. 
'__mac_atom'([L,A]) ->
    {atom,L,A}.

%% Note that negative integer and float literals do not occur as such; they are parsed as an application of the unary negation operator.

%% %%atomic_literal({i_string,_Line,_S}) -> error("Interpolated string not supported.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.3 Pattern
%%
%% If Ps is a sequence of patterns P_1, ..., P_k, then Rep(Ps) = [Rep(P_1), ..., Rep(P_k)]. Such sequences occur as the list of arguments to a function or fun.
%%
%% Individual patterns are represented as follows: 
%%  If P is an atomic literal L, then Rep(P) = Rep(L).

pattern_sequence(Patterns) ->
    map(fun pattern/1,Patterns).

pattern(P) ->
    %% TODO should check if pattern is valid. 
    transform(P).
	
	
%%  If P is a compound pattern P_1 = P_2, then Rep(P) = {match,LINE,Rep(P_1),Rep(P_2)}.

%%  If P is a variable pattern V, then Rep(P) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V.

%%  If P is a universal pattern _, then Rep(P) = {var,LINE,'_'}.

%%  If P is a tuple pattern {P_1, ..., P_k}, then Rep(P) = {tuple,LINE,[Rep(P_1), ..., Rep(P_k)]}.

%%  If P is a nil pattern [], then Rep(P) = {nil,LINE}.

%%  If P is a cons pattern [P_h | P_t], then Rep(P) = {cons,LINE,Rep(P_h),Rep(P_t)}.

%%  If E is a binary pattern <<P_1:Size_1/TSL_1, ..., P_k:Size_k/TSL_k>>, then Rep(E) = {bin,LINE,[{bin_element,LINE,Rep(P_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,LINE,Rep(P_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see below. An omitted Size is represented by default. An omitted TSL (type specifier list) is represented by default.

%%  If P is P_1 Op P_2, where Op is a binary operator (this is either an occurrence of ++ applied to a literal string or character list, or an occurrence of an expression that can be evaluated to a number at compile time), then Rep(P) = {op,LINE,Op,Rep(P_1),Rep(P_2)}. 
%%  If P is Op P_0, where Op is a unary operator (this is an occurrence of an expression that can be evaluated to a number at compile time), then Rep(P) = {op,LINE,Op,Rep(P_0)}. 
%%  If P is a record pattern #Name{Field_1=P_1, ..., Field_k=P_k}, then Rep(P) = {record,LINE,Name, [{record_field,LINE,Rep(Field_1),Rep(P_1)}, ..., {record_field,LINE,Rep(Field_k),Rep(P_k)}]}. 
%%  If P is #Name.Field, then Rep(P) = {record_index,LINE,Name,Rep(Field)}. 
%%  If P is ( P_0 ), then Rep(P) = Rep(P_0), i.e., patterns cannot be distinguished from their bodies. 
%% Note that every pattern has the same source form as some expression, and is represented the same way as the corresponding expression.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.4 Expressions
%
%% A body B is a sequence of expressions E_1, ..., E_k, and Rep(B) = [Rep(E_1), ..., Rep(E_k)].
%
%% An expression E is one of the following alternatives:
%
%%  If P is an atomic literal L, then Rep(P) = Rep(L).

%% mk_atomic_literal(P) ->
%%     atomic_litera(P).

%%  If E is P = E_0, then Rep(E) = {match,LINE,Rep(P),Rep(E_0)}.
'__mac_='([P,E]) ->
    {match,lineno(),transform(P),transform(E)}.

%%  If E is a variable V, then Rep(E) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V.
%%HY: It's silly to call variables variables when they don't vary... I call them bindings.

'__mac_var'([L,Name]) ->
    {var,L,Name}.

%%  If E is a tuple skeleton {E_1, ..., E_k}, then Rep(E) = {tuple,LINE,[Rep(E_1), ..., Rep(E_k)]}.
'__mac_tuple'([[block|Es]]) ->
    {tuple,lineno(),transform_each(Es)}.

%%  If E is [], then Rep(E) = {nil,LINE}.
'__mac_list'([]) ->
    {nil,lineno()};
'__mac_list'([[block|[]]]) ->
    {nil,lineno()};
'__mac_list'([[block,H|T]]) ->
    [cons,H,[list,[block|T]]].

%%  If E is a cons skeleton [E_h | E_t], then Rep(E) = {cons,LINE,Rep(E_h),Rep(E_t)}.
'__mac_cons'([Car,Cdr]) ->
    {cons,lineno(),transform(Car),transform(Cdr)}.

%%  If E is a binary constructor <<V_1:Size_1/TSL_1, ..., V_k:Size_k/TSL_k>>, then Rep(E) = {bin,LINE,[{bin_element,LINE,Rep(V_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,LINE,Rep(V_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see below. An omitted Size is represented by default. An omitted TSL (type specifier list) is represented by default.

%%TODO

%%  If E is E_1 Op E_2, where Op is a binary operator, then Rep(E) = {op,LINE,Op,Rep(E_1),Rep(E_2)}.
%% %% binary Ops,
%% / * rem band and
%% + - bor bxor bsl bsr or xor
%% ++ --
%% == /= =< < >= > =:= =/=
%% andalso
%% orelse
%% =
%% catch

'__mac_op'([Op,Arg1,Arg2]) ->
    {op,lineno(),Op,transform(Arg1),transform(Arg2)};

%%  If E is Op E_0, where Op is a unary operator, then Rep(E) = {op,LINE,Op,Rep(E_0)}.
%% %% unary Ops:
%% + - bnot not
'__mac_op'([Op,Arg1]) ->
    {op,lineno(),Op,transform(Arg1)}.

nest_binary(Head,Op,Exp) ->
    case Exp of
	[Arg1,Arg2] -> [op,Op,Arg1,Arg2];
	[[block,Arg1,Arg2]] -> [Head,Arg1,Arg2];
	[[block,Arg1|Args]] -> [Head,Arg1,[Head,[block|Args]]]
    end.

%% +
'__mac_+'(E) ->
    nest_binary('+','+',E).

%% -
'__mac_-'(E) ->
    nest_binary('-','-',E).

%% *
'__mac_*'(E) ->
    nest_binary('*','*',E).

%% /
'__mac_/'(E) ->
    nest_binary('/','/',E).

%% rem
'__mac_rem'([Arg1,Arg2]) ->
    [op,'rem',Arg1,Arg2].

%% and
'__mac_and'(E) ->
    nest_binary('and','and',E).

%% or
'__mac_or'(E) ->
    nest_binary('or','or',E).

%% ++
'__mac_++'(E) ->
    nest_binary('++','++',E).

%% --

%% TODO I don't know what this is used for. It removes the first occurence of a subsequence from a list.

%% == /= =< < >= > =:= =/=
'__mac_=='([E1,E2]) ->
    [op,'==',E1,E2].

'__mac_<'([E1,E2]) ->
    [op,'<',E1,E2].

'__mac_>'([E1,E2]) ->
    [op,'>',E1,E2].

'__mac_>='([E1,E2]) ->
    [op,'>=',E1,E2].

'__mac_=<'([E1,E2]) ->
    [op,'=<',E1,E2].

%% andalso
'__mac_andalso'(E) ->
    nest_binary('andalso','andalso',E).

%% orelse
'__mac_orelse'(E) ->
    nest_binary('orelse','andalso',E).

%% =
%% TODO isn't this the matching operator? Probably doesn't belong here.

%% Unary
%% + - bnot not

% not

'__mac_not'([E]) ->
    [op,'not',E].

%%  If E is #Name{Field_1=E_1, ..., Field_k=E_k}, then Rep(E) = {record,LINE,Name, [{record_field,LINE,Rep(Field_1),Rep(E_1)}, ..., {record_field,LINE,Rep(Field_k),Rep(E_k)}]}.

'__mac_rec'([Atom,[block|Fs]]) ->
    {atom,L,Name}=transform(Atom),
    {record,L,Name,
     lists:map(fun ([F,V]) -> record_field(F,V) end,Fs)};

%%  If E is E_0#Name{Field_1=E_1, ..., Field_k=E_k}, then Rep(E) = {record,LINE,Rep(E_0),Name, [{record_field,LINE,Rep(Field_1),Rep(E_1)}, ..., {record_field,LINE,Rep(Field_k),Rep(E_k)}]}.

'__mac_rec'([E,Atom,[block|Fs]]) ->
    {atom,L,Name}=transform(Atom),
    {record,L,transform(E),Name,
     lists:map(fun ([F,V]) -> record_field(F,V) end,Fs)}.

%%  If E is #Name.Field, then Rep(E) = {record_index,LINE,Name,Rep(Field)}.
'__mac_rec-index'([Atom,Field]) ->
    {atom,L,Name}=transform(Atom),
    {record_index,L,Name,transform(Field)}.

%%  If E is E_0#Name.Field, then Rep(E) = {record_field,LINE,Rep(E_0),Name,Rep(Field)}.

'__mac_rec-val'([E,Atom,Field]) ->
    {atom,_,Name}=transform(Atom),
    TE=transform(E),
    {record_field,lineno(),TE,Name,transform(Field)}.


%%  If E is catch E_0, then Rep(E) = {'catch',LINE,Rep(E_0)}.
'__mac_catch'([E]) ->
    {'catch',lineno(),transform(E)}.
    
%%  If E is E_m:E_0(E_1, ..., E_k), then Rep(E) = {call,LINE,{remote,LINE,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}. 
'__mac_call'([[Mod,Fn],[block|Body]]) ->
    {call,lineno(),{remote,transform(Mod),transform(Fn),transform_each(Body)}};

%%  If E is E_0(E_1, ..., E_k), then Rep(E) = {call,LINE,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}.
'__mac_call'([Fn|Args]) ->
    {call,lineno(),transform(Fn),transform_each(Args)}.


%%  If E is a list comprehension [E_0 || W_1, ..., W_k], where each W_i is a generator or a filter, then Rep(E) = {lc,LINE,Rep(E_0),[Rep(W_1), ..., Rep(W_k)]}. For Rep(W), see below.

%%  If E is a binary comprehension <<E_0 || W_1, ..., W_k>>, where each W_i is a generator or a filter, then Rep(E) = {bc,LINE,Rep(E_0),[Rep(W_1), ..., Rep(W_k)]}. For Rep(W), see below.

%%  If E is begin B end, where B is a body, then Rep(E) = {block,LINE,Rep(B)}.

'__mac_do'([[block|Body]]) ->
    {block,lineno(),transform_each(Body)}.

%%  If E is if Ic_1 ; ... ; Ic_k end, where each Ic_i is an if clause then Rep(E) = {'if',LINE,[Rep(Ic_1), ..., Rep(Ic_k)]}.


'__mac_if'([[block]]) ->
    error("No clasues in 'if'.");
'__mac_if'([[block|Cases]]) ->
    {'if',lineno(),build_if_clauses(Cases,[])}.

build_if_clauses([],Acc) -> lists:reverse(Acc);
build_if_clauses([Exp],Acc) ->
    build_if_clauses([],[clause_if([brace,[block,[atom,lineno(),true]]],[Exp])|Acc]);
build_if_clauses([Test,Exp|Cases],Acc) ->
    Body=[Exp], % Erlang's if-clause expects a list of expressions as body.
    C=case Test of
	  %% God awful without quasiquoting.
	  [brace|_] -> clause_if(Test,Body);
	  [block|_] -> clause_if([brace,Test],Body);
	  _ -> clause_if([brace,[block,Test]],Body)
    end,
    build_if_clauses(Cases,[C|Acc]).

%%  If E is case E_0 of Cc_1 ; ... ; Cc_k end, where E_0 is an expression and each Cc_i is a case clause then Rep(E) = {'case',LINE,Rep(E_0),[Rep(Cc_1), ..., Rep(Cc_k)]}. 
%%  If E is try B catch Tc_1 ; ... ; Tc_k end, where B is a body and each Tc_i is a catch clause then Rep(E) = {'try',LINE,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k)],[]}. 
%%  If E is try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n end, where B is a body, each Cc_i is a case clause and each Tc_j is a catch clause then Rep(E) = {'try',LINE,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],[]}. 
%%  If E is try B after A end, where B and A are bodies then Rep(E) = {'try',LINE,Rep(B),[],[],Rep(A)}. 
%%  If E is try B of Cc_1 ; ... ; Cc_k after A end, where B and A are a bodies and each Cc_i is a case clause then Rep(E) = {'try',LINE,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[],Rep(A)}. 
%%  If E is try B catch Tc_1 ; ... ; Tc_k after A end, where B and A are bodies and each Tc_i is a catch clause then Rep(E) = {'try',LINE,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k)],Rep(A)}. 
%%  If E is try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n after A end, where B and A are a bodies, each Cc_i is a case clause and each Tc_j is a catch clause then Rep(E) = {'try',LINE,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],Rep(A)}. 
%%  If E is receive Cc_1 ; ... ; Cc_k end, where each Cc_i is a case clause then Rep(E) = {'receive',LINE,[Rep(Cc_1), ..., Rep(Cc_k)]}. 
%%  If E is receive Cc_1 ; ... ; Cc_k after E_0 -> B_t end, where each Cc_i is a case clause, E_0 is an expression and B_t is a body, then Rep(E) = {'receive',LINE,[Rep(Cc_1), ..., Rep(Cc_k)],Rep(E_0),Rep(B_t)}. 
%%  If E is fun Name / Arity, then Rep(E) = {'fun',LINE,{function,Name,Arity}}. 
%%  If E is fun Module:Name/Arity, then Rep(E) = {'fun',LINE,{function,Module,Name,Arity}}. 
%%  If E is fun Fc_1 ; ... ; Fc_k end where each Fc_i is a function clause then Rep(E) = {'fun',LINE,{clauses,[Rep(Fc_1), ..., Rep(Fc_k)]}}. 
%%  If E is query [E_0 || W_1, ..., W_k] end, where each W_i is a generator or a filter, then Rep(E) = {'query',LINE,{lc,LINE,Rep(E_0),[Rep(W_1), ..., Rep(W_k)]}}. For Rep(W), see below. 
%%  If E is E_0.Field, a Mnesia record access inside a query, then Rep(E) = {record_field,LINE,Rep(E_0),Rep(Field)}. 
%%  If E is ( E_0 ), then Rep(E) = Rep(E_0), i.e., parenthesized expressions cannot be distinguished from their bodies.

%% pattern(A) ->
%%     A.

%% 4.5 Clauses
%% There are function clauses, if clauses, case clauses and catch clauses. 
%% A clause C is one of the following alternatives:

%%  If C is a function clause ( Ps ) -> B where Ps is a pattern sequence and B is a body, then Rep(C) = {clause,LINE,Rep(Ps),[],Rep(B)}.

clause_function(Patterns,Body) ->
    clause_function(Patterns,[],Body).

%%  If C is a function clause ( Ps ) when Gs -> B where Ps is a pattern sequence, Gs is a guard sequence and B is a body, then Rep(C) = {clause,LINE,Rep(Ps),Rep(Gs),Rep(B)}.

clause_function(Patterns,Guards,Body) ->
    Ps=map(fun pattern_sequence/1, Patterns),
    B=transform_each(Body),
    Gs=map(fun guard_sequence/1, Guards),
    {clause,lineno(),Ps,Gs,B}.

% (if a)
%%  If C is an if clause Gs -> B where Gs is a guard sequence and B is a body, then Rep(C) = {clause,LINE,[],Rep(Gs),Rep(B)}.

clause_if(GuardSequence,Body) ->
    {clause,lineno(),[],
     guard_sequence(GuardSequence),
     transform_each(Body)}.

%%  If C is a case clause P -> B where P is a pattern and B is a body, then Rep(C) = {clause,LINE,[Rep(P)],[],Rep(B)}.
clause_case(Pattern,Body) ->
    clause_case(Pattern,[],Body).
%%  If C is a case clause P when Gs -> B where P is a pattern, Gs is a guard sequence and B is a body, then Rep(C) = {clause,LINE,[Rep(P)],Rep(Gs),Rep(B)}.

clause_case(Pattern,Guards,Body) ->
    {clause,lineno(),[pattern(Pattern)],guard_sequence(Guards),transform_each(Body)}.

%%  If C is a catch clause P -> B where P is a pattern and B is a body, then Rep(C) = {clause,LINE,[Rep({throw,P,_})],[],Rep(B)}. 
%%  If C is a catch clause X : P -> B where X is an atomic literal or a variable pattern, P is a pattern and B is a body, then Rep(C) = {clause,LINE,[Rep({X,P,_})],[],Rep(B)}. 
%%  If C is a catch clause P when Gs -> B where P is a pattern, Gs is a guard sequence and B is a body, then Rep(C) = {clause,LINE,[Rep({throw,P,_})],Rep(Gs),Rep(B)}. 
%%  If C is a catch clause X : P when Gs -> B where X is an atomic literal or a variable pattern, P is a pattern, Gs is a guard sequence and B is a body, then Rep(C) = {clause,LINE,[Rep({X,P,_})],Rep(Gs),Rep(B)}.


%% %% 4.6 Guards

%% A guard sequence Gs is a sequence of guards G_1; ...; G_k, and Rep(Gs) = [Rep(G_1), ..., Rep(G_k)]. If the guard sequence is empty, Rep(Gs) = [].
%% HY: Hmmm. A guard sequence is true if at least one guard test is true. But this is not short-circuiting.
%%%% what's the point of having it? Just syntatic sugar?

guard_sequence([brace|Guards]) -> 
    map(fun guard/1,Guards).

%% A guard G is a nonempty sequence of guard tests Gt_1, ..., Gt_k, and Rep(G) = [Rep(Gt_1), ..., Rep(Gt_k)].

guard([block|GuardTests]) ->
    %% TODO should maybe check if the guard_test is valid. 
    transform_each(GuardTests).

%% I completely fail to undertand. All the constants except the atom 'true' are considered false. What's the point of allowing them in a guard sequence?
%% And why not just (and test1 test2).
	    

%% A guard test Gt is one of the following alternatives:

%%  If Gt is an atomic literal L, then Rep(Gt) = Rep(L).

%%  If Gt is a variable pattern V, then Rep(Gt) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V.

%%  If Gt is a tuple skeleton {Gt_1, ..., Gt_k}, then Rep(Gt) = {tuple,LINE,[Rep(Gt_1), ..., Rep(Gt_k)]}.

%%  If Gt is [], then Rep(Gt) = {nil,LINE}.

%%  If Gt is a cons skeleton [Gt_h | Gt_t], then Rep(Gt) = {cons,LINE,Rep(Gt_h),Rep(Gt_t)}.

%%  If Gt is a binary constructor <<Gt_1:Size_1/TSL_1, ..., Gt_k:Size_k/TSL_k>>, then Rep(Gt) = {bin,LINE,[{bin_element,LINE,Rep(Gt_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,LINE,Rep(Gt_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see above. An omitted Size is represented by default. An omitted TSL (type specifier list) is represented by default.

%%  If Gt is Gt_1 Op Gt_2, where Op is a binary operator, then Rep(Gt) = {op,LINE,Op,Rep(Gt_1),Rep(Gt_2)}.

%%  If Gt is Op Gt_0, where Op is a unary operator, then Rep(Gt) = {op,LINE,Op,Rep(Gt_0)}.

%%  If Gt is #Name{Field_1=Gt_1, ..., Field_k=Gt_k}, then Rep(E) = {record,LINE,Name, [{record_field,LINE,Rep(Field_1),Rep(Gt_1)}, ..., {record_field,LINE,Rep(Field_k),Rep(Gt_k)}]}.

%%  If Gt is #Name.Field, then Rep(Gt) = {record_index,LINE,Name,Rep(Field)}.

%%  If Gt is Gt_0#Name.Field, then Rep(Gt) = {record_field,LINE,Rep(Gt_0),Name,Rep(Field)}.

%%  If Gt is A(Gt_1, ..., Gt_k), where A is an atom, then Rep(Gt) = {call,LINE,Rep(A),[Rep(Gt_1), ..., Rep(Gt_k)]}.

%%  If Gt is A_m:A(Gt_1, ..., Gt_k), where A_m is the atom erlang and A is an atom or an operator, then Rep(Gt) = {call,LINE,{remote,LINE,Rep(A_m),Rep(A)},[Rep(Gt_1), ..., Rep(Gt_k)]}.

%%  If Gt is {A_m,A}(Gt_1, ..., Gt_k), where A_m is the atom erlang and A is an atom or an operator, then Rep(Gt) = {call,LINE,Rep({A_m,A}),[Rep(Gt_1), ..., Rep(Gt_k)]}.

%%  If Gt is ( Gt_0 ), then Rep(Gt) = Rep(Gt_0), i.e., parenthesized guard tests cannot be distinguished from their bodies.

%% Note that every guard test has the same source form as some expression, and is represented the same way as the corresponding expression.



%% The set of valid guard expressions (sometimes called guard tests) is a subset of the set of valid Erlang expressions. The reason for restricting the set of valid expressions is that evaluation of a guard expression must be guaranteed to be free of side effects. Valid guard expressions are:

%%  the atom true, 
%%  other constants (terms and bound variables), all regarded as false, 
%%  calls to the BIFs specified below, 
%%  term comparisons, 
%%  arithmetic expressions, 
%%  boolean expressions, and 
%%  short-circuit boolean expressions.

%% Type Test BIFs.
%% is_atom/1 
%%   is_binary/1 
%%   is_constant/1 
%%   is_float/1 
%%   is_function/1 
%%   is_function/2 
%%   is_integer/1 
%%   is_list/1 
%%   is_number/1 
%%   is_pid/1 
%%   is_port/1 
%%   is_reference/1 
%%   is_tuple/1 
%%   is_record/2 
%%   is_record/3

%% Other BIFs Allowed in Guard Expressions.
%% abs(Number) 
%%   element(N, Tuple) 
%%   float(Term) 
%%   hd(List) 
%%   length(List) 
%%   node() 
%%   node(Pid|Ref|Port) 
%%   round(Number) 
%%   self() 
%%   size(Tuple|Binary) 
%%   tl(List) 
%%   trunc(Number)


compile_test_() ->
[
?_assert(erl_parse_f("-module(a).") ==
	 compile("(module a)")),
%% {attribute,1,module,a}.
?_assert(erl_parse_f("-import(a,[a/1,b/2,c/3]).") ==
	 compile("(import a: (a 1) (b 2) (c 3))")),
%% {attribute,1,import,{a,[{a,1},{b,2},{c,3}]}}.     
?_assert(erl_parse_f("-record(foo,{a=1,b,c}).") ==
	 compile("(record foo: (a 1) b c)")),
%% {attribute,1,record,
%% 	   {foo,[{record_field,1,{atom,1,a},{integer,1,1}},
%% 		 {record_field,1,{atom,1,b}},
%% 		 {record_field,1,{atom,1,c}}]}}.
%%%% 4.2 Atomic Literals
?_assert(erl_parse_e("1.") ==
	 compile("1")),
%% {integer,1,1.53}.
?_assert(erl_parse_e("1.53.") ==
	 compile("1.53")),
%% {float,1,1.53}.
?_assert(erl_parse_e("\"abcd\".") ==
	 compile("\"abcd\"")),
%% {string,1,"abcd"}.
?_assert(erl_parse_e("atom.") ==
	 compile("atom")),
%% {atom,1,atom}.
%%%% 4.4 Expressions
?_assert(erl_parse_e("X=foo().") ==
	 compile("(= X (foo))")),
%% {match,1,{var,1,'X'},{call,1,{atom,1,foo},[]}}.
?_assert(compile("(foo .= X)") ==
	 compile("(= X (foo))")),
%% {match,1,{var,1,'X'},{call,1,{atom,1,foo},[]}}.
?_assert(erl_parse_e("X.") ==
	 compile("X")),
%% {var,1,'X'}.
?_assert(erl_parse_e("{1,2,3}.") ==
	 compile("(tuple: 1 2 3)")),
%% {tuple,1,[{integer,1,1},{integer,1,2},{integer,1,3}]}.
?_assert(compile("(tuple: 1 2 3)") ==
	 compile("{1 2 3}")),
%% {tuple,1,[{integer,1,1},{integer,1,2},{integer,1,3}]}.
?_assert(erl_parse_e("[].") == 
	 compile("[]")),
%% {nil,1}.
?_assert(compile("[]") ==
	 compile("(list)")),
%% {nil,1}.
?_assert(compile("(list)") ==
	 compile("(list:)")),
%% {nil,1}.
?_assert(erl_parse_e("[1,2,3].") ==
	 compile("[1 2 3]")),
%% {cons,1,
%%       {integer,1,1},
%%       {cons,1,{integer,1,2},{cons,1,{integer,1,3},{nil,1}}}}.
?_assert(compile("[1 2 3]") ==
	 compile("(list: 1 2 3)")),
%% {cons,1,
%%       {integer,1,1},
%%       {cons,1,{integer,1,2},{cons,1,{integer,1,3},{nil,1}}}}.
?_assert(erl_parse_e("[1|[]].") ==
	 compile("(cons 1 [])")),
%% {cons,1,{integer,1,1},{nil,1}}.
?_assert(erl_parse_e("3 + (4 + 5).") ==
	 compile("(+: 3 4 5)")),
%% {op,1,'+',
%%     {integer,1,3},
%%     {op,1,'+',{integer,1,4},{integer,1,5}}}.

%%%% Record
?_assert(erl_parse_e("#foo{a=1,b=2}.") ==
	 compile("(rec foo: (a 1) (b 2))")),
%% {record,1,foo,
%% 	[{record_field,1,{atom,1,a},{integer,1,1}},
%% 	 {record_field,1,{atom,1,b},{integer,1,2}}]}.
?_assert(erl_parse_e("A#foo{a=1,b=2}.") ==
	 compile("(rec A foo: (a 1) (b 2))")),
%% {record,1,
%% 	{var,1,'A'},
%% 	foo,
%% 	[{record_field,1,{atom,1,a},{integer,1,1}},
%% 	 {record_field,1,{atom,1,b},{integer,1,2}}]}.
?_assert(erl_parse_e("#foo.a.") ==
	 compile("(rec-index foo a)")),
%% {record_index,1,foo,{atom,1,a}}.
?_assert(erl_parse_e("A#foo.a.") ==
	 compile("(rec-val A foo a)")),
%% {record_field,1,{var,1,'A'},foo,{atom,1,a}}.



?_assert(erl_parse_e("begin a,b,c end.") ==
	 compile("(do: a b c)")),
%% {block,1,[{atom,1,a},{atom,1,b},{atom,1,c}]}.
     
?_assert(erl_parse_e("if Foo -> a(b); Bar -> c; true -> d end.") ==
	 compile("(if: Foo (a b) Bar c d )"))

%% {'if',1,
%%       [{clause,1,[],
%% 	       [[{var,1,'Foo'}]],
%% 	       [{call,1,{atom,1,a},[{atom,1,b}]}]},
%%        {clause,1,[],[[{var,1,'Bar'}]],[{atom,1,c}]},
%%        {clause,1,[],[[{atom,1,true}]],[{atom,1,d}]}]}.

     
].
