-module(serl_large).
-include("ast.hrl").
-include("serl.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(scompile,[error/1,
		  error/2,
		  curmod/0,
		  lineno/0,
		  transform/2,
		  transform_each/2
		 ]).
-import(lists,[map/2,member/2]).

%% -define(atomic_literals,[integer,float,string,atom]).
%% -define(patterns,[match,var,tuple,nil,cons,op,record,record_index]++?atomic_literals).
%% -define(guards,[var,tuple,nil,cons,bin,op,record,record_index,record_field,call]++?atomic_literals).

-define(erl_integer(L,I),{integer,L,I}).
-define(erl_float(L,F),{float,L,F}).
-define(erl_string(L,S),{string,L,S}).
-define(erl_atom(L,A),{atom,L,A}).
-define(erl_var(L,A),{var,L,Name}).

%% Serl Extension

'__rm_lit'([],Here) ->
    ?cast_string(Here).

%% '__mac_defm'([Name,Body]) ->
%%     {def,foo,Name,Body}.

%% Translate [e0 e1 ...] to a list if not handled by some other macro

'__mac_block'([Exps],Env) ->
    transform(?cast_paren([?cast_atom(list),?cast_block(Exps)]),Env).

%% Translate {e0 e1 ...} to a tuple if not handled by some other macro
'__mac_brace'([Exps],Env) ->
    transform(?cast_paren([?cast_atom(tuple),?cast_block(Exps)]),Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.1 Module declarations and forms
%
%% A module declaration consists of a sequence of forms that are either function declarations or attributes.
%

%%  If D is a module declaration consisting of the forms F_1, ..., F_k, then Rep(D) = [Rep(F_1), ..., Rep(F_k)].

%%  If F is an attribute -module(Mod), then Rep(F) = {attribute,LINE,module,Mod}.

%% '__mac_module'([A]) ->
%%     ?erl_atom(_,Name)=transform(A),
%%     {attribute,lineno(),module,Name}.

%% %%  If F is a function declaration Name Fc_1 ; ... ; Name Fc_k, where each Fc_i is a function clause with a pattern sequence of the same length Arity, then Rep(F) = {function,LINE,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}.

%% '__mac_def'([H,T]) ->
%%     ?erl_atom(_,Name)=transform(H),
%%     ?ast_block([[Params|_]|_]=Clauses)=T, 
%%     {function,lineno(),
%%      Name,
%%      length(Params),
%%      map(fun clause_function/1,Clauses)}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% 4.2 Atomic literals
%% %
%% %% There are five kinds of atomic literals, which are represented in the same way in patterns, expressions and guards:
%% %%%% HY: Hmmm... I count only four kinds...

%% %%  If L is an integer or character literal, then Rep(L) = {integer,LINE,L}.

'__mac_integer'([I],Env) ->
    {Env,?erl_integer(lineno(),I)}.

%% %%  If L is a float literal, then Rep(L) = {float,LINE,L}. 
%% '__mac_float'([L,F]) ->
%%     ?erl_float(L,F).

%%  If L is a string literal consisting of the characters C_1, ..., C_k, then Rep(L) = {string,LINE,[C_1, ..., C_k]}.
'__mac_string'([S],Env) ->
    {Env,?erl_string(lineno(),S)}.

%%  If L is an atom literal, then Rep(L) = {atom,LINE,L}. 
'__mac_atom'([A],Env) ->
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

%% patterns(Patterns) ->
%%     map(fun pattern/1,Patterns).

%% pattern(P) ->
%%     %% TODO should check if pattern is valid. 
%%     transform(P).
	

%% %%  If P is a variable pattern V, then Rep(P) = {var,LINE,A}, where A is an atom with a printname consisting of the same characters as V.

%% %%  If P is a universal pattern _, then Rep(P) = {var,LINE,'_'}.

%% %%  If E is a tuple skeleton {E_1, ..., E_k}, then Rep(E) = {tuple,LINE,[Rep(E_1), ..., Rep(E_k)]}.

'__mac_tuple'([?ast_block(Es)],Env) ->
    %% doesn't respect "eval in some order". See transform_each in scompile.erl
    {Env2,Rs}=transform_each(Es,Env),
    {Env2,{tuple,lineno(),Rs}}.

%% %%  If E is [], then Rep(E) = {nil,LINE}.

'__mac_list'([],Env) ->
    {Env,{nil,lineno()}};
'__mac_list'([?ast_block([])],Env) ->
    {Env,{nil,lineno()}};
'__mac_list'([?ast_block([H|T])],Env) ->
    transform(?cast_paren([?cast_atom(cons),H,
			   ?cast_paren([?cast_atom(list),?cast_block(T)])]),
	     Env).

%% %%  If E is a cons skeleton [E_h | E_t], then Rep(E) = {cons,LINE,Rep(E_h),Rep(E_t)}.

'__mac_cons'([Car,Cdr],Env) ->
    {Env1,A}=transform(Car,Env),
    {Env2,Z}=transform(Cdr,Env1),
    {Env2,{cons,lineno(),A,Z}}.


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
