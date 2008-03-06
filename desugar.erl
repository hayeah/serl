%% It is best if I can use transform to keep track of the line count, using a process store.



-module(desugar).
-include("ast.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([renest/1 
	 ]).

-define(stored_exp_key,'__stored_exp').

error(Message) ->
    error(Message,[]).
error(Message,Args) ->
    io:format(Message,Args),
    throw({desugar_error,Message}).
    
%% implementation for "~" is somewhat kludgey. Eleganify later, or never.
renest(Ast) ->
    put(?stored_exp_key,undefined),
    Ast2=normalize(Ast),
    %% check stored value had been inserted.
    case get(?stored_exp_key) of
	undefined -> ok;
	_ -> error("No corresponding '~'")
    end, 
    Ast2. 


%% normalize({block,Lst}) ->
%%     {block,lists:reverse(normalize(Lst,[]))};
normalize(Lst) when is_list(Lst) ->
    lists:reverse(normalize(Lst,[])); % reverse Acc
normalize(O) -> O. %identity for everything else.
normalize([],Acc) -> Acc; % this is the internal base case, more convenient not to reverse.
normalize([H|T]=Lst,Acc) ->
    case H of
	?ast_satom($1) -> normalize_splice(normal,T,Acc,[]);
	?ast_satom(C) -> normalize_op(C,tl(Lst),Acc,[]);
	_ when is_list(H) -> NH=normalize(H),normalize(T,[NH|Acc]);
	_ -> normalize(T,[H|Acc])
    end.

%% (:) => ([])
%% (a : b c : d) => (a [b c] [d])
%% (a : b (:c d) : e) => (a [b [c d]] [e])
normalize_op($:,[],PrefixAcc,Acc) -> normalize([],[?ast_block(lists:reverse(Acc))|PrefixAcc]);
normalize_op($:,[H|T],PrefixAcc,Acc) ->
    case H of
	%%$: -> normalize(Lst,[lists:reverse(Acc)|PrefixAcc]);
	?ast_satom($~) -> normalize_splice($:,T,PrefixAcc,Acc);
	?ast_satom(C) -> normalize_op(C,T,[?ast_block(lists:reverse(Acc))|PrefixAcc],[]);
	_ when is_list(H) -> NH=normalize(H),normalize_op($:,T,PrefixAcc,[NH|Acc]);
	_  -> normalize_op($:,T,PrefixAcc,[H|Acc])
    end;

%% (a b . c) => (c (a b))
%% (a b . c d) => (c d (a b))
normalize_op($.,Lst,PrefixAcc,_Acc) ->
    Wrap=normalize(Lst,[]), % note that the result Acc is not yet reversed.
    [lists:reverse(PrefixAcc)|Wrap].


%% (a ~ b ~) => (b (a))
%% (a b ~ c ~)) => (c (a b))
%% (~) => error
%% (~~) => (())
%% (a ~ (b ~)) => ((b (a)))
%% (a ~ b (c ~)) => (b (c (a)))
%% (a ~ b ~ c) => (b (a) c)
%% (a ~ b ~ c ~) => error
%% (a ~ b : c ~ d) => (b [c (a) d])
%% (a ~ b : ~ . d) => (d (b [(a)]))
normalize_splice(Mode,Lst,PrefixAcc,Acc) ->
    %io:format("~nSplicing for ~p",[Mode]),
    case get(?stored_exp_key) of
	%% kludge... if a stored expression is found, collect it, then proceed.
	{?stored_exp_key,Exp} ->
	    put(?stored_exp_key,undefined),
	    case Mode of
		$: -> normalize_op($:,Lst,PrefixAcc,[Exp|Acc]); 
		normal -> normalize(Lst,[Exp|PrefixAcc])
	    end;
	%% kludge... wipes out whatever already stored.
	_ ->
	    case Mode of
		$: -> Exp=lists:reverse([[block|lists:reverse(Acc)]|PrefixAcc]);
		normal -> Exp=lists:reverse(PrefixAcc)
	    end,
	    put(?stored_exp_key,{?stored_exp_key,Exp}),
	    normalize(Lst,[])
    end.
	


%% desugar_test_() ->
%%     [
%% ?_assert(desugar(p("(:)")) ==
%%      [{block,[]}]), 


%% %desugar(p("(: a b : c d)")).
%% ?_assert(desugar(p("(: a b : c d)")) ==
%%      [{block,[{atom,"a"},{atom,"b"}]},
%%       {block,[{atom,"c"},{atom,"d"}]}]), 


%% %desugar(p("(: a b : : c d)")).
%% ?_assert(desugar(p("(: a b : : c d)")) ==
%%      [{block,[{atom,"a"},{atom,"b"}]},
%%       {block,[]},
%%       {block,[{atom,"c"},{atom,"d"}]}]), 


%% %desugar(p("(a b (c d (e : f)))")).
%% ?_assert(desugar(p("(a b (c d (e : f)))")) ==
%%      [{atom,"a"},
%%       {atom,"b"},
%%       [{atom,"c"},{atom,"d"},[{atom,"e"},{block,[{atom,"f"}]}]]]), 


%% %desugar(p("(: a (b: c d): e f (:g h))")).
%% ?_assert(desugar(p("(: a (b: c d): e f (:g h))")) ==
%%      [{block,[{atom,"a"},
%% 	      [{atom,"b"},{block,[{atom,"c"},{atom,"d"}]}]]},
%%       {block,[{atom,"e"},
%% 	      {atom,"f"},
%% 	      [{block,[{atom,"g"},{atom,"h"}]}]]}]), 


%% %desugar(p("(a b . c d)")).
%% ?_assert(desugar(p("(a b . c d)")) ==
%% 	  [{atom,"c"},{atom,"d"},[{atom,"a"},{atom,"b"}]]), 


%% %desugar(p("(. c d)")).
%% ?_assert(desugar(p("(. c d)")) ==
%% 	  [{atom,"c"},{atom,"d"},[]]), 


%% %desugar(p("(.)")).
%% ?_assert(desugar(p("(.)")) ==
%% 	  [[]]), 


%% %desugar(p("(a : c d . e)")).
%% ?_assert(desugar(p("(a : c d . e)")) ==
%% 	  [{atom,"e"},[{atom,"a"},{block,[{atom,"c"},{atom,"d"}]}]]), 


%% %desugar(p("(a : c d : e .f)")).
%% ?_assert(desugar(p("(a : c d : e .f)")) ==
%%      [{atom,"f"},
%%       [{atom,"a"},
%%        {block,[{atom,"c"},{atom,"d"}]},
%%        {block,[{atom,"e"}]}]]), 


%% %desugar(p("(a : c d : e .f :g)")).
%% ?_assert(desugar(p("(a : c d : e .f :g)")) ==
%%      [{atom,"f"},
%%       {block,[{atom,"g"}]},
%%       [{atom,"a"},
%%        {block,[{atom,"c"},{atom,"d"}]},
%%        {block,[{atom,"e"}]}]]),


%% %%desugar(p("(~~)")).
%% ?_assert(desugar(p("(~~)")) ==
%%      [[]]),

%% %%desugar(p("(a ~ b ~)")).
%% ?_assert(desugar(p("(a ~ b ~)")) ==
%%      [{atom,"b"},[{atom,"a"}]]),

%% %%desugar(p("(a ~ b (c ~))")).
%% ?_assert(desugar(p("(a ~ b (c ~))")) ==
%%      [{atom,"b"},[{atom,"c"},[{atom,"a"}]]]),

%% %%desugar(p("(:a b ~ c ~)")).
%% ?_assert(desugar(p("(:a b ~ c ~)")) ==
%%      [{atom,"c"},[{block,[{atom,"a"},{atom,"b"}]}]]),

%% %%desugar(p("(a b ~ c : d ~)")).
%% ?_assert(desugar(p("(a b ~ c : d ~)")) ==
%%      [{atom,"c"},{block,[{atom,"d"},[{atom,"a"},{atom,"b"}]]}]),

%% %%desugar(p("(:a b ~ c : d ~)")).
%% ?_assert(desugar(p("(:a b ~ c : d ~)")) ==
%%      [{atom,"c"},
%%       {block,[{atom,"d"},[{block,[{atom,"a"},{atom,"b"}]}]]}]),

%% %%desugar(p("(a b ~ c : d ~ : e)")).
%% ?_assert(desugar(p("(a b ~ c : d ~ : e)")) ==
%%      [{atom,"c"},
%%       {block,[{atom,"d"},[{atom,"a"},{atom,"b"}]]},
%%       {block,[{atom,"e"}]}]),

%% %%desugar(p("(a b ~ c . d ~)")).
%% ?_assert(desugar(p("(a b ~ c . d ~)")) ==
%%      [{atom,"d"},[{atom,"a"},{atom,"b"}],[{atom,"c"}]]),


%% %% (~.~) => reg=[],(. ~) => (() ~) => (() ())
%% %%desugar(p("(~.~)")).
%% ?_assert(desugar(p("(~.~)")) ==
%%      [[],[]])


%%     ].

