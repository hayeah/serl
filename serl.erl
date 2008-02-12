% check out John Hugue's talk on Erlang QuickCheck
% a shame that it is commercial. Well, the guy needs to make money. Too bad.
%% http://video.google.com/videoplay?docid=4655369445141008672
%% http://www.quviq.com/index.html

-module(serl).
-export([normalize/1]).
-include_lib("eunit/include/eunit.hrl").

error(Message) ->
    throw({compile_error,Message}).

%% compile(In) ->
%%     Ast=parse:p(In),
%%     transform(Ast).

%% transform(Lst) when is_list(Lst) ->
%%     Lst2=normalize(Lst),
%%     if Lst2==Lst -> macroexpand(Lst);
%%        true -> transform(Lst) 
%%     end.


%(a . b c) => (b c (a))

is_special_op({special_atom,C}) -> C;
is_special_op(_) -> false.

%% ohhhh fuck. Ignore ineffiencies. This is version 1!
normalize(Lst) when is_list(Lst) ->
    lists:reverse(normalize(Lst,[])). % reverse Acc
normalize([],Acc) -> Acc; % this is the internal base case, more convenient not to reverse.
normalize(Lst,Acc) ->
    [H|T]=Lst,
    case H of
	{special_atom,C} -> normalize_op(C,tl(Lst),Acc,[]);
	_ when is_list(H) -> NH=normalize(H),normalize(T,[NH|Acc]);
	_ -> normalize(T,[H|Acc])
    end.

%% (:) => ()
%% (a : b c : d) => (a (b c) (d))
%% (a : b (:c d) : e) => (a (b (c d)) (e))
normalize_op($:,[],PrefixAcc,Acc) -> normalize([],[lists:reverse(Acc)|PrefixAcc]);
normalize_op($:,Lst,PrefixAcc,Acc) ->
    [H|T]=Lst,
    Op = is_special_op(H),
    case Op of
	%%$: -> normalize(Lst,[lists:reverse(Acc)|PrefixAcc]);
	_ when is_number(Op) -> normalize_op(Op,T,[lists:reverse(Acc)|PrefixAcc],[]);
	_ when is_list(H) -> NH=normalize(H),normalize_op($:,T,PrefixAcc,[NH|Acc]);
	_  -> normalize_op($:,T,PrefixAcc,[H|Acc])
    end;

%% (a b . c) => (c (a b))
%% (a b . c d) => (c d (a b))
normalize_op($.,Lst,PrefixAcc,_Acc) ->
    Wrap=normalize(Lst,[]), % note that the result Acc is not yet reversed.
    [lists:reverse(PrefixAcc)|Wrap].

	    
p(In) -> parse:p(In).

normalize_test_() ->
    [?_assert(normalize(p("(:)")) == [[]]),
     ?_assert(normalize(p("(: a b : c d)")) == [[{atom,"a"},{atom,"b"}],[{atom,"c"},{atom,"d"}]]),
     ?_assert(normalize(p("(: a b : : c d)")) == [[{atom,"a"},{atom,"b"}],[],[{atom,"c"},{atom,"d"}]]),
     ?_assert(normalize(p("(a b (c d (e : f)))")) == 
			[{atom,"a"}, {atom,"b"},
			 [{atom,"c"},{atom,"d"},
			  [{atom,"e"},
			   [{atom,"f"}]]]]),
     ?_assert(normalize(p("(: a (b: c d): e f (:g h))")) ==
	      [[{atom,"a"},[{atom,"b"},[{atom,"c"},{atom,"d"}]]],
	       [{atom,"e"},{atom,"f"},[[{atom,"g"},{atom,"h"}]]]]),
     ?_assert(normalize(p("(a b . c d)")) == [{atom,"c"},{atom,"d"},[{atom,"a"},{atom,"b"}]]),
     ?_assert(normalize(p("(. c d)")) ==
	      [{atom,"c"},{atom,"d"},[]]),
     ?_assert(normalize(p("(.)")) ==
	      [[]]),
     ?_assert(normalize(p("(a : c d . e)")) ==
	      [{atom,"e"},[{atom,"a"},[{atom,"c"},{atom,"d"}]]]),
     ?_assert(normalize(p("(a : c d : e .f)")) ==
	      [{atom,"f"}, [{atom,"a"},[{atom,"c"},{atom,"d"}],[{atom,"e"}]]]),
     ?_assert(normalize(p("(a : c d : e .f :g)")) ==
	      [{atom,"f"}, [{atom,"g"}], [{atom,"a"},[{atom,"c"},{atom,"d"}],[{atom,"e"}]]])
    ].




%% transform({Type,Ast}) -> 
%%     dispatch(Type,Ast).

