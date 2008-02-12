% check out John Hugue's talk on Erlang QuickCheck
% a shame that it is commercial. Well, the guy needs to make money. Too bad.
%% http://video.google.com/videoplay?docid=4655369445141008672
%% http://www.quviq.com/index.html

-module(serl).
-export([normalize/1]).

error(Message) ->
    throw({compile_error,Message}).

%% compile(In) ->
%%     Ast=parse:p(In),
%%     transform(Ast).

%% transform(Lst) when is_list(Lst) ->
%%     Lst2=normalize(Lst),
%%     if Lst2==Lst -> macroexpand(Lst); %% not necessary to check for structural equivalence. Just check length.
%%        true -> transform(Lst) 
%%     end.


%(a . b c) => (b c (a))

is_special_op({special_atom,C}) -> C;
is_special_op(_) -> false.

normalize(Lst) when is_list(Lst) -> normalize(Lst,[]).
normalize([],Acc) ->
    %transform(lists:reverse(Acc)).
    lists:reverse(Acc);
normalize(Lst,Acc) ->
    [H|T]=Lst,
    case H of
	{special_atom,C} -> normalize_op(C,tl(Lst),Acc,[]);
	_ -> normalize(T,[H|Acc])
    end.


%(: a b) => ((a b))
%(: a b : c d) => ((a b) : c d) => ((a b) (c d))

normalize_op($:,[],PrefixAcc,Acc) -> normalize([],[lists:reverse(Acc)|PrefixAcc]);
normalize_op($:,Lst,PrefixAcc,Acc) ->
    [H|T]=Lst,
    Op = is_special_op(H),
    case Op of
	$: -> normalize(Lst,[lists:reverse(Acc)|PrefixAcc]);
	_ -> normalize_op($:,T,PrefixAcc,[H|Acc])
    end.
	    
	
    

%% transform({Type,Ast}) -> 
%%     dispatch(Type,Ast).


