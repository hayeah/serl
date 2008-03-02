-module(streamer).
-include_lib("eunit/include/eunit.hrl").

% port of Oleg Kiselyov's stream parsing utilities:
% http://okmij.org/ftp/Scheme/parsing.html
-export([set_port/1,
	 get_port/0,
	 lineno/0,
	 lineno/1,
	 
	 residue/0, 
	 peek/0,
	 read/0,
	 char/1,
	 char_if/1,
	 skip_until/1,
	 skip_while/1,
	 token/2,
	 token_of/1,
	 read_until/1,
	 string/1

	]).
-import(lists,[member/2]).


-define(line_count,'__stream_line_count').

error(Message) ->
    io:fwrite("Stream error, remaining:\n~p~n~n",[residue()]),
    throw({stream_error,Message}).
    
lineno() -> get(?line_count).

lineno(L) -> put(?line_count,L).

peek() ->
    peek_port(get_port()).
read() ->
    C=read_port(get_port()),
    if C==$\n -> put(?line_count,lineno()+1);
       true -> nil
    end,
    C.

residue() ->
    residue_port(get_port()).
    
residue_port(In) when is_list(In) ->
    set_port([]),In.

peek_port([]) -> eof;
peek_port([I|_In]) -> I.

read_port([]) -> eof;    
read_port([I|In]) ->
    set_port(In),I.

get_port() ->
    get(input).

set_port(In) ->
    put(input,In).

char(Chs) ->
    char(Chs,"").
char(Chs,_Comment) ->
    C=peek(),
    Mp=lists:member(C,Chs),
    if Mp -> read(), C;
       true -> error("Expecting one of "++Chs)
    end.

char_if(Fun) ->
    C=peek(),
    T=Fun(C),
    if T -> read() end.

skip_until(Chs) when is_list(Chs) ->
    C=read(),
    Mp=lists:member(C,Chs),
    if Mp -> C;
       C==eof -> error("Unexpected EOF while skipping: "++Chs);
       true -> skip_until(Chs)
    end;
skip_until(0) -> 
    false;
skip_until(N) when is_number(N) ->
    C=read(),
    if C==eof -> error("Unexpected EOF while skipping characters");
       true -> skip_until(N-1) 
    end.

skip_while(Ch) when is_list(Ch) ->
    C=peek(),
    Mp=lists:member(C,Ch),
    if Mp -> read(),skip_while(Ch); 
       true -> C
    end.

token(PrefixChs,BreakChs) -> 
    token(PrefixChs,BreakChs,"").
token(PrefixChs,BreakChs,_Comment) -> 
    skip_while(PrefixChs), 
    token_helper(BreakChs,_Comment,[]).

token_helper(BreakChs,_Comment,Acc) ->
    C=peek(),
    Mp=lists:member(C,BreakChs),
    if Mp -> lists:reverse(Acc);
       C==eof -> error("EOF while reading a token");
       true -> C2=read(),token_helper(BreakChs,_Comment,[C2|Acc]) 
    end.

token_of(Pred) when is_function(Pred) ->
    token_of(Pred,[]);
token_of(Chs) when is_list(Chs) ->
    token_of(fun (C) -> lists:member(C,Chs) end,[]).
token_of(Pred,Acc) ->
    C=peek(),
    T=Pred(C),
    if C==eof -> lists:reverse(Acc);
       T -> read(),token_of(Pred,[C|Acc]);
       true -> lists:reverse(Acc)
    end.

read_until(Chs) ->
    token_of(fun (C) -> not member(C,Chs) end).
    
string(Str) -> 
    StrT=list_to_tuple(Str),
    if size(StrT) == 0 -> error("pattern string() must be non-empty.");
       true -> string(StrT,1,0) 
    end.
string(StrT,1,N) when is_tuple(StrT) ->
    C=read(),
    if C==eof -> false;
       C==element(1,StrT) -> string(StrT,2,N+1);
       true -> string(StrT,1,N+1) 
    end;
string(StrT,CurPos,N) ->
    C=peek(),
    io:format("\tchecking ~w is ~w of: ~p\n",[C,CurPos,StrT]),
    if CurPos > size(StrT) -> N; 
       C==eof -> false;
       C==element(CurPos,StrT) -> read(), string(StrT,CurPos+1,N+1);
       true -> backtrack(StrT,2,CurPos,N)
    end.
       
%% extremely clumsy. I hate it.
backtrack(StrT,I,M,N) ->
    J=M-I,
    if J =< 0 -> string(StrT,1,N); 
       true -> T=check_prefix(StrT,I,J,1,N), 
	       if T == true -> string(StrT,J+1,N);
		  true ->backtrack(StrT,I+1,M,N)
	       end
    end.

check_prefix(StrT,I,J,K,N) ->
    if K >= J -> true;
       element(I,StrT)==element(I+K,StrT) -> check_prefix(StrT,I,J,K+1,N);
       true -> false
    end.
    





%% %% Tests
%% do_test_error(RaiseP,Test,In) ->
%%     set_port(In),
%%     if RaiseP ->
%% 	    try ?MODULE:Test(),ok of
%% 		_ -> {no,"Expecting Parse Error"}
%% 	    catch {parse_error,Msg} -> {ok,"Raised: "++Msg}
%% 	    end;
%%        RaiseP==false -> 
%% 	    try {ok,?MODULE:Test()}
%% 	    catch {parse_error,Msg} -> {no,Msg}
%% 	    end
%%     end.
	     
		      
%% do_tests(TestSpecs) ->
%%     Rs=lists:map(fun (TestSpec) -> 
%% 			 io:format("Testing: ~p~n",[TestSpec]),
%% 			 R=case TestSpec of 
%% 			       {Test,In} -> do_test_error(false,Test,In);
%% 			       {Test,In,error} -> do_test_error(true,Test,In);
%% 			       {Test,In,Ans} -> 
%% 				   case do_test_error(false,Test,In) of 
%% 				       {ok,R2} when R2==Ans -> {ok,R2};
%% 				       {ok,R2} -> {no,lists:flatten(io_lib:format("Expected ~w /= ~w",[R2,Ans]))};
%% 				       Fail -> Fail
%% 				   end
%% 			   end,
%% 			 {R,TestSpec,residue()}
%% 		 end,
%% 		 TestSpecs),
%%     Rs.

%% test_char() ->
%%     char("abc").

%% test_skip_until_5() ->
%%     skip_until(5).

%% test_skip_until_chs() ->
%%     skip_until(";,").

%% test_skip_until_eof() ->
%%     skip_until([eof,","]).

%% test_skip_while() ->
%%     R1=skip_while("abc"),R2=char("d"),R3=char("abc"++[eof]),[R1,R2,R3].

%% test_token() ->
%%     T=token("abc",";"),
%%     C=read(),
%%     T++[C].

%% test_token1() ->
%%     T1=token("abc",","),C1=read(),T2=token("def",";"++[eof]),C2=read(),
%%     T1++[C1|T2]++[C2].

%% test_token_of() ->
%%     S=token_of("abcde"),C1=read(),S++[C1].

%% test_string() ->    
%%     string("aabbaacc").

%% test(Fun,In) ->
%%     set_port(In),
%%     Fun().

stream_test_() ->
[
?_assert(begin set_port("ab"), (char("ab")==$a) and (read() == $b) end),
?_assert(begin set_port("ba"), (char("ab")==$b) and (read() == $a) end),
?_assert(1==1)


].

%% tests() ->
%%     do_tests([{test_char,"abcd"},
%% 	      {test_char,"abcd",$a} ,
%% 	      {test_char,"dce",error},
%% 	      {test_skip_until_5,"abc",error},
%% 	      {test_skip_until_5,"abcde",false},
%% 	      {test_skip_until_chs,"abcde;",$;},
%% 	      {test_skip_until_chs,"abcde,",$,},
%% 	      {test_skip_until_chs,"abcde",error},
%% 	      {test_skip_until_eof,"abcdef",eof},
%% 	      {test_skip_until_chs,"abcde,",$,},
%% 	      {test_skip_while,"abcabcda","dda"},
%% 	      {test_skip_while,"bd","dd"++[eof]},
%% 	      {test_skip_while,"b",error},
%% 	      {test_skip_while,"",error},
%% 	      {test_token,"abcdddeee; ","dddeee;"},
%% 	      {test_token,"abcdddeee",error},
%% 	      {test_token1,"abccbadddd,defabbc;","dddd,abbc;"},
%% 	      {test_token1,"abccbadddd,defabbc","dddd,abbc"++[eof]},
%% 	      {test_token_of,"abcdefg","abcdef"},
%% 	      {test_token_of,"fff","f"},
%% 	      {test_token_of,"",[eof]},
%% 	      {test_string,"aabbaacc",8},
%% 	      {test_string,"zzzzaabbaaccf",12},
%% 	      {test_string,"zzzzaabbaabbaacc",16}

%% %% 	     ]).
    
