%% as version one, just read from a list.
%% as version one, don't even bother to count the lines.

-module(read).
-export([read/1
	 
%% 	 tests/0,
%% 	 test_char/0,
%% 	 test_skip_until_chs/0,
%% 	 test_skip_until_5/0,
%% 	 test_skip_until_eof/0,
%% 	 test_skip_while/0,
%% 	 test_token/0,
%% 	 test_token1/0,
%% 	 test_token_of/0,
%% 	 test_string/0,
%% 	 test/2, 
	]).

-include("ast.hrl").
-define(spacen,"\t\s\n").
-define(space,"\t\s").
-define(special_atom_chars,".~:;,'`").
-define(reserved_chars,"!@#$?").
-define(delimiters,"#(){}[]\""++?special_atom_chars++?spacen).

%% bdefnrstv#\"
-define(string_escapes,
	[{$b,$\b},{$d,$\d},{$e,$\e},
	 {$f,$\f},{$n,$\n},{$r,$\r},
	 {$s,$\s},{$t,$\t},{$v,$\v},
	 {$#,$\#},{$\\,$\\},{$\",$\"}]).

-define(line_count,'__serl_parse_line_count').

read(In) ->
    put(?line_count,1),
    set_port(In),
    R=exp(),
    case peek() of
	eof -> R;
	_ -> error("Parse has leftover text:")
    end. 

% port of Oleg Kiselyov's stream parsing utilities:
% http://okmij.org/ftp/Scheme/parsing.html

error(Message) ->
    io:fwrite("\t with remaining input:\n~p~n~n",[residue()]),
    throw({read_error,Message}).

lineno() -> get(?line_count).

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
    

%% serl lexer/parser

is_digit(C) -> ($0 =< C) and (C =< $9).
is_upper(C) -> ($A =< C) and (C =< $Z).
is_alpha(C) -> (($a =< C) and (C =< $z)) or is_upper(C).
is_atom_char(C) -> not (lists:member(C,?delimiters)).
is_atom_first_char(C) -> not (is_digit(C) or is_delimiter(C)).
is_special_atom_char(C) -> lists:member(C,?special_atom_chars). 
is_delimiter(C) -> lists:member(C,?delimiters).

%% digit() -> char_if(is_digit). 
%% alpha() -> char_if(is_alpha).

a_symbol() ->
    a_symbol("").
a_symbol(Prefix) ->
    Token=token_of(fun is_atom_char/1),
    Name=Prefix++Token,
    case Name of
	[] -> error("Expecting a symbol name.");
	[H|_] -> VarP=is_upper(H),
		 if VarP -> ?ast_var(lineno(),list_to_atom(Name));
		    true -> ?ast_atom(lineno(),list_to_atom(Name))
		 end
    end.


a_number(S) -> 
    if S>0 -> Sign=""; 
       S<0 -> Sign="-" 
    end,
    Int=token_of(fun (C) -> is_digit(C) end), 
    P1=peek(), 
    %% 46 == $.
    R=if P1==46 -> read(),P2=peek(), %reading float requires peek-2
		 IsD = is_digit(P2), 
		 if IsD ->
			 Float = token([],?delimiters++[eof]),
			 %% read an erlang-style floating point
			 %% P1 is the integral part, P2 is the decimal part, conforming to erlang syntax.
			 In=Sign++Int++[$.|Float],
			 case io_lib:fread("~f",In) of
			     {ok,[F],_} -> ?ast_float(lineno(),F);
			     _ -> error("Error parsing floating point.")
			 end;
		    true -> error("Error parsing floating point")
		 end;
       true -> case io_lib:fread("~d",Sign++Int) of
		   {ok,[I],_} -> ?ast_integer(lineno(),I); 
		   _ -> error("Error parsing integer: ")
	       end
    end,
    End=is_delimiter(C=peek()),
    if End;C==eof -> R; 
       true -> error("Error parsing number.")
    end.

a_list([OpenParen,CloseParen]) -> char([OpenParen]),a_list_rec(CloseParen,[]).

a_list_rec(CloseParen,Acc) ->
    C=peek(),
    % 41 == $)
    if C==CloseParen -> read(), lists:reverse(Acc); 
       C==eof -> error("Unexpected eof while reading list.");
       true -> a_list_rec(CloseParen,[exp()|Acc])
    end.

a_string() ->
    read(),
    a_string([],[]).

%% should do the interpolation optimization as macro. Just don't bother about it now.
a_string(Acc,Segs) ->
    C=read(),
    case C of
	$\\ -> a_string([string_escape()|Acc],Segs);
	$# -> error("String interpolation not supported");

	%% $# -> Seg=string_interpolate(),
%% 	      NewSegs=string_cat_seg(Acc,Segs),
%% 	      NewSegs2=string_cat_seg(Seg,NewSegs),
%% 	      a_string([],NewSegs2);
	%% $" = 34
	%% 34 -> NewSegs=string_cat_seg(Acc,Segs),
%% 	      case NewSegs of
%% 		  [R] -> R;
%% 		  _ -> {i_string,lists:reverse(NewSegs)}
%% 	      end;
	34 -> ?ast_string(lineno(),lists:reverse(Acc));
	_ -> a_string([C|Acc],Segs)
    end.

%% string_cat_seg(Acc,Segs) when is_list(Acc) ->
%%     if Acc == [] -> Segs;
%%        true -> string_cat_seg([?serl_string,lists:reverse(Acc)],Segs)
%%     end;
%% string_cat_seg(Seg,Segs) ->
%%     %io:format("cat seg: ~p to ~n\t~p~n",[Seg,Segs]),
%%     case {Seg,Segs} of
%% 	{_,[]} -> [Seg];
%% 	{{?serl_string,Str2},[{?serl_string,Str1}|RSegs]} -> [{?serl_string,Str1++Str2}|RSegs]; %%collapse literal strings 
%% 	%% whatever. Collapsing not done entirely correctly for nested interpolated strings. 
%% 	% {{i_string,IStr},RSeqs} -> IStr++RSeqs;
%% 	_ -> [Seg|Segs]
%%     end. 
    

%%TODO
%%no octal support yet.
%%no support for control chars
string_escape() ->
    C=read(),
    R=lists:keysearch(C,1,?string_escapes),
    case R of
	{value,{_,EC}} -> EC; 
	_ -> error(io_lib:format("Escape char not supported: ~p",[R]))
    end.
string_interpolate() ->
    char("{"), E=exp(), char("}"),
    E.
    
%% $( == 40,  but it messes up indentation
%% $) == 41
exp_dispatch(40) ->
    a_list("()"); %% ()
exp_dispatch(123) ->
    ?ast_brace(a_list("{}")); %% {}
exp_dispatch(91) ->
    ?ast_block(a_list("[]")); %% []
%% $" == 34, ditto
exp_dispatch(34) ->
    a_string();
exp_dispatch(C) ->
    SpecialAtom=is_special_atom_char(C),
    Digit=is_digit(C) or (C==$-),
    Symbol=is_atom_first_char(C),
    if SpecialAtom -> read(),?ast_satom(C);
       %% If the first peeked char is $-, there is an overlap between Symbol and Digit, peek one more.
       Digit,Symbol -> read(),
		       C2=peek(),
		       Digit2=is_digit(C2),
		       if Digit2 -> a_number(-1);
			  true -> a_symbol("-") %% unfortunate that the prefix of the symbol has to be passed in this way.
		       end;
       Digit -> a_number(1);
       Symbol -> a_symbol();
       true -> error("Parsing error: ")
    end.

exp() -> skip_while(?spacen),
	 R=exp_dispatch(peek()),
	 skip_while(?spacen),
	 R.






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

%% 	     ]).
    
