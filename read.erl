%% as version one, just read from a list.
%% as version one, don't even bother to count the lines.

-module(read).
-include_lib("eunit/include/eunit.hrl").

-export([exp/1,
	 exps/1
	 
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
-define(delimiters,"#(){}[]\""++?special_atom_chars++?spacen++?reserved_chars).
-import(lists,[reverse/1,member/2]).
-import(streamer,[
		  lineno/0,
		  residue/0,
		  peek/0,
		  read/0,
		  char/1,
		  skip_until/1,
		  skip_while/1,
		  token/2, 
		  token_of/1,
		  read_until/1
		 ]).

%% bdefnrstv#\"
-define(string_escapes,
	[{$b,$\b},{$d,$\d},{$e,$\e},
	 {$f,$\f},{$n,$\n},{$r,$\r},
	 {$s,$\s},{$t,$\t},{$v,$\v},
	 {$#,$\#},{$\\,$\\},{$\",$\"}]).


%%%% TODO. Maybe do the reading in a separate process so it's the streamer state is encapsulated on this input.
%% exps(In) ->
%%     Pid=self(),
%%     Resp=spawn_link(fun () -> streamer:init(In),
%% 			      Pid ! {self(),all_exps()}
%% 	       end),
%%     receive {Resp,Exps} -> Exps. 

exps(In) ->
    streamer:init(In),
    all_exps().
    
exp(In) ->
    streamer:init(In),
    R=an_exp(),
    case peek() of
	eof -> R;
	_ -> error("Parse has leftover text:")
    end. 

error(Message) ->
    io:fwrite("\t with remaining input:\n~p~n~n",[residue()]),
    throw({read_error,Message}).

%% serl lexer/parser

is_digit(C) -> ($0 =< C) and (C =< $9).
is_upper(C) -> ($A =< C) and (C =< $Z).
is_alpha(C) -> (($a =< C) and (C =< $z)) or is_upper(C).
is_alpha_num(C) -> is_digit(C)  or is_alpha(C).
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
       true -> a_list_rec(CloseParen,[an_exp()|Acc])
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
    char("{"), E=an_exp(), char("}"),
    E.

a_reader_macro() ->
    read(),
    ?ast_atom(_,Name)=a_symbol(),
    {Args,Here}=reader_macro_args([]),
    {heredoc,Name,Args,Here}
    %get_module():apply_reader_macro(Name,Args)
	.

reader_macro_args(Acc) ->
    skip_while(?spacen),
    C=peek(), 
    %% ${ == 123
    IsArg=(C==123),
    IsOpen=lists:member(C,"(["),
    IsLong=(not IsOpen) and is_atom_first_char(C),
    if C==eof -> error("while parsing reader macro");
       IsArg -> read(), Arg=an_exp(), char("}"),
		reader_macro_args([Arg|Acc]);
       IsLong -> HereTag=read_until(?spacen),
		 %% TODO should only allow comment following a HereTag.
		 %%%% currently just skip the line.
		 skip_until("\n"), read(),
		 {lists:reverse(Acc),here_long(HereTag,[])};
       IsOpen -> 
	    Close=case C of
		      $( -> $);
		      $[ -> $]
		      end,
	    read(),
	    {lists:reverse(Acc),here_short(Close)};
       true -> read(), {lists:reverse(Acc),here_short(C)}
    end.

here_long(Tag,Acc) when is_list(Tag) ->
    case peek() of
	%%upon starting a new line, check if it's the end tag.
	$\n -> here_long_check(Tag,[read()|Acc]);
	eof -> error("prematured end of heredoc");
	_ -> here_long(Tag,[read()|Acc])
    end.

here_long_check(Tag,Acc) ->
    C=peek(),
    Sp=member(C,?space),
    if Sp -> here_long_check(Tag,[read()|Acc]);
       true -> here_long_check(Tag,Tag,Acc)
    end.
here_long_check(OTag,[],Acc) ->
    C=peek(),
    Sp=member(C,?spacen),
    if Sp -> skip_while(?spacen),
	     reverse(Acc);
       C==eof -> reverse(Acc);
       true -> here_long(OTag,reverse(OTag)++Acc)
    end; 
here_long_check(OTag,Tag,Acc) -> 
    C1=peek(),
    C2=hd(Tag),
    if C1==C2 -> read(),here_long_check(OTag,tl(Tag),Acc); 
       true ->
	    %% collect the partial match then retry.
	    here_long(OTag,reverse(OTag--Tag)++Acc)
    end.


here_short(Close) when is_integer(Close) ->
    Str=token_of(fun (C) -> C/=Close end),
    char([Close]),
    %% Squeeze the input.
    Str1=lists:dropwhile(fun (C) -> member(C,?spacen) end,Str),
    Str2=lists:dropwhile(fun (C) -> member(C,?spacen) end,reverse(Str1)),
    reverse(Str2).
    

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
exp_dispatch($#) -> 
    a_reader_macro();
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

an_exp() -> skip_while(?spacen),
	 case peek() of
	     eof -> eof;
	     _ -> R=exp_dispatch(peek()),
		  skip_while(?spacen), 
		  R
	 end.

all_exps() -> all_exps([]).
all_exps(Acc) ->
    case E=an_exp() of
	eof -> lists:reverse(Acc);
	_ -> all_exps([E|Acc])
    end.

    
