%% as version one, just read from a list.
%% as version one, don't even bother to count the lines.

-module(reader).

-export([exp/1,
	 exp/2
	]).

-include("ast.hrl").
-include("state.hrl").
-include_lib("eunit/include/eunit.hrl").



-define(spacen,"\t\s\n").
-define(space,"\t\s").
-define(nesting_chars,".~:").
-define(quoting_chars,";,'`").
-define(reserved_chars,"!@#$%^&\\|?").
-define(delimiters,
	"(){}[]\""++
	?nesting_chars++
	?quoting_chars++
	?spacen++
	?reserved_chars).

-import(lists,[reverse/1,member/2]).
-import(streamer,[lineno/1,
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


%% TODO think of a better way for streamer to keep state.
-define(state,{?MODULE,'state'}).

get_state() ->
    S=get(?state),
    S#scompile_S{reader_lineno=lineno(),input=streamer:get_port()}.

set_state(S) when is_record(S,scompile_S) ->
    put(?state,S),
    lineno(S#scompile_S.reader_lineno),
    streamer:set_port(S#scompile_S.input). 

curmod() ->
    S=get_state(),
    S#scompile_S.curmod.

-define(env,{?MODULE,'env'}).

%% curenv() is used only for reader macro lookup.
curenv() ->
    get(?env). 
curenv(Env) ->
    put(?env,Env).    

exp(In) -> 
    exp(env:new(serl),#scompile_S{input=In}).
    
exp(Env,S) ->
    set_state(S),
    curenv(Env),
    Ast=an_exp(),
    S2=get_state(), %% sequentially!
    {S2,curenv(),Ast}. 

error(Message) ->
    error(Message,[]).
error(Message,Args) ->
    io:fwrite(Message++"\n\tWith remaining input:\n~p\n\n",Args++[residue()]),
    throw({read_error,Message}).

%% serl lexer/parser

is_digit(C) -> ($0 =< C) and (C =< $9).
is_upper(C) -> ($A =< C) and (C =< $Z).
is_alpha(C) -> (($a =< C) and (C =< $z)) or is_upper(C).
is_alpha_num(C) -> is_digit(C)  or is_alpha(C).
is_atom_char(C) -> not (lists:member(C,?delimiters)).
is_atom_first_char(C) -> not (is_digit(C) or is_delimiter(C)).
is_delimiter(C) -> lists:member(C,?delimiters).

spacen() ->
    skip_while(?spacen),
    case peek() of
	$# -> a_comment(),spacen();
	_ -> nil
    end.


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
		 if VarP -> ?cast_var(list_to_atom(Name));
		    true -> ?cast_atom(list_to_atom(Name))
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
			     {ok,[F],_} -> ?cast_float(F);
			     _ -> error("Error parsing floating point.")
			 end;
		    true -> error("Error parsing floating point")
		 end;
       true -> case io_lib:fread("~d",Sign++Int) of
		   {ok,[I],_} -> ?cast_integer(I); 
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
    if C==CloseParen -> read(), lists:reverse(Acc); 
       C==eof -> error("Unexpected eof while reading list.");
       true -> a_list_rec(CloseParen,[an_exp()|Acc])
    end.

a_paren() ->
    char("("), 
    paren([],lineno()).

%% (a b . if :c d e ) => (if :c d e (a b))
%% (a b . if :c d e ) => (if [c d e] (a b))
%%%% my original implementation does the second way.
%%%% Let me think about which way is better.
%%%% The second is more syntatically consistent.

paren(Acc,Line) ->
    spacen(),
    case peek() of
	%% $) == 41
	41 -> read(), ?ast_paren2(Line,reverse(Acc));
	$: -> read(), paren_block([],Acc,lineno(),Line);
	%% $. == 46
	46 -> read(), paren_nest(Acc,Line);
	%% $~ == 126
	%% 126 -> read(), paren_splice(Acc); 
	eof -> error("Unexpected eof while reading paren.");
	_ -> paren([an_exp()|Acc],Line)
    end.

paren_nest(Acc,ContainerLine) ->
    LineofNest=lineno(),
    Nest=paren([],LineofNest),
    ?ast_paren2(LineofNest,
		Nest++[?ast_paren2(ContainerLine,reverse(Acc))]). 
    
paren_block(Block,Acc,Line,ContainerLine) ->
    %% Line is the start of :
    %% ContainerLine is the opening ( or . where : is contained within.
    spacen(),
    case peek() of
	41 -> read(),?ast_paren2(ContainerLine,
				 reverse([?ast_block2(Line,reverse(Block))|Acc]));
	$: -> read(),paren_block([],
				 [?ast_block2(Line,reverse(Block))|Acc],
				 lineno(),ContainerLine);
	46 -> read(),
	      paren_nest(reverse([?ast_block2(Line,reverse(Block))|Acc]),
			 ContainerLine);
	eof -> error("Unexpected eof while reading paren.");
	_ -> paren_block([an_exp()|Block],Acc,Line,ContainerLine)
    end.

a_block() ->
    char("["),
    block([],lineno()).

block(Acc,Line) ->
    spacen(),
    case peek() of
	%% $] 93
	93-> read(),
	     ?ast_paren2(Line,[?ast_atom3(Line,curmod(),'ls'),
			       ?ast_block2(Line,reverse(Acc))]);
	$| -> if Acc==[] -> error("No head for literal list: [|...]");
		 true -> ok
	      end,
	      read(),
	      Tail=an_exp(),
	      case peek() of
		  93 -> ?ast_paren2(Line,[?ast_atom3(Line,curmod(),'ls*'),
					  ?ast_block2(Line,reverse(Acc)),
					  ?ast_block2(Line,[Tail])]);
		  _ -> error("Expecting literal list to close.")
	      end; 
	_ -> block([an_exp()|Acc],Line)
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
	34 -> ?cast_string(lists:reverse(Acc));
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
    ?ast_atom(Name)=case peek() of
	%% ${ == 123}
	123 -> exp_dispatch(123); %% possibly a remote call.
	_ -> a_symbol()
    end,
    Env=curenv(),
    case env:toplevel_lookup(Env,rmacro,Name) of
	{ok,F} ->
	    {Args,Here}=reader_macro_args([]),
	    {Env2,Ast}=F(Args,Here,Env),
	    curenv(Env2),
	    Ast;
	_ -> error("Undefined reader macro: \n~p\n",[Name])
    end.

reader_macro_args(Acc) ->
    spacen(),
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
    if Sp -> spacen(),
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
    
a_comment() ->
    skip_until("\n"),
    nil.

%% $( == 40,  but it messes up indentation
%% $) == 41
exp_dispatch(40) ->
    a_paren(); %% ()
exp_dispatch(123) ->
    ?cast_brace(a_list("{}")); %% {}
exp_dispatch(91) ->
    a_block(); %% []
%% $" == 34
exp_dispatch(34) ->
    a_string();
exp_dispatch($#) ->
    read(),
    a_comment(); 
%% $\% == 37
exp_dispatch(37) ->
    read(),
    a_reader_macro(); 
%% $\' == 39  quote
exp_dispatch(39) ->
    read(),
    E=an_exp(),
    ?cast_quote(E);
%% $` backquote
exp_dispatch($`) ->
    read(),
    E=an_exp(),
    ?cast_bquote(E);
exp_dispatch($,) ->
    read(),
    E=an_exp(),
    ?cast_unquote(E);
exp_dispatch($;) ->
    read(),
    E=an_exp(),
    ?cast_sunquote(E); 
exp_dispatch(C) ->
    Digit=is_digit(C) or (C==$-),
    Symbol=is_atom_first_char(C),
    if %% If the first peeked char is $-, there is an overlap between Symbol and Digit, peek one more.
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

an_exp() -> spacen(),
	 case peek() of
	     eof -> eof; 
	     _ -> case exp_dispatch(peek()) of
		      nil -> an_exp();
		      R-> spacen(),
			  R
		  end
	 end.

    
