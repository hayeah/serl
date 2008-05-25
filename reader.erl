%% as version one, just read from a list.
%% as version one, don't even bother to count the lines.

-module(reader).

-export([
	 exp/3
	]).

-include("ast.hrl").


%% symbol chars: [a-zA-Z0-9_+_*/\]
-define(spacen,"\t\s\n").
-define(space,"\t\s").
-define(nesting_chars,".~:").
-define(quoting_chars,";,'`").
-define(module_separator,$\\).
-define(reserved_chars,"!@#$%^&|?").
-define(delimiters,
	"(){}[]\""++
	?nesting_chars++
	?quoting_chars++
	[?module_separator]++
	?spacen++
	?reserved_chars).

-import(lists,[reverse/1,member/2]).
-import(streamer,[lineno/0,
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

-import(scompile,[curmod/0]).

%% bdefnrstv#\"
-define(string_escapes,
	[{$b,$\b},{$d,$\d},{$e,$\e},
	 {$f,$\f},{$n,$\n},{$r,$\r},
	 {$s,$\s},{$t,$\t},{$v,$\v},
	 {$#,$\#},{$\\,$\\},{$\",$\"}]).

-define(env,{?MODULE,'env'}).

error(Message) ->
    error(Message,[]).
error(Message,Args) ->
    scompile:error(Message++"\nRemaining Input:\n~p\n\n",Args++[residue()]).

get_state() ->
    {curenv(), streamer:get_port(), lineno()}.

set_state(Env,In,Line) ->
    streamer:set_port(In),
    streamer:set_lineno(Line),
    put(?env,Env). 

%% curenv() is used only for reader macro lookup.
%% in the future may be used to lookup readertable.
curenv() -> get(?env).
    

exp(In,Env,LineNo) ->
    set_state(Env,In,LineNo), 
    Ast=an_exp(),
    {_Env2,In2,LineNo2}=get_state(),
    {In2,LineNo2,Ast}. 

%% serl lexer/parser

is_digit(C) -> ($0 =< C) and (C =< $9).
is_upper(C) -> ($A =< C) and (C =< $Z). 

is_symbol_charn(C) ->
    not is_delimiter(C).
is_symbol_char1(C) ->
    not (is_digit(C) or is_delimiter(C)).
is_delimiter(C) -> lists:member(C,?delimiters).

%% is_alpha(C) -> (($a =< C) and (C =< $z)) or is_upper(C).
%% is_alpha_num(C) -> is_digit(C)  or is_alpha(C).

spacen() ->
    skip_while(?spacen),
    case peek() of
	$# -> a_comment(),spacen();
	_ -> nil
    end.

a_symbol() ->
    a_symbol("").
a_symbol(Prefix) ->
    Token=token_of(fun is_symbol_charn/1),
    Name=Prefix++Token,
    UniVarP=(hd(Name)==$_), 
    AfterSymbolC=peek(),
    {M,A}=
	if AfterSymbolC==?module_separator and UniVarP ->
		error("module of a symbol cannot be the universal pattern.");
	   AfterSymbolC==?module_separator ->
		read(),
		Atom=token_of(fun is_symbol_charn/1),
		{Name, Atom};
	   true -> {atom_to_list(curmod()),Name}
	end, 
    AfterAllC=peek(),
    if AfterAllC==?module_separator ->
	    error("Only one module specifier allowed.");
       true -> ok
    end,
    %% var:  module must be an atom
    %% atom: module can be a var or an atom, cannot be a universal pattern
    %% %% if module is a var, transfrom to a tuple:  Mod\a -> {Mod a}
    MVarP=is_upper(hd(M)),
    MUniVarP=(hd(M)==$_),
    MAtomP=not (MVarP or MUniVarP),
    AVarP=is_upper(hd(A)),
    AUniVarP=(hd(A)==$_) ,
    AAtomP=not (AVarP or AUniVarP), 
    if (AVarP or AUniVarP) ->
	    if MAtomP ->
		    if AUniVarP -> ?ast_var3(lineno(),list_to_atom(M),'_');
		       AVarP -> ?ast_var3(lineno(),list_to_atom(M),list_to_atom(A))
		    end;
	       true -> error("Home module of a var must be an atom")
	    end;
       AAtomP ->
	    if MVarP -> ?cast_brace([?cast_var(M),?cast_atom(A)]);
	       MAtomP -> ?ast_atom3(lineno(),list_to_atom(M),list_to_atom(A));
	       true -> error("Home module of an atom must be a var or an atom.")
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
    paren(lineno()).

paren(Line) ->
    %% generate the instructions for the nesting reduction machine 
    Insts=paren_segments([],[],Line),
    %% run the instructions
    paren_renest(Insts,Line).

%% (a . b ~ c . d) =>
%% (b (a) ~ c . d)
%% (c (b (a)) . d)
%% (d (c (b (a))))

%% (a b . c d ~ e f . g h) =>
%% (c d (a b) ~ e f . g h)
%% (e (c d (a b)) f . g h)
%% (g h (e (c d (a b)) f))

%% the instructions are: nest_tail, nest_head, close
%% the machine is like a RPN reduction of ~ and .
paren_renest([Items,{close}],Line) ->
    ?ast_paren2(Line,Items);
paren_renest([Items1,Op,Items2|Insts],Line) ->
    %% Line is the lineno of the previous nesting operator.
    %% %% For the first instruction, it is the lineno of paren-open.
    %% NestLine is the lineno where the nesting operator appeared.
    case Op of
	{nest_tail,NestLine} ->
	    if Items2==[] ->
		    error("No elements for nesting operator \".\"");
	       true -> ok
	    end,
	    paren_renest(
	      [Items2++[?ast_paren2(Line,Items1)]
	       |Insts],
	      NestLine);
	{nest_head,NestLine} ->
	    if Items2==[] ->
		    error("No elements for nesting operator \"~\"");
	       true -> ok
	    end,
	    [H|T]=Items2,
	    paren_renest(
	      [[H,?ast_paren2(Line,Items1)|T]
	       |Insts],
	      NestLine)
    end.

paren_segments(Acc,Inst,Line) ->
    %% the nest operators are ~ .
    %% ":" collects a list of elements as a single block (until ":~.)").
    %% %% ":" is transparent to the nesting operators.
    case peek() of
	$\) -> read(), reverse([{close},reverse(Acc)|Inst]);
	$\. -> read(), paren_segments([],[{nest_tail,lineno()},reverse(Acc)|Inst],Line);
	$\~ -> read(), paren_segments([],[{nest_head,lineno()},reverse(Acc)|Inst],Line);
       	$\: -> read(), paren_segments(paren_block([],Acc,Line),Inst,Line); 
	eof -> error("Unexpected eof while reading paren.");
	_ -> paren_segments([an_exp()|Acc],Inst,Line)
    end.

paren_block(BlkAcc,SegAcc,Line) ->
    case peek() of 
	$: -> read(),
	      paren_block([],
			  [?ast_block2(Line,reverse(BlkAcc))|SegAcc],
			  lineno());
	C when C==$\.;C==$\~;C==$\) ->
	    [?ast_block2(Line,reverse(BlkAcc))|SegAcc]; 
	eof -> error("Unexpected eof while reading paren.");
	_ -> paren_block([an_exp()|BlkAcc],SegAcc,Line)
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
		  93 -> read(),
			?ast_paren2(Line,[?ast_atom3(Line,curmod(),'ls'),
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

%% string_interpolate() ->
%%     char("{"), E=an_exp(), char("}"),
%%     E.


a_reader_macro() ->
    ?ast_atom3(_L,Mod,Atom)=a_symbol(), 
    Env=curenv(),
    case scompile:lookup_meta_fun(Env,rmacros,{Mod,Atom}) of
	{ok,F} ->
	    Args=reader_macro_args(),
	    spacen(),
	    HereDoc=reader_macro_here(),
	    F(Args,HereDoc);
	false -> error("Undefined reader macro: \n~p\n",[Atom]) 
    end.

reader_macro_args() ->
    spacen(),
    %% $( == 40 
    case peek() of
	40 -> read(),
	      reader_macro_args([]);
	_ -> []
    end.

reader_macro_args(Acc) ->
    spacen(),
    case peek() of
	%% $) == 41
	41 -> read(), reverse(Acc); 
	_ -> reader_macro_args([an_exp()|Acc])
    end.

reader_macro_here() ->
    C=peek(), 
    %% $( == 40
    case lists:member(C,"{[") of
	%% inline reader macro
	true ->
	    read(),
	    Close=case C of
		      ${ -> $};
		      $[ -> $]
		      end, 
	    here_short(Close);
	%% multiline reader macro
	_ -> HereTag=read_until(?spacen),
	     %% TODO should only allow comment following a HereTag.
	     %% %% currently just skip the line.
	     skip_until("\n"),
	     here_long(HereTag,[])
    end.

here_long(Tag,Acc) when is_list(Tag) ->
    case peek() of
	%%upon starting a new line, check if it's the end tag.
	$\n -> here_long_check(Tag,[read()|Acc]);
	eof -> error("EOF: unexpected end of heredoc\nString:\\~s",[reverse(Acc)]);
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
    Sp=member(C,[eof|?spacen]), %% the current char is a space directly after the Tag. We are done.
    if Sp -> spacen(),
	     %% strip white spaces on the same line as the closing tag.
	     %% also drop the newline starting the closing tag line.
	     [_|Acc2]=lists:dropwhile(fun (C2) ->
					      member(C2,?space)
				      end,
				      Acc),
	     reverse(Acc2);
       %% not a match after all: HEREsomething
       true -> here_long(OTag,reverse(OTag)++Acc)
    end; 
here_long_check(OTag,Tag,Acc) -> 
    C1=peek(),
    C2=hd(Tag),
    if C1==C2 -> read(),here_long_check(OTag,tl(Tag),Acc); 
       true ->
	    %% collect the partial match then retry starting at next line.
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
    skip_until([$\n,eof]),
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
    L=lineno(),
    E=an_exp(),
    ?ast_quote3(L,curmod(),E);
%% $` backquote
exp_dispatch($`) ->
    read(),
    L=lineno(),
    E=an_exp(),
    ?ast_bquote3(L,curmod(),E);
exp_dispatch($,) ->
    read(),
    L=lineno(),
    E=an_exp(),
    ?ast_unquote3(L,curmod(),E);
exp_dispatch($;) ->
    read(),
    L=lineno(),
    E=an_exp(),
    ?ast_sunquote3(L,curmod(),E); 
exp_dispatch(C) ->
    Digit=is_digit(C) or (C==$-),
    Symbol=is_symbol_char1(C),
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

    
