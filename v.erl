-module(v).

-compile(export_all).
%-import(verl,[serlenv/1]).



serlenv() ->
    env:import(env:new(verl),serl).


read(In) ->
    {_,_,Ast}=scompile:read(In,serlenv()),
    Ast.

mexpand(In) ->
    scompile:mexpand(read(In),serlenv()).

expand(In) ->
    scompile:expand(read(In),serlenv()). 

%% pexpand1(In) ->
%%     {Env,Ast}=sread(In),
%%     {_,R}=scompile:expand1(Ast,Env),
%%     printer:p(R).

%% pexpand(In) ->
%%     {Env,Ast}=sread(In),
%%     {_,R}=scompile:expand(Ast,Env),
%%     printer:p(R).


eval(In) ->
    eval(In,[]). 
eval(In,Bindings) ->
    evaln(1,In,Bindings).

evaln(N,In) ->
    evaln(N,In,[]). 
evaln(N,In,Bindings) ->
    Ast=read(In),
    evaln_(N,Ast,serlenv(),Bindings).

evaln_(0,Ast,_Env,Bindings) ->
    {Ast,Bindings};
evaln_(N,Ast,Env,Bindings) ->
    {Ast2,Bs2}=scompile:eval(Ast,Env,Bindings),
    evaln_(N-1,Ast2,Env,Bs2).
    

compile(Mod) ->
    compile(Mod,[ast,report]).

compile(Mod,Options) ->
    scompile:compile(Mod,serlenv(),Options).

%% verl     ## base
%% serl     ## stable
%% serl_new ## unstable

%% bootstrap

%% forward   # compile a new version
%% backward  # rollback by n cycles

t() ->
    compile(serl,[strong_validation,ast,report,dry]).


version() ->
    length(filelib:wildcard("bootstrap/*__meta.beam")).

next() ->
    R=compile(serl,[bin,report,meta]),
    %% I absolutely loath how Erlang makes binding in cases visible outside.
    ModBin=case env:assoc(R,[bin]) of
	       {ok,[_,Bin|_]} -> Bin
	   end,
    MetaBin=case env:assoc(R,[meta,bin]) of
	       {ok,[_,Bin2|_]} -> Bin2
	    end,
    %% ehh... should probably at the filenames to figure out version numbers. But I don't want to figure out erlang regex.
    %% So, never delete from the directory.
    Version=version()+1,
    ModPath="bootstrap/"++integer_to_list(Version)++".beam",
    MetaModPath="bootstrap/"++integer_to_list(Version)++"__meta.beam",
    file:write_file(ModPath,ModBin),
    file:write_file(MetaModPath,MetaBin),
    reload(Version).

back(N) ->
    Version=version()-N,
    if Version < 1 -> reload(1);
       true -> reload(Version)
    end.

goto(N) ->
    reload(N).

reload(Version) ->
    ModPath="bootstrap/"++integer_to_list(Version)++".beam",
    MetaModPath="bootstrap/"++integer_to_list(Version)++"__meta.beam",
    ModBin=case file:read_file(ModPath) of
	       {ok,Bin} -> Bin
	   end,
    MetaModBin=case file:read_file(MetaModPath) of
		    {ok,Bin2} -> Bin2
	       end,
    code:purge(serl),
    code:purge(serl_meta),
    {Version,
     code:load_binary(serl,"",ModBin),
     code:load_binary(serl__meta,"",MetaModBin)}.


%% c(Mod) ->
%%     %% refresh the unstable version
%%     Bin=compile(Mod,[bin]),
%%     code:purge(serl_new),
%%     code:load_binary(serl_new,"",Bin).    

%% cv() ->
%%     R1=c(verl2),
%%     compile:file("verl2__meta"),
%%     code:purge(verl2__meta),
%%     R2=code:load_file(verl2__meta), 
%%     {R1,R2}. 


%% save the boostrap beams in a directory, with numbered names.
%% serlenv(0) is the latest
%% serlenv(1) is the last version ...
