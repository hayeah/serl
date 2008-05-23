-module(v).

-compile(export_all).
%-import(verl,[serlenv/1]).


bootup() ->
    dbg:tracer(),
    dbg:p(new,[c]),
    reload(version()).

profile() ->
    fprof:apply(v,test,[test,[],[dry,expand_only]]),
    fprof:profile(),
    fprof:analyse({dest,"profile.dat"}).

env() ->
    env:import(env:new(verl),serl).

%% env() ->
%%     env:new(verl). 

p(FnName,In) ->
    Ast=?MODULE:FnName(In),
    printer:p(Ast),
    io:nl(),
    Ast.

read(In) ->
    {_,_,Ast}=scompile:read(In,env()),
    Ast.

mexpand(In) ->
    scompile:mexpand(read(In),env()).

expand(In) ->
    scompile:expand(read(In),env()).

expand1(In) ->
    expandn(In,1).
expandn(In,N) ->
    expandn_(read(In),env(),N).


expandn_(Ast,_Env,0) ->
    Ast;
expandn_(Ast,Env,N) ->
    expandn_(scompile:expand1(Ast,Env),Env,N-1).

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
    evaln_(N,Ast,env(),Bindings).

evaln_(0,Ast,_Env,Bindings) ->
    {Ast,Bindings};
evaln_(N,Ast,Env,Bindings) ->
    {Ast2,Bs2}=scompile:eval(Ast,Env,Bindings),
    evaln_(N-1,Ast2,Env,Bs2).
    
test(Mod) ->
    compile(Mod,[strong_validation,report,dry]),
    ok.

test(Mod,Defs) ->
    case Defs of
	all -> compile(Mod,[strong_validation,report,dry,ast]);
	_ -> compile(Mod,[strong_validation,report,dry,{def,Defs}])
    end.

test(Mod,Defs,Options) ->
    case Defs of
	all -> compile(Mod,[strong_validation,report,dry,ast]++Options);
	_ -> compile(Mod,[strong_validation,report,dry,{def,Defs}]++Options)
    end.


compile(Mod) ->
    compile(Mod,[report]).

compile(Mod,Options) ->
    scompile:compile(Mod,env(),Options).

%% verl     ## base
%% serl     ## stable
%% serl_new ## unstable

%% bootstrap

%% forward   # compile a new version
%% backward  # rollback by n cycles

t() ->
    test(serl).

t(Defs) ->
    test(serl,Defs).

t(Defs,Options) ->
    test(serl,Defs,Options).

tt() ->
    test(test).

tt(Defs) ->
    test(test,Defs).

tt(Defs,Options) ->
    test(test,Defs,Options).


version() ->
    length(filelib:wildcard("bootstrap/*.beam"))-1.

r() ->
    recompile(0).

r(N) ->
     %% recompile with the nth version back.
     back(N),
     r().

next() ->
    %% step to next version
    recompile(1).

back(N) ->
    Version=version()-N,
    if Version < 1 -> reload(1);
       true -> reload(Version)
    end.

goto(N) ->
    reload(N).

%% hand compile the meta module for now
recompile(VersionIncrement) ->
    R=compile(serl,[bin,report,meta]),
    %% I absolutely loath how Erlang makes binding in cases visible outside.
    ModBin=case env:assoc(R,[bin]) of
	       {ok,[_,Bin|_]} -> Bin
	   end,
    
%%     MetaBin=case env:assoc(R,[meta,bin]) of
%% 	       {ok,[_,Bin2|_]} -> Bin2
%% 	    end,
    
    %% ehh... should probably at the filenames to figure out version numbers. But I don't want to figure out erlang regex.
    %% So, never delete from the directory.
    Version=version()+VersionIncrement,
    ModPath="bootstrap/"++integer_to_list(Version)++".beam",
    %MetaModPath="bootstrap/"++integer_to_list(Version)++"__meta.beam",
    file:write_file(ModPath,ModBin),
    %file:write_file(MetaModPath,MetaBin),
    compile:file(serl__meta),
    reload(Version).
    
reload(Version) ->
    ModPath="bootstrap/"++integer_to_list(Version)++".beam",
    %MetaModPath="bootstrap/"++integer_to_list(Version)++"__meta.beam",
    ModBin=case file:read_file(ModPath) of
	       {ok,Bin} -> Bin
	   end,
    %% MetaModBin=case file:read_file(MetaModPath) of
%% 		    {ok,Bin2} -> Bin2
%% 	       end,
    
    code:purge(serl),
    code:purge(serl__meta),
    {Version,
     code:load_binary(serl,"",ModBin),
     code:load_file(serl__meta)}.


%% recompile(VersionIncrement) ->
%%     R=compile(serl,[bin,report,meta]),
%%     %% I absolutely loath how Erlang makes binding in cases visible outside.
%%     ModBin=case env:assoc(R,[bin]) of
%% 	       {ok,[_,Bin|_]} -> Bin
%% 	   end,
%%     MetaBin=case env:assoc(R,[meta,bin]) of
%% 	       {ok,[_,Bin2|_]} -> Bin2
%% 	    end,
%%     %% ehh... should probably at the filenames to figure out version numbers. But I don't want to figure out erlang regex.
%%     %% So, never delete from the directory.
%%     Version=version()+VersionIncrement,
%%     ModPath="bootstrap/"++integer_to_list(Version)++".beam",
%%     MetaModPath="bootstrap/"++integer_to_list(Version)++"__meta.beam",
%%     file:write_file(ModPath,ModBin),
%%     file:write_file(MetaModPath,MetaBin),
%%     reload(Version).
    
%% reload(Version) ->
%%     ModPath="bootstrap/"++integer_to_list(Version)++".beam",
%%     MetaModPath="bootstrap/"++integer_to_list(Version)++"__meta.beam",
%%     ModBin=case file:read_file(ModPath) of
%% 	       {ok,Bin} -> Bin
%% 	   end,
%%     MetaModBin=case file:read_file(MetaModPath) of
%% 		    {ok,Bin2} -> Bin2
%% 	       end,
%%     code:purge(serl),
%%     code:purge(serl_meta),
%%     {Version,
%%      code:load_binary(serl,"",ModBin),
%%      code:load_binary(serl__meta,"",MetaModBin)}.


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

