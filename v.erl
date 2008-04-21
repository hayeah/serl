-module(v).

-compile(export_all).
-import(verl,[serlenv/1]).


read(In) ->
    read(In,0).

read(In,N) ->
    {_,_,Ast}=scompile:read(In,serlenv(N)),
    Ast.

mexpand(In) ->
    mexpand(In,0).
mexpand(In,N) ->
    scompile:mexpand(read(In,N),serlenv(N)).

expand(In) ->
    expand(In,0).
expand(In,N) ->
    scompile:expand(read(In,N),serlenv(N)). 

%% pexpand1(In) ->
%%     {Env,Ast}=sread(In),
%%     {_,R}=scompile:expand1(Ast,Env),
%%     printer:p(R).

%% pexpand(In) ->
%%     {Env,Ast}=sread(In),
%%     {_,R}=scompile:expand(Ast,Env),
%%     printer:p(R).


eval(In) ->
    eval(In,[],0).
eval(In,Bindings) ->
    eval(In,Bindings,0).
eval(In,Bindings,N) ->
    evaln(1,In,Bindings,N).

evaln(Times,In) ->
    evaln(Times,In,[],0).
evaln(Times,In,Bindings) ->
    evaln(Times,In,Bindings,0).
evaln(Times,In,Bindings,N) ->
    Ast=read(In,N),
    evaln_(Times,Ast,serlenv(N),Bindings).

evaln_(0,Ast,_Env,Bindings) ->
    {Ast,Bindings};
evaln_(N,Ast,Env,Bindings) ->
    {Ast2,Bs2}=scompile:eval(Ast,Env,Bindings),
    evaln_(N-1,Ast2,Env,Bs2).
    

compile(Mod) ->
    compile(Mod,[ast,report]).

compile(Mod,Options) ->
    scompile:compile(Mod,serlenv(0),Options).

%% verl     ## base
%% serl     ## stable
%% serl_new ## unstable

c(Mod) ->
    %% refresh the unstable version
    Bin=compile(Mod,[bin]),
    code:purge(serl_new),
    code:load_binary(serl_new,"",Bin).    

cv() ->
    R1=c(verl2),
    compile:file("verl2__meta"),
    code:purge(verl2__meta),
    R2=code:load_file(verl2__meta), 
    {R1,R2}. 
