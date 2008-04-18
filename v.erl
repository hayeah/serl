-module(v).

-compile(export_all).

serlenv(0) ->
    env:new(verl);
serlenv(1) ->
    env:import(serlenv(0),serl); 
serlenv(2) ->
    env:import(serlenv(1),serl_new).


sread(In) ->
    {Env,_,_,Ast}=scompile:read(In,serlenv(0)),
    {Env,Ast}.

read(In) ->
    {_,Ast}=sread(In),
    Ast.

mexpand(In) ->
    {Env,Ast}=sread(In),
    scompile:mexpand(Ast,Env).

expand1(In) ->
    {Env,Ast}=sread(In),
    scompile:expand1(Ast,Env).

expand(In) ->
    {Env,Ast}=sread(In),
    scompile:expand(Ast,Env).


pexpand1(In) ->
    {Env,Ast}=sread(In),
    {_,R}=scompile:expand1(Ast,Env),
    printer:p(R).

pexpand(In) ->
    {Env,Ast}=sread(In),
    {_,R}=scompile:expand(Ast,Env),
    printer:p(R).

eval(In) ->
    eval(In,erl_eval:new_bindings()). 
eval(In,Bindings) ->
    {Env,Ast}=sread(In),
    scompile:eval(Ast,Env,Bindings).


evaln(N,In) ->
    evaln(N,In,erl_eval:new_bindings()).
evaln(N,In,Bindings) ->
    {Env,Ast}=sread(In),
    evaln_(N,Ast,Env,Bindings).

evaln_(0,Ast,Env,Bindings) ->
    {Env,Ast,Bindings};
evaln_(N,Ast,Env,Bindings) ->
    {Env2,Ast2,Bs2}=scompile:eval(Ast,Env,Bindings),
    evaln_(N-1,Ast2,Env2,Bs2).
    

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
