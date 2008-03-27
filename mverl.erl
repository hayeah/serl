-module(mverl).
-compile(export_all). 
-include("verl.hrl").

-import(scompile,[error/1,error/2]).

'__sp_def'(Es,Env) ->
    {FName,Arity,Ast}=verl:parse_def(Es,Env),
    case env:assoc(Env,[definitions,functions,FName,Arity]) of
	{ok,_} -> error("Function already defined: ~p/~p\n", [FName,Arity]); 
	_ -> ok
    end,
    FVal=erl_eval:expr(Ast,erl_eval:new_bindings()),
    Env2=env:assoc_put(Env,[definitions,functions,FName,Arity], FVal),
    Env3=env:assoc_put(Env2,[definitions,functions_def,FName,Arity], Ast),
    {Env3,?def_sec}.

