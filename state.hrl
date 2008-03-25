
%% -record(reader_S,
%% 	{input,
%% 	 lineno=1, 
%% 	 curmod=serl
%% 	}).
	 
-record(scompile_S,
	{curmod=serl_eval,
	 input,
	 reader_lineno=1,

	 lineno=1,
	 gensym_counter=0
%	 namespace_safety=1,

	 %% user should not manually set the namespaces
%	 namespaces=[]
	 
	 }).
