
-record(reader_S,
	{input,
	 lineno=1, 
	 curmod=serl
	}).
	 
-record(scompile_S,
	{lineno=1,
	 curmod=serl,
	 namespace_safety=1, %% none, whiny (gives warning when shadowed), anal (error when shadowed)

	 %% user should not manually set the namespaces
	 namespaces=[]
	 
	 }).
