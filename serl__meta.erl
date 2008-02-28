-module(serl__meta).

-serl(true).

-serl_namespaces(
   [{special_forms,
     [%% ast literals
      {'float',{?MODULE,'__mac_float'}}, 
      {'integer',{?MODULE,'__mac_integer'}}, 
      {'string',{?MODULE,'__mac_string'}}, 
      {'atom',{?MODULE,'__mac_atom'}}, 
      {'var',{?MODULE,'__mac_var'}},
      {'call',{?MODULE,'__mac_call'}},

	 
    %% 4.1
	 {module,{?MODULE,'__mac_module'}},
	 {export,{?MODULE,'__mac_export'}},
	 {import,{?MODULE,'__mac_import'}},
	 {record,{?MODULE,'__mac_record'}},
	 {def,{?MODULE,'__mac_def'}},

	 %% 4.2
	 {integer,{?MODULE,'__mac_integer'}},
	 {float,{?MODULE,'__mac_float'}},
	 {string,{?MODULE,'__mac_string'}},
	 {atom,{?MODULE,'__mac_atom'}},

	 %% 4.4
	 {'=',{?MODULE,'__mac_='}},
	 {var,{?MODULE,'__mac_var'}},
	 {tuple,{?MODULE,'__mac_tuple'}},
	 {list,{?MODULE,'__mac_list'}},
	 {cons,{?MODULE,'__mac_cons'}},

	 {op,{?MODULE,'__mac_op'}},

	 {'!',{?MODULE,'__mac_!'}},
	 {'+',{?MODULE,'__mac_+'}},
	 {'-',{?MODULE,'__mac_-'}},
	 {'*',{?MODULE,'__mac_*'}},
	 {'/',{?MODULE,'__mac_/'}},	 
	 {'rem',{?MODULE,'__mac_rem'}},
	 
	 {'++',{?MODULE,'__mac_++'}},
	 
	 {'==',{?MODULE,'__mac_=='}},
	 {'<',{?MODULE,'__mac_<'}},
	 {'>',{?MODULE,'__mac_>'}},
	 {'>=',{?MODULE,'__mac_>='}},
	 {'=<',{?MODULE,'__mac_=<'}},
	 
	 
	 {'and',{?MODULE,'__mac_and'}},
	 {'or',{?MODULE,'__mac_or'}},
	 {'andalso',{?MODULE,'__mac_andalso'}},
	 {'orelse',{?MODULE,'__mac_orelse'}},
	 {'not',{?MODULE,'__mac_not'}},
	 
	 {'rec',{?MODULE,'__mac_rec'}},
	 {'rec-index',{?MODULE,'__mac_rec-index'}},
	 {'rec-val',{?MODULE,'__mac_rec-val'}},
	 {'catch',{?MODULE,'__mac_catch'}},

	 {'do',{?MODULE,'__mac_do'}},
	 {'if',{?MODULE,'__mac_if'}},
	 {'case',{?MODULE,'__mac_case'}},
     {'try',{?MODULE,'__mac_try'}}
     
    ]},
    {macros,[]}
   ]).
