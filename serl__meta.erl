-module(serl__meta).

-serl(true).

-define(MOD,serl).

-serl_namespaces(
   [{specials,
     [%% ast literals
      {'__float',{?MOD,'__mac_float'}}, 
      {'__integer',{?MOD,'__mac_integer'}}, 
      {'__string',{?MOD,'__mac_string'}}, 
      {'__atom',{?MOD,'__mac_atom'}}, 
      {'__var',{?MOD,'__mac_var'}},
      {'call',{?MOD,'__mac_call'}},

	 
    %% 4.1
	 {module,{?MOD,'__mac_module'}},
	 {export,{?MOD,'__mac_export'}},
	 {import,{?MOD,'__mac_import'}},
	 {record,{?MOD,'__mac_record'}},
	 {def,{?MOD,'__mac_def'}},

	 %% 4.2
	 {integer,{?MOD,'__mac_integer'}},
	 {float,{?MOD,'__mac_float'}},
	 {string,{?MOD,'__mac_string'}},
	 {atom,{?MOD,'__mac_atom'}},

	 %% 4.4
	 {'=',{?MOD,'__mac_='}},
	 {var,{?MOD,'__mac_var'}},
	 {tuple,{?MOD,'__mac_tuple'}},
	 {list,{?MOD,'__mac_list'}},
	 {cons,{?MOD,'__mac_cons'}},

	 {op,{?MOD,'__mac_op'}},

	 {'!',{?MOD,'__mac_!'}},
	 {'+',{?MOD,'__mac_+'}},
	 {'-',{?MOD,'__mac_-'}},
	 {'*',{?MOD,'__mac_*'}},
	 {'/',{?MOD,'__mac_/'}},	 
	 {'rem',{?MOD,'__mac_rem'}},
	 
	 {'++',{?MOD,'__mac_++'}},
	 
	 {'==',{?MOD,'__mac_=='}},
	 {'<',{?MOD,'__mac_<'}},
	 {'>',{?MOD,'__mac_>'}},
	 {'>=',{?MOD,'__mac_>='}},
	 {'=<',{?MOD,'__mac_=<'}},
	 
	 
	 {'and',{?MOD,'__mac_and'}},
	 {'or',{?MOD,'__mac_or'}},
	 {'andalso',{?MOD,'__mac_andalso'}},
	 {'orelse',{?MOD,'__mac_orelse'}},
	 {'not',{?MOD,'__mac_not'}},
	 
	 {'rec',{?MOD,'__mac_rec'}},
	 {'rec-index',{?MOD,'__mac_rec-index'}},
	 {'rec-val',{?MOD,'__mac_rec-val'}},
	 {'catch',{?MOD,'__mac_catch'}},

	 {'do',{?MOD,'__mac_do'}},
	 {'if',{?MOD,'__mac_if'}},
	 {'case',{?MOD,'__mac_case'}},
     {'try',{?MOD,'__mac_try'}}
     
    ]},
    {macros,[]},
    {reader_macros,[]}
   ]).
