
-define(module_macros,
	[
	 % serl extensions
	 {'block',{?MODULE,'__mac_block'}},
	 {'brace',{?MODULE,'__mac_brace'}},
	 
	 % 4.1
	 {module,{?MODULE,'__mac_module'}},
	 {export,{?MODULE,'__mac_export'}},
	 {import,{?MODULE,'__mac_import'}},
	 {record,{?MODULE,'__mac_record'}},

	 % 4.2
	 {integer,{?MODULE,'__mac_integer'}},
	 {float,{?MODULE,'__mac_float'}},
	 {string,{?MODULE,'__mac_string'}},
	 {atom,{?MODULE,'__mac_atom'}},

	 % 4.4
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
	 {'call',{?MODULE,'__mac_call'}},

	 {'do',{?MODULE,'__mac_do'}},
	 {'if',{?MODULE,'__mac_if'}},
	 {'case',{?MODULE,'__mac_case'}},
	 {'try',{?MODULE,'__mac_try'}}

	]).


-export([

	 '__mac_block'/1,
	 '__mac_brace'/1,
	 
	 % 4.1
	 '__mac_module'/1,
	 '__mac_export'/1,
	 '__mac_import'/1,
	 '__mac_record'/1,

	 % 4.2
	 '__mac_integer'/1,
	 '__mac_float'/1,
	 '__mac_string'/1,
	 '__mac_atom'/1,
	 

	 % 4.4
	 '__mac_='/1,
	 '__mac_var'/1,
	 '__mac_tuple'/1,
	 '__mac_list'/1,
	 '__mac_cons'/1,

	 '__mac_op'/1,

	 '__mac_!'/1,
	 '__mac_+'/1,
	 '__mac_-'/1,
	 '__mac_*'/1,
	 '__mac_/'/1, 
	 '__mac_rem'/1,

	 '__mac_++'/1, 

	 '__mac_=='/1,
	 '__mac_<'/1,
	 '__mac_>'/1,
	 '__mac_>='/1,
	 '__mac_=<'/1,

	 '__mac_and'/1,
	 '__mac_or'/1, 
	 '__mac_andalso'/1,
	 '__mac_orelse'/1,
	 '__mac_not'/1,

	 '__mac_rec'/1,
	 '__mac_rec-index'/1,
	 '__mac_rec-val'/1,

	 '__mac_catch'/1,
	 '__mac_call'/1,
	 
	 '__mac_do'/1,
	 '__mac_if'/1,

	 '__mac_case'/1,
	 '__mac_try'/1
	 
	 
	]). 
