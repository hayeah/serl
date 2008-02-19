
-define(module_macros,
	[
	 % serl extensions
	 {'block','__mac_block'},
	 {'brace','__mac_brace'},
	 
	 % 4.1
	 {module,'__mac_module'},
	 {export,'__mac_export'},
	 {import,'__mac_import'},
	 {record,'__mac_record'},

	 % 4.2
	 {integer,'__mac_integer'},
	 {float,'__mac_float'},
	 {string,'__mac_string'},
	 {atom,'__mac_atom'},

	 % 4.4
	 {'=','__mac_='},
	 {var,'__mac_var'},
	 {tuple,'__mac_tuple'},
	 {list,'__mac_list'},
	 {cons,'__mac_cons'},

	 {op,'__mac_op'},

	 {'+','__mac_+'},
	 {'-','__mac_-'},
	 {'*','__mac_*'},
	 {'/','__mac_/'},	 
	 {'rem','__mac_rem'},
	 
	 {'++','__mac_++'},
	 
	 {'==','__mac_=='},
	 {'<','__mac_<'},
	 {'>','__mac_>'},
	 {'>=','__mac_>='},
	 {'=<','__mac_=<'},
	 
	 
	 {'and','__mac_and'},
	 {'or','__mac_or'},
	 {'andalso','__mac_andalso'},
	 {'orelse','__mac_orelse'},
	 {'not','__mac_not'},
	 
	 {'rec','__mac_rec'},
	 {'rec-index','__mac_rec-index'},
	 {'rec-val','__mac_rec-val'},
	 {'catch','__mac_catch'},
	 {'call','__mac_call'},

	 {'do','__mac_do'},
	 {'if','__mac_if'}
	     


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
	 '__mac_if'/1
	 
	 
	]). 
