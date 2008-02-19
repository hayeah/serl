
-define(module_macros,
	[
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
	 {var,'__mac_var'},
	 {tuple,'__mac_tuple'},
	 {list,'__mac_list'},
	 {cons,'__mac_cons'},

	 {op,'__mac_op'},
	 {plus,'__mac_plus'},
	 {minus,'__mac_minus'},
	 {multiply,'__mac_multiply'},
	 {divide,'__mac_divide'},
	 {'rem','__mac_rem'},
	 {'and','__mac_and'},
	 {'or','__mac_or'},
	 {concat,'__mac_concat'},
	 {'equal','__mac_equal'},
	 {'lt','__mac_lt'},
	 {'gt','__mac_gt'},
	 {'gte','__mac_gte'},
	 {'lte','__mac_lte'},
	 {'andalso','__mac_andalso'},
	 {'orelse','__mac_orelse'},
	 {'not','__mac_not'},
	 
	 {'rec','__mac_rec'},
	 {'rec-index','__mac_rec-index'},
	 {'rec-val','__mac_rec-val'},
	 {'catch','__mac_catch'},
	 {'call','__mac_call'}


	]).

-export([
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
	 '__mac_var'/1,
	 '__mac_tuple'/1,
	 '__mac_list'/1,
	 '__mac_cons'/1,

	 '__mac_op'/1,
	 '__mac_plus'/1,
  	 '__mac_minus'/1,
  	 '__mac_multiply'/1,
  	 '__mac_divide'/1,
	 '__mac_rem'/1,
	 '__mac_and'/1,
	 '__mac_or'/1,
	 '__mac_equal'/1,
	 '__mac_lt'/1,
	 '__mac_gt'/1,
	 '__mac_gte'/1,
	 '__mac_lte'/1,
	 '__mac_andalso'/1,
	 '__mac_orelse'/1,
	 '__mac_not'/1,

	 '__mac_rec'/1,
	 '__mac_rec-index'/1,
	 '__mac_rec-val'/1,

	 '__mac_catch'/1,
	 '__mac_call'/1,
	 
	 
	 '__mac_match'/1
	]). 
