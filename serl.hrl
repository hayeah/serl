
-export([
	 '__rm_lit'/2,

	 '__mac_block'/2,
	 '__mac_brace'/2,
	 
	 % 4.1
%%	 '__mac_module'/2,

%% 	 '__mac_export'/2,
%% 	 '__mac_import'/2,
%% 	 '__mac_record'/2,
%% 	 '__mac_def'/2,

%% 	 % 4.2
 	 '__mac_integer'/2,
%% 	 '__mac_float'/2,
 	 '__mac_string'/2,
 	 '__mac_atom'/2,
	 

%% 	 % 4.4
%% 	 '__mac_='/2,
%% 	 '__mac_var'/2,
 	 '__mac_tuple'/2,
 	 '__mac_list'/2,
 	 '__mac_cons'/2,

%% 	 '__mac_op'/2,

%% 	 '__mac_!'/2,
%% 	 '__mac_+'/2,
%% 	 '__mac_-'/2,
%% 	 '__mac_*'/2,
%% 	 '__mac_/'/2, 
%% 	 '__mac_rem'/2,

%% 	 '__mac_++'/2, 

%% 	 '__mac_=='/2,
%% 	 '__mac_<'/2,
%% 	 '__mac_>'/2,
%% 	 '__mac_>='/2,
%% 	 '__mac_=<'/2,

%% 	 '__mac_and'/2,
%% 	 '__mac_or'/2, 
%% 	 '__mac_andalso'/2,
%% 	 '__mac_orelse'/2,
%% 	 '__mac_not'/2,

%% 	 '__mac_rec'/2,
%% 	 '__mac_rec-index'/2,
%% 	 '__mac_rec-val'/2,

%% 	 '__mac_catch'/2,
%% 	 '__mac_call'/2
	 
%% 	 '__mac_do'/2,
%% 	 '__mac_if'/2,

%% 	 '__mac_case'/2,
%% 	 '__mac_try'/2
	 erl_parse_e/1
	 
	]). 
