
-export([
	 '__rm_lit'/2,

	 '__sp_block'/2,
	 '__sp_brace'/2,
	 
	 % 4.1
%%	 '__sp_module'/2,

%% 	 '__sp_export'/2,
%% 	 '__sp_import'/2,
%% 	 '__sp_record'/2,
%% 	 '__sp_def'/2,

%% 	 % 4.2
 	 '__sp_integer'/2,
%% 	 '__sp_float'/2,
 	 '__sp_string'/2,
 	 '__sp_atom'/2,
	 

%% 	 % 4.4
 	 '__sp_='/2,
 	 '__sp_var'/2,
 	 '__sp_tuple'/2,
 	 '__sp_list'/2,
 	 '__sp_cons'/2,
	 '__sp_do'/2,
	 '__sp_let'/2,

%% 	 '__sp_op'/2,

%% 	 '__sp_!'/2,
%% 	 '__sp_+'/2,
%% 	 '__sp_-'/2,
%% 	 '__sp_*'/2,
%% 	 '__sp_/'/2, 
%% 	 '__sp_rem'/2,

%% 	 '__sp_++'/2, 

%% 	 '__sp_=='/2,
%% 	 '__sp_<'/2,
%% 	 '__sp_>'/2,
%% 	 '__sp_>='/2,
%% 	 '__sp_=<'/2,

%% 	 '__sp_and'/2,
%% 	 '__sp_or'/2, 
%% 	 '__sp_andalso'/2,
%% 	 '__sp_orelse'/2,
%% 	 '__sp_not'/2,

%% 	 '__sp_rec'/2,
%% 	 '__sp_rec-index'/2,
%% 	 '__sp_rec-val'/2,

%% 	 '__sp_catch'/2,
%% 	 '__sp_call'/2
	 
%% 	 '__sp_do'/2,
%% 	 '__sp_if'/2,

%% 	 '__sp_case'/2,
%% 	 '__sp_try'/2
	 erl_parse_e/1
	 
	]). 
