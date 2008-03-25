
-define(erl_integer(L,I),{integer,L,I}).
-define(erl_float(L,F),{float,L,F}).
-define(erl_string(L,S),{string,L,S}).
-define(erl_atom(L,A),{atom,L,A}).
-define(erl_var(L,A),{var,L,A}).

-define(module_sec,1).
-define(header_sec,2).
-define(def_sec,10).


%% -export([
%% 	 '__rm_lit'/2,
%% 	 '__sp_quote'/2,
%% 	 '__sp_bquote'/2,
%% 	 '__sp_eof'/2,
%% 	 '__sp_block'/2,
%% 	 '__sp_brace'/2,
	 
%% 	 % 4.1
%% 	 '__sp_module'/2,

%%  	 '__sp_export'/2,
%%  	 '__sp_import'/2,
%% %% 	 '__sp_record'/2,
%%  	 '__sp_def'/2,

%% %% 	 % 4.2
%%  	 '__sp_integer'/2,
%% %% 	 '__sp_float'/2,
%%  	 '__sp_string'/2,
%%  	 '__sp_atom'/2,
	 

%% %% 	 % 4.4
%%  	 '__sp_='/2,
%%  	 '__sp_var'/2,
%%  	 '__sp_tuple'/2,
%%  	 '__sp_list'/2,
%%  	 '__sp_cons'/2,
%% 	 '__sp_do'/2,

%% %% 	 '__sp_op'/2,

%% %% 	 '__sp_!'/2,
%% %% 	 '__sp_+'/2,
%% %% 	 '__sp_-'/2,
%% %% 	 '__sp_*'/2,
%% %% 	 '__sp_/'/2, 
%% %% 	 '__sp_rem'/2,

%% %% 	 '__sp_++'/2, 

%% %% 	 '__sp_=='/2,
%% %% 	 '__sp_<'/2,
%% %% 	 '__sp_>'/2,
%% %% 	 '__sp_>='/2,
%% %% 	 '__sp_=<'/2,

%% %% 	 '__sp_and'/2,
%% %% 	 '__sp_or'/2, 
%% %% 	 '__sp_andalso'/2,
%% %% 	 '__sp_orelse'/2,
%% %% 	 '__sp_not'/2,

%% %% 	 '__sp_rec'/2,
%% %% 	 '__sp_rec-index'/2,
%% %% 	 '__sp_rec-val'/2,

%% %% 	 '__sp_catch'/2,
%%  	 '__sp_call'/2,
	 
%% %% 	 '__sp_do'/2,
%% %% 	 '__sp_if'/2,

%% %% 	 '__sp_case'/2,
%% %% 	 '__sp_try'/2

%% 	 '__mac_block'/1,
%% 	 '__mac_paren'/1,
%% 	 '__mac_brace'/1,
	 
%% 	 erl_parse_f/1,
%% 	 erl_parse_e/1
	 
%% 	]). 
