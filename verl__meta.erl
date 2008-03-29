-module(verl__meta).
-define(MOD,verl).

-serl_exports(
   [{specials,
     [%% pseudo forms
      {'__bof',{?MOD,'__sp_bof'}},
      {'__eof',{?MOD,'__sp_eof'}},
      %% ast specials 
      {'__float',{?MOD,'__sp_float'}}, 
      {'__integer',{?MOD,'__sp_integer'}}, 
      {'__string',{?MOD,'__sp_string'}}, 
      {'__atom',{?MOD,'__sp_atom'}}, 
      {'__var',{?MOD,'__sp_var'}},
      {'__block',{?MOD,'__sp_block'}},
      {'__brace',{?MOD,'__sp_brace'}}, 
      {'__call',{?MOD,'__sp_call'}},
      {'__quote',{?MOD,'__sp_quote'}},
      {'__bquote',{?MOD,'__sp_bquote'}},

      %% extensions
      {'eval-binding',{?MOD,'__sp_eval-binding'}},
      %% 4.1
      {'module',{?MOD,'__sp_module'}},
      {'export',{?MOD,'__sp_export'}},
      {'import',{?MOD,'__sp_import'}},
      {'record',{?MOD,'__sp_record'}},
      {'def',{?MOD,'__sp_def'}},
      {'defm',{?MOD,'__sp_defm'}},

      %% 	 %% 4.2
      %% 	 {'integer',{?MOD,'__sp_integer'}},
      %% 	 {'float',{?MOD,'__sp_float'}},
      %% 	 {'string',{?MOD,'__sp_string'}},
      %% 	 {'atom',{?MOD,'__sp_atom'}},

      %% 4.4
      {'=',{?MOD,'__sp_='}},

      %%       {'var',{?MOD,'__sp_var'}},

      {'tuple',{?MOD,'__sp_tuple'}},
      {'ls',{?MOD,'__sp_ls'}},
      {'ls*',{?MOD,'__sp_ls*'}},
      {'cons',{?MOD,'__sp_cons'}},
      {'do',{?MOD,'__sp_do'}},
      {'let',{?MOD,'__sp_let'}},

      {'op',{?MOD,'__sp_op'}},

      {'+',{?MOD,'__sp_+'}},
      {'-',{?MOD,'__sp_-'}},
      {'*',{?MOD,'__sp_*'}},
      {'/',{?MOD,'__sp_/'}},	 
      {'rem',{?MOD,'__sp_rem'}},

      {'++',{?MOD,'__sp_++'}},

      {'==',{?MOD,'__sp_=='}},
      {'<',{?MOD,'__sp_<'}},
      {'>',{?MOD,'__sp_>'}},
      {'>=',{?MOD,'__sp_>='}},
      {'=<',{?MOD,'__sp_=<'}},


      {'and',{?MOD,'__sp_and'}},
      {'or',{?MOD,'__sp_or'}},
      {'andalso',{?MOD,'__sp_andalso'}},
      {'orelse',{?MOD,'__sp_orelse'}},
      {'not',{?MOD,'__sp_not'}},

      {'rec',{?MOD,'__sp_rec'}},
      {'rec-index',{?MOD,'__sp_rec-index'}},
      {'rec-val',{?MOD,'__sp_rec-val'}},
      {'catch',{?MOD,'__sp_catch'}},

      {'if',{?MOD,'__sp_if'}},
      {'case',{?MOD,'__sp_case'}},
      {'try',{?MOD,'__sp_try'}},

      {'begin',{?MOD,'__sp_begin'}}

     ]},
    {macros,
     [{'block',{?MOD,'__mac_block'}},
      {'paren',{?MOD,'__mac_paren'}},
      {'brace',{?MOD,'__mac_brace'}}
     ]},
    {rmacros,
     [
      {'lit',{?MOD,'__rm_lit'}}
     ]},
    {functions,
     [
      {'fmt',[{name,{io,format}},
	      {arity,[1,2]}]}
     ]}
   ]).



%% these are the definitions of the module.
%% normally they contain all the definitions, but sensitive internal definitions may be omitted.
%% even though they appear in this list, they can't be used unless exported.
-serl_definitions(
   []).


    
