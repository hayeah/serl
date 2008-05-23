-module(verl__meta).
-define(MOD,verl).

-serl_info(
   [{exports,[{specials,
	       [%% pseudo forms
		{'__bof',{{?MOD,'__sp_bof'}}},
		{'__eof',{{?MOD,'__sp_eof'}}},
		%% ast specials 
		{'__float',{{?MOD,'__sp_float'}}}, 
		{'__integer',{{?MOD,'__sp_integer'}}}, 
		{'__string',{{?MOD,'__sp_string'}}}, 
		{'__atom',{{?MOD,'__sp_atom'}}}, 
		{'__var',{{?MOD,'__sp_var'}}},
		{'__block',{{?MOD,'__sp_block'}}},
		{'__brace',{{?MOD,'__sp_brace'}}}, 
		{'__call',{{?MOD,'__sp_call'}}},
		{'__quote',{{?MOD,'__sp_quote'}}},
		{'__bquote',{{?MOD,'__sp_bquote'}}},

		%% extensions
		{'defm',{{?MOD,'__sp_defm'}}},
		{'eval-binding',{{?MOD,'__sp_eval-binding'}}},
		{'let',{{?MOD,'__sp_let'}}}, 

		%% 4.1
		{'module',{{?MOD,'__sp_module'}}},
		{'export',{{?MOD,'__sp_export'}}},
		{'import',{{?MOD,'__sp_import'}}},
		{'record',{{?MOD,'__sp_record'}}},
		{'def',{{?MOD,'__sp_def'}}}, 

		%%       {'=',{{?MOD,'__sp_='}}},

		{'tuple',{{?MOD,'__sp_tuple'}}}, 
		{'cons',{{?MOD,'__sp_cons'}}},
		{'nil',{{?MOD,'__sp_nil'}}}, 

		{'op',{{?MOD,'__sp_op'}}},

		{'+',{{?MOD,'__sp_+'}}},
		{'-',{{?MOD,'__sp_-'}}},
		{'*',{{?MOD,'__sp_*'}}},
		{'/',{{?MOD,'__sp_/'}}},	 
		{'rem',{{?MOD,'__sp_rem'}}},

		{'++',{{?MOD,'__sp_++'}}},

		{'==',{{?MOD,'__sp_=='}}},
		{'<',{{?MOD,'__sp_<'}}},
		{'>',{{?MOD,'__sp_>'}}},
		{'>=',{{?MOD,'__sp_>='}}},
		{'=<',{{?MOD,'__sp_=<'}}},


		{'and',{{?MOD,'__sp_and'}}},
		{'or',{{?MOD,'__sp_or'}}},
		{'andalso',{{?MOD,'__sp_andalso'}}},
		{'orelse',{{?MOD,'__sp_orelse'}}},
		{'not',{{?MOD,'__sp_not'}}},

		%%       {'rec',{{?MOD,'__sp_rec'}}},
		%%       {'rec-index',{{?MOD,'__sp_rec-index'}}},
		%%       {'rec-val',{{?MOD,'__sp_rec-val'}}},

		{'catch',{{?MOD,'__sp_catch'}}},

		{'if',{{?MOD,'__sp_if'}}},
		{'case',{{?MOD,'__sp_case'}}},
		{'try',{{?MOD,'__sp_try'}}},

		{'fn',{{?MOD,'__sp_fn'}}},
		{'do',{{?MOD,'__sp_begin'}}},
		{'begin',{{?MOD,'__sp_begin'}}}

	       ]},
	      {macros,
	       [{'ls',{{?MOD,'__mac_ls'}}},
		{'>>',{{?MOD,'__mac_>>'}}}, 

		{'float',{{?MOD,'__mac_float'}}},
		{'integer',{{?MOD,'__mac_integer'}}},
		{'string',{{?MOD,'__mac_string'}}},
		{'atom',{{?MOD,'__mac_atom'}}},
		{'var',{{?MOD,'__mac_var'}}},
		{'block',{{?MOD,'__mac_block'}}},
		{'paren',{{?MOD,'__mac_paren'}}},
		{'brace',{{?MOD,'__mac_brace'}}},

		{'floats',{{?MOD,'__mac_floats'}}},
		{'integers',{{?MOD,'__mac_integers'}}},
		{'strings',{{?MOD,'__mac_strings'}}},
		{'atoms',{{?MOD,'__mac_atoms'}}},
		{'vars',{{?MOD,'__mac_vars'}}},
		{'blocks',{{?MOD,'__mac_blocks'}}},
		{'parens',{{?MOD,'__mac_parens'}}},
		{'braces',{{?MOD,'__mac_braces'}}}

	       ]},
	      {rmacros,
	       [
		{'lit',{{?MOD,'__rm_lit'}}}
	       ]},
	      {functions,
	       [{'cat',{{lists,append}}},
		{'fmt',{{io,format}}}
	       ]}
	     ]}]).

