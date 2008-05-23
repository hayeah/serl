-module(serl__meta).

-define(MOD,serl).

-serl_info(
   [{exports,[{specials,
	       [{'__block',{{?MOD,'sp-block'}}},
		{'__brace',{{?MOD,'sp-brace'}}},
		{'__bof',{{?MOD,'sp-bof'}}},
		{'__eof',{{?MOD,'sp-eof'}}}, 
		{'def',{{?MOD,'sp-def'}}},
		{'defm',{{?MOD,'sp-defm'}}},
		{'defspecial',{{?MOD,'sp-defspecial'}}},
						%{'let2',{{?MOD,'sp-let'}}},
		{'lsc',{{?MOD,'sp-lsc'}}},
		{'case',{{?MOD,'sp-case'}}},
		{'if',{{?MOD,'sp-if'}}},
		{'fn',{{?MOD,'sp-fn'}}},
		{'call',{{?MOD,'sp-call'}}},
		{'toplevel',{{?MOD,'sp-toplevel'}}},

		{'functions-import',{{?MOD,'sp-functions-import'}}},
		{'functions-export',{{?MOD,'sp-functions-export'}}},
		{'functions-output',{{?MOD,'sp-functions-output'}}},

		{'macros-import',{{?MOD,'sp-macros-import'}}},
		{'macros-export',{{?MOD,'sp-macros-export'}}},
		{'macros-output',{{?MOD,'sp-macros-output'}}},

		{'specials-import',{{?MOD,'sp-specials-import'}}},
		{'specials-export',{{?MOD,'sp-specials-export'}}},
		{'specials-output',{{?MOD,'sp-specials-output'}}},


		{'+',{{?MOD,'sp-+'}}},
		{'-',{{?MOD,'sp--'}}}, 
		{'==',{{?MOD,'sp-=='}}},
		{'not',{{?MOD,'sp-not'}}}, 
		{'>',{{?MOD,'sp->'}}}

	       ]
	       %%{'defm',{{?MOD,'__sp_defm'}}}
	      },
	      {macros,
	       [{'with-genvar',{{?MOD,'mac-with-genvar'}}},
		{'let*',{{?MOD,'mac-let*'}}},
		{'import',{{?MOD,'mac-import'}}},
		{'export',{{?MOD,'mac-export'}}},
		{'import-from',{{?MOD,'mac-import-from'}}},
		{'export-from',{{?MOD,'mac-export-from'}}},
		{'>>',{{?MOD,'mac->>'}}}
	       ]}
	     ]},
    {imports,[{verl,
	       [{specials,all},
		{macros,all},
		{rmacros,all},
		{functions,all}
	       ]}
	     ]}]).
