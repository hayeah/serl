-module(serl__meta).

-define(MOD,serl).

-serl_exports(
   [{specials,
     [{'__block',{{?MOD,'sp-block'}}},
      {'__brace',{{?MOD,'sp-brace'}}}, 
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
      
      {'import-from',{{?MOD,'sp-import-from'}}},
      {'functions-import',{{?MOD,'sp-functions-import'}}},
      {'functions-export',{{?MOD,'sp-functions-export'}}},
      
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

      {'>>',{{?MOD,'mac->>'}}}
     ]}
   ]).


-serl_imports(
   [{verl,
     [{specials,all},
      {macros,all},
      {rmacros,all},
      {functions,all}
     ]}
   ]).
