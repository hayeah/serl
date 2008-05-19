-module(serl__meta).

-define(MOD,serl).

-serl_exports(
   [{specials,
     [{'__block',{{?MOD,'sp-block'}}},
      {'__brace',{{?MOD,'sp-brace'}}},
      {'defm',{{?MOD,'sp-defm'}}},
      {'defspecial',{{?MOD,'sp-defspecial'}}},
      %{'let2',{{?MOD,'sp-let'}}},
      {'lsc',{{?MOD,'sp-lsc'}}},
      %{'case',{{?MOD,'sp-case'}}},
      {'if',{{?MOD,'sp-if'}}},
      %{'fn',{{?MOD,'sp-fn'}}},
      

      
      {'==',{{?MOD,'sp-=='}}}, 
      {'>',{{?MOD,'sp->'}}}
      
     ]
     %%{'defm',{{?MOD,'__sp_defm'}}}
    },
    {macros,
     [{'with-genvar',{{?MOD,'mac-with-genvar'}}},
      {'let*',{{?MOD,'mac-let*'}}},

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
