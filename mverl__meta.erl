-module(mverl__meta).

-define(MOD,mverl).

-serl_exports([]).

-serl_definitions(
  [{specials,
    [{'def',{?MOD,'__sp_def'}}]
    %%{'defm',{?MOD,'__sp_defm'}}
   }
  ]).

-serl_imports(
   [{verl,
     [{specials,all},
      {macros,all},
      {rmacros,all},
      {functions,all}
     ]}
   ]).

