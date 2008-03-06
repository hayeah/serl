-module(metaserl__meta).

-define(MOD,metaserl).

-serl_namespaces([]).

-serl_exports([]).

-serl_imports(
   [{serl,
     [{specials,all},
      {macros,all},
      {rmacros,all},
      {functions,all}
     ]}
   ]).
