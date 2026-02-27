  $ seacoral check --init --config dynamic.toml
  [A]{Sc} Starting to log into `_sc/dynamic.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  $ seacoral check --init --config dynamic.toml --treat-pointer-as-string 'a'
  [A]{Sc} Starting to log into `_sc/dynamic.c-DC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [W]{Sc_project} Ignoring `string' specification for pointer `a' that is
                  already constrained as an array with size variable `n'.
  $ seacoral check --init --config simple.toml
  [A]{Sc} Starting to log into `_sc/simple.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  $ seacoral check --init --config simple.toml --treat-pointer-as-string '{struct t}.a'
  [A]{Sc} Starting to log into `_sc/simple.c-CC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [W]{Sc_project} Ignoring `string' specification for pointer field `a' in
                  `struct t' that is already constrained as an array with size
                  field `n'.
