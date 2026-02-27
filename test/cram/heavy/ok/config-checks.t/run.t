  $ seacoral check --init --config dynamic.toml
  [A]{Sc} Starting to log into `_sc/dynamic.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...

Note: this shall emit a warning (PR pending)
  $ seacoral check --init --config dynamic.toml --treat-pointer-as-string 'a'
  [A]{Sc} Starting to log into `_sc/dynamic.c-DC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
