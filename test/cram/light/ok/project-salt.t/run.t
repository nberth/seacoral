First, run for `foo`:
  $ seacoral check --config config4foo.toml
  [A]{Sc} Starting to log into `_sc/basic.c-CC-@1/logs/1.log'

Then for `bar`:
  $ seacoral check --config config4bar.toml
  [A]{Sc} Starting to log into `_sc/basic.c-CC-@2/logs/1.log'

And finally for `foo` from `bar`'s configuration.  This re-uses
the workspace created for `foo` above since the other project
parameters are identical.
  $ seacoral check --config config4bar.toml --entrypoint foo
  [A]{Sc} Starting to log into `_sc/basic.c-CC-@3/logs/2.log'
