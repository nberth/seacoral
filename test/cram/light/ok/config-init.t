Generate a first TOML configuration:
  $ seacoral config init
  Default configuration saved in seacoral.toml

Check the configuration can indeed be loaded:
  $ touch empty.c
  $ seacoral check --tools "luncov" --files empty.c --entrypoint missing --log-level 4
  [A]{Sc} Starting to log into `_sc/empty.c-CC-@1/logs/1.log'
  [I]{Sc} Configuration loaded
          from `$TESTCASE_ROOT/seacoral.toml'

Try to generate a second one (should fail):
  $ seacoral config init
  File seacoral.toml exists: not overriding
  [124]
