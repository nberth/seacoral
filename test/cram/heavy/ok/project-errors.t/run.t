An empty C file does what?
  $ seacoral --config empty-c-file.toml
  [A]{Sc} Starting to log into `_sc/empty.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [E]{Sc} Elaboration failed: function `foo' not found in empty.c
  [124]

Meaningful errors should be reported when entries of
`array-size-mapping` do not correspond to formal inputs.
  $ seacoral --config missing-length-field.toml
  [A]{Sc} Starting to log into `_sc/basic-array.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [E]{Sc} Elaboration failed:
          function `f' does not have any formal argument named `n', nor does
          exist a global variable with this name
  [124]
  $ seacoral --config missing-array-field.toml
  [A]{Sc} Starting to log into `_sc/basic-array.c-CC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Skipping `g' (no objective left undecided)
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `g': cov: 0 uncov: 0 unkwn: 0 with 0 test
  [A]{Sc} Covered labels: {}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc} No label, no coverage
  $ seacoral --config invalid-length-field.toml
  [A]{Sc} Starting to log into `_sc/basic-array.c-CC-@3/logs/1.log'
  [A]{Sc} Initializing working environment...
  [E]{Sc} Elaboration failed:
          Unexpected type int* for field `n' (integral type expected)
  [124]

What happens on invalid coverage criteria?
  $ seacoral --config invalid-criterion.toml
  [A]{Sc} Starting to log into `_sc/basic-array.c-foo-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [E]{Sc} Labeling failed:
          Unknown coverage criterion "FOO".
          Supported criteria: [CUSTOM, EMPTY, BC, CB, CC, DC, ELO, FC, GACC, GICC, IDP, IOB, LIMIT, MCC, NCC, RCC, SLO, STMT, WM].
  [124]

What happens on unsupported coverage criteria?
  $ seacoral --config unsupported-criterion.toml
  [A]{Sc} Starting to log into `_sc/basic-array.c-cacc-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [E]{Sc} Labeling failed:
          SeaCoral does not support the coverage criterion "CACC".
          Supported criteria: [CUSTOM, EMPTY, BC, CB, CC, DC, ELO, FC, GACC, GICC, IDP, IOB, LIMIT, MCC, NCC, RCC, SLO, STMT, WM].
  [124]

What happens on an unexpected initialization function?
  $ seacoral --config bad-fixture.toml
  [A]{Sc} Starting to log into `_sc/bad-fixture.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [E]{Sc} Elaboration failed: initialization function `init' must not accept
          any argument
  [124]
