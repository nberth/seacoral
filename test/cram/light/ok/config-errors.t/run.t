An empty TOML file should not pass validation stage.
  $ seacoral --config empty.toml
  Error in project configuration: missing input file(s)
  [124]

An entrypoint field is always required.
  $ seacoral --config missing-entrypoint.toml
  Error in project configuration: missing entrypoint
  [124]

The tool should abort if every given input file is missing.
  $ seacoral --config missing-input-file.toml
  [E]{Sc} Ignoring missing input file ./missing.c
  Error in project configuration: missing input file(s)
  [124]

The tool should also abort if no tool is given...
  $ seacoral --config missing-tools.toml
  Error in project configuration: missing tools
  [124]

... or if an unknown tool is specified.
  $ seacoral --config unknown-tools.toml
  Error in project configuration: unknown tools tool1 and tool2
  [124]
  $ seacoral --config unknown-tools.toml --tools tool1
  Error in project configuration: unknown tool tool1
  [124]
  $ seacoral --config unknown-tools.toml --tools tool1,tool2,tool3
  Error in project configuration: unknown tools tool1, tool2, and tool3
  [124]
