This first file uses a forbidden character in one of its table keys.
  $ seacoral --config invalid_key.toml
  Error in invalid_key.toml at line 1 at column 4 (position 4): lexing: empty token
  [afl++]
   ^^^^^ 
  Did you forget quotes?
  
  [124]

This file uses an unknown key
  $ seacoral --config unknown_key.toml
  Error in section [project]: Unknown toml key "entry-point" (accepted entries
  are: ignored-globals, external-libs, name, files, criterion, entrypoint,
  functions-to-cover, include-dirs)
  [124]

In this file, the entrypoint is not quoted
  $ seacoral --config missing_quotes.toml
  Error in missing_quotes.toml at line 3 at column 18 (position 51)
    entrypoint = bar
                 ^^^
  Unexpected end of line, did you forget quotes?
  [124]
