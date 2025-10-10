.. _configuration:

Tool configuration
==================

Configuration file
------------------

SeaCoral can be configured differently via a `toml` file with the option `-c`.

Options can be `project-wide` or `runtime`, depending on how they influence
the analysis. If a `project-wide` option is changed between two analyses,
a new separate project will be created and the old results will not be used.
If a `runtime` option is changed beween two analyses, it will re-use the
previous project.

Note that you can generate a new configuration file with the option `--init`.
It will create a `seacoral.toml` that you can use to start the analysis.

Main configuration options [sc]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- `force_preprocess`: Forces the preprocessing, even if it already has been performed. (default: false) (runtime knob)
- `seacoral_srcdir`: Selects the orchestrator's source path (default is current directory) (project-wide knob)
- `files`: The list of files of the project (default: []) (project-wide knob)
- `I`: Adds the directory to the list of directories to be searched for header files during preprocessing (default: []) (project-wide knob)
- `output_path`: Sets the path where the output files are stored. (default: "_sc") (runtime knob)
- `entrypoint`: Select the entrypoint to analyze (default: ""). (project-wide knob)
- `strategy`: Uses a specific orchestration strategy: 's' for sequential, 'p' for parallel and 'o' for optimized (default: "p"). Custom strategies can be used (see Sc_strategy for more information) (runtime knob)
- `debug`: Selects the console log level: 0 for no message, 1 for default messages, 2 for error messages, 3 for warnings, 4 for information, 5 or any other value for debug messages (default: 2). Selecting a level automatically activates all lower level messages. (runtime knob)
- `tools`: List the tools to use. (default: []) (runtime knob)

Core configuration options [core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- `array_size_mapping`: Lists the pair of variables `(arr, size)` of the form `"arr:size"` where `arr` is an array (formal or global) and `size` is an integer variable representing its size (default: []) (runtime knob)
- `max_cstring_length`: Maximal size of C strings, excluding any additional terminating '\000'.  Default is 20. (project-wide knob)
- `treat_pointer_as_array`: Formal arguments or global variables that are declared as naked pointers (like `int *p` or `struct t * x`), but need to be treated as arrays (default: []) (project-wide knob)
- `treat_pointer_as_string`: Formal arguments or global variables that are declared as naked pointers to characters (`char \*p`), but need to be treated as '\000'-terminated C-strings (default: []) (project-wide knob)
- `criterion`: Selects the cover criterion. Launch `frama-c -lannot-list` for a list of available criteria (default: "CC"). If you used custom labels, you can use the criteria "custom" or "Empty". (project-wide knob)
- `max_non_nil_ptr_depth`: Maximal depth at which pointers may be non-NULL, measured in number of pointer dereferences from function arguments. Default is 3. (project-wide knob)
- `external_libs`: Lists external libraries required to compile the project, like "m" for the math library for example (default: []) (runtime knob)
- `max_ptr_array_length`: Maximal size of C arrays that are induced by naked pointers. Default is 1. (project-wide knob)
- `functions-to-annot`: Selects the functions to cover. (default: all).  Use "*", which is the default, to select all functions. (project-wide knob)

Project configuration options [project-manager]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- `lreplay`: Enables post-processing with lreplay. (runtime knob)
- `eacsl`: Enables post-processing with eacsl. There are two modes: parallel (activated with "p") and sequential (activated with "s"). The parallel mode will switch to sequential if it fails too many times. (project-wide knob)

Klee configuration options [klee]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- `label_handling`: Label handling mode (default: "optimize"; possible values: {ignore, naive, optimize, optimize+avoid, optimize+keepall}) (project-wide knob)
- `timeout`: Sets the timeout of the tool, in seconds; 0 means no timeout (default: 10.000) (runtime knob)
- `libc`: Choose libc version (default: "none") (runtime knob)
- `search`: Specify klee search strategy (see `klee --help` for details, use an empty listfor klee defaults --- default: []) (runtime knob)
- `keep_error_cases`: Keep tests that are generated because they raise errors (default: true) (runtime knob)
- `only_new_tests`: Activate option '--only-output-states-covering-new' (default: true) (runtime knob)
- `replay_max_concurrency`: Maximum number of replayers running concurrently (default: 1; a null or negative value disables on-the-fly replay of test cases) (runtime knob)
- `replay`: On-the-fly replay of generated test cases (default: "yes"; possible values: {inhibit, yes, delayed, force}) (runtime knob)

Cbmc configuration options ([cbmc])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- `timeout`: Sets the timeout of the tool (default: 0.000). If set to 0, the process will not stop before its end. (runtime knob)
- `unwind`: Sets the loop unwinding (default: 100). If set to 0, there will not be unwinding. (runtime knob)

Test_runner configuration options [test_runner]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- `custom_tests`: Adds custom tests to the test suite and plays them before starting the analysis (default: []) (runtime knob)

Libfuzzer configuration options [libfuzzer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- `init_corpus_size`: Size of the initial input corpus (default: 10; must be strictly positive) (runtime knob)
- `use_counters`: Whether libfuzzer's coverage counters may be used as features (default: false; using counters may increase the amount of generated test cases) (runtime knob)
- `init_corpus_with_nulls`: Whether to include an input that only consists of null bytes into the initial corpus (default: true) (runtime knob)
- `use_cmp`: Whether tracing of comparison instructions may be used to guide mutations (default: true; using this feature may increase the amount of generated test cases) (runtime knob)
- `timeout`: Total time to spend while fuzzing (in seconds, default: 30.000; 0 means run indefinitely) (runtime knob)
- `labels_only`: Whether to restrict libfuzzers' sensitivity to labels instead of to its own counters (default: false; `use_counters` and `use_cmp` have no effect when used in combination with this option) (project-wide knob)
- `runs`: Number of individual test runs (default: -1; a strictly negative value indicates an infinite number of runs) (runtime knob)
- `micro_timeout`: Single-test timeout (in seconds, default: 1200.000) (runtime knob)
- `triage_timeout`: Total time to spend while in each triage phase (in seconds, default: 0.000; 0 means run indefinitely) (runtime knob)
- `seed`: Seed to pass to libfuzzer.  Default is 0, which instructs libfuzzer to generate a seed internally. (runtime knob)

Afl++ configuration options ["afl++"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- `init_corpus_size`: Size of the initial input corpus (default: 1; must be strictly positive) (runtime knob)
- `init_corpus_with_nulls`: Whether to include an input that only consists of null bytes into the initial corpus (default: true) (runtime knob)
- `labels_only`: Whether to restrict AFL++'s sensitivity to labels instead of to its own counters (default: false); not yet operational (project-wide knob)
- `runs`: Number of individual test runs (default: 1000; a negative value indicates an infinite number of runs) (runtime knob)
- `seed`: Seed to pass to AFL++. Default is 0. (runtime knob)

Luncov configuration options [luncov]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- `extra-args`: Adds extra arguments to frama-c as pair of "cmd_key:cmd_value" separated by a comma (default: []) (runtime knob)
- `plugins`: Sets the plugins tools used by luncov. Default is ["wp"]. "eva" (or "value") is also possible, but not recommended as it may report coverable labels uncoverable (runtime knob)


About named locations
.....................

The core configuration options `treat_pointer_as_array/cstring` require a list
of "named locations". Named locations represent a position in the memory layout
of the test input.
It is possible to directly write variables (for example, `foo` in
`treat_pointer_as_array` will specify the pointer `foo` must be treated as a reference to an array).

It is also possible to specify indirect pointers. For example, for an input 
`char **foo`, you may add `foo[_]` in `treat_pointer_as_cstring`
to specify that pointers accessed via `*foo` or `foo[i]`, for some `i`, 
point to C character strings.
You may also specify a path in a structure (`foo.bar.tri...`), or even every
structure of a given type (`{struct:t}.foo.bar.tri...`).
Use of the `->` operator is also permitted to represent the access of a pointer
field (`x->y` is equivalent to `x[_].y`).

A similar syntax is used for `array_size_mapping`: this list is a list of pairs of
named locations separated by ':'.
For example, the string `x.y:N` states that the field `y` of `x` is an array that
must have size `N`, where `x` and `N` either are globals or formals of the
tested function.
It also is possible for a structure to reference itself:
`{struct:t}.y:.N` states that for any structure `s` of type `t` in the input,
`s.y` is an array whose size must be equal to `s.N`.
..

Environment variables
---------------------

================== ================================================== ==============
Variable           Documentation                                      Default value
================== ================================================== ==============
CLANG              Executable name of `clang`                         `clang`
CLANGXX            Executable name of `clang++`                       `clang++`
CPPFLAGS           Flags to use with `cpp`                            ""
EACSL_GCC          Executable name of `e-acsl-gcc`                    `e-acsl-gcc.sh`
LDFLAGS            Flags to use with `clang`                          ""
OPAM_SWITCH_PREFIX The prefix of your opam switch
SC_TIDY_COMMANDS   Prints processes logs better                       "false"
================== ================================================== ==============
