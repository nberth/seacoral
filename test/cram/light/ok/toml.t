Generate a first TOML configuration:
  $ seacoral config init
  Default configuration saved in seacoral.toml

Show its contents:
  $ cat seacoral.toml
  [logs]
  # Console log level: use 0 for no message, 1 for application-level messages
  # only, 2 to include error messages, 3 to include warnings as well, 4 for more
  # information, and 5 or any other value for full debug messages (default: 3).
  console-level = 3
  
  
  [run]
  # Directory where the working files are stored (defaults to "_sc").
  workdir = "_sc"
  
  # Maximum number of concurrent validations (defaults to 16). A null value
  # enables unlimitted parallelism.
  max-validation-concurrency = 16
  
  # When set, show labels reached during test validation in logs (false by
  # default).
  verbose-validation = false
  
  # Force preprocessing, even if it has already been performed.
  force-preprocess = false
  
  # Whether to check for syntax errors in input files prior to labeling and test
  # generation (true by default).
  enable-syntax-check = true
  
  # Employ the given orchestration strategy: 's' stands for sequential, 'p' for
  # parallel, and 'o' for optimized.  The default is "p".
  strategy = "p"
  
  # Time (in seconds) after which a test execution is considered to fail (1.000
  # by default).
  test-timeout = 1.000
  
  # Tools to use. Defaults to []. Use ["*"] to select every known tool.
  tools = []
  
  
  [project]
  # Global variables that should be ignored.
  ignored-globals = []
  
  # External libraries required to compile the project, like "m" for the math
  # library, for instance. Defaults to [].
  external-libs = []
  
  # Name for the project: this is used in logs and to name the working directory.
  # name =
  
  # C files to consider.
  # files =
  
  # Coverage criterion (case-insensitive). Launch `frama-c -lannot-list` for a
  # list of available criteria (default: "CC"). If you used custom labels, you
  # can use the "Custom" or "Empty" criterion.
  criterion = "CC"
  
  # Entrypoint for the project.
  # entrypoint =
  
  # Comma-separated list of functions to cover. Use an empty string to
  # automatically cover the entrypoint and any function it may call (this is the
  # default); note this is achieved using an analysis of the syntactic callgraph,
  # so function pointers are ignored. Use `*' to select every function defined in
  # the project.
  functions-to-cover = ""
  
  # Directories where header files are searched for (default: []).
  include-dirs = []
  
  
  [pointer-handling]
  # Maximal size of C arrays that are induced by naked pointers. Default is 1.
  max-ptr-array-length = 1
  
  # Maximal size of C strings, excluding any additional terminating '\000'.
  # Default is 20.
  max-cstring-length = 20
  
  # Maximal depth at which pointers may be non-NULL, measured in number of
  # pointer dereferences from function arguments. Default is 3.
  max-non-nil-ptr-depth = 3
  
  # Formal arguments or global variables that are declared as naked pointers to
  # characters (`char *p`), but need to be treated as '\000'-terminated C-strings
  # (default: []).
  treat-pointer-as-string = []
  
  # Pairs of variables of the form `"p:n"`, where `p` is a pointer (formal
  # argument to the entrypoint, or global variable), and `n` is a variable of
  # integral type that represents the length of the array pointed to by `p` (in
  # number of elements of type `t`, if `p` is of type `t*`).
  # array-size-mapping =
  
  # Formal arguments or global variables that are declared as naked pointers
  # (like `int *p` or `struct t * x`), but need to be treated as arrays (default:
  # []).
  treat-pointer-as-array = []
  
  
  [build-tools]
  # Path to the clang++ executable (defaults to "clang++")
  clangxx-path = "clang++"
  
  # Default pre-processor flags
  # c-pre-processor-flags =
  
  # Path to the clang executable (defaults to "clang")
  clang-path = "clang"
  
  # Path to the objcopy executable (defaults to "objcopy")
  objcopy-path = "objcopy"
  
  # Default linker flags
  # c-linker-flags =
  
  # Path to the ld executable (defaults to "ld")
  ld-path = "ld"
  
  
  [cbmc]
  # Sets the timeout of the tool (default: 0.000). If set to 0, no timeout is
  # imposed on the CBMC process.
  timeout = 0.000
  
  # Sets CBMC's mode. It can either be "cover" (standard coverage analysis),
  # "assert" (uncoverability detection with assertions) or "clabel"
  # (uncoverability detection with C labels). (default: "cover").
  mode = "cover"
  
  # Sets the loop unwinding (default: 10). If set to 0, there will not be
  # unwinding.
  unwind = 10
  
  
  [eacsl]
  # Path to the e-acsl-gcc.sh executable
  path = "e-acsl-gcc.sh"
  
  # Enables post-processing with E-ACSL (default: false)
  enable = false
  
  # E-ACSL post-processing mode. Available modes are: parallel (activated with
  # "p"), and sequential (activated with "s"). The parallel mode will switch to
  # sequential if it fails too many times.  Defaults to "sequential".
  mode = "sequential"
  
  
  [export]
  # Use `malloc` to allocate heap-side memory in generated C code. When false,
  # heap-side memory is allocated statically. Defaults to true. Forced to true in
  # combined testsuites
  malloc-heapside-memory = true
  
  # Whether to put individual tests in separate C files, or to combine them.
  # Defaults to false
  split-suite = false
  
  
  [fixtures]
  # Whether to instruct analysis tools to aim at generating tests that violate
  # the oracle, in addition to a testsuite that targets coverage.
  seek-oracle-failures = false
  
  # Initialization function for the entrypoint.  If given, this setting must be
  # the name of a function defined in either the project codebase or within a
  # file listed in `files`.  This function must not accept any argument.
  # init =
  
  # Oracle function for the entrypoint.  Constraints of `init` also apply, with
  # the requirements that the given function: (i) must accept an additional last
  # argument that is given the entrypoint's return value (if non-void); and (ii)
  # return a zero value of an integral type (like `int`) if and only if the
  # oracle fails.
  # oracle =
  
  # C files where to find additional test fixtures like initialization and oracle
  # functions.
  # files =
  
  
  [klee]
  # Activate option '--only-output-states-covering-new' (default: true)
  only-new-tests = true
  
  # Sets the timeout of the tool, in seconds; 0 means no timeout (default:
  # 30.000)
  timeout = 30.000
  
  # Choose libc version (default: "none")
  libc = "none"
  
  # Specify klee search strategy (see `klee --help` for details, use an empty
  # listfor klee defaults --- default: [])
  search = []
  
  # Maximum number of replayers running concurrently. A null value enables full
  # parallelism (this is the default), and a negative value disables on-the-fly
  # replay of test cases
  replay-max-concurrency = 0
  
  # On-the-fly replay of generated test cases (default: "yes"; possible values:
  # {inhibit, yes, delayed, force})
  replay = "yes"
  
  # Label handling mode (default: "optimize"; possible values: {ignore, naive,
  # optimize, optimize+avoid, optimize+keepall})
  label-handling = "optimize"
  
  # Keep tests that are generated because they raise errors (default: true)
  keep-error-cases = true
  
  
  [lcov]
  # Path to a directory where to output the report; if "", the report is
  # generated in a directory `lcov/report' that is situated within the project
  # working directory (defaults to "")
  output-directory = ""
  
  # Include a textual representation of the coverage report in logs
  #               (default is false)
  report-in-logs = false
  
  # Include coverage of generated testsuite files in lcov report (default is
  # false)
  include-testsuite = false
  
  # Path to the llvm-cov executable (defaults to "llvm-cov")
  llvm-cov-path = "llvm-cov"
  
  # Enable report generation via lcov (false by default). NOTE: custom
  # user-provided tests are not taken into account when computing coverage for
  # LCOV
  enable = false
  
  # Path to the lcov executable (defaults to "lcov")
  executable-path = "lcov"
  
  # Path to the genhtml executable (defaults to "genhtml")
  genhtml-path = "genhtml"
  
  # Path to the llvm-profdata executable (defaults to "llvm-profdata")
  llvm-profdata-path = "llvm-profdata"
  
  # Unless "", which is the default, name of a file where to export a textual
  # representation of the coverage report
  text-report = ""
  
  
  [libfuzzer]
  # Maximum number of seconds to wait for valid inputs from the corpus in case no
  # randomly generated seed is valid (in seconds, default: 60)
  max-starvation-time = 60
  
  # Size of the initial input corpus (default: 10; must be strictly positive)
  init-corpus-size = 10
  
  # Whether tracing of comparison instructions may be used to guide mutations
  # (default: true; using this feature may increase the amount of generated test
  # cases)
  use-cmp = true
  
  # Total time to spend while fuzzing (in seconds, default: 30.000; 0 means run
  # indefinitely)
  timeout = 30.000
  
  # Whether to restrict libfuzzers' sensitivity to labels instead of to its own
  # counters (default: false; `use-counters` and `use-cmp` have no effect when
  # used in combination with this option)
  labels-only = false
  
  # Number of individual test runs (default: -1; a strictly negative value
  # indicates an infinite number of runs)
  runs = -1
  
  # Whether to include inputs that only consist of bytes of the given values into
  # the initial corpus (default: ["\u0000", "\u0001"])
  init-corpus-with-uniform-bytes = ["\u0000", "\u0001"]
  
  # Whether libfuzzer's coverage counters may be used as features (default:
  # false; using counters may increase the amount of generated test cases)
  use-counters = false
  
  # Whether to include an input that only consists of null bytes into the initial
  # corpus (default: true); equivalent to specifying '\000' in
  # `init-corpus-with-uniform-bytes`
  init-corpus-with-nulls = true
  
  # Total time to spend while in each triage phase (in seconds, default: 0.000; 0
  # means run indefinitely)
  triage-timeout = 0.000
  
  # Seed to pass to libfuzzer.  Default is 0, which instructs libfuzzer to
  # generate a seed internally.
  seed = 0
  
  # Single-test timeout (in seconds, default: 1.000)
  micro-timeout = 1.000
  
  
  [lreplay]
  # Enable post-processing with lreplay (defaults to true)
  enable = true
  
  
  [luncov]
  # Adds extra arguments to frama-c as pair of "cmd_key:cmd_value" separated by a
  # comma (default: [])
  extra-args = []
  
  # Sets the plugins tools used by luncov. Default is ["wp"]. "eva" (or "value")
  # is also possible, but not recommended as it may report coverable labels
  # uncoverable.
  plugins = ["wp"]
  
  
  [report]
  # Output directory (default: "report")
  output-dir = "report"
  
  # Generates an HTML report (default: false)
  enable = false
  
  # Selects whether the report must be in a single page ("onepage") or with tabs
  # ("pretty") (default: "pretty")
  format = "pretty"
  
  
  [test-runner]
  # Maximum number of replayers running concurrently (default: 1; 1 or less
  # disables parallelism)
  max-concurrency = 1
  
  # Custom tests to be added to the testsuite; they are played before any
  # analysis or tests generation is started (default: [])
  custom-tests = []
  
  
