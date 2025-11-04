(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Types

(** {2 Run Section} *)

let run_section =
  let default =
    {
      config_input = None;
      workdir = "_sc";
      force_preprocess = false;
      enable_syntax_check = true;
      tools = [];
      strategy = AllParallel;
      test_timeout = 1.;
      max_validation_concurrency = 16;
      verbose_validation = false;
    }
  in
  Sc_config.Section.define "run" ~default ~entries:Sc_config.Eztoml.[
      bool'
        ~key:"force-preprocess"
        ~doc:"Force preprocessing, even if it has already been performed."
        ~default:default.force_preprocess
        ~runtime:true
        ~as_flag:(Positive { keys = `Same; doc = `Same })
        (fun c f -> {c with force_preprocess = f})
        (fun c -> c.force_preprocess);
      string
        ~key:"workdir"
        ~doc:"Directory where the working files are stored (defaults to %a)."
        ~default:default.workdir
        ~runtime:true
        (fun c o -> {c with workdir = o})
        (fun c -> c.workdir);
      string
        ~key:"strategy"
        ~doc:"Employ the given orchestration strategy: 's' stands for \
              sequential, 'p' for parallel, and 'o' for optimized.  The default \
              is %a."
        ~default:"p"
        ~runtime:true
        (fun c s ->
           let s, tools = match s with
             | "p" | "parallel" ->
                 AllParallel, c.tools
             | "s" | "sequential" ->
                 AllSequential, c.tools
             | "o" | "optimized" ->
                 Optimized, c.tools
             | str ->
                 let strat = Sc_strategy.read str in
                 let tools = Sc_strategy.tools strat in
                 Custom strat, tools
           in
           {c with strategy = s; tools})
        (fun c -> match c.strategy with
           | AllParallel -> "p"
           | AllSequential -> "s"
           | Optimized -> "o"
           | Custom s -> Format.asprintf "%a" Sc_strategy.Printer.print s);
      string_list
        ~key:"tools"
        ~doc:"Tools to use. Defaults to %a. Use [\"*\"] to select every known \
              tool."
        ~default:default.tools
        ~runtime:true
        (fun c tools -> { c with tools })
        (fun c -> c.tools);
      bool
        ~key:"enable-syntax-check"
        ~doc:"Whether to check for syntax errors in input files prior to \
              labeling and test generation (%a by default)."
        ~default:default.enable_syntax_check
        ~runtime:true
        ~as_flag:(Both { pos_keys = `Same;
                         pos_doc = `Alt "Enable check for syntax errors in input \
                                         files prior to labeling and test \
                                         generation";
                         neg_keys = `Alt ["disable-syntax-check"];
                         neg_doc = `Alt "Disable check for syntax errors in \
                                         input files prior to labeling and test \
                                         generation" })
        (fun c b -> { c with enable_syntax_check = b })
        (fun c -> c.enable_syntax_check);
      float
        ~key:"test-timeout"
        ~doc:"Time (in seconds) after which a test execution is considered to \
              fail (%a by default)."
        ~default:default.test_timeout
        ~runtime:true
        (fun c t -> { c with test_timeout = t })
        (fun c -> c.test_timeout);
      positive_int
        ~key:"max-validation-concurrency"
        ~doc:"Maximum number of concurrent validations (defaults to %a). A null \
              value enables unlimitted parallelism."
        ~env:"MAX_VALIDATION_CONCURRENCY"
        ~default:default.max_validation_concurrency
        ~runtime:true
        (fun c i -> { c with max_validation_concurrency = i })
        (fun c -> c.max_validation_concurrency);
      bool
        ~key:"verbose-validation"
        ~doc:"When set, show labels reached during test validation in logs (%a \
              by default)."
        ~env:"VERBOSE_VALIDATION"
        ~runtime:true       (* CHECKME: validator recompilation may be needed *)
        ~as_flag:(Positive { keys = `Same; doc = `Same })
        ~default:default.verbose_validation
        (fun c b -> { c with verbose_validation = b })
        (fun c -> c.verbose_validation);
    ]

(** {2 Project Section} *)

let cover_functions c functions =
  { c with
    functions_to_cover = match String.(List.map trim functions) with
      | [] | [""] -> `Auto
      | ["*"] -> `All
      | funs -> `Only funs }

let annoted_functions_to_string = function
  | `All -> "*"
  | `Auto -> ""
  | `Only l -> String.concat "," l

let project_section =
  let default =
    {
      name = "";                                               (* equiv. None *)
      input_files = [];
      entrypoint = "";
      cover_criterion = "CC";
      functions_to_cover = `Auto;
      header_dirs = [];
      external_libs = [];
      ignored_globals = [];
    }
  in
  Sc_config.Section.define "project" ~default ~entries:Sc_config.Eztoml.[
      string'
        ~key:"name"
        ~doc:"Name for the project: this is used in logs and to name the working \
              directory."
        ~runtime:false
        (fun c n -> { c with name = n })
        (fun c -> c.name);
      string_list'
        ~key:"files"
        ~arg_alias:["sources"; "inputs"]
        ~doc:"C files to consider."
        ~runtime:false
        (fun c f -> {c with input_files = f})
        (fun c -> c.input_files);
      string'
        ~key:"entrypoint"
        ~doc:"Entrypoint for the project."
        ~runtime:false
        (fun c e -> {c with entrypoint = e})
        (fun c -> c.entrypoint);
      string
        ~key:"criterion"
        ~doc:"Coverage criterion (case-insensitive). Launch `frama-c \
              -lannot-list` for a list of available criteria (default: %a). If \
              you used custom labels, you can use the \"Custom\" or \"Empty\" \
              criterion."
        ~default:default.cover_criterion
        ~runtime:false
        (fun c cc -> {c with cover_criterion = cc})
        (fun c -> c.cover_criterion);
      string'
        ~key:"functions-to-cover"
        ~doc:"Comma-separated list of functions to cover. Use an empty string to \
              automatically cover the entrypoint and any function it may call \
              (this is the default); note this is achieved using an analysis of \
              the syntactic callgraph, so function pointers are ignored. Use `*' \
              to select every function defined in the project."
        ~default:""
        ~runtime:false
        (fun c s -> cover_functions c (String.split_on_char ',' s))
        (fun c -> annoted_functions_to_string c.functions_to_cover);
      string_list
        ~key:"include-dirs"
        ~arg_alias:["I"; "include"]
        ~doc:"Directories where header files are searched for (default: %a)."
        ~default:default.header_dirs
        ~runtime:false
        (fun c h -> {c with header_dirs = h})
        (fun c -> c.header_dirs);
      string_list
        ~key:"external-libs"
        ~doc:"External libraries required to compile the project, like \"m\" for \
              the math library, for instance. Defaults to %a."
        ~default:default.external_libs
        ~runtime:true
        (fun c sl -> { c with external_libs = sl })
        (fun c -> c.external_libs);
      string_list'
        ~key:"ignored-globals"
        ~doc:"Global variables that should be ignored."
        ~default:default.ignored_globals
        ~runtime:false
        (fun c sl -> { c with ignored_globals = sl })
        (fun c -> c.ignored_globals);
    ]

(** {2 Fixtures Section} *)

let fixtures_section =
  let default =
    {
      files = [];
      init = "";
      oracle = "";
      seek_oracle_failures = false;
    }
  in
  Sc_config.Section.define "fixtures" ~default ~entries:Sc_config.Eztoml.[
      string_list'
        ~key:"files"
        ~doc:"C files where to find additional test fixtures like initialization \
              and oracle functions."
        ~runtime:false
        (fun c f -> {c with files = f})
        (fun c -> c.files);
      string'
        ~key:"init"
        ~doc:"Initialization function for the entrypoint.  If given, this \
              setting must be the name of a function defined in either the \
              project codebase or within a file listed in `files`.  Arguments of \
              this function must also exactly match those of the entrypoint."
        ~runtime:false
        (fun c e -> {c with init = e})
        (fun c -> c.init);
      string'
        ~key:"oracle"
        ~doc:"Oracle function for the entrypoint.  Constraints of `init` also \
              apply, with the requirements that the given function: (i) must \
              accept an additional last argument that is given the entrypoint's \
              return value (if non-void); and (ii) return a zero value of an \
              integral type (like `int`) if and only if the oracle fails."
        ~runtime:false
        (fun c cc -> {c with oracle = cc})
        (fun c -> c.oracle);
      bool'
        ~key:"seek-oracle-failures"
        ~doc:"Whether to instruct analysis tools to aim at generating tests that \
              violate the oracle, in addition to a testsuite that targets \
              coverage."
        ~default:default.seek_oracle_failures
        ~runtime:false
        ~as_flag:(Both { pos_keys = `Alt ["seek-oracle-failures"];
                         pos_doc = `Same;
                         neg_keys = `Alt ["ignore-oracle-failures"];
                         neg_doc = `Alt "Opposite of $(b,--seek-oracle-failures)"})
        (fun c b -> {c with seek_oracle_failures = b})
        (fun c -> c.seek_oracle_failures);
    ]

(** {2 Pointer-handling Section} *)

let pointer_handling_section =
  let default =
    {
      max_ptr_array_length = 1;
      max_non_nil_ptr_depth = 3;
      max_cstring_length = 20;
      treat_pointer_as_array = [];
      treat_pointer_as_cstring =  [];
      array_size_mapping = [];
    }
  in
  Sc_config.Section.define "pointer-handling" ~default ~entries:Sc_config.Eztoml.[
      int
        ~key:"max-ptr-array-length"
        ~doc:"Maximal size of C arrays that are induced by naked \
              pointers. Default is %a."
        ~default:default.max_ptr_array_length
        ~runtime:false
        (fun c i -> { c with max_ptr_array_length = i})
        (fun c -> c.max_ptr_array_length);
      int
        ~key:"max-non-nil-ptr-depth"
        ~doc:"Maximal depth at which pointers may be non-NULL, measured in \
              number of pointer dereferences from function arguments. Default is \
              %a."
        ~default:default.max_non_nil_ptr_depth
        ~runtime:false
        (fun c i -> { c with max_non_nil_ptr_depth = i})
        (fun c -> c.max_non_nil_ptr_depth);
      int
        ~key:"max-cstring-length"
        ~doc:"Maximal size of C strings, excluding any additional terminating \
              '\\000'. Default is %a."
        ~default:default.max_cstring_length
        ~runtime:false
        (fun c i -> { c with max_cstring_length = i})
        (fun c -> c.max_cstring_length);
      string_list
        ~key:"treat-pointer-as-array"
        ~doc:"Formal arguments or global variables that are declared as naked \
              pointers (like `int *p` or `struct t * x`), but need to be treated \
              as arrays (default: %a)."
        ~default:default.treat_pointer_as_array
        ~runtime:false
        (fun c sl -> { c with treat_pointer_as_array = sl })
        (fun c -> c.treat_pointer_as_array);
      string_list
        ~key:"treat-pointer-as-string"
        ~doc:"Formal arguments or global variables that are declared as naked \
              pointers to characters (`char *p`), but need to be treated as \
              '\\000'-terminated C-strings (default: %a)."
        ~default:default.treat_pointer_as_cstring
        ~runtime:false
        (fun c sl -> { c with treat_pointer_as_cstring = sl })
        (fun c -> c.treat_pointer_as_cstring);
      string_list'
        ~key:"array-size-mapping"
        ~doc:"Pairs of variables of the form `\"p:n\"`, where `p` is a pointer \
              (formal argument to the entrypoint, or global variable), and `n` \
              is a variable of integral type that represents the length of the \
              array pointed to by `p` (in number of elements of type `t`, if `p` \
              is of type `t*`)."
        ~runtime:false
        (fun c sl -> { c with array_size_mapping = sl })
        (fun c -> c.array_size_mapping);
    ]
