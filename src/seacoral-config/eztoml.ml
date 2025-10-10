(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Basics
open Toml.Types

module TYPES = struct

  type toml_file =
    {
      file: [`toml] Sc_sys.File.file;
      toml_contents: string;
    }

end
type file = TYPES.toml_file

open TYPES

(* Internal row descriptions *)

type 'c spec =
  | Spec: 'a typ * ('a, 'c) act * 'a doc -> 'c spec

and _ typ =
  | Unit: unit typ
  | Bool: { as_flag: cmdline_flag_type } -> bool typ
  | Int: int typ
  | Float: float typ
  | String: string typ
  | List: 'a typ -> 'a list typ
  | Dict: 'a typ -> (string * 'a) list typ

and cmdline_flag_type =
  | Valued
  | Positive of
      {
        keys: [`Same | `Alt of string list];
        doc:  [`Same | `Alt of string];
      }
  | Negative of
      {
        keys: [`Same | `Alt of string list];
        doc:  [`Same | `Alt of string];
      }
  | Both of
      {
        pos_keys: [`Same | `Alt of string list];
        pos_doc:  [`Same | `Alt of string];
        neg_keys: [`Same | `Alt of string list];
        neg_doc:  [`Same | `Alt of string];
      }

and ('a, 'c) act =
  {
    get: ('c -> 'a);
    set: ('c -> 'a -> 'c);
    check: ('a -> bool) option;
    check_descr: PPrt.ufmt option;
  }
and 'a doc =
  | With_default of
      {
        default: 'a;
        doc: ('a PPrt.pp -> 'a -> unit) PPrt.fmt;
      }
  | Without_default of
      {
        default: 'a option;
        doc: PPrt.ufmt;
      }

let pp_doc: 't PPrt.pp -> 't doc -> PPrt.pu = fun pp doc ppf ->
  match doc with
  | With_default { default; doc } ->
      PPrt.string_to (Format.pp_print_text ppf) doc pp default
  | Without_default { doc; _ } ->
      PPrt.string_to (Format.pp_print_text ppf) doc

let rec string_of_typ: type a. a typ -> string = function
  | Unit -> "unit"
  | Bool _ -> "bool"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | List t -> Fmt.str "%s list" (string_of_typ t)
  | Dict t -> Fmt.str "%s dict" (string_of_typ t)

(* Row definitions: *)

open Eztoml_helpers

type 'c row =
  {
    key: string;
    arg_alias: string list;                     (* alias when on command line *)
    env: string option;
    spec: 'c spec;

    runtime : bool;
    (* If a row is project related, changing it between two analysis will
       make them two different analysis. If not, the results of the previous
       analysis will be re-used by the previous one. *)
  }

type ('a, 'c) row_def_with_default_in_doc =
  ('a, ('c -> 'a -> 'c) -> ('c -> 'a) -> 'c row) def_with_default_in_doc
type ('a, 'c) row_def_without_default_in_doc =
  ('a, ('c -> 'a -> 'c) -> ('c -> 'a) -> 'c row) def_without_default_in_doc
type ('a, 'c) row_def_checked =
  ('a, ('c -> 'a -> 'c) -> ('c -> 'a) -> 'c row) checked_def_with_default_in_doc

let invalid_arg fmt =
  Fmt.invalid_arg "%s.%(%)" __MODULE__ fmt

let check_key k =
  String.iter begin function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' ->
        ()
    | '_' ->
        (* Using standard eprintf because logs are not configured when sections
           are defined. Besides, these errors are meant for the developpers,
           not the user. *)
        Fmt.epr "Forbidden character '_' in key %S, please replace with '-'@." k;
        invalid_arg "check_key"
    | c ->
        Fmt.epr "Forbidden characher %C in key %S@." c k;
        invalid_arg "check_key"
  end k

let _mk typ ~key ?(arg_alias = []) ?env ~doc ?(runtime = false)
    ?check ?check_descr ~default set get =
  check_key key;
  List.iter check_key arg_alias;
  {
    key;
    arg_alias;
    env;
    spec = Spec (typ, { get; set; check; check_descr },
                 With_default { default; doc });
    runtime;
  }
let _mk' typ ~key ?(arg_alias = []) ?env ~doc ?(runtime = false)
    ?check ?check_descr ?default set get =
  check_key key;
  List.iter check_key arg_alias;
  {
    key;
    arg_alias;
    env;
    spec = Spec (typ, { get; set; check; check_descr },
                 Without_default { default; doc });
    runtime;
  }

let bool_as_flag = Positive { keys = `Same; doc = `Same }

let bool  ?(as_flag = bool_as_flag) ~key = _mk  (Bool { as_flag }) ~key
let bool' ?(as_flag = bool_as_flag) ~key = _mk' (Bool { as_flag }) ~key

let _Bool = Bool { as_flag = bool_as_flag }

let unit         ~key = _mk          Unit  ~key

let bool_list    ~key = _mk  (List  _Bool) ~key
let bool_list'   ~key = _mk' (List  _Bool) ~key
let bool_dict    ~key = _mk  (Dict  _Bool) ~key
let bool_dict'   ~key = _mk' (Dict  _Bool) ~key

let int          ~key = _mk           Int  ~key
let int'         ~key = _mk'          Int  ~key
let int_list     ~key = _mk  (List    Int) ~key
let int_list'    ~key = _mk' (List    Int) ~key
let int_dict     ~key = _mk  (Dict    Int) ~key
let int_dict'    ~key = _mk' (Dict    Int) ~key

let float        ~key = _mk         Float  ~key
let float'       ~key = _mk'        Float  ~key
let float_list   ~key = _mk  (List  Float) ~key
let float_list'  ~key = _mk' (List  Float) ~key
let float_dict   ~key = _mk  (Dict  Float) ~key
let float_dict'  ~key = _mk' (Dict  Float) ~key

let string       ~key = _mk        String  ~key
let string'      ~key = _mk'       String  ~key
let string_list  ~key = _mk  (List String) ~key
let string_list' ~key = _mk' (List String) ~key
let string_dict  ~key = _mk  (Dict String) ~key
let string_dict' ~key = _mk' (Dict String) ~key

let strictly_positive_int =
  int
    ~check:(fun i -> i > 0)
    ~check_descr:"strictly@ positive@ Integer@ expected"

let positive_int =
  int
    ~check:(fun i -> i >= 0)
    ~check_descr:"positive@ Integer@ expected"

(* --- *)

let apply: type t. (t, 'c) act -> 'c -> t -> ('c, [> `BadValue]) result =
  fun { set; check; _ } c v ->
  match check with
  | Some check when not (check v) -> Error `BadValue
  | _ -> Ok (set c v)

let rec pp_of_typ: type t. t typ -> Format.formatter -> t -> unit = function
  | Unit ->
      Fmt.any "()"
  | Bool _ ->
      Fmt.bool
  | String ->
      Fmt.using (fun s -> Toml.Printer.string_of_value (TString s)) Fmt.string
  | Float ->
      Fmt.fmt "%.3f"
  | Int ->
      Fmt.int
  | List t ->
      PPrt.pp_lst (pp_of_typ t)
  | Dict t ->                                 (* FIXME: not proper inline TOML *)
      PPrt.pp_lst Fmt.(pair ~sep:(any ":") string (pp_of_typ t))

let apply_conv convert a c v =
  try apply a c (convert v)
  with Invalid_argument _ -> Error `BadType

let apply_dict convert a c l =
  let exception Err of [`BadType | `BadValue] in
  try
    apply a c @@ List.map begin fun elt ->
      match String.split_on_char ':' elt with
      | [key; bnd] -> key, convert bnd
      | _ -> raise (Err `BadValue)
    end l
  with Err e -> Error e

(** Marshall the OCaml value of a given configuration row into a string. *)
let marshall_row { spec = Spec (_, { get; _ }, _); _ } config : string =
  Marshal.to_string (get config) []

let apply_row (c: 'config) (row: 'config row) (key: Table.key) value : 'config =
  let res = match row.spec, value with
    | Spec (Unit, a, _), _ -> apply a c ()

    | Spec (Bool _, a, _), TBool b   -> apply a c b
    | Spec (Bool _, a, _), TString s -> apply_conv bool_of_string a c s

    | Spec (String, a, _), TString s -> apply a c s
    | Spec (String, a, _), TInt i    -> apply a c (string_of_int i)
    | Spec (String, a, _), TFloat fl -> apply a c (string_of_float fl)
    | Spec (String, a, _), TBool b   -> apply a c (string_of_bool b)

    | Spec (Float, a, _), TFloat fl -> apply a c fl
    | Spec (Float, a, _), TInt i    -> apply a c (float_of_int i)

    | Spec (Int, a, _), TInt i    -> apply a c i
    | Spec (Int, a, _), TString i -> apply_conv int_of_string a c i

    | Spec (List Bool _, a, _), TArray NodeBool l   -> apply a c l
    | Spec (List Int   , a, _), TArray NodeInt l    -> apply a c l
    | Spec (List Float , a, _), TArray NodeFloat l  -> apply a c l
    | Spec (List String, a, _), TArray NodeString l -> apply a c l
    | Spec (List _     , a, _), TArray NodeEmpty    -> apply a c []

    | Spec (Dict Bool _, a, _), TString s -> apply_dict bool_of_string a c [s]
    | Spec (Dict Int   , a, _), TString s -> apply_dict int_of_string a c [s]
    | Spec (Dict Float , a, _), TString s -> apply_dict float_of_string a c [s]
    | Spec (Dict String, a, _), TString s -> apply_dict Fun.id a c [s]

    | Spec (Dict Bool _, a, _),
      TArray NodeString l      -> apply_dict bool_of_string a c l
    | Spec (Dict Int   , a, _),
      TArray NodeString l      -> apply_dict int_of_string a c l
    | Spec (Dict Float , a, _),
      TArray NodeString l      -> apply_dict float_of_string a c l
    | Spec (Dict String, a, _),
      TArray NodeString l      -> apply_dict Fun.id a c l
    | Spec (Dict _     , a, _),
      TArray NodeEmpty         -> apply a c []

    | Spec (_, _, _), _ -> Error `BadType
  in
  match res, row.spec with
  | Ok res, _ ->
      res
  | Error `BadType, Spec (t, _, _) ->
      raise @@ Errors.Bad_value_type_in_toml_table (key, value, string_of_typ t)
  | Error `BadValue, Spec (t, { check_descr; _ }, i) ->
      raise @@ Errors.Bad_value_in_toml_table {
        key;
        value;
        expect = (match check_descr with Some f -> ";@ " ^^ f | None -> "");
        pp_doc = pp_doc (pp_of_typ t) i;
      }

(* Whole schema definitions *)

type 'config schema = (Table.Key.t, 'config row) Hashtbl.t

let build (rl : 'config row list) : 'config schema =
  let res = Hashtbl.create 7 in
  List.iter (fun r -> Hashtbl.add res (Table.Key.of_string r.key) r) rl;
  res

let keys (schema: _ schema) =
  List.of_seq @@
  Seq.map Toml.Types.Table.Key.to_string @@
  Hashtbl.to_seq_keys schema

let core_digest (schema: _ schema) section =
  let buff = Buffer.create 42 in
  Hashtbl.iter begin fun _ row ->
    if row.runtime
    then ()
    else Buffer.add_string buff (marshall_row row section)
  end schema;
  Digest.bytes (Buffer.to_bytes buff)

let parse (config: 'config) table schema : 'config =
  Table.fold begin fun key value config ->
    match Hashtbl.find_opt schema key with
    | None -> raise @@ Errors.Unknown_toml_key (Table.Key.to_string key)
    | Some row -> apply_row config row key value
  end table config

let print_row ppf { key; spec = Spec (t, _, doc); runtime; _ } =
  let pp_kind ppf runtime =
    if runtime
    then Fmt.(styled `Faint @@ any "[runtime@ knob]") ppf ()
    else Fmt.(styled `Bold @@ any "[project-defining@ knob]") ppf ()
  in
  Fmt.pf ppf "@[<v 4>  @[<h>%a@ %a@]:@;@[<hov>%t@]@]"
    Fmt.(styled `Cyan string) key
    (Fmt.styled `Yellow @@ pp_kind) runtime
    (pp_doc (pp_of_typ t) doc)

let print_doc (ppf: Format.formatter) (schema: 'config schema) =
  Hashtbl.iter (fun _ -> Fmt.pf ppf "%a@;" print_row) schema

let pp_spec_doc ppf (Spec (t, _, doc)) =
  pp_doc (pp_of_typ t) doc ppf

let buff = Buffer.create 80
let pp_spec_doc_comments: Format.formatter -> _ spec -> unit = fun ppf spec ->
  Buffer.clear buff;
  let bppf = Fmt.with_buffer buff in
  Format.pp_set_geometry ~max_indent:72 ~margin:78 bppf;
  Fmt.pf bppf "@[%a@]%!" pp_spec_doc spec;
  let doc_lines = String.split_on_char '\n' (Buffer.contents buff) in
  List.iter (Fmt.pf ppf "# %s@;") doc_lines

let print_row_as_toml_line ?(with_doc=true) ?value
    ppf { key; spec = Spec (t, {get; _}, doc) as spec; _} =
  if with_doc then pp_spec_doc_comments ppf spec;
  match value with
  | Some v ->
      Fmt.pf ppf "%s = %a@;" key (pp_of_typ t) (get v)
  | None ->
      match doc with
      | With_default { default; _ }
      | Without_default { default = Some default; _ } ->
          Fmt.pf ppf "%s = %a@;" key (pp_of_typ t) default
      | Without_default { default = None; _ } ->
          Fmt.pf ppf "# %s =@;" key

(* https://toml.io/en/v1.0.0#keys *)
let is_valid_key =
  let rx = Str.regexp "[A-Za-z0-9_-]+$" in
  fun str -> Str.string_match rx str 0

let print_key fmt str =
  Format.fprintf fmt begin
    if is_valid_key str then "%s" else "%S"
  end str

let print_as_toml_file ?with_doc ?value ppf ((main_key, schema): string * 'a schema) =
  Fmt.pf ppf "@[<v 0>[%a]@;<0>@[<v 0>" print_key main_key;
  Hashtbl.iter begin fun _ row ->
    Fmt.pf ppf "%a@;" (print_row_as_toml_line ?with_doc ?value) row
  end schema;
  Fmt.pf ppf "@]@\n@]"

(** Cmdliner helpers *)

let sanitize s =
  String.map (function '_' -> '-' | c -> c) s

let key_with_prefix ?prefix key =
  let prefix: string option = Option.map sanitize prefix in
  let key = sanitize key in
  match prefix with
  | None -> key
  | Some p -> Format.sprintf "%s-%s" p key

let pp_spec_doc ppf (Spec (t, _, doc)) =
  pp_doc (pp_of_typ t) doc ppf

(* --- *)

let cmdline_flag
    ~(bool_conv: 'bool Cmdliner.Arg.conv) ~(tt: 'bool) ~(neg: 'bool -> 'bool)
    ~keys ?env ~doc ?docs (sdoc: bool doc)
  : cmdline_flag_type -> 'bool option Cmdliner.Term.t =
  let open Cmdliner in
  let ikeys = function `Same -> keys | `Alt keys -> keys in
  let info_ = Arg.info ?env:(Option.map (Cmd.Env.info ?docs) env) ?docs in
  function
  | Valued ->
      Arg.(value & opt (some bool_conv) None & info_ keys ~doc)
  | Positive { keys; doc = kdoc } ->
      let doc = match kdoc with `Same -> doc | `Alt doc -> doc in
      Arg.(value & vflag None [Some      tt , info_ (ikeys keys) ~doc])
  | Negative { keys; doc = kdoc } ->
      let doc = match kdoc with `Same -> doc | `Alt doc -> doc in
      Arg.(value & vflag None [Some (neg tt), info_ (ikeys keys) ~doc])
  | Both { pos_keys; pos_doc; neg_keys; neg_doc } ->
      let pos_doc = match pos_doc with `Same -> doc | `Alt doc -> doc
      and neg_doc = match neg_doc with `Same -> doc | `Alt doc -> doc
      and default = match sdoc with
        | With_default { default; _ }
        | Without_default { default = Some default; _ } -> Some default
        | _ -> None
      in
      let pos_doc, neg_doc = match default with
        | Some true  -> pos_doc ^ " (the default)", neg_doc
        | Some false -> pos_doc, neg_doc ^ " (the default)"
        | None       -> pos_doc, neg_doc
      in
      if env <> None then
        failwith "Internal: flag type `Both` cannot be paired with an \
                  environment variable";
      Arg.(value & vflag None
             [Some      tt , info (ikeys pos_keys) ~doc:pos_doc ?docs;
              Some (neg tt), info (ikeys neg_keys) ~doc:neg_doc ?docs])

let bool_cmdline_flag
  : keys:string list -> ?env:string -> doc:string -> ?docs:string -> bool doc ->
    cmdline_flag_type -> bool option Cmdliner.Term.t =
  cmdline_flag
    ~bool_conv:Cmdliner.Arg.bool ~tt:true ~neg:(not)

(* --- *)

let assign_optional_arg act c = function
  | None -> c
  | Some i -> act.set c i

(** Escape backslashes for cmdliner documentation strings. *)
let cmdliner_doc: string -> string =
  Str.(global_replace (regexp {|\\|}) {|\\\\|})

let row_to_term ?prefix ?docs row config : 'config Cmdliner.Term.t =
  let open Cmdliner in
  let keys = List.map (key_with_prefix ?prefix) (row.key :: row.arg_alias) in
  let doc = cmdliner_doc @@ Format.asprintf "%a" pp_spec_doc row.spec in
  match row.spec with
  | Spec (Bool { as_flag }, act, sdoc) ->
      Term.(const (assign_optional_arg act) $ config $
            bool_cmdline_flag ~keys ~doc ?docs ?env:row.env sdoc as_flag)
  | Spec (t, act, _sdoc) ->
      let conv_type (type a) : a typ -> a Arg.conv =
        let separators = [|','; ';'|] in
        (* Separators for lists and lists of lists. Limited to 2. *)
        let rec f: type a k. (k Arg.conv -> a)  -> int -> k typ -> a =
          fun cont cpt -> function
            | Unit -> invalid_arg "Eztoml.conv_type"
            | Bool _ -> cont Arg.bool
            | Int -> cont Arg.int
            | Float -> cont Arg.float
            | String -> cont Arg.string
            | List t ->
                let sep = separators.(cpt) in
                let new_cont = Arg.list ~sep in
                cont @@ f new_cont (cpt + 1) t
            | Dict t ->
                let sep = separators.(cpt) in
                let new_cont a =
                  Arg.list ~sep (Arg.pair ~sep:':' Arg.string a)
                in
                cont @@ f new_cont (cpt + 1) t
        in
        f Fun.id 0
      in
      let arg = Arg.some' (conv_type t) in
      let env = Option.map (Cmd.Env.info ?docs) row.env in
      let v = Arg.(value & opt arg None & info keys ?env ~doc ?docs) in
      Term.(const (assign_optional_arg act) $ config $ v)

let manpage_section_name section_name =
  String.uppercase_ascii (sanitize section_name) ^ " OPTIONS"

let as_section_update_cmdliner_term ?prefix ~section_name (schema: _ schema)
    term =
  let docs = manpage_section_name section_name in
  Hashtbl.fold (fun _ row term -> row_to_term ?prefix ~docs row term)
    schema term

(* --- *)

let rec assign_optional_arg keys optional_value : table -> table =
  match keys, optional_value with
  | [], _ | _, None ->
      Fun.id
  | [key], Some v ->
      Table.update (Table.Key.of_string key) (fun _ -> Some v)
  | tk :: keys, ov ->
      Table.update (Table.Key.of_string tk) begin function
        | Some TTable tbl ->
            Some (TTable (assign_optional_arg keys ov tbl))
        | _ ->                                                        (* erase *)
            Some (TTable (assign_optional_arg keys ov Table.empty))
      end

(** TOML converters for {!Cmdliner} *)
module Toml_conv = struct
  let table_of_list value_to_toml l =
    Table.of_seq @@
    Seq.map (fun (k, v) -> Table.Key.of_string k, value_to_toml v) @@
    List.to_seq l

  open Cmdliner

  let value_conv aconv to_toml_value : value Arg.conv =
    Arg.conv ((fun s -> Arg.conv_parser aconv s |> Result.map to_toml_value),
              Toml.Printer.value)

  let array_conv ?(sep = ',') aconv to_array_node =
    value_conv (Arg.list ~sep aconv) (fun e -> TArray (to_array_node e))

  let array_of_arrays ?(sep = ';') ?(inner_sep = ',') aconv amap =
    array_conv ~sep
      (Arg.list ~sep:inner_sep aconv)
      (fun e -> NodeArray (List.map amap e))

  let table_conv ?(sep = ',') ?(binding_sep = ':') vconv value_to_toml =
    value_conv
      Arg.(list ~sep @@ pair ~sep:binding_sep string vconv)
      (fun l -> TTable (table_of_list value_to_toml l))

  let tbool   e = TBool   e and nbool   e = NodeBool   e
  and tint    e = TInt    e and nint    e = NodeInt    e
  and tfloat  e = TFloat  e and nfloat  e = NodeFloat  e
  and tstring e = TString e and nstring e = NodeString e

  let bool         = value_conv Arg.bool   tbool
  and int          = value_conv Arg.int    tint
  and float        = value_conv Arg.float  tfloat
  and string       = value_conv Arg.string tstring
  and bools        = array_conv Arg.bool   nbool
  and ints         = array_conv Arg.int    nint
  and floats       = array_conv Arg.float  nfloat
  and strings      = array_conv Arg.string nstring
  and bools_list   = array_of_arrays Arg.bool   nbool
  and ints_list    = array_of_arrays Arg.int    nint
  and floats_list  = array_of_arrays Arg.float  nfloat
  and strings_list = array_of_arrays Arg.string nstring
  and bool_dict    = table_conv Arg.bool   tbool
  and int_dict     = table_conv Arg.int    tint
  and float_dict   = table_conv Arg.float  tfloat
  and string_dict  = table_conv Arg.string tstring
end

let toml_bool_cmdline_flag
  : keys:string list -> ?env:string -> doc:string -> ?docs:string -> bool doc ->
    cmdline_flag_type -> value option Cmdliner.Term.t =
  cmdline_flag
    ~bool_conv:Toml_conv.bool ~tt:(TBool true)
    ~neg:(function TBool b -> TBool (not b) | x -> x)

let row_toml_update ?prefix ?docs ~section_name row table : table Cmdliner.Term.t =
  let open Cmdliner in
  match
    let keys = List.map (key_with_prefix ?prefix) (row.key :: row.arg_alias) in
    let doc = cmdliner_doc @@ Format.asprintf "%a" pp_spec_doc row.spec in
    match row.spec with
    | Spec (Bool { as_flag }, _act, sdoc) ->
        Some (toml_bool_cmdline_flag ~keys ~doc ?docs ?env:row.env sdoc as_flag)
    | Spec (t, _act, _sdoc) ->
        let env = Option.map (Cmd.Env.info ?docs) row.env in
        match match t with
          | Unit             -> None
          | Bool _           -> Some Toml_conv.bool
          | Int              -> Some Toml_conv.int
          | Float            -> Some Toml_conv.float
          | String           -> Some Toml_conv.string
          | List Bool _      -> Some Toml_conv.bools
          | List Int         -> Some Toml_conv.ints
          | List Float       -> Some Toml_conv.floats
          | List String      -> Some Toml_conv.strings
          | Dict Bool _      -> Some Toml_conv.bool_dict
          | Dict Int         -> Some Toml_conv.int_dict
          | Dict Float       -> Some Toml_conv.float_dict
          | Dict String      -> Some Toml_conv.string_dict
          | List List Bool _ -> Some Toml_conv.bools_list
          | List List Int    -> Some Toml_conv.ints_list
          | List List Float  -> Some Toml_conv.floats_list
          | List List String -> Some Toml_conv.strings_list
          | List _ | Dict _  -> None
        with
        | Some c -> Some Arg.(value & opt (some' c) None &
                              info keys ?env ~doc ?docs)
        | None -> None
  with
  | Some v -> Term.(map (assign_optional_arg [section_name; row.key]) v $ table)
  | None -> table

let acc_toml_table_for_cmdliner ?prefix ~section_name (schema: _ schema) table =
  let section_name = sanitize section_name
  and docs = manpage_section_name section_name in
  Hashtbl.fold begin fun _ row table ->
    row_toml_update ?prefix ~docs ~section_name row table
  end schema table

(* --- *)

let read_file file =
  {
    toml_contents = Sc_sys.File.read file;
    file;
  }

let load_string ~filename toml =
  let lexbuf = Lexing.from_string ~with_positions:true toml in
  match Toml.Parser.parse lexbuf filename with
  | `Ok table ->
      Ok table
  | `Error (msg, loc)
  | exception Toml.Parser.Error (msg, loc) ->
      Error (Errors.TOML_error { toml; msg; loc })

let load_file { file; toml_contents } =
  load_string ~filename:(Sc_sys.File.name file) toml_contents
