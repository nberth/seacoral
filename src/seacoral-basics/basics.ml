(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Format

module Memo = Memo

module PPrt = struct
  type pu = formatter -> unit
  type 'a pp = formatter -> 'a -> unit
  type 'a fmt = ('a, formatter, unit) format
  type ufmt = unit fmt

  (** Type of functions that use a format string with ['a] arguments to return a
      ['b]. *)
  type ('a, 'b) func = ('a, Format.formatter, unit, 'b) format4 -> 'a

  (** Type of procedures that use a format string with ['a] arguments. *)
  type 'a proc = ('a, Format.formatter, unit) format -> 'a

  let pp f fmt = fprintf fmt f

  let pseq
      ~fopen
      ~fsep
      ~fclose
      ~fempty
      pp_e fmt seq =
    let length =
      Seq.fold_left begin fun i e ->
        (if i = 0 then pp fopen else pp fsep) fmt; pp_e fmt (e, i); i + 1
      end 0 seq
    in
    if length = 0 then
      match fempty with
      | None -> fprintf fmt "%(%)%(%)" fopen fclose
      | Some fempty -> fprintf fmt fempty
    else pp fclose fmt

  let pli
      ?(fopen: ufmt = "[@[")
      ?(fsep: ufmt = ",@ ")
      ?(fclose: ufmt = "@]]")
      ?(fempty: ufmt option)
    = fun pp_e fmt lst ->
      pseq ~fopen ~fsep ~fclose ~fempty pp_e fmt (List.to_seq lst)

  let pl ?fopen ?fsep ?fclose ?fempty pp_e =
    pli ?fopen ?fsep ?fclose ?fempty (fun fmt (e, _i) -> pp_e fmt e)

  let pls ?fsep =
    pl ~fopen:"{@[" ~fclose:"@]}" ~fempty:"{}" ?fsep

  let pp_lst = pl
  let pp_lst_i = pli
  let pp_lstset = pls

  let pp_arr_i
      ?(fopen: ufmt = "[|@[")
      ?(fsep: ufmt = ",@ ")
      ?(fclose: ufmt = "@]|]")
      ?fempty pp_e fmt a =
    pseq ~fopen ~fsep ~fclose ~fempty pp_e fmt (Array.to_seq a)

  let pp_arr ?fopen ?fsep ?fclose ?fempty pp_e fmt a =
    pp_arr_i ?fopen ?fsep ?fclose ?fempty (fun fmt (e, _i) -> pp_e fmt e) fmt a

  let pp_nodelim ?fsep ?(fterm: ufmt = "") =
    pl ~fopen:"" ~fempty:"" ~fclose:fterm ?fsep

  let pp_nodelim_i ?fsep ?(fterm: ufmt = "") =
    pli ~fopen:"" ~fempty:"" ~fclose:fterm ?fsep

  module Strings = struct
    let pp_comma_separated = pp_nodelim ~fsep:"," ~fterm:"" pp_print_string
    let pp_comma__separated = pp_nodelim ~fsep:", " ~fterm:"" pp_print_string
    let pp_space_separated = pp_nodelim ~fsep:" " ~fterm:"" pp_print_string
    let pp_space_separated_ = pp_nodelim ~fsep:" " ~fterm:" " pp_print_string
  end

  let with_oxford_comma pp ppf lst =
    let rec aux ?(deep = false) = function
      | [] -> ()
      | [x] -> pp ppf x
      | [x;y] when not deep -> Fmt.fmt "%a@ and@ %a" ppf pp x pp y
      | [x;y] when   deep -> Fmt.fmt "%a,@ and@ %a" ppf pp x pp y
      | x :: tl -> Fmt.fmt "%a,@ " ppf pp x; aux ~deep:true tl
    in
    aux lst

  module UFmt = struct
    let from_format format_string =
      Format.kasprintf (fun s -> Scanf.format_from_string s "") format_string
    let char: char -> ufmt = from_format "%c"
    let string: string -> ufmt = from_format "%s"
    let string_set: string list -> ufmt = from_format "%a" @@ pp_lstset Fmt.string
  end

  module Record = struct
    type field =
      | Field : string * 'a pp * 'a -> field
      | OptField: string * 'a pp * 'a option -> field

    let field fname pp v = Field (fname, pp, v)

    let optional_field ?default fname pp v =
      match default with
      | None ->
          OptField (fname, pp, v)
      | Some d ->
          let pp ppf = function
            | None -> Fmt.pf ppf d
            | Some v -> pp ppf v
          in
          Field (fname, pp, v)

    let pp ppf fields =
      pp_lst ~fopen:"{ @[" ~fclose:"@] }" ~fsep:";@;"
        begin fun ppf -> function
          | Field    (field, pp,      v) ->
              Fmt.pf ppf "@[<2>%s =@ %a@]" field pp v
          | OptField (field, pp, Some v) ->
              Fmt.pf ppf "@[<2>%s =@ %a@]" field pp v
          | OptField (_, _, None) ->
              ()
        end ppf fields

  end

  (* --- *)

  let pp_time fmt (t : Unix.tm) =
    fprintf fmt "%04u-%02u-%02u-%02u:%02u:%02u"
      (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
      t.tm_hour t.tm_min t.tm_sec

  (** Sends right margin to virtual infinity *)
  let blast_margin ppf =
    (* see https://github.com/ocaml/ocaml/issues/10592 *)
    pp_set_geometry ppf
      ~max_indent:(Format_shims.pp_infinity - 2)
      ~margin:(Format_shims.pp_infinity - 1)

  (** Version of {!Format.asprintf} with virtually no right margin *)
  let asprintf fmt = asprintf ("%t"^^fmt) blast_margin

  (** Version of {!Format.kasprintf} with virtually no right margin *)
  let kasprintf f fmt = kasprintf f ("%t"^^fmt) blast_margin

  let to_string fmt = asprintf ("@[<h>"^^fmt^^"@]")
  let string_to f fmt = kasprintf f ("@[<h>"^^fmt^^"@]")

  let straighten_if_tty oc fo cols =
    if Unix.isatty (Unix.descr_of_out_channel oc) then
      pp_set_margin fo cols

  let init_formatters ?style_renderer ?utf_8 () =
    Fmt_tty.setup_std_outputs ?style_renderer ?utf_8 ();
    match Terminal_size.get_columns () with
    | Some columns ->
        straighten_if_tty stderr err_formatter columns;
        straighten_if_tty stdout std_formatter columns
    | None ->
        ()                                                 (* Just do nothing *)

end

open PPrt

(* Scanf-based parsing *)

type 'a string_parser = string -> 'a
let mk_parser spec cstr : _ string_parser =
  fun str -> Scanf.sscanf str spec cstr
let try_parse (specs: 'a string_parser list) str =
  let rec aux = function
    | [] ->
        None
    | f :: tl ->
        try Some (f str) with Scanf.Scan_failure _ | End_of_file -> aux tl
  in
  aux specs

(* Printable collections *)

module type ORDERED_TYPE = sig
  include Set.OrderedType
  val print: t pp
end

module type SET = sig
  include Set.S
  val print: t pp
  (* val print': ?empty:ufmt -> ?left:ufmt -> ?sep:ufmt -> ?right:ufmt -> t pp *)
end

module MakeSet (E: ORDERED_TYPE) : (SET with type elt = E.t) = struct
  include Set.Make (E)
  let print'
      ?(empty: ufmt option)
      ?(left: ufmt = "@[<1>{")
      ?(sep: ufmt = ",@ ")
      ?(right: ufmt = "}@]")
      fmt set
    =
    match empty with
    | Some e when is_empty set ->
        fprintf fmt e
    | _ ->
        fprintf fmt left;
        fold (fun e first ->
            if not first then fprintf fmt sep; E.print fmt e; false)
          set true |> ignore;
        fprintf fmt right
  let print fmt = print' fmt
end

module type MAP = sig
  include Map.S
  val print: ?left:ufmt -> ?right:ufmt -> ?sep:ufmt -> ?assoc:ufmt ->
    ?skip_key:(key -> 'a -> bool) -> ?skip_val:(key -> 'a -> bool) ->
    'a pp -> 'a t pp
  val printkv: ?left:ufmt -> ?right:ufmt -> ?sep:ufmt -> ?assoc:ufmt ->
    ?skip_key:(key -> 'a -> bool) -> ?skip_val:(key -> 'a -> bool) ->
    (key * 'a) pp -> 'a t pp
  (* val print':
   *   ?empty:ufmt ->
   *   ?left:ufmt ->
   *   ?sep:ufmt ->
   *   ?right:ufmt ->
   *   ?skip_key:(key -> 'a -> bool) ->
   *   ?skip_val:(key -> 'a -> bool) ->
   *   ?rev_assoc:bool ->
   *   ?aleft:ufmt ->
   *   ?assoc:ufmt ->
   *   ?aright:ufmt -> 'a pp -> 'a t pp *)
end
module MakeMap (E: ORDERED_TYPE) : (MAP with type key = E.t) = struct
  include Map.Make (E)
  let print'
      ?(empty: ufmt option)
      ?(left: ufmt = "@[<1>@<1>⦃")
      ?(sep: ufmt = ",@ ")
      ?(right: ufmt = "@<1>⦄@]")
      ?(skip_key = fun _ _ -> false)
      ?(skip_val = fun _ _ -> false)
      ?(rev_assoc = false)
      ?(aleft: ufmt = "@[<2>")
      ?(assoc: ufmt = " =>@ ")
      ?(aright: ufmt = "@]")
      pv fmt map
    =
    match empty with
    | Some e when is_empty map ->
        fprintf fmt e
    | _ ->
        fprintf fmt left;
        fold begin fun k v first ->
          let skip_key = skip_key k v and skip_val = skip_val k v in
          if skip_key && skip_val then first else begin
            if not first then fprintf fmt sep;
            fprintf fmt aleft;
            if skip_key
            then pv fmt (k, v)
            else if skip_val
            then E.print fmt k
            else if not rev_assoc
            then fprintf fmt "%a%(%)%a" E.print k assoc pv (k, v)
            else fprintf fmt "%a%(%)%a" pv (k, v) assoc E.print k;
            fprintf fmt aright;
            false
          end
        end map true |> ignore;
        fprintf fmt right
  let printkv ?left ?right ?sep ?assoc ?skip_key ?skip_val pkv fmt =
    print' ?left ?right ?sep ?assoc ?skip_key ?skip_val
      pkv fmt
  let print ?left ?right ?sep ?assoc ?skip_key ?skip_val pv fmt =
    print' ?left ?right ?sep ?assoc ?skip_key ?skip_val
      (fun ppf (_, v) -> pv ppf v) fmt
end

module String = struct
  include String
  let print = pp_print_string
  let hash = Hashtbl.hash
  let replace_spaces ~by =
    String.map
      (function
       | ' ' | '\x0C' | '\n' | '\r' | '\t' -> by
       | c -> c)
end
module Strings = MakeSet (String)
module StrMap = MakeMap (String)
module StrTbl = Hashtbl.Make (String)

module Integer = struct
  include Stdlib.Int
  let print = pp_print_int
  let hash = Fun.id
end
module Ints = MakeSet (Integer)
module IntMap = MakeMap (Integer)

module Digest = struct
  include Digest
  let hash = Hashtbl.hash
  let print fmt h = pp_print_string fmt (to_hex h)
end
module Digests = MakeSet (Digest)
(* module DigestMap = MakeMap (Digest) *)

module SeqUtils = struct
  let seq n =
    let rec aux = function
      | i when i < n -> fun () -> Seq.Cons (i, aux (i + 1))
      | _ -> fun () -> Seq.Nil
    in
    aux 0
end
