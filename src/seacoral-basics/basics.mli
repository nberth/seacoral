(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Miscellaneous helpers. *)

open Format

module Memo : module type of Memo

(** {2 Print helpers} *)

module PPrt : sig

  (** {3 Useful aliases for printers} *)

  type pu = formatter -> unit
  type 'a pp = formatter -> 'a -> unit
  type 'a fmt = ('a, formatter, unit) format
  type ufmt = unit fmt
  type ('a, 'b) func = ('a, Format.formatter, unit, 'b) format4 -> 'a
  type 'a proc = ('a, Format.formatter, unit) format -> 'a

  (** {3 Printers} *)

  (** [pp_lst ~fopen ~fsep ~fclose ~fempty pp_e]
      Returns a list printer with a custom open delimiter (fopen), a custom
      close delimiter (fclose) and a custom separator (fsep). If the list is
      empty, prints (fempty). *)
  val pp_lst :
    ?fopen:ufmt ->
    ?fsep:ufmt ->
    ?fclose:ufmt ->
    ?fempty:ufmt ->
    'a pp -> 'a list pp

  (** Same as [pl], but with a counter incremented for each element of the
      list. *)
  val pp_lst_i :
    ?fopen:ufmt ->
    ?fsep:ufmt ->
    ?fclose:ufmt ->
    ?fempty:ufmt ->
    ('a * int) pp -> 'a list pp

  (** Same as [pl], but prints the list with a set-like notation
      ({elt1, elt2, ...}) *)
  val pp_lstset :
    ?fsep:ufmt ->
    'a pp -> 'a list pp

  (** Same as [pl], but for arrays. *)
  val pp_arr :
    ?fopen:ufmt ->
    ?fsep:ufmt ->
    ?fclose:ufmt ->
    ?fempty:ufmt ->
    'a pp -> 'a array pp

  (** Same as [pli], but for arrays. *)
  val pp_arr_i :
    ?fopen:ufmt ->
    ?fsep:ufmt ->
    ?fclose:ufmt ->
    ?fempty:ufmt ->
    ('a * int) pp -> 'a array pp

  (** Same as [pl], but without an opening delimiter. *)
  val pp_nodelim :
    ?fsep:ufmt ->
    ?fterm:ufmt ->
    'a pp -> 'a list pp

  (** Same as [pp_no_delim], but with a counter. *)
  val pp_nodelim_i :
    ?fsep:ufmt ->
    ?fterm:ufmt ->
    ('a * int) pp -> 'a list pp

  (** [with_oxford_comma pp ppf lst] pretty-prints [lst] using [ppf] by
      enumerating its items and inserting Oxford-style comma.  In particular,
      this function:

      - does nothing on an empty list;

      - acts like [pp ppf e] if [lst = [e]];

      - prints "[a] and [b]" (without comma) in case [lst = [a;b]];

      - otherwise prints every element followed by a comma, except for the last
        that is preceded with "and", as for instance in "a, b, c, and d". *)
  val with_oxford_comma: 'a pp -> 'a list pp

  (** {3 Helpers for printing string lists} *)

  module Strings : sig
    (** Prints a list of strings separated with ','. *)
    val pp_comma_separated : string list pp

    (** Prints a list of strings separated with ", ". *)
    val pp_comma__separated : string list pp

    (** Prints a list of string separated with a space.*)
    val pp_space_separated : string list pp

    (** Prints a list of string separated with a space and ending with a
        space. *)
    val pp_space_separated_ : string list pp
  end

  (** {3 Helpers for pretty printing simple data structures} *)

  module UFmt :
  sig
    (** Prints a char. *)
    val char : char -> ufmt

    (** Prints a string. *)
    val string : string -> ufmt

    (** Prints a string list as a set. *)
    val string_set : string list -> ufmt
  end

  (** {3 Helpers for pretty printing records} *)
  module Record: sig
    (** A record field. *)
    type field

    (** [field fname pp v]

        Defines a record field that will print [v]. *)
    val field : string -> 'a pp -> 'a -> field

    (** [optional_field ?default fname pp v]

        Same as [field] but with an optional value. If no default value is
        given, will not print the record field if [v = None].  *)
    val optional_field : ?default:ufmt -> string -> 'a pp -> 'a option -> field

    (** Prints a record. *)
    val pp : field list pp
  end

  (** {3 Miscellaneous printing helpers} *)

  (** Prints the time with the format year-month-day-hour-min-sec. *)
  val pp_time: formatter -> Unix.tm -> unit

  (** Version of {!Format.asprintf} with virtually no right margin. *)
  val asprintf: ('a, formatter, unit, unit, unit, string) format6 -> 'a

  (** Version of {!Format.kasprintf} with virtually no right margin. *)
  val kasprintf: (string -> 'a) -> ('b, formatter, unit, unit, unit, 'a) format6 -> 'b

  (** {!asprintf} in a horizontal box. *)
  val to_string: ('a, formatter, unit, unit, unit, tag) format6 -> 'a
  val string_to: (string -> 'a) -> ('b, formatter, unit, unit, unit, 'a) format6 -> 'b

  (** Setups the formatters with the correct margins (defined with the
      environment variable. *)
  val init_formatters: ?style_renderer:Fmt.style_renderer -> ?utf_8:bool -> unit -> unit
end

(** {2 Scanf-based parsing} *)

type 'a string_parser = string -> 'a

val mk_parser: ('a, Scanf.Scanning.in_channel, 'b, 'c -> 'd, 'a -> 'e, 'e) format6 ->
  'c -> 'd string_parser
val try_parse: 'a string_parser list -> string -> 'a option

(** {2 Printable collections} *)

(** The signature of the collection builders below. *)
module type ORDERED_TYPE = sig
  include Set.OrderedType
  val print: t PPrt.pp
end

(** The signature for sets, extended with printers. *)
module type SET = sig
  include Set.S

  (** Prints the contents of a set. *)
  val print: t PPrt.pp
end

(** The signature for maps, extended with printers. *)
module type MAP = sig
  include Map.S

  (** Prints the contents of a map. *)
  val print: ?left:PPrt.ufmt -> ?right:PPrt.ufmt -> ?sep:PPrt.ufmt ->
    ?assoc:PPrt.ufmt -> ?skip_key:(key -> 'a -> bool) -> ?skip_val:(key -> 'a -> bool) ->
    'a PPrt.pp -> 'a t PPrt.pp
  val printkv: ?left:PPrt.ufmt -> ?right:PPrt.ufmt -> ?sep:PPrt.ufmt ->
    ?assoc:PPrt.ufmt -> ?skip_key:(key -> 'a -> bool) -> ?skip_val:(key -> 'a -> bool) ->
    (key * 'a) PPrt.pp -> 'a t PPrt.pp
end

(** Builds a printable set module. *)
module MakeSet : functor (E : ORDERED_TYPE) -> (SET with type elt = E.t)

(** Builds a printable map module. *)
module MakeMap : functor (E : ORDERED_TYPE) -> (MAP with type key = E.t)

(** The Stdlib String module, extended with a printer and a hash function. *)
module String : sig
  include module type of String
  val print : formatter -> t -> unit
  val hash : t -> int
  val replace_spaces : by:char -> string -> string
end

(** Printable string set module. *)
module Strings : SET with type elt = String.t

(** Printable string map module. *)
module StrMap : MAP with type key = String.t

(** String hashtbl module. *)
module StrTbl : Hashtbl.S with type key = String.t

module Integer : sig
  type t = int
  val compare : t -> t -> t
  val print : formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
end

(** Printable integer set module. *)
module Ints : SET with type elt = Integer.t

(** Printable integer map module. *)
module IntMap : MAP with type key = Integer.t

module Digest : sig
  include module type of Digest
  val hash : t -> int
  val print : formatter -> t -> unit
end

(** Printable digest set module. *)
module Digests : SET with type elt = Digest.t

(** {2 Miscellaneous helpers} *)

module SeqUtils : sig

  (** [seq n]

      Returns a sequence of integers starting from 0 to (n - 1)
  *)
  val seq : int -> int Seq.t
end
