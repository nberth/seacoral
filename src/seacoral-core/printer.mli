(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Pretty-printing utilities *)

(** [pp_generic_preprocessing_error ~pp_operation ~pp_error ppf error] prints
    using [ppf] a message about a failed pre-processing operation;
    [pp_operation] should give a short description of what failed, and
    [pp_error] print details about the error itself. *)
val pp_generic_preprocessing_error
  : pp_operation:(Format.formatter -> unit)
  -> pp_error:'a Fmt.t
  -> 'a Types.generic_preprocessing_error Fmt.t
