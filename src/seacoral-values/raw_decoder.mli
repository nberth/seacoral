(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES

val install_resources_in: workspace:Sc_core.Types.workspace -> dir Lwt.t

val declare_struct: Cvalues.Struct.typ Fmt.t
val emit_struct_ptr_typ: Cvalues.Struct.typ Fmt.t
val emit_struct_decoder_symbol: Cvalues.Struct.typ Fmt.t
val emit_struct_decoder: ((module Cvalues.Struct.REPR) * Cvalues.Struct.typ) Fmt.t
