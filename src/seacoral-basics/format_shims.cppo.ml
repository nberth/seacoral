(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

#if OCAML_MAJOR >= 5 && OCAML_MINOR >= 2
let pp_infinity = Format.pp_infinity
#else
let pp_infinity = max_int
#endif
