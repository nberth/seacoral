(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type 'raw_test t

val make
  : workspace: Sc_core.Types.workspace
  -> 'raw_test Types.decoder_params
  -> 'raw_test t Lwt.t

(** Note: [env] is only used in case the library needs to be pre-loaded;
    otherwise using [libs] as linking flags is enough. *)
val for_compiled_subprocess
  : ?ld_preload_var:string
  -> _ t
  -> Sc_core.Types.compiled_subprocess_info Lwt.t
