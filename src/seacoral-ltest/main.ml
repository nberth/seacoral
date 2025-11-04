(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(* If you delete or rename this file, you should add
   'src/sc_ltest/main.ml' to the 'skip' field in "drom.toml" *)

let log_src = Logs.Src.create ~doc:"Logs of Ltest" "Sc_ltest"
module Log = (val (Ez_logs.from_src log_src))

module type S = sig
  module Lannot : Lannot.S
  module Lreplay : Lreplay.S
  module Eacsl : Eacsl.S
  module Label_database : Label_database.S
end

module Make (Conf: Types.CONFIG) : S = struct
  module Lannot = Lannot.Make (Conf)
  module Lreplay = Lreplay.Make (Conf)
  module Eacsl = Eacsl.Make (Conf)
  module Label_database = Label_database.Make (Conf)
end
