(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  module Lannot : Lannot.S
  module Lreplay : Lreplay.S
  module Eacsl : Eacsl.S
  module Label_database : Label_database.S
end

module Make (_: Types.CONFIG) : S
