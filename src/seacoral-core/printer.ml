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

let pp_generic_preprocessing_error ~pp_operation ~pp_error ppf = function
  | { workdir_status = `Created; error } ->
      Fmt.pf ppf "@[<hov>%t@ failed:@;%a\
                  @]" pp_operation pp_error error
  | { workdir_status = `Reused; error } ->
      Fmt.pf ppf "@[<v>\
                  @[<hov>\
                  %t@ was@ inhibitted,@ however@ this@ led@ to@ an@ error:@;%a\
                  @]@;\
                  Hint:@ retry@ with@ --force-preprocess\
                  @]" pp_operation pp_error error
