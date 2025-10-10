(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Printers *)

open Types

(** Pretty-prints coverage statistics. *)
let pp_covinfo ppf { covered_ids; uncoverable_ids; num_ids } =
  let open Fmt in
  let ids_persentage ids = float_of_int ids *. 100. /. float_of_int num_ids in
  let pers =
    if num_ids = 0
    then nop                                                 (* print nothing *)
    else using ids_persentage @@ sp ++ fmt "(%.1f%%)"
  in
  let bld = styled `Bold @@ fun ppf i -> fmt "%u%a" ppf i pers i in
  let ifstl ~p ~s f ppf x = if p x then styled s f ppf x else f ppf x in
  let covrd = (ifstl ~p:((=) num_ids) ~s:`Green @@
               ifstl ~p:((=) 0) ~s:`Red @@ any "cov: " ++ bld) in
  let uncov = (ifstl ~p:((=) 0) ~s:`Green @@
               ifstl ~p:((>) 0) ~s:`Red @@ any "uncov: " ++ bld) in
  let unkwn = (ifstl ~p:((=) 0) ~s:`Green @@
               ifstl ~p:((<) 0) ~s:`Yellow @@ any "unkwn: " ++ bld) in
  let c = Basics.Ints.cardinal covered_ids
  and u = Basics.Ints.cardinal uncoverable_ids in
  Fmt.fmt "@[<h>%a@;%a@;%a@]" ppf covrd c uncov u unkwn (num_ids - u - c)

(** {2 Exceptions printers} *)

let pp_proof_inconsistency ppf { new_status; old_status; toolname } =
  let str_status = function
    | `Unk -> "unknown"
    | `Cov -> "coverable"
    | `Uncov -> "uncoverable"
  in
  Basics.IntMap.printkv ~left:"@[" ~right:"@]" ~sep:"@;"
    ~skip_key:(fun _ _ -> true)
    begin fun ppf (id, (toolname', old_status)) ->
      Fmt.pf ppf
        "@[For@ label@ %u,@ tool@ %s@ concluded@ \"%s\"@ while@ tool@ %s@ \
         concluded@ \"%s\".@]"
        id toolname
        (str_status new_status) toolname'
        (str_status old_status)
    end ppf old_status

;; Printexc.register_printer begin function
  | Proof_inconsistency pi ->
      Some (Fmt.str "@[<v>Proof inconsistency:@;<1 2>%a\
                     @]" pp_proof_inconsistency pi)

  | _ ->
      None
end;;
