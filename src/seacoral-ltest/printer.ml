(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Types

let pp_lreplay_results ppf ({ lreplay_covered = cov; lreplay_unconclusive = unc;
                              lreplay_unknown = unk; lreplay_labels = lbls },
                            display_style) =
  let decided = cov + unc in
  let total = decided + unk in
  let percentage =
    let fdecided = float_of_int decided in
    let ftotal = float_of_int total in
    (fdecided /. ftotal *. 100.)
  in
  let pp_summary ppf =
    Fmt.(styled `Bold @@ fun ppf () ->
         fmt "(%i/%i)@ %.1f%%" ppf cov total percentage) ppf ()
  in
  if total = 0 then
    Fmt.pf ppf "@[No@ label, no@ coverage@]"
  else begin                     (* only print if there's at least one label. *)
    Fmt.pf ppf "@[<v>";
    let open_, sep, padding, close = match display_style with
      | Compact -> Fmt.(any "@[<hov>", semi, 0, any "@]@;")
      | One_per_line -> Fmt.(nop, any "@;", String.length "Coverage", any "@;")
    in
    open_ ppf ();
    Fmt.list ~sep begin fun ppf (lbl: _ Sc_C.Cov_label.t) ->
      let id = Sc_C.Cov_label.id lbl and status = Sc_C.Cov_label.status lbl in
      Fmt.pf ppf "%*i: %a" padding id Sc_C.Cov_label.print_status status
    end ppf lbls;
    close ppf ();
    Fmt.pf ppf "@[Coverage: %t@]" pp_summary;          (* <- summary at the end *)
    Fmt.pf ppf "@]"                                    (* vbox *)
  end

(*
open Format;;
let p x = print_string x;;
let i x = print_int x;;

let pp_header () =   set_tab (); p "a  ";   set_tab (); p "b  "; set_tab ();   p "c  ";;
let pp_line   () = print_tab (); i 100  ; print_tab (); i 2    ; print_tab (); i 2    ;;
let full () =
  open_tbox ();
  pp_header ();
  pp_line ();
  close_tbox ();
;;
full ();;
 *)

let pp_error: error Fmt.t = fun ppf -> function
  | Missing_frama_c_plugin { name } ->
      Fmt.pf ppf "Frama-C@ plugin@ %S@ is@ either@ unavailable@ or@ its@ \
                  version@ is@ unsupported" name
  | Unsupported_criterion { crit; supported_criteria } ->
      Fmt.pf ppf "@[<v>@[SeaCoral@ does@ not@ support@ the@ coverage@ criterion@ \
                  %S.@]@;@[Supported@ criteria:@;%a.@]@]\
                 " crit (Basics.PPrt.pp_lst Fmt.string) supported_criteria
  | Unknown_criterion { crit; supported_criteria } ->
      Fmt.pf ppf "@[<v>@[Unknown@ coverage@ criterion@ %S.@]@;@[Supported@ \
                  criteria:@;%a.@]@]\
                 " crit (Basics.PPrt.pp_lst Fmt.string) supported_criteria

;; Printexc.register_printer begin function
  | ERROR e ->
      Some (Basics.PPrt.to_string "%a" pp_error e)
  | _ ->
      None
end;;
