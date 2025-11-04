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
open OPTIONS
open DATA

(* A few helpful aliases *)

let field = Basics.PPrt.Record.field
let opt_field = Basics.PPrt.Record.optional_field
let pp_record = Basics.PPrt.Record.pp
let pp_str_list = Basics.PPrt.Strings.pp_comma_separated


let pp_mode ppf m =
  Fmt.pf ppf @@
  match m with
  | Cover -> "Cover"
  | Assert -> "Assert"
  | CLabel -> "CLabel"

let pp_options ppf {mode; timeout; unwind} =
  pp_record ppf [
    field "mode" pp_mode mode;
    field "timeout" Fmt.float timeout;
    field "unwind" Fmt.int unwind;
  ]

let pp_malloc_may_fail ppf m =
  Fmt.pf ppf @@
  match m with
  | CantFail -> "Can't fail"
  | WithFailAssertion -> "Fail with assertion"
  | WithNull -> "Returns null"
  | DefaultFail -> "Can fail"

let pp_json_options ppf o =
  pp_record ppf [
    field "arguments" pp_str_list o.oarguments;
    field "function"  Fmt.string o.ofunction;
    opt_field ~default:"unlimited" "unwind" Fmt.int o.ounwind;
    opt_field ~default:"all" "properties" pp_str_list o.oproperties;
    field "show-properties" Fmt.bool o.oshow_properties;
    opt_field "cover" Fmt.string o.ocover;
    opt_field "error-label" pp_str_list o.oerror_label;
    field "pointer-check" Fmt.bool o.opointer_check;
    field "nondet-static" Fmt.bool o.onondet_static;
    field "malloc-may-fail" pp_malloc_may_fail o.omalloc_may_fail;
  ]

let pp_source_loc ppf sl =
  match sl.slfunction with
  | None ->   Fmt.pf ppf "%s:%s"     sl.slfile   sl.slline
  | Some f -> Fmt.pf ppf "%s(%s):%s" sl.slfile f sl.slline

let pp_message ppf m =
  Fmt.pf ppf "%s: %s" m.mtyp m.mtext;
  match m.msource_location with
  | None -> ()
  | Some sl -> Fmt.pf ppf " (at %a)" pp_source_loc sl

let pp_status ppf s =
  match s with
  | Success -> Fmt.string ppf "Success"
  | Failure_ -> Fmt.string ppf "Failure"
  | Unknown s -> Fmt.pf ppf "%s" s

let pp_cell ~pp_data ppf c =
  match c with
  | ProgramInfo i -> Fmt.pf ppf "Info: %s" i
  | Message m -> Fmt.pf ppf "%a" pp_message m
  | CProverStatus s -> Fmt.pf ppf "Status: %a" pp_status s
  | Data d -> Fmt.pf ppf "%a" pp_data d

let pp_property_class ppf c =
  match c with
  | ErrorLabel -> Fmt.string ppf "Error label"
  | Assertion -> Fmt.string ppf "Assertion"
  | Coverage -> Fmt.string ppf "Coverage"
  | Unknown s -> Fmt.pf ppf "%s" s

let pp_property ppf p =
  pp_record ppf [
    field "class" pp_property_class p.pclass;
    field "description" Fmt.string p.pdescription;
    field "expression" Fmt.string p.pexpression;
    field "name" Fmt.string p.pname;
    field "psource_location" pp_source_loc p.psource_location;
  ]

let pp_base_value ppf b =
  match b with
  | Unknown (n, t) ->
     Fmt.pf ppf "{name = %s; type = %a}"
       n
       Fmt.(option ~none:(fun ppf () -> Fmt.string ppf "unknown") Fmt.string) t
  | Value {vbinary; vdata; vname; vtype; vwidth} ->
      pp_record ppf
        [
          field "name" Fmt.string vname;
          field "data" Fmt.string vdata;
          opt_field "typ" Fmt.string vtype;
          opt_field "binary" Fmt.string vbinary;
          opt_field "width" Fmt.int vwidth
        ]

let rec pp_structure_field ppf {sfvalue; sfname} =
  Fmt.pf ppf "%s = %a" sfname pp_value sfvalue

and pp_structured ppf {smembers; sname} =
  Fmt.pf ppf "%s = %a"
    sname
    (Basics.PPrt.pp_lst ~fopen:"{" ~fclose:"}" pp_structure_field) smembers

and pp_value ppf = function
  | Base b ->
      pp_base_value ppf b
  | Structured s ->
      pp_structured ppf s
  | Array l ->
      Basics.PPrt.pp_lst ~fsep:","
        (fun ppf (i,v) -> Fmt.pf ppf "[%i]=%a" i pp_value v) ppf l
  | Union s ->
      pp_structure_field ppf s

let pp_input ppf i =
  Fmt.pf ppf "%s = %a" i.iid pp_value i.ivalue

let pp_test ppf t =
  pp_record ppf
    [
      field "covered" pp_str_list t.tcovered_goals;
      field "inputs" (Basics.PPrt.pp_lst pp_input) t.tinputs;
    ]

let pp_goal ppf g =
  pp_record ppf
    [
      field "description" Fmt.string g.gdescription;
      field "goal" Fmt.string g.ggoal;
      field "location" pp_source_loc g.gsource_location;
      field "status" pp_status g.gstatus;
    ]

let pp_goals_details ppf g =
  Fmt.pf ppf "%a (%i/%i)"
    (Basics.PPrt.pp_lst pp_goal) g.gdgoals g.gdgoals_covered g.gdtotal_goals

let pp_cbmc_cover_output ppf c =
  match c with
  | Goals gd -> Fmt.pf ppf "Goals: %a" pp_goals_details gd
  | Tests l -> Fmt.pf ppf "Tests: %a" (Basics.PPrt.pp_lst pp_test) l

let pp_function ppf f =
  Fmt.pf ppf "%s:%a" f.fidentifier pp_source_loc f.fsource_location

let pp_assignment ppf a =
  Fmt.pf ppf "%s = %a : %a" a.alhs pp_value a.avalue pp_source_loc a.asource_location

let pp_location_only ppf l =
  Fmt.pf ppf "(location-only) %a" pp_source_loc l.losource_location

let pp_failure_step ppf fs =
  Fmt.pf ppf "Failure on %s:%a (%s)"
    fs.fsproperty pp_source_loc fs.fssource_location fs.fsreason

let pp_input_step ppf is =
  Fmt.pf ppf "Input: %s = %a : %a"
    is.iinputID (Basics.PPrt.pp_lst pp_value) is.ivalues
    pp_source_loc is.isource_location

let pp_output_step ppf os =
  Fmt.pf ppf "Output %s = %a : %a"
    os.oid (Basics.PPrt.pp_lst pp_value) os.ovalues
    pp_source_loc os.oloc

let pp_instruction ppf = function
  | Function fi -> pp_function ppf fi.fifunction
  | Assignment a -> pp_assignment ppf a
  | Location l -> pp_location_only ppf l
  | FailureStep s -> pp_failure_step ppf s
  | Input is -> pp_input_step ppf is
  | Output os -> pp_output_step ppf os

let pp_trace = Basics.PPrt.pp_lst pp_instruction

let pp_assertion_check ppf ac =
  pp_record ppf
    [
      field "description" Fmt.string ac.acdescription;
      field "property" Fmt.string ac.acproperty;
      opt_field "source location" pp_source_loc ac.acsource_location;
      field "status" pp_status ac.acstatus;
      opt_field "trace" pp_trace ac.actrace;
    ]

let pp_cbmc_assert_output = Basics.PPrt.pp_lst pp_assertion_check

let pp_simple_label_env ppf env =
  pp_record ppf
    [
      field "proof objectives" PropertyMap.print env.proof_objectives;
      field "already proven" PropertyMap.print env.already_proven;
      field "extra" (Basics.PPrt.pp_lst pp_property) env.extra_required_properties;
    ]

let pp_analysis_env (type t) ppf (e : t analysis_env) =
  match e with
  | SimpleLabelEnv e -> pp_simple_label_env ppf e

;; Printexc.register_printer begin function
  | FAILED_JSON_PARSING {exn; json = _}
  | FAILED_JSON_DESTRUCT {exn; json = _} ->
     Some (Printexc.to_string exn)
  | UNKNOWN_PROPERTY pname ->
     Some (Fmt.str "Unknown property %s" pname)
  | _ -> None
  end;;
