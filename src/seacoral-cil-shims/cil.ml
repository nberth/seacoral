(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

include GoblintCil.Cil

(* Patch bug in pretty-printer for enumerations in goblint-cil.2.0.6.  Fixed by
   https://github.com/goblint/cil/pull/185, likely to be in 2.0.7 *)
class defaultCilPrinterClass : cilPrinter =
  let open Cil_pretty in
  object (self)
    inherit GoblintCil.Cil.defaultCilPrinterClass as super
    method! pGlobal () (g:global) =             (* global (vars, types, etc.) *)
      match g with
      | GEnumTag (enum, l) ->
          self#pLineDirective l ++
          text "enum" ++ align ++ text (" " ^ enum.ename) ++
          text " {" ++ line
          ++ (docList ~sep:(chr ',' ++ line)
                (fun (n, attrs, i, _loc) ->
                   text n
                   ++ self#pAttrs () attrs
                   ++ text " = "
                   ++ self#pExp () i)
                () enum.eitems)
          ++ unalign ++ line ++ text "} "
          ++ self#pAttrs () enum.eattr ++ text";\n"
      | _ ->
          super#pGlobal () g
  end
let defaultCilPrinter = new defaultCilPrinterClass
