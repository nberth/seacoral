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

;; Printexc.register_printer begin function
  | Parsing_failure s ->
      Some (Format.sprintf "Failure while parsing: %s" s)

  | _ -> None
end


let rec print ppf = function
  | Nothing ->
      Fmt.pf ppf "--nothing--"
  | Tool s ->
      Fmt.pf ppf "%s" s
  | Parallel l ->
      NEL.pp ~fopen:"(" ~fsep:" // " ~fclose:")" print ppf l
  | Sequence l ->
      NEL.pp ~fopen:"[" ~fsep:" ~> " ~fclose:"]" print ppf l
