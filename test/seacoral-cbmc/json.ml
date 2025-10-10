(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Sc_cbmc.Json

let program = [{|
{
  "program": "CBMC 5.51.0 (cbmc-5.51.0-72-g412d04059)"
}
|}
]

let base_values = [
  {|{
      "binary": "10000000000000000000000000000001",
      "data": "-2147483647",
      "name": "integer",
      "type": "signed int",
      "width": 32
    }
|};
]

let structured = [
  {|{
     "members": [
       {
          "name": "entier",
          "value": {
            "binary": "00000000000000000000001000000000",
            "data": "512",
            "name": "integer",
            "type": "signed int",
            "width": 32
          }
       },{
          "name": "lettre",
          "value": {
            "binary": "00101100",
            "data": "','",
            "name": "integer",
            "type": "char",
            "width": 8
          }
       },{
        "name": "$pad2",
        "value": {
          "binary": "000000000000000000000000",
          "data": "0",
          "name": "integer",
          "type": "unsigned __CPROVER_bitvector[24]",
          "width": 24
       }
     }
   ],
   "name": "struct"
  }|}
]

let values = [
  {| {
         "binary": "10000000000000000000000000000001",
         "data": "-2147483647",
         "name": "integer",
         "type": "signed int",
         "width": 32
     }
  |};
  {|{
    "members": [
                {
        "name": "entier",
        "value": {
          "binary": "00000000000000000000001000000000",
          "data": "512",
          "name": "integer",
          "type": "signed int",
          "width": 32
                  }
                },
                {
        "name": "lettre",
        "value": {
          "binary": "00101100",
          "data": "','",
          "name": "integer",
          "type": "char",
          "width": 8
                  }
                },
                {
        "name": "$pad2",
        "value": {
          "binary": "000000000000000000000000",
          "data": "0",
          "name": "integer",
          "type": "unsigned __CPROVER_bitvector[24]",
          "width": 24
                  }
                }
              ],
    "name": "struct"
    }
  |}
]

let inputs = [
  {|{
            "id": "u",
            "value": {
              "members": [
                {
                  "name": "entier",
                  "value": {
                    "binary": "00000000000000000000001000000000",
                    "data": "512",
                    "name": "integer",
                    "type": "signed int",
                    "width": 32
                  }
                },
                {
                  "name": "lettre",
                  "value": {
                    "binary": "00101100",
                    "data": "','",
                    "name": "integer",
                    "type": "char",
                    "width": 8
                  }
                },
                {
                  "name": "$pad2",
                  "value": {
                    "binary": "000000000000000000000000",
                    "data": "0",
                    "name": "integer",
                    "type": "unsigned __CPROVER_bitvector[24]",
                    "width": 24
                  }
                }
              ],
              "name": "struct"
            }
          }
  |}
]

let goals_details = [{| {
    "goals": [
      {
        "description": "condition 'u.entier == 0'",
        "goal": "f.coverage.1",
        "sourceLocation": {
          "file": "/home/ovenstent/gits/sc/_sc/56344ec47627ecde4f03d0a13cf75348/CC_f_labels.c",
          "function": "f",
          "line": "23",
          "workingDirectory": "/home/ovenstent/gits/sc"
        },
        "status": "satisfied"
      },
      {
        "description": "condition '!(u.entier == 0)'",
        "goal": "f.coverage.2",
        "sourceLocation": {
          "file": "/home/ovenstent/gits/sc/_sc/56344ec47627ecde4f03d0a13cf75348/CC_f_labels.c",
          "function": "f",
          "line": "24",
          "workingDirectory": "/home/ovenstent/gits/sc"
        },
        "status": "satisfied"
      }
    ],
    "goalsCovered": 2,
    "totalGoals": 2
  }
|}]

let data = [
  {|{"program": "CBMC 5.51.0 (cbmc-5.51.0-72-g412d04059)"}|};
  {|{
    "messageText": "CBMC version 5.51.0 (cbmc-5.51.0-72-g412d04059) 64-bit x86_64 linux",
    "messageType": "STATUS-MESSAGE"
  }|};
  {|{
    "messageText": "the program has no entry point",
    "messageType": "ERROR"
  }|};{|
  {
    "tests": [
      {
        "coveredGoals": [ "f.coverage.2" ],
        "inputs": [
          {
            "id": "u",
            "value": {
              "members": [
                {
                  "name": "entier",
                  "value": {
                    "binary": "00000000000000000000001000000000",
                    "data": "512",
                    "name": "integer",
                    "type": "signed int",
                    "width": 32
                  }
                },
                {
                  "name": "lettre",
                  "value": {
                    "binary": "00101100",
                    "data": "','",
                    "name": "integer",
                    "type": "char",
                    "width": 8
                  }
                },
                {
                  "name": "$pad2",
                  "value": {
                    "binary": "000000000000000000000000",
                    "data": "0",
                    "name": "integer",
                    "type": "unsigned __CPROVER_bitvector[24]",
                    "width": 24
                  }
                }
              ],
              "name": "struct"
            }
          }
        ]
      },
      {
        "coveredGoals": [ "f.coverage.1" ],
        "inputs": [
          {
            "id": "u",
            "value": {
              "members": [
                {
                  "name": "entier",
                  "value": {
                    "binary": "00000000000000000000000000000000",
                    "data": "0",
                    "name": "integer",
                    "type": "signed int",
                    "width": 32
                  }
                },
                {
                  "name": "lettre",
                  "value": {
                    "binary": "00101100",
                    "data": "','",
                    "name": "integer",
                    "type": "char",
                    "width": 8
                  }
                },
                {
                  "name": "$pad2",
                  "value": {
                    "binary": "000000000000000000000000",
                    "data": "0",
                    "name": "integer",
                    "type": "unsigned __CPROVER_bitvector[24]",
                    "width": 24
                  }
                }
              ],
              "name": "struct"
            }
          }
        ]
      }
    ]
  }
|};{|
{
    "goals": [
      {
        "description": "condition 'u.entier == 0'",
        "goal": "f.coverage.1",
        "sourceLocation": {
          "file": "/home/ovenstent/gits/sc/_sc/56344ec47627ecde4f03d0a13cf75348/CC_f_labels.c",
          "function": "f",
          "line": "23",
          "workingDirectory": "/home/ovenstent/gits/sc"
        },
        "status": "satisfied"
      },
      {
        "description": "condition '!(u.entier == 0)'",
        "goal": "f.coverage.2",
        "sourceLocation": {
          "file": "/home/ovenstent/gits/sc/_sc/56344ec47627ecde4f03d0a13cf75348/CC_f_labels.c",
          "function": "f",
          "line": "24",
          "workingDirectory": "/home/ovenstent/gits/sc"
        },
        "status": "satisfied"
      }
    ],
    "goalsCovered": 2,
    "totalGoals": 2
  }

|}
]

let properties = [
  {|
    {
        "class": "coverage",
        "description": "condition 'u.entier == 0'",
        "expression": "!(u.entier == 0)",
        "name": "f.coverage.1",
        "sourceLocation": {
          "file": "/home/ovenstent/gits/sc/_sc/56344ec47627ecde4f03d0a13cf75348/CC_f_labels.c",
          "function": "f",
          "line": "23",
          "workingDirectory": "/home/ovenstent/gits/sc"
        }
     }
  |};{|
      {
        "class": "coverage",
        "description": "condition '!(u.entier == 0)'",
        "expression": "!(!(u.entier == 0))",
        "name": "f.coverage.2",
        "sourceLocation": {
          "file": "/home/ovenstent/gits/sc/_sc/56344ec47627ecde4f03d0a13cf75348/CC_f_labels.c",
          "function": "f",
          "line": "24",
          "workingDirectory": "/home/ovenstent/gits/sc"
        }
      }
  |};
]

let data_properties = [{|[
  {
    "program": "CBMC 5.51.0 (cbmc-5.51.0-72-g412d04059)"
  },
  {
    "messageText": "CBMC version 5.51.0 (cbmc-5.51.0-72-g412d04059) 64-bit x86_64 linux",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "messageText": "Parsing _sc/56344ec47627ecde4f03d0a13cf75348/cbmc/fCC_f_labels.c",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "messageText": "Converting",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "messageText": "Type-checking fCC_f_labels",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "messageText": "Generating GOTO Program",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "messageText": "Adding CPROVER library (x86_64)",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "messageText": "Removal of function pointers and virtual functions",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "messageText": "Generic Property Instrumentation",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "messageText": "Rewriting existing assertions as assumptions",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "messageText": "Running with 8 object bits, 56 offset bits (default)",
    "messageType": "STATUS-MESSAGE"
  },
  {
    "properties": [
      {
        "class": "coverage",
        "description": "condition 'u.entier == 0'",
        "expression": "!(u.entier == 0)",
        "name": "f.coverage.1",
        "sourceLocation": {
          "file": "/home/ovenstent/gits/sc/_sc/56344ec47627ecde4f03d0a13cf75348/CC_f_labels.c",
          "function": "f",
          "line": "23",
          "workingDirectory": "/home/ovenstent/gits/sc"
        }
      },
      {
        "class": "coverage",
        "description": "condition '!(u.entier == 0)'",
        "expression": "!(!(u.entier == 0))",
        "name": "f.coverage.2",
        "sourceLocation": {
          "file": "/home/ovenstent/gits/sc/_sc/56344ec47627ecde4f03d0a13cf75348/CC_f_labels.c",
          "function": "f",
          "line": "24",
          "workingDirectory": "/home/ovenstent/gits/sc"
        }
      }
    ]
  }
]|}]

let assignments = [ {|
{
  "assignmentType": "variable",
  "hidden": true,
  "internal": false,
  "lhs": "__CPROVER_malloc_is_new_array",
  "mode": "C",
  "sourceLocation": {
    "file": "<built-in-additions>",
    "line": "16",
    "workingDirectory": "/home/ovenstent/gits/thales_test_tool/cbmc"
  },
  "stepType": "assignment",
  "thread": 0,
  "value": {
    "binary": "0",
    "data": false,
    "name": "boolean"
  }
}
|};{|
{
            "assignmentType": "variable",
            "hidden": true,
            "internal": false,
            "lhs": "__CPROVER_malloc_is_new_array",
            "mode": "C",
            "sourceLocation": {
              "file": "<built-in-additions>",
              "line": "16",
              "workingDirectory": "/home/ovenstent/gits/sc"
            },
            "stepType": "assignment",
            "thread": 0,
            "value": {
              "binary": "0",
              "data": false,
              "name": "boolean"
            }
          }
|}]

let rec print_exn exn =
  match exn with
  | Json_encoding.Cannot_destruct (path, exn) ->
      Format.printf "Error while destructing: %a@." (Json_query.print_path_as_json_path ~wildcards:true) path;
      print_exn exn
  | Json_encoding.No_case_matched el -> List.iter print_exn el
  | _ -> Format.printf "Error %s@." (Printexc.to_string exn)

let test_elt enc elt =
  try
    let yoj = Yojson.Safe.from_string elt in
    let js = Json_repr.from_yojson yoj in
    ignore @@ Json_encoding.destruct enc js
  with
  | exn -> print_exn exn

let test_cbmc_output data = test_elt Output.(cell cbmc_cover_output) data
let test_value v = test_elt Output.value v
let test_structured v = test_elt Output.structured v
let test_base_value v = test_elt Output.base_value v
let test_inputs v = test_elt Output.input v
let test_goals_details v = test_elt Output.goals_details v
let test_property_data v = test_elt Output.property_data v
let test_property v = test_elt Output.property v
let test_assignment v = test_elt Output.assignment v

let%expect_test "Cbmc_data.encodings" =
  List.iter test_assignment assignments;
  Format.printf "()";
  [%expect {| () |}]
