(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf

open Camlp4.PreCast
open Syntax
open Ast

module C = Cstruct

let parse_field _loc fname fty =
  match C.create_field fname fty with
  |Some field -> field
  |None -> Loc.raise _loc (Failure (sprintf "Unknown type %s" fty))

(* TODO *)
let output_struct _loc s =
  <:expr<  $str:C.to_string s$ >>

EXTEND Gram
  GLOBAL: expr;

  constr_field: [
    [ fty = LIDENT; fname = LIDENT ->
        parse_field _loc fname fty
    ]
  ];

  constr_fields: [
    [ "{"; fields = LIST0 constr_field SEP ";"; "}" ->
	fields
    ]
  ];

  expr: LEVEL ";" [
    [ "cstruct"; name = LIDENT; fields = constr_fields ->
	output_struct _loc (C.create_struct name fields)
    ]
  ];

END
