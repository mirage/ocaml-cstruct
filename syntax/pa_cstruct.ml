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

let getter_name s f = sprintf "get_%s_%s" s.C.name f.C.field
let setter_name s f = sprintf "set_%s_%s" s.C.name f.C.field

let output_get _loc s f =
  let open Cstruct in
  <:str_item<
    let $lid:getter_name s f$ v = 
      $match f.ty with
       |UInt8 -> <:expr< Cstruct.get_uint8 v $int:string_of_int f.off$ >>
       |UInt16 -> <:expr< Cstruct.get_uint16 v $int:string_of_int f.off$ >>
       |UInt32 -> <:expr< Cstruct.get_uint32 v $int:string_of_int f.off$ >>
      $
  >>

let output_set _loc s f =
  let open Cstruct in
  <:str_item<
    let $lid:setter_name s f$ v x = 
      $match f.ty with
       |UInt8 -> <:expr< Cstruct.set_uint8 v $int:string_of_int f.off$ x >>
       |UInt16 -> <:expr< Cstruct.set_uint16 v $int:string_of_int f.off$ x >>
       |UInt32 -> <:expr< Cstruct.set_uint32 v $int:string_of_int f.off$ x >>
       |_ -> <:expr< -1 >>
      $
  >>

let output_struct _loc s =
  (* Generate functions of the form {get/set}_<struct>_<field> *)
  let expr = List.fold_left (fun a f ->
      <:str_item< $a$ ;; $output_get _loc s f$ ;; $output_set _loc s f$ >>
    ) <:str_item< >> s.C.fields
  in
  expr

EXTEND Gram
  GLOBAL: str_item;

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

  str_item: [
    [ "cstruct"; name = LIDENT; fields = constr_fields ->
	output_struct _loc (C.create_struct name fields)
    ]
  ];

END
