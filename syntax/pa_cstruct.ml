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

type mode = Big_endian | Little_endian | Host_endian

type ty =
  |UInt8
  |UInt16
  |UInt32
  |Buffer of int

type field = {
  field: string;
  ty: ty;
  off: int;
}

type t = {
  name: string; 
  fields: field list;
  len: int;
  endian: mode;
}

let ty_of_string =
  function
  |"uint8_t" -> Some UInt8
  |"uint16_t" -> Some UInt16
  |"uint32_t" -> Some UInt32
  |_ -> None

let width_of_field f =
  match f.ty with
  |UInt8 -> 1
  |UInt16 -> 2
  |UInt32 -> 4
  |Buffer len -> len

let field_to_string f =
  sprintf "%s %s" 
    (match f.ty with
     |UInt8 -> "uint8_t"
     |UInt16 -> "uint16_t"
     |UInt32 -> "uint32_t"
     |Buffer len -> sprintf "uint8_t[%d]" len
    ) f.field

let to_string t =
  sprintf "cstruct[%d] %s { %s }" t.len t.name
    (String.concat "; " (List.map field_to_string t.fields))

let loc_err _loc err = Loc.raise _loc (Failure err)

let parse_field _loc field field_type sz =
  match ty_of_string field_type with
  |None -> loc_err _loc (sprintf "Unknown type %s" field_type)
  |Some ty -> begin
    let ty = match ty,sz with
      |_,None -> ty
      |UInt8,Some sz -> Buffer (int_of_string sz)
      |_,Some sz -> loc_err _loc "only uint8_t buffers supported"
    in
    let off = -1 in
    { field; ty; off }
  end

let create_struct _loc endian name fields =
  let endian = match endian with 
    |"little_endian" -> Little_endian
    |"big_endian" -> Big_endian
    |"host_endian" -> Host_endian
    |_ -> Loc.raise _loc (Failure (sprintf "unknown endian %s, should be little_endian, big_endian or host_endian" endian))
  in
  let len, fields =
    List.fold_left (fun (off,acc) field ->
      let field = {field with off=off} in 
      let off = width_of_field field + off in
      let acc = acc @ [field] in
      (off, acc)
    ) (0,[]) fields
  in
  { fields; name; len; endian }

let mode_mod _loc =
  function
  |Big_endian -> <:expr< Cstruct.BE >>
  |Little_endian -> <:expr< Cstruct.LE >>
  |Host_endian -> <:expr< Cstruct.HE >>

let getter_name s f = sprintf "get_%s_%s" s.name f.field
let setter_name s f = sprintf "set_%s_%s" s.name f.field

let output_get _loc s f =
  let m = mode_mod _loc s.endian in
  let num x = <:expr< $int:string_of_int x$ >> in 
  <:str_item<
    let $lid:getter_name s f$ v = 
      $match f.ty with
       |UInt8 -> <:expr< $m$.get_uint8 v $num f.off$ >>
       |UInt16 -> <:expr< $m$.get_uint16 v $num f.off$ >>
       |UInt32 -> <:expr< $m$.get_uint32 v $num f.off$ >>
       |Buffer len -> <:expr< $m$.get_buffer v $num f.off$ $num len$ >>
      $
  >>

let output_set _loc s f =
  let m = mode_mod _loc s.endian in
  let num x = <:expr< $int:string_of_int x$ >> in 
  <:str_item<
    let $lid:setter_name s f$ v x = 
      $match f.ty with
       |UInt8 -> <:expr< $m$.set_uint8 v $num f.off$ x >>
       |UInt16 -> <:expr< $m$.set_uint16 v $num f.off$ x >>
       |UInt32 -> <:expr< $m$.set_uint32 v $num f.off$ x >>
       |Buffer len -> <:expr< $m$.set_buffer v $num f.off$ $num len$ x >>
      $
  >>

let output_sizeof _loc s =
  <:str_item<
    let $lid:"sizeof_"^s.name$ = $int:string_of_int s.len$
  >>

let output_struct _loc s =
  (* Generate functions of the form {get/set}_<struct>_<field> *)
  let expr = List.fold_left (fun a f ->
      <:str_item< 
          $a$ ;; 
          $output_sizeof _loc s$ ;;
          $output_get _loc s f$ ;; 
          $output_set _loc s f$ 
      >>
    ) <:str_item< >> s.fields
  in
  expr

EXTEND Gram
  GLOBAL: str_item;

  constr_field: [
    [ fty = LIDENT; fname = LIDENT; sz = OPT [ "["; sz = INT; "]" -> sz ] ->
        parse_field _loc fname fty sz
    ]
  ];

  constr_fields: [
    [ "{"; fields = LIST0 constr_field SEP ";"; "}" ->
	fields
    ]
  ];

  str_item: [
    [ "cstruct"; name = LIDENT; fields = constr_fields;
      "as"; endian = LIDENT ->
	output_struct _loc (create_struct _loc endian name fields)
    ]
  ];

END
