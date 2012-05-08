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

type ty =
  |UInt8
  |UInt16
  |UInt32

type field = {
  field: string;
  ty: ty;
  off: int;
  
}

type t = {
  name: string; 
  fields: field list;
  len: int;
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

let field_to_string f =
  sprintf "%s %s" 
    (match f.ty with
     |UInt8 -> "uint8_t"
     |UInt16 -> "uint16_t"
     |UInt32 -> "uint32_t"
    ) f.field

let to_string t =
  sprintf "cstruct[%d] %s { %s }" t.len t.name
    (String.concat "; " (List.map field_to_string t.fields))

let create_field field field_type =
  match ty_of_string field_type with
  |None -> None
  |Some ty ->
    let off = 0 in (* XXX *)
    Some { field; ty; off }

let create_struct name fields =
  let len, fields =
    List.fold_left (fun (off,acc) field ->
      let field = {field with off=off} in 
      let off = width_of_field field + off in
      let acc = acc @ [field] in
      (off, acc)
    ) (0,[]) fields
  in
  { fields; name; len }

