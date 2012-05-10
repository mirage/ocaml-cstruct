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

open Bigarray
open Array1 

type buf = (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t

type uint8 = int
type uint16 = int
type uint32 = int32

module BE = struct

  let get_uint8 s off =
    Char.code (get s off)

  let get_uint16 s off =
    let a = get_uint8 s off in
    let b = get_uint8 s (off+1) in
    (a lsl 8) + b

  let get_uint32 s off =
    let a = Int32.of_int (get_uint16 s off) in
    let b = Int32.of_int (get_uint16 s (off+2)) in
    Int32.(add (shift_left a 16) b)

  let set_uint8 s off v =
    set s off (Char.chr v)

  let set_uint16 s off v =
    set_uint8 s off (v lsr 8);
    set_uint8 s (off+1) (v land 0xff)

  let set_uint32 s off v =
    set_uint16 s off (Int32.(to_int (shift_right_logical v 16)));
    set_uint16 s (off+2) (Int32.(to_int (logand v 0xffffl)))
end

module LE = struct
  open Bigarray.Array1 

  let get_uint8 s off =
    Char.code (get s off)
  (* TODO *)
end

let len buf = dim buf

external base_offset : buf -> int = "caml_bigarray_base_offset"

let sub buf off len = sub buf off len
