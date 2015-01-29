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
open Sexplib.Std

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  buffer: buffer;
  off   : int;
  len   : int;
}

let of_bigarray ?(off=0) ?len buffer =
  let dim = Bigarray.Array1.dim buffer in
  let len =
    match len with
    | None     -> dim - off
    | Some len -> len in
  if off < 0 || len < 0 || off + len > dim then
    raise (Invalid_argument "Cstruct.of_bigarray");
  { buffer; off; len }

let to_bigarray buffer =
  Bigarray.Array1.sub buffer.buffer buffer.off buffer.len

let create len =
  let buffer = Bigarray.(Array1.create char c_layout len) in
  { buffer ; len ; off = 0 }

let check_bounds t len =
  Bigarray.Array1.dim t.buffer >= len

type byte = char

let byte (i:int) : byte = Char.chr i
let byte_to_int (b:byte) = int_of_char b

type bytes = string

let bytes (s:string) : bytes = s

type uint8 = int

let uint8 (i:int) : uint8 = min i 0xff

type uint16 = int

let uint16 (i:int) : uint16 = min i 0xffff

type uint32 = int32
type uint64 = int64

let debug t =
  let max_len = Bigarray.Array1.dim t.buffer in
  let str = Printf.sprintf "t=[%d,%d](%d)" t.off t.len max_len in
  if t.off+t.len > max_len || t.len < 0 || t.off < 0 then (
    Printf.printf "ERROR: t.off+t.len=%d %s\n" (t.off+t.len) str;
    assert false;
  );
  str

let sub t off0 len =
  let off = t.off + off0 in
  if off0 < 0 ||
     len < 0 ||
     not (check_bounds t (off+len)) then
    raise (Invalid_argument "Cstruct.sub");
  { t with off; len }

let shift t amount =
  let off = t.off + amount in
  let len = t.len - amount in
  if amount < 0 ||
     amount > t.len ||
     not (check_bounds t (off+len)) then
    raise (Invalid_argument "Cstruct.shift");
  { t with off; len }

let set_len t len =
  if len < 0 || not (check_bounds t (t.off+len)) then
    raise (Invalid_argument "Cstruct.set_len");
  { t with len }

let add_len t len =
  let len = t.len + len in
  if len < 0 || not (check_bounds t (t.off+len)) then
    raise (Invalid_argument "Cstruct.add_len");
  { t with len }

let invalid_arg fmt =
  let b = Buffer.create 20 in (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf = Format.pp_print_flush ppf (); invalid_arg (Buffer.contents b) in
  Format.kfprintf k ppf fmt

let invalid_bounds j l = invalid_arg "invalid bounds (index %d, length %d)" j l

external unsafe_blit_bigstring_to_bigstring : buffer -> int -> buffer -> int -> int -> unit = "caml_blit_bigstring_to_bigstring" "noalloc"

external unsafe_blit_string_to_bigstring : string -> int -> buffer -> int -> int -> unit = "caml_blit_string_to_bigstring" "noalloc"

external unsafe_blit_bigstring_to_string : buffer -> int -> string -> int -> int -> unit = "caml_blit_bigstring_to_string" "noalloc"

let copy src srcoff len =
  if len < 0 || srcoff < 0 || src.len - srcoff < len then raise (Invalid_argument (invalid_bounds srcoff len));
  let s = String.create len in
  unsafe_blit_bigstring_to_string src.buffer (src.off+srcoff) s 0 len;
  s

let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || src.len - srcoff < len then raise (Invalid_argument (invalid_bounds srcoff len));
  if dst.len - dstoff < len then raise (Invalid_argument (invalid_bounds dstoff len));
  unsafe_blit_bigstring_to_bigstring src.buffer (src.off+srcoff) dst.buffer (dst.off+dstoff) len

let blit_from_string src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || String.length src - srcoff < len then raise (Invalid_argument (invalid_bounds srcoff len));
  if dst.len - dstoff < len then raise (Invalid_argument (invalid_bounds dstoff len));
  unsafe_blit_string_to_bigstring src srcoff dst.buffer (dst.off+dstoff) len

let blit_to_string src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || src.len - srcoff < len then raise (Invalid_argument (invalid_bounds srcoff len));
  if String.length dst - dstoff < len then raise (Invalid_argument (invalid_bounds dstoff len));
  unsafe_blit_bigstring_to_string src.buffer (src.off+srcoff) dst dstoff len

let set_uint8 t i c =
  if i >= t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 1)) ;
  EndianBigstring.BigEndian.set_int8 t.buffer (t.off+i) c

let set_char t i c =
  if i >= t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 1)) ;
  EndianBigstring.BigEndian.set_char t.buffer (t.off+i) c

let get_uint8 t i =
  if i >= t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 1)) ;
  EndianBigstring.BigEndian.get_uint8 t.buffer (t.off+i)

let get_char t i =
  if i >= t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 1)) ;
  EndianBigstring.BigEndian.get_char t.buffer (t.off+i)

module BE = struct
  include EndianBigstring.BigEndian

  let set_uint16 t i c =
    if (i+2) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 2));
    set_int16 t.buffer (t.off+i) c

  let set_uint32 t i c =
    if (i+4) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 4));
    set_int32 t.buffer (t.off+i) c

  let set_uint64 t i c =
    if (i+8) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 8));
    set_int64 t.buffer (t.off+i) c

  let get_uint16 t i = 
    if (i+2) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 2));
    get_uint16 t.buffer (t.off+i)

  let get_uint32 t i =
    if (i+4) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 4));
    get_int32 t.buffer (t.off+i)

  let get_uint64 t i =
    if (i+8) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 8));
    get_int64 t.buffer (t.off+i)
end

module LE = struct
  include EndianBigstring.LittleEndian

  let set_uint16 t i c =
    if (i+2) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 2));
    set_int16 t.buffer (t.off+i) c

  let set_uint32 t i c =
    if (i+4) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 4));
    set_int32 t.buffer (t.off+i) c

  let set_uint64 t i c =
    if (i+8) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 8));
    set_int64 t.buffer (t.off+i) c

  let get_uint16 t i = 
    if (i+2) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 2));
    get_uint16 t.buffer (t.off+i)

  let get_uint32 t i =
    if (i+4) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 4));
    get_int32 t.buffer (t.off+i)

  let get_uint64 t i =
    if (i+8) > t.len || i < 0 then raise (Invalid_argument (invalid_bounds i 8));
    get_int64 t.buffer (t.off+i)
end

let len t =
  t.len

let lenv = function
  | []  -> 0
  | [t] -> len t
  | ts  -> List.fold_left (fun a b -> len b + a) 0 ts

let copyv ts =
  let sz = lenv ts in
  let dst = String.create sz in
  let _ = List.fold_left
    (fun off src ->
      let x = len src in
      unsafe_blit_bigstring_to_string src.buffer src.off dst off x;
      off + x
    ) 0 ts in
  dst

let fillv ~src ~dst =
  let rec aux dst n = function
    | [] -> n, []
    | hd::tl ->
        let avail = len dst in
        let first = len hd in
        if first <= avail then (
          blit hd 0 dst 0 first;
          aux (shift dst first) (n + first) tl
        ) else (
          blit hd 0 dst 0 avail;
          let rest_hd = shift hd first in
          (n + avail, rest_hd :: tl)
        ) in
  aux dst 0 src


let to_string t =
  let sz = len t in
  let s = String.create sz in
  unsafe_blit_bigstring_to_string t.buffer t.off s 0 sz;
  s

let of_string ?allocator buf =
  let buflen = String.length buf in
  match allocator with
  |None -> 
    let c = create buflen in
    blit_from_string buf 0 c 0 buflen;
    c
  |Some fn -> 
    let c = fn buflen in
    blit_from_string buf 0 c 0 buflen;
    set_len c buflen
    
let hexdump t =
  let c = ref 0 in
  for i = 0 to len t - 1 do
    if !c mod 16 = 0 then print_endline "";
    printf "%.2x " (Char.code (Bigarray.Array1.get t.buffer (t.off+i)));
    incr c;
  done;
  print_endline ""

let hexdump_to_buffer buf t =
  let c = ref 0 in
  for i = 0 to len t - 1 do
    if !c mod 16 = 0 then Buffer.add_char buf '\n';
    bprintf buf "%.2x " (Char.code (Bigarray.Array1.get t.buffer (t.off+i)));
    incr c;
  done;
  Buffer.add_char buf '\n'

let split ?(start=0) t off =
  let header = sub t start off in
  let body = sub t (start+off) (len t - off - start) in
  header, body

type 'a iter = unit -> 'a option
let iter lenfn pfn buf =
  let body = ref (Some buf) in
  fun () ->
    match !body with
      |Some buf when len buf = 0 ->
        body := None;
        None
      |Some buf -> begin
        match lenfn buf with
        |None ->
          body := None;
          None
        |Some plen ->
          let p,rest = split buf plen in
          body := Some rest;
          Some (pfn p)
      end
      |None -> None

let rec fold f next acc = match next () with
  | None -> acc
  | Some v -> fold f next (f acc v)

let buffer_of_sexp b = Sexplib.Conv.bigstring_of_sexp b
let sexp_of_buffer b = Sexplib.Conv.sexp_of_bigstring b

let t_of_sexp s = of_bigarray (buffer_of_sexp s)
let sexp_of_t t = sexp_of_buffer (to_bigarray t)
