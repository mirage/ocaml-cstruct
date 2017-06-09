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

(* Note:
 *
 * We try to maintain the property that no constructed [t] can ever point out of
 * its underlying buffer. This property is guarded by all of the constructing
 * functions and the fact that the type is private, and used by various
 * functions that would otherwise be completely unsafe.
 *
 * All well-intended souls are kindly invited to cross-check that the code
 * indeed maintains this invariant.
 *)

type t = {
  buffer: buffer;
  off   : int;
  len   : int;
}

let pp_t ppf t =
  Format.fprintf ppf "[%d,%d](%d)" t.off t.len (Bigarray.Array1.dim t.buffer)
let string_t ppf str =
  Format.fprintf ppf "[%d]" (String.length str)
let bytes_t ppf str =
  Format.fprintf ppf "[%d]" (Bytes.length str)

let err fmt =
  let b = Buffer.create 20 in                         (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf = Format.pp_print_flush ppf (); invalid_arg (Buffer.contents b) in
  Format.kfprintf k ppf fmt

let err_of_bigarray t = err "Cstruct.of_bigarray off=%d len=%d" t
let err_sub t = err "Cstruct.sub: %a off=%d len=%d" pp_t t
let err_shift t = err "Cstruct.shift %a %d" pp_t t
let err_set_len t = err "Cstruct.set_len %a %d" pp_t t
let err_add_len t = err "Cstruct.add_len %a %d" pp_t t
let err_copy t = err "Cstruct.copy %a off=%d len=%d" pp_t t
let err_blit_src src dst =
  err "Cstruct.blit src=%a dst=%a src-off=%d len=%d" pp_t src pp_t dst
let err_blit_dst src dst =
  err "Cstruct.blit src=%a dst=%a dst-off=%d len=%d" pp_t src pp_t dst
let err_blit_from_string_src src dst =
  err "Cstruct.blit_from_string src=%a dst=%a src-off=%d len=%d"
    string_t src pp_t dst
let err_blit_from_string_dst src dst =
  err "Cstruct.blit_from_string src=%a dst=%a dst-off=%d len=%d"
    string_t src pp_t dst
let err_blit_from_bytes_src src dst =
  err "Cstruct.blit_from_bytes src=%a dst=%a src-off=%d len=%d"
    bytes_t src pp_t dst
let err_blit_from_bytes_dst src dst =
  err "Cstruct.blit_from_bytes src=%a dst=%a dst-off=%d len=%d"
    bytes_t src pp_t dst
let err_blit_to_bytes_src src dst =
  err "Cstruct.blit_to_bytes src=%a dst=%a src-off=%d len=%d"
    pp_t src bytes_t dst
let err_blit_to_bytes_dst src dst=
  err "Cstruct.blit_to_bytes src=%a dst=%a dst-off=%d len=%d"
    pp_t src bytes_t dst
let err_invalid_bounds f =
  err "invalid bounds in Cstruct.%s %a off=%d len=%d" f pp_t
let err_split t = err "Cstruct.split %a start=%d off=%d" pp_t t
let err_iter t = err "Cstruct.iter %a i=%d len=%d" pp_t t

let of_bigarray ?(off=0) ?len buffer =
  let dim = Bigarray.Array1.dim buffer in
  let len =
    match len with
    | None     -> dim - off
    | Some len -> len in
  if off < 0 || len < 0 || off + len > dim then err_of_bigarray off len
  else { buffer; off; len }

let to_bigarray buffer =
  Bigarray.Array1.sub buffer.buffer buffer.off buffer.len

let create_unsafe len =
  let buffer = Bigarray.(Array1.create char c_layout len) in
  { buffer ; len ; off = 0 }

let check_bounds t len =
  Bigarray.Array1.dim t.buffer >= len

external check_alignment_bigstring : buffer -> int -> int -> bool = "caml_check_alignment_bigstring"

let check_alignment t alignment =
  if alignment > 0 then
    check_alignment_bigstring t.buffer t.off alignment
  else invalid_arg "check_alignment must be positive integer"

type byte = char

let byte (i:int) : byte = Char.chr i
let byte_to_int (b:byte) = int_of_char b

type uint8 = int

let uint8 (i:int) : uint8 = min i 0xff

type uint16 = int

let uint16 (i:int) : uint16 = min i 0xffff

type uint32 = int32
type uint64 = int64

let debug t =
  let max_len = Bigarray.Array1.dim t.buffer in
  if t.off+t.len > max_len || t.len < 0 || t.off < 0 then (
    Format.printf "ERROR: t.off+t.len=%d %a\n%!" (t.off+t.len) pp_t t;
    assert false;
  ) else
    Format.asprintf "%a" pp_t t

let sub t off0 len =
  let off = t.off + off0 in
  if off0 < 0 || len < 0 || not (check_bounds t (off+len)) then err_sub t off0 len
  else { t with off; len }

let shift t amount =
  let off = t.off + amount in
  let len = t.len - amount in
  if amount < 0 || amount > t.len || not (check_bounds t (off+len)) then
    err_shift t amount
  else { t with off; len }

let set_len t len =
  if len < 0 || not (check_bounds t (t.off+len)) then err_set_len t len
  else { t with len }

let add_len t len =
  let len = t.len + len in
  if len < 0 || not (check_bounds t (t.off+len)) then err_add_len t len
  else { t with len }

external unsafe_get_pointer : buffer -> int option = "caml_get_pointer"

let get_pointer x = unsafe_get_pointer x.buffer

external unsafe_blit_bigstring_to_bigstring : buffer -> int -> buffer -> int -> int -> unit = "caml_blit_bigstring_to_bigstring" "noalloc"

external unsafe_blit_string_to_bigstring : string -> int -> buffer -> int -> int -> unit = "caml_blit_string_to_bigstring" "noalloc"

external unsafe_blit_bytes_to_bigstring : Bytes.t -> int -> buffer -> int -> int -> unit = "caml_blit_string_to_bigstring" "noalloc"

external unsafe_blit_bigstring_to_bytes : buffer -> int -> Bytes.t -> int -> int -> unit = "caml_blit_bigstring_to_string" "noalloc"

external unsafe_blit_bigstring_to_string : buffer -> int -> string -> int -> int -> unit = "caml_blit_bigstring_to_string" "noalloc"

external unsafe_compare_bigstring : buffer -> int -> buffer -> int -> int -> int = "caml_compare_bigstring" "noalloc"

external unsafe_fill_bigstring : buffer -> int -> int -> int -> unit = "caml_fill_bigstring" "noalloc"

let copy src srcoff len =
  if len < 0 || srcoff < 0 || src.len - srcoff < len then
    err_copy src srcoff len
  else
    let b = Bytes.create len in
    unsafe_blit_bigstring_to_bytes src.buffer (src.off+srcoff) b 0 len;
    (* The following call is safe, since b is not visible elsewhere. *)
    Bytes.unsafe_to_string b

let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || src.len - srcoff < len then
    err_blit_src src dst srcoff len
  else if dst.len - dstoff < len then
    err_blit_dst src dst dstoff len
  else
    unsafe_blit_bigstring_to_bigstring src.buffer (src.off+srcoff) dst.buffer
      (dst.off+dstoff) len

let blit_from_string src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || String.length src - srcoff < len then
    err_blit_from_string_src src dst srcoff len
  else if dst.len - dstoff < len then
    err_blit_from_string_dst src dst dstoff len
  else
    unsafe_blit_string_to_bigstring src srcoff dst.buffer (dst.off+dstoff) len

let blit_from_bytes src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || Bytes.length src - srcoff < len then
    err_blit_from_bytes_src src dst srcoff len
  else if dst.len - dstoff < len then
    err_blit_from_bytes_dst src dst dstoff len
  else
    unsafe_blit_bytes_to_bigstring src srcoff dst.buffer (dst.off+dstoff) len

let blit_to_bytes src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || src.len - srcoff < len then
    err_blit_to_bytes_src src dst srcoff len
  else if Bytes.length dst - dstoff < len then
    err_blit_to_bytes_dst src dst dstoff len
  else
    unsafe_blit_bigstring_to_bytes src.buffer (src.off+srcoff) dst dstoff len

let blit_to_string = blit_to_bytes

let compare t1 t2 =
  let l1 = t1.len
  and l2 = t2.len in
  match compare l1 l2 with
  | 0 ->
    ( match unsafe_compare_bigstring t1.buffer t1.off t2.buffer t2.off l1 with
      | 0 -> 0
      | r -> if r < 0 then -1 else 1 )
  | r -> r

let equal t1 t2 = compare t1 t2 = 0

(* Note that this is only safe as long as all [t]s are coherent. *)
let memset t x = unsafe_fill_bigstring t.buffer t.off t.len x

let create len =
  let t = create_unsafe len in
  memset t 0;
  t

let set_uint8 t i c =
  if i >= t.len || i < 0 then err_invalid_bounds "set_uint8" t i 1
  else EndianBigstring.BigEndian.set_int8 t.buffer (t.off+i) c

let set_char t i c =
  if i >= t.len || i < 0 then err_invalid_bounds "set_char" t i 1
  else EndianBigstring.BigEndian.set_char t.buffer (t.off+i) c

let get_uint8 t i =
  if i >= t.len || i < 0 then err_invalid_bounds "get_uint8" t i 1
  else EndianBigstring.BigEndian.get_uint8 t.buffer (t.off+i)

let get_char t i =
  if i >= t.len || i < 0 then err_invalid_bounds "get_char" t i 1
  else EndianBigstring.BigEndian.get_char t.buffer (t.off+i)

module BE = struct
  include EndianBigstring.BigEndian

  let set_uint16 t i c =
    if (i+2) > t.len || i < 0 then err_invalid_bounds "BE.set_uint16" t i 2
    else set_int16 t.buffer (t.off+i) c

  let set_uint32 t i c =
    if (i+4) > t.len || i < 0 then err_invalid_bounds "BE.set_uint32" t i 4
    else set_int32 t.buffer (t.off+i) c

  let set_uint64 t i c =
    if (i+8) > t.len || i < 0 then err_invalid_bounds "BE.set_uint64" t i 8
    else set_int64 t.buffer (t.off+i) c

  let get_uint16 t i =
    if (i+2) > t.len || i < 0 then err_invalid_bounds "BE.get_uint16" t i 2
    else get_uint16 t.buffer (t.off+i)

  let get_uint32 t i =
    if (i+4) > t.len || i < 0 then err_invalid_bounds "BE.get_uint32" t i 4
    else get_int32 t.buffer (t.off+i)

  let get_uint64 t i =
    if (i+8) > t.len || i < 0 then err_invalid_bounds "BE.uint64" t i 8
    else get_int64 t.buffer (t.off+i)
end

module LE = struct
  include EndianBigstring.LittleEndian

  let set_uint16 t i c =
    if (i+2) > t.len || i < 0 then err_invalid_bounds "LE.set_uint16" t i 2
    else set_int16 t.buffer (t.off+i) c

  let set_uint32 t i c =
    if (i+4) > t.len || i < 0 then err_invalid_bounds "LE.set_uint32" t i 4
    else set_int32 t.buffer (t.off+i) c

  let set_uint64 t i c =
    if (i+8) > t.len || i < 0 then err_invalid_bounds "LE.set_uint64" t i 8
    else set_int64 t.buffer (t.off+i) c

  let get_uint16 t i =
    if (i+2) > t.len || i < 0 then err_invalid_bounds "LE.get_uint16" t i 2
    else get_uint16 t.buffer (t.off+i)

  let get_uint32 t i =
    if (i+4) > t.len || i < 0 then err_invalid_bounds "LE.get_uint32" t i 4
    else get_int32 t.buffer (t.off+i)

  let get_uint64 t i =
    if (i+8) > t.len || i < 0 then err_invalid_bounds "LE.get_uint64" t i 8
    else get_int64 t.buffer (t.off+i)
end

let len t =
  t.len

let lenv = function
  | []  -> 0
  | [t] -> len t
  | ts  -> List.fold_left (fun a b -> len b + a) 0 ts

let copyv ts =
  let sz = lenv ts in
  let dst = Bytes.create sz in
  let _ = List.fold_left
    (fun off src ->
      let x = len src in
      unsafe_blit_bigstring_to_bytes src.buffer src.off dst off x;
      off + x
    ) 0 ts in
  (* The following call is safe, since dst is not visible elsewhere. *)
  Bytes.unsafe_to_string dst

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
          let rest_hd = shift hd avail in
          (n + avail, rest_hd :: tl)
        ) in
  aux dst 0 src


let to_string t =
  let sz = len t in
  let b = Bytes.create sz in
  unsafe_blit_bigstring_to_bytes t.buffer t.off b 0 sz;
  (* The following call is safe, since b is not visible elsewhere. *)
  Bytes.unsafe_to_string b

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

let of_bytes ?allocator buf =
  let buflen = Bytes.length buf in
  match allocator with
  |None ->
    let c = create buflen in
    blit_from_bytes buf 0 c 0 buflen;
    c
  |Some fn ->
    let c = fn buflen in
    blit_from_bytes buf 0 c 0 buflen;
    set_len c buflen

let hexdump_pp fmt t =
  for i = 0 to len t - 1 do
    Format.fprintf fmt "%.2x " (Char.code (Bigarray.Array1.get t.buffer (t.off+i)));
    if i mod 16 = 15 then Format.pp_print_space fmt ();
  done

let hexdump = Format.printf "@\n%a@." hexdump_pp

let hexdump_to_buffer buf t =
  let f = Format.formatter_of_buffer buf in
  Format.fprintf f "@\n%a@." hexdump_pp t

let split ?(start=0) t off =
  try
    let header =sub t start off in
    let body = sub t (start+off) (len t - off - start) in
    header, body
  with Invalid_argument _ -> err_split t start off

type 'a iter = unit -> 'a option
let iter lenfn pfn t =
  let body = ref (Some t) in
  let i = ref 0 in
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
          incr i;
          let p,rest =
            try split buf plen with Invalid_argument _ -> err_iter buf !i plen
          in
          body := Some rest;
          Some (pfn p)
      end
      |None -> None

let rec fold f next acc = match next () with
  | None -> acc
  | Some v -> fold f next (f acc v)

let append cs1 cs2 =
  let l1 = len cs1 and l2 = len cs2 in
  let cs = create (l1 + l2) in
  blit cs1 0 cs 0  l1 ;
  blit cs2 0 cs l1 l2 ;
  cs

let concat = function
  | []   -> create 0
  | [cs] -> cs
  | css  ->
      let result = create (lenv css) in
      let aux off cs =
        let n = len cs in
        blit cs 0 result off n ;
        off + n in
      ignore @@ List.fold_left aux 0 css ;
      result

open Sexplib

let buffer_of_sexp b = Conv.bigstring_of_sexp b
let sexp_of_buffer b = Conv.sexp_of_bigstring b

let t_of_sexp = function
  | Sexp.Atom str ->
      let n = String.length str in
      let t = create n in
      blit_from_string str 0 t 0 n ;
      t
  | sexp -> Conv.of_sexp_error "Cstruct.t_of_sexp: atom needed" sexp

let sexp_of_t t =
  let n   = len t in
  let str = Bytes.create n in
  blit_to_bytes t 0 str 0 n ;
  (* The following call is safe, since str is not visible elsewhere. *)
  Sexp.Atom (Bytes.unsafe_to_string str)
