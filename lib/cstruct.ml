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

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  buffer: buffer;
  off   : int;
  len   : int;
}

let of_bigarray ?(off=0) ?len buffer =
  let len =
    match len with
    |None -> Bigarray.Array1.dim buffer
    |Some len -> min len (Bigarray.Array1.dim buffer)
  in { buffer; off; len }

let create len =
  let ba = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
  of_bigarray ba

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

type ipv4 = int32

let ipv4_to_string i =
  let (&&&) x y = Int32.logand x y in
  let (>>>) x y = Int32.shift_right_logical x y in
  sprintf "%ld.%ld.%ld.%ld"
    ((i &&& 0x0_ff000000_l) >>> 24) ((i &&& 0x0_00ff0000_l) >>> 16)
    ((i &&& 0x0_0000ff00_l) >>>  8) ((i &&& 0x0_000000ff_l)       )

let bytes_to_ipv4 bs =
  let (|||) x y = Int32.logor x y in
  let (<<<) x y = Int32.shift_left x y in
  let a = Int32.of_int (byte_to_int bs.[0]) in
  let b = Int32.of_int (byte_to_int bs.[1]) in
  let c = Int32.of_int (byte_to_int bs.[2]) in
  let d = Int32.of_int (byte_to_int bs.[3]) in
  (a <<< 24) ||| (b <<< 16) ||| (c <<< 8) ||| d

type ipv6 = int64 * int64
let ipv6_to_string (hi, lo) =
  let (&&&&) x y = Int64.logand x y in
  let (>>>>) x y = Int64.shift_right_logical x y in
  sprintf "%Lx:%Lx:%Lx:%Lx:%Lx:%Lx:%Lx:%Lx"
    ((hi >>>> 48) &&&& 0xffff_L) ((hi >>>> 32) &&&& 0xffff_L)
    ((hi >>>> 16) &&&& 0xffff_L) ( hi          &&&& 0xffff_L)
    ((lo >>>> 48) &&&& 0xffff_L) ((lo >>>> 32) &&&& 0xffff_L)
    ((lo >>>> 16) &&&& 0xffff_L) ( lo          &&&& 0xffff_L)

let bytes_to_ipv6 bs =
  let (++++) x y = Int64.add x y in
  let (<<<<) x y = Int64.shift_left x y in
  let hihi = bytes_to_ipv4 (String.sub bs 0 4) in
  let hilo = bytes_to_ipv4 (String.sub bs 4 4) in
  let lohi = bytes_to_ipv4 (String.sub bs 8 4) in
  let lolo = bytes_to_ipv4 (String.sub bs 12 4) in
  ((Int64.of_int32 hihi) <<<< 48) ++++ (Int64.of_int32 hilo),
  ((Int64.of_int32 lohi) <<<< 48) ++++ (Int64.of_int32 lolo)

let debug t =
  let max_len = Bigarray.Array1.dim t.buffer in
  let str = Printf.sprintf "t=[%d,%d](%d)" t.off t.len max_len in
  if t.off+t.len > max_len || t.len < 0 || t.off < 0 then (
    Printf.printf "ERROR: t.off+t.len=%d %s\n" (t.off+t.len) str;
    assert false;
  );
  str

let sub t off len =
  { t with off = t.off + off; len }

let shift t off =
  { t with off = t.off + off; len = t.len - off }

let set_len t len =
  { t with len = len }

let add_len t len =
  { t with len = t.len + len }

external unsafe_blit_bigstring_to_bigstring : buffer -> int -> buffer -> int -> int -> unit = "caml_blit_bigstring_to_bigstring" "noalloc"

external unsafe_blit_string_to_bigstring : string -> int -> buffer -> int -> int -> unit = "caml_blit_string_to_bigstring" "noalloc"

external unsafe_blit_bigstring_to_string : buffer -> int -> string -> int -> int -> unit = "caml_blit_bigstring_to_string" "noalloc"

let copy src srcoff len =
  if src.len - srcoff < len then raise (Failure "copy");
  let s = String.create len in
  unsafe_blit_bigstring_to_string src.buffer (src.off+srcoff) s 0 len;
  s

let blit src srcoff dst dstoff len =
  if src.len - srcoff < len then raise (Failure "blit");
  if dst.len - dstoff < len then raise (Failure "blitdst");
  unsafe_blit_bigstring_to_bigstring src.buffer (src.off+srcoff) dst.buffer (dst.off+dstoff) len

let blit_from_string src srcoff dst dstoff len =
  if String.length src - srcoff < len then raise (Failure "blit_from_string");
  if dst.len - dstoff < len then raise (Failure "blit_from_string dst");
  unsafe_blit_string_to_bigstring src srcoff dst.buffer (dst.off+dstoff) len

let blit_to_string src srcoff dst dstoff len =
  if src.len - srcoff < len then raise (Failure "blit_to_string");
  if String.length dst - dstoff < len then raise (Failure "blit_to_string dst");
  unsafe_blit_bigstring_to_string src.buffer (src.off+srcoff) dst dstoff len

let set_uint8 t i c =
  EndianBigstring.BigEndian.set_int8 t.buffer (t.off+i) c

let set_char t i c =
  EndianBigstring.BigEndian.set_char t.buffer (t.off+i) c

let get_uint8 t i =
  EndianBigstring.BigEndian.get_uint8 t.buffer (t.off+i)

let get_char t i =
  EndianBigstring.BigEndian.get_char t.buffer (t.off+i)

module BE = struct
  include EndianBigstring.BigEndian

  let set_uint16 t i c = set_int16 t.buffer (t.off+i) c
  let set_uint32 t i c = set_int32 t.buffer (t.off+i) c
  let set_uint64 t i c = set_int64 t.buffer (t.off+i) c

  let get_uint16 t i = get_uint16 t.buffer (t.off+i)
  let get_uint32 t i = get_int32 t.buffer (t.off+i)
  let get_uint64 t i = get_int64 t.buffer (t.off+i)
end

module LE = struct
  include EndianBigstring.LittleEndian

  let set_uint16 t i c = set_int16 t.buffer (t.off+i) c
  let set_uint32 t i c = set_int32 t.buffer (t.off+i) c
  let set_uint64 t i c = set_int64 t.buffer (t.off+i) c

  let get_uint16 t i = get_uint16 t.buffer (t.off+i)
  let get_uint32 t i = get_int32 t.buffer (t.off+i)
  let get_uint64 t i = get_int64 t.buffer (t.off+i)
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
