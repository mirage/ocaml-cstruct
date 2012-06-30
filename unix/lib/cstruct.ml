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

type buf = (char, int8_unsigned_elt, c_layout) t

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

let get_char s off =
  get s off

let get_uint8 s off =
  Char.code (get s off)

let set_char s off =
  set s off

let set_uint8 s off v =
  set s off (Char.chr v)

let sub_buffer src srcoff len =
  sub src srcoff len

let copy_buffer src srcoff len =
  let s = String.create len in
  for i = 0 to len - 1 do
    s.[i] <- get src (srcoff+i)
  done;
  s

let blit_buffer src srcoff dst dstoff len =
  let src = sub src srcoff len in
  let dst = sub dst dstoff len in
  blit src dst

let set_buffer src srcoff dst dstoff len =
  for i = 0 to len - 1 do
    set dst (dstoff+i) src.[srcoff+i]
  done

module BE = struct

  let get_uint16 s off =
    let hi = get_uint8 s off in
    let lo = get_uint8 s (off+1) in
    (hi lsl 8) + lo

  let get_uint32 s off =
    let hi = get_uint16 s off in
    let lo = get_uint16 s (off+2) in
    Int32.(add (shift_left (of_int hi) 16) (of_int lo))

  let get_uint64 s off =
    let hi = get_uint32 s off in
    let lo = get_uint32 s (off+4) in 
    Int64.(add (shift_left (of_int32 hi) 32) (of_int32 lo))

  let set_uint16 s off v =
    set_uint8 s off (v lsr 8);
    set_uint8 s (off+1) (v land 0xff)

  let set_uint32 s off v =
    set_uint16 s off (Int32.(to_int (shift_right_logical v 16)));
    set_uint16 s (off+2) (Int32.(to_int (logand v 0xffff_l)))
  
  let set_uint64 s off v =
    set_uint32 s off (Int64.(to_int32 (shift_right_logical v 32)));
    set_uint32 s (off+4) (Int64.(to_int32 (logand v 0xffffffff_L)))

end

module LE = struct

  let get_uint16 s off =
    let lo = get_uint8 s off in
    let hi = get_uint8 s (off+1) in
    (hi lsl 8) + lo

  let get_uint32 s off =
    let lo = get_uint16 s off in
    let hi = get_uint16 s (off+2) in
    Int32.(add (shift_left (of_int hi) 16) (of_int lo))

  let get_uint64 s off =
    let lo = get_uint32 s off in
    let hi = get_uint32 s (off+4) in 
    Int64.(add (shift_left (of_int32 hi) 32) (of_int32 lo))
  
  let set_uint16 s off v =
    set_uint8 s off (v land 0xff);
    set_uint8 s (off+1) (v lsr 8)

  let set_uint32 s off v =
    set_uint16 s off (Int32.(to_int (logand v 0xffff_l)));
    set_uint16 s (off+2) (Int32.(to_int (shift_right_logical v 16)))

  let set_uint64 s off v =
    set_uint32 s off (Int64.(to_int32 (logand v 0xffffffff_L)));
    set_uint32 s (off+4) (Int64.(to_int32 (shift_right_logical v 32)))
end

let len buf = dim buf
let lenv bufv =
  match bufv with
  |[] -> 0
  |[d] -> len d
  |ds -> List.fold_left (fun a b -> len b + a) 0 ds

let copy_buffers bufs =
  let sz = lenv bufs in
  let dst = String.create sz in
  let _ = List.fold_left
    (fun off src ->
      let x = len src in
      for i = 0 to x - 1 do
        dst.[off+i] <- get src i;
      done;
      off + x
    ) 0 bufs in
  dst

external base_offset : buf -> int = "caml_bigarray_base_offset"
external shift_left : buf -> int -> bool = "caml_bigarray_shift_left"

let sub buf off len = sub buf off len

let to_string buf = 
  let sz = len buf in
  let s = String.create sz in
  for i = 0 to sz - 1 do
    s.[i] <- get buf i
  done;
  s

let hexdump buf =
  let c = ref 0 in
  for i = 0 to len buf - 1 do
    if !c mod 16 = 0 then print_endline "";
    printf "%.2x " (Char.code (get buf i));
    incr c;
  done;
  print_endline ""

let shift buf off =
  sub buf off (len buf - off)

let split ?(start=0) buf off =
  let header = sub buf start off in
  let body = sub buf (start+off) (len buf - off - start) in
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
