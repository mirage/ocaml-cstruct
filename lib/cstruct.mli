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

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = private {
  buffer: buffer;
  off   : int;
  len   : int;
}

val of_bigarray: ?off:int -> ?len:int -> buffer -> t
val create : int -> t
val check_bounds : t -> int -> bool

type byte = char
val byte : int -> byte
val byte_to_int : byte -> int

type bytes = string

type uint8 = int
type uint16 = int
type uint32 = int32
type uint64 = int64

type ipv4 = int32

val ipv4_to_string: ipv4 -> string

type ipv6 = int64 * int64

val ipv6_to_string: ipv6 -> string

val get_char: t -> int -> char

val get_uint8: t -> int -> uint8

val set_char: t -> int -> char -> unit

val set_uint8: t -> int -> uint8 -> unit

val sub: t -> int -> int -> t

val shift: t -> int -> t

val copy: t -> int -> int -> string

val blit: t -> int -> t -> int -> int -> unit

val blit_from_string: string -> int -> t -> int -> int -> unit

val blit_to_string: t -> int -> string -> int -> int -> unit

val len: t -> int

val split: ?start:int -> t -> int -> t * t

val to_string: t -> string

val hexdump: t -> unit

module BE : sig
  val get_uint16: t -> int -> uint16
  val get_uint32: t -> int -> uint32
  val get_uint64: t -> int -> uint64

  val set_uint16: t -> int -> uint16 -> unit
  val set_uint32: t -> int -> uint32 -> unit
  val set_uint64: t -> int -> uint64 -> unit
end

module LE : sig
  val get_uint16: t -> int -> uint16
  val get_uint32: t -> int -> uint32
  val get_uint64: t -> int -> uint64

  val set_uint16: t -> int -> uint16 -> unit
  val set_uint32: t -> int -> uint32 -> unit
  val set_uint64: t -> int -> uint64 -> unit
end

(** {2 List of buffers} *)

val lenv: t list -> int

val copyv: t list -> string

(** {2 Iterations} *)

type 'a iter = unit -> 'a option

val iter: (t -> int option) -> (t -> 'a) -> t -> 'a iter

val fold: ('b -> 'a -> 'b) -> 'a iter -> 'b -> 'b
