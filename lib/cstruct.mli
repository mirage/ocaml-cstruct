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

(** Manipulate external buffers as C-like structs *)

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Type of a buffer. A cstruct is composed of an underlying buffer
    and position/length within this buffer. *)

type t = private {
  buffer: buffer;
  off   : int;
  len   : int;
}
(** Type of a cstruct. *)

(** Functions that create a new cstruct. *)

val of_bigarray: ?off:int -> ?len:int -> buffer -> t
(** [of_bigarray ~off ~len b] is the cstruct contained in [b] starting
    at [off], of length [len]. *)

val create : int -> t
(** [create len] is a cstruct of size [len] with an offset of 0. *)

val of_string: ?allocator:(int -> t) -> string -> t
(** [of_string ~alloc str] is the cstruct representation of [str],
    with the underlying buffer allocated by [alloc]. If [alloc] is not
    provided, [create] is used. *)

(** Functions that operate over cstructs. *)

val check_bounds : t -> int -> bool
(** [check_bounds cstr len] is [true] if [cstr.buffer]'s size is
    greater or equal than [len], [false] otherwise. *)

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
(** [sub cstr off len] is [{ t with off = t.off + off; len }] *)

val shift: t -> int -> t
(** [shift cstr len] is [{ t with off = t.off + off; len = t.len - off
    }] *)

val copy: t -> int -> int -> string
(** [copy cstr off len] is the string representation of the segment of
    [t] starting at [off] of size [len].

    Raise [Invalid_argument] if [off] and [len] do not designate a
    valid segment of [t]. *)

val blit: t -> int -> t -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] characters from
    cstruct [src], starting at index [srcoff], to cstruct [dst],
    starting at index [dstoff]. It works correctly even if [src] and
    [dst] are the same string, and the source and destination
    intervals overlap.

    Raise [Invalid_argument] if [srcoff] and [len] do not designate a
    valid segment of [src], or if [dstoff] and [len] do not designate
    a valid segment of [dst]. *)

val blit_from_string: string -> int -> t -> int -> int -> unit
(** [blit_from_string src srcoff dst dstoff len] copies [len]
    characters from string [src], starting at index [srcoff], to
    string [dst], starting at index [dstoff].

    Raise [Invalid_argument] if [srcoff] and [len] do not designate a
    valid substring of [src], or if [dstoff] and [len] do not
    designate a valid segment of [dst]. *)

val blit_to_string: t -> int -> string -> int -> int -> unit
(** [blit_to_string src srcoff dst dstoff len] copies [len] characters
    from cstruct [src], starting at index [srcoff], to string [dst],
    starting at index [dstoff].

    Raise [Invalid_argument] if [srcoff] and [len] do not designate a
    valid segment of [src], or if [dstoff] and [len] do not designate
    a valid substring of [dst]. *)

val len: t -> int

val set_len : t -> int -> t

val add_len : t -> int -> t

val split: ?start:int -> t -> int -> t * t
(** [split ~start cstr len] is a tuple containing the cstruct
    extracted from [cstr] at offset [start] (default: 0) of length
    [len] as first element, and the rest of [cstr] as second
    element. *)

val to_string: t -> string

val hexdump: t -> unit
val hexdump_to_buffer: Buffer.t -> t -> unit
val debug: t -> string

module BE : sig

  (** Get/set big-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16: t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long big-endian unsigned
      integer stored in [cstr] at offset [off]. *)

  val get_uint32: t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long big-endian unsigned
      integer stored in [cstr] at offset [off]. *)

  val get_uint64: t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long big-endian unsigned
      integer stored in [cstr] at offset [off]. *)

  val set_uint16: t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr]. *)

  val set_uint32: t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr]. *)

  val set_uint64: t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr]. *)

end

module LE : sig

  (** Get/set little-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16: t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long little-endian unsigned
      integer stored in [cstr] at offset [off]. *)

  val get_uint32: t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long little-endian unsigned
      integer stored in [cstr] at offset [off]. *)

  val get_uint64: t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long little-endian unsigned
      integer stored in [cstr] at offset [off]. *)

  val set_uint16: t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr]. *)

  val set_uint32: t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr]. *)

  val set_uint64: t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr]. *)

end

(** {2 List of buffers} *)

val lenv: t list -> int
(** [lenv cstrs] is the combined length of all cstructs in [cstrs]. *)

val copyv: t list -> string
(** [copyv cstrs] is the string representation of the concatenation of
    all cstructs in [cstrs]. *)

(** {2 Iterations} *)

type 'a iter = unit -> 'a option
(** Type of an iterator. *)

val iter: (t -> int option) -> (t -> 'a) -> t -> 'a iter
(** [iter lenf of_cstr cstr] is an iterator over [cstr] that returns
    elements of size [lenf cstr] and type [of_cstr cstr]. *)

val fold: ('b -> 'a -> 'b) -> 'a iter -> 'b -> 'b
(** [fold f iter acc] is [(f iterN accN ... (f iter acc)...)]. *)
