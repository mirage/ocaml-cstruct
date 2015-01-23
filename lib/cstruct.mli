(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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

(** Manipulate external memory buffers as C-like structures.

Cstruct is a library and syntax extension to make it easier to access C-like
structures directly from OCaml.  It supports both reading and writing to these
memory buffers, and they are accessed via the [Bigarray] module.

The library interface below is intended to be used in conjunction with the
[pa_cstruct] camlp4 syntax extension that is also supplied with this library
(in the [cstruct.syntax] ocamlfind package).

An example description for the pcap packet format is:

{[
cstruct pcap_header {
  uint32_t magic_number;   (* magic number *)
  uint16_t version_major;  (* major version number *)
  uint16_t version_minor;  (* minor version number *)
  uint32_t thiszone;       (* GMT to local correction *)
  uint32_t sigfigs;        (* accuracy of timestamps *)
  uint32_t snaplen;        (* max length of captured packets, in octets *)
  uint32_t network         (* data link type *)
} as little_endian

cstruct pcap_packet {
  uint32_t ts_sec;         (* timestamp seconds *)
  uint32_t ts_usec;        (* timestamp microseconds *)
  uint32_t incl_len;       (* number of octets of packet saved in file *)
  uint32_t orig_len        (* actual length of packet *)
} as little_endian

cstruct ethernet {
  uint8_t        dst[6];
  uint8_t        src[6];
  uint16_t       ethertype
} as big_endian

cstruct ipv4 {
  uint8_t        hlen_version;
  uint8_t        tos;
  uint16_t       len;
  uint16_t       id;
  uint16_t       off;
  uint8_t        ttl;
  uint8_t        proto;
  uint16_t       csum;
  uint8_t        src[4];
  uint8_t        dst[4]
} as big_endian
]}

These will expand to get and set functions for every field, with types
appropriate to the particular definition.  For instance:

{[
val get_pcap_packet_ts_sec : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_ts_sec : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_ts_usec : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_ts_usec : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_incl_len : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_incl_len : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_orig_len : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_orig_len : Cstruct.t -> Cstruct.uint32 -> unit
val hexdump_pcap_packet_to_buffer : Buffer.t -> Cstruct.t -> unit
]}

The buffers generate a different set of functions. For the  [ethernet]
definitions, we have:

{[
val sizeof_ethernet : int
val get_ethernet_dst : Cstruct.t -> Cstruct.t
val copy_ethernet_dst : Cstruct.t -> string
val set_ethernet_dst : string -> int -> Cstruct.t -> unit
val blit_ethernet_dst : Cstruct.t -> int -> Cstruct.t -> unit
val get_ethernet_src : Cstruct.t -> Cstruct.t
val copy_ethernet_src : Cstruct.t -> string
]}

You can also declare C-like enums:

{[
cenum foo32 {
  ONE32;
  TWO32 = 0xfffffffel;
  THREE32
} as uint32_t

cenum bar16 {
  ONE = 1;
  TWO;
  FOUR = 4;
  FIVE
} as uint16_t
]}

This generates signatures of the form:

{[
type foo32 = | ONE32 | TWO32 | THREE32
val int_to_foo32 : int32 -> foo32 option
val foo32_to_int : foo32 -> int32
val foo32_to_string : foo32 -> string
val string_to_foo32 : string -> foo32 option
type bar16 = | ONE | TWO | FOUR | FIVE
val int_to_bar16 : int -> bar16 option
val bar16_to_int : bar16 -> int
val bar16_to_string : bar16 -> string
val string_to_bar16 : string -> bar16 option
]}

*)

(** {2 Base types } *)

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t with sexp
(** Type of a buffer. A cstruct is composed of an underlying buffer
    and position/length within this buffer. *)

type t = private {
  buffer: buffer;
  off   : int;
  len   : int;
} with sexp
(** Type of a cstruct. *)

type byte = char
(** A single byte type *)

val byte : int -> byte
(** [byte v] convert [v] to a single byte.
    @raise Invalid_argument if [v] is negative or greater than 255. *)

type uint8 = int
(** 8-bit unsigned integer.  The representation is currently an
    unboxed OCaml integer. *)

type uint16 = int
(** 16-bit unsigned integer.  The representation is currently an
    unboxed OCaml integer. *)

type uint32 = int32
(** 32-bit unsigned integer.  The representation is currently a
    boxed OCaml int32. *)

type uint64 = int64
(** 64-bit unsigned integer.  The representation is currently a
    boxed OCaml int64. *)

(** {2 Creation and conversion} *)

val of_bigarray: ?off:int -> ?len:int -> buffer -> t
(** [of_bigarray ~off ~len b] is the cstruct contained in [b] starting
    at [off], of length [len]. *)

val to_bigarray: t -> buffer 
(** [to_bigarray t] converts a {!t} into a {!buffer} Bigarray, using
    the Bigarray slicing to allocate a fresh array that preserves
    sharing of the underlying buffer. *)

val create : int -> t
(** [create len] is a cstruct of size [len] with an offset of 0. *)

val of_string: ?allocator:(int -> t) -> string -> t
(** [of_string ~allocator str] is the cstruct representation of [str],
    with the underlying buffer allocated by [alloc]. If [allocator] is not
    provided, [create] is used. *)

(** {2 Getters and Setters } *)

val byte_to_int : byte -> int
(** Convert a byte to an integer *)

val check_bounds : t -> int -> bool
(** [check_bounds cstr len] is [true] if [cstr.buffer]'s size is
    greater or equal than [len], [false] otherwise. *)

val get_char: t -> int -> char
(** [get_char t off] returns the character contained in the cstruct
    at offset [off].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val get_uint8: t -> int -> uint8
(** [get_uint8 t off] returns the byte contained in the cstruct
    at offset [off].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val set_char: t -> int -> char -> unit
(** [set_char t off c] sets the byte contained in the cstruct
    at offset [off] to character [c].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val set_uint8: t -> int -> uint8 -> unit
(** [set_uint8 t off c] sets the byte contained in the cstruct
    at offset [off] to byte [c].
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val sub: t -> int -> int -> t
(** [sub cstr off len] is [{ t with off = t.off + off; len }]
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val shift: t -> int -> t
(** [shift cstr len] is [{ t with off=t.off+off; len=t.len-off }]
    @raise Invalid_argument if the offset exceeds cstruct length. *)

val copy: t -> int -> int -> string
(** [copy cstr off len] is the string representation of the segment of
    [t] starting at [off] of size [len].
    @raise Invalid_argument if [off] and [len] do not designate a
    valid segment of [t]. *)

val blit: t -> int -> t -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] characters from
    cstruct [src], starting at index [srcoff], to cstruct [dst],
    starting at index [dstoff]. It works correctly even if [src] and
    [dst] are the same string, and the source and destination
    intervals overlap.

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid segment of [src], or if [dstoff] and [len] do not designate
    a valid segment of [dst]. *)

val blit_from_string: string -> int -> t -> int -> int -> unit
(** [blit_from_string src srcoff dst dstoff len] copies [len]
    characters from string [src], starting at index [srcoff], to
    string [dst], starting at index [dstoff].

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid substring of [src], or if [dstoff] and [len] do not
    designate a valid segment of [dst]. *)

val blit_to_string: t -> int -> string -> int -> int -> unit
(** [blit_to_string src srcoff dst dstoff len] copies [len] characters
    from cstruct [src], starting at index [srcoff], to string [dst],
    starting at index [dstoff].

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid segment of [src], or if [dstoff] and [len] do not designate
    a valid substring of [dst]. *)

val len: t -> int
(** Returns the length of the current cstruct view.  Note that this
    length is potentially smaller than the actual size of the underlying
    buffer, as the [sub] or [set_len] functions can construct a smaller view. *)

val set_len : t -> int -> t
(** [set_len t len] sets the length of the cstruct [t] to a new absolute
    value, and returns a fresh cstruct with these settings.
    @raise Invalid_argument if [len] exceeds the size of the buffer. *)

val add_len : t -> int -> t
(** [add_len t l] will add [l] bytes to the length of the buffer, and return
    a fresh cstruct with these settings.
    @raise Invalid_argument if [len] exceeds the size of the buffer. *)

val split: ?start:int -> t -> int -> t * t
(** [split ~start cstr len] is a tuple containing the cstruct
    extracted from [cstr] at offset [start] (default: 0) of length
    [len] as first element, and the rest of [cstr] as second
    element. *)

val to_string: t -> string
(** [to_string t] will allocate a fresh OCaml [string] and copy the
    contents of the cstruct into it, and return that string copy. *)

(** {2 Debugging } *)

val hexdump: t -> unit
(** When the going gets tough, the tough hexdump their cstructs
    and peer at it until the bug disappears.  This will directly
    prettyprint the contents of the cstruct to the standard output. *)

val hexdump_to_buffer: Buffer.t -> t -> unit
(** [hexdump_to_buffer buf c] will append the pretty-printed hexdump
    of the cstruct [c] to the buffer [buf]. *)
 
val debug: t -> string
(** [debug t] will print out the internal details of a cstruct such
    as its base offset and the length, and raise an assertion failure
    if invariants have been violated.  Not intended for casual use. *)

module BE : sig

  (** Get/set big-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16: t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long big-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint32: t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long big-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint64: t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long big-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint16: t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint32: t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint64: t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long big-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)
end

module LE : sig

  (** Get/set little-endian integers of various sizes. The second
      argument of those functions is the position relative to the
      current offset of the cstruct. *)

  val get_uint16: t -> int -> uint16
  (** [get_uint16 cstr off] is the 16 bit long little-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint32: t -> int -> uint32
  (** [get_uint32 cstr off] is the 32 bit long little-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val get_uint64: t -> int -> uint64
  (** [get_uint64 cstr off] is the 64 bit long little-endian unsigned
      integer stored in [cstr] at offset [off].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint16: t -> int -> uint16 -> unit
  (** [set_uint16 cstr off i] writes the 16 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint32: t -> int -> uint32 -> unit
  (** [set_uint32 cstr off i] writes the 32 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

  val set_uint64: t -> int -> uint64 -> unit
  (** [set_uint64 cstr off i] writes the 64 bit long little-endian
      unsigned integer [i] at offset [off] of [cstr].
      @raise Invalid_argument if the buffer is too small. *)

end

(** {2 List of buffers} *)

val lenv: t list -> int
(** [lenv cstrs] is the combined length of all cstructs in [cstrs]. *)

val copyv: t list -> string
(** [copyv cstrs] is the string representation of the concatenation of
    all cstructs in [cstrs]. *)

val blitv: t list -> t -> int * t list
(** [blitv src dst] will copy the list of [src] buffers into [dst] and
    return a tuple of the total number of bytes written and a list of
    any uncopied buffers.  [blitv] will never raise an exception, since
    it returns any uncopied bytes if the [dst] buffer is not big enough
    to fit the full list of [src] buffers. *)

(** {2 Iterations} *)

type 'a iter = unit -> 'a option
(** Type of an iterator. *)

val iter: (t -> int option) -> (t -> 'a) -> t -> 'a iter
(** [iter lenf of_cstr cstr] is an iterator over [cstr] that returns
    elements of size [lenf cstr] and type [of_cstr cstr]. *)

val fold: ('b -> 'a -> 'b) -> 'a iter -> 'b -> 'b
(** [fold f iter acc] is [(f iterN accN ... (f iter acc)...)]. *)
