(*
 * Copyright (c) 2012-2019 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2019 Romain Calascibetta <romain.calascibetta@gmail.com>
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

type 'a rd = < rd: unit; .. > as 'a
(** Type of read capability. *)

type 'a wr = < wr: unit; .. > as 'a
(** Type of write capability. *)

type 'a t
(** Type of cstruct with capabilities ['a]. *)

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Type of buffer. A {!t} is composed of an underlying buffer. *)

type rdwr =  < rd: unit; wr: unit; >
(** Type of read-and-capability. *)

type ro = < rd: unit; >
(** Type of read-only capability. *)

type wo = < wr: unit; >
(** Type of write-only capability. *)

type uint8 = int
(** 8-bit unsigned integer. *)

type uint16 = int
(** 16-bit unsigned integer. *)

type uint32 = int32
(** 32-bit unsigned integer. *)

type uint64 = int64
(** 64-bit unsigned integer. *)

val create : int -> rdwr t
(** [create len] is a fresh read-and-write {!t} of size [len]. with an offset of
    0, filled with zero bytes. *)

val create_unsafe : int -> rdwr t
(** [create_unsafe len] is a fresh read-and-write {!t} of size [len] with an
    offset of 0.

    Note that the returned cstruct will contain arbitrary data, likely including
    the contents of previously-deallocated cstructs.

    Beware!

    Forgetting to replace this data could cause your application to leak
    sensitive information. *)

val ro : 'a rd t -> ro t
(** [ro t] has [t] to be read-only then. *)

val wo : 'a wr t -> wo t
(** [wo t] has [t] to be write-only then. *)

val of_string : ?off:int -> ?len:int -> string -> rdwr t
(** [of_string ~off ~len s] is a fresh read-and-write {!t} of [s] sliced on
    [off] (default is [0]) and of [len] (default is [String.length s]) length. *)

val of_bytes : ?off:int -> ?len:int -> bytes -> rdwr t val of_hex : string -> rdwr t
(** [of_bytes ~off ~len x] is a fresh read-and-write {!t} of [x] sliced on
    [off] (default is [0]) and of [len] (default is [Bytes.length x]) length. *)

val to_bigarray : 'a t -> buffer
(** [to_bigarray t] converts {!t} into a {!buffer} Bigarray, using the Bigarray
    slicing to allocate a fresh {i proxy} Bigarray that preserves sharing of the
    underlying buffer.

    In other words:

    {[let t = Cstruct_cap.create 10 in
      let b = Cstruct_cap.to_bigarray t in
      Bigarray.Array1.set b 0 '\x42' ;
      assert (Cstruct_cap.get_char t 0 = '\x42')]} *)

val equal : 'a rd t -> 'b rd t -> bool
(** [equal a b] is [true] iff [a] and [b] correspond to the same sequence of
    bytes (it uses [memcmp] internally). Both need at least read capability
    {!rd}. *)

val compare : 'a rd t -> 'b rd t -> int
(** [compare a b] gives an unspecified total ordering over {!t}. Both need at
    least read capability {!rd}. *)

val check_alignment : 'a rd t -> int -> bool
(** [check_alignment t alignment] is [true] if the first byte stored
    within [t] is at a memory address where [address mod alignment = 0],
    [false] otherwise.  The [mod] used has the C/OCaml semantic (which differs
    from Python).
    Typical uses are to check a buffer is aligned to a page or disk sector
    boundary. [t] needs at least read capability {!rd}.

    @raise Invalid_argument if [alignment] is not a positive integer. *)

val get_char : 'a rd t -> int -> char
(** [get_char t off] returns the character contained in [t] at offset [off].
    [t] needs at least read capability {!rd}.

    @raise Invalid_argument if the offset exceeds [t] length (which can differ
    from underlying buffer length). *)

val get_uint8 : 'a rd t -> int -> uint8
(** [get_uint8 t off] returns the byte contained in [t] at offset [off].
    [t] needs at least read capability {!rd}.

    @raise Invalid_argument if the offset exceeds [t] length (which can differ
    from underlying buffer length). *)

val set_char : 'a wr t -> int -> char -> unit
(** [set_char t off c] sets the character contained in [t] at offset [off]
    to character [c]. [t] needs at least write capability {!wr}.

    @raise Invalid_argument if the offset exceeds [t] length (which can differ
    from underlying buffer length). *)

val set_uint8 : 'a wr t -> int -> uint8 -> unit
(** [set_uint8 t off x] sets the byte contained in [t] at offset [off]
    to byte [x]. [t] needs at least write capability {!wr}.

    @raise Invalid_argument if the offset exceeds [t] length (which can differ
    from underlying buffer length). *)

val sub : 'a rd t -> off:int -> len:int -> 'a rd t
(** [sub t ~off ~len] returns a fresh {!t} with the shared underlying buffer of
    [t] sliced on [off] and of [len] length. New {!t} shares same capabilities
    than [t].

    @raise Invalid_argument if the offset exceeds [t] length. *)

val shift : 'a rd t -> int -> 'a rd t
(** [shift t len] returns a fresh {!t} with the shared underlying buffer of [t]
    shifted to [len] bytes. New {!t} shares same capabilities than [t].

    @raise Invalid_argument if the offset exceeds [t] length. *)

val to_string : ?off:int -> ?len:int -> 'a rd t -> string
(** [to_string ~off ~len t] is the string representation of the segment of [t]
    starting at [off] (default is [0]) of size [len] (default is [length t]).
    [t] needs at least read-capability {!rd}.

    @raise Invalid_argument if [off] and [len] does not designate a valid
    segment of [t]. *)

val to_bytes : ?off:int -> ?len:int -> 'a rd t -> bytes
(** [to_bytes ~off ~len t] is the bytes representation of the segment of [t]
    starting at [off] (default is [0]) of size [len] (default is [length t]).
    [t] needs at least read-capability {!rd}.

    @raise Invalid_argument if [off] and [len] do not designate a valid
    segment of [t]. *)

val blit : 'a rd t -> src_off:int -> 'b wr t -> dst_off:int -> len:int -> unit
(** [blit src ~src_off dst ~dst_off ~len] copies [len] characters from [src],
    starting at index [src_off], to [dst], starting at index [dst_off]. It works
    correctly even if [src] and [dst] have the same underlying {!buffer}, and the
    [src] and [dst] intervals overlap.  This function uses [memmove] internally.

    [src] needs at least read-capability {!rd}. [dst] needs at least
    write-capability {!wr}. Both don't share capabilities.

    @raise Invalid_argument if [src_off] and [len] do not designate a valid segment of [src],
    or if [dst_off] and [len] do not designate a valid segment of [dst]. *)

val blit_from_string : string -> src_off:int -> 'a wr t -> dst_off:int -> len:int -> unit
(** [blit_from_string src ~src_off dst ~dst_off ~len] copies [len] characters from [src],
    starting at index [src_off], to [dst], starting at index [dst_off]. This function
    uses [memcpy] internally.

    [dst] needs at least write-capability {!wr}.

    @raise Invalid_argument if [src_off] and [len] do not designate a valid
    sub-string of [src], or if [dst_off] and [len] do not designate a valid
    segment of [dst]. *)

val blit_from_bytes : bytes -> src_off:int -> 'a wr t -> dst_off:int -> len:int -> unit
(** [blit_from_bytes src ~src_off dst ~dst_off ~len] copies [len] characters from [src],
    starting at index [src_off], to [dst], starting at index [dst_off]. This uses
    [memcpy] internally.

    [dst] needs at least write-capability {!wr}.

    @raise Invalid_argument if [src_off] and [len] do not designate a
    valid sub-sequence of [src], or if [dst_off] and [len] do no designate
    a valid segment of [dst]. *)

val blit_to_bytes : 'a rd t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit
(** [blit_to_bytes src ~src_off dst ~dst_off ~len] copies [len] characters
    from [src], starting at index [src_off], to sequences [dst], starting at index [dst_off].
    [blit_to_bytes] uses [memcpy] internally.

    [src] needs at least read-capability {!rd}.

    @raise Invalid_argument if [src_off] and [len] do not designate a
    valid segment of [src], or if [dst_off] and [len] do not designate
    a valid sub-seuqnce of [dst]. *)

val memset : 'a wr t -> int -> unit
(** [memset t x] sets all bytes of [t] to [x land 0xff]. [t] needs at least
    write-capability {!wr}. *)

val length : 'a rd t -> int
(** [length t] return length of [t]. Note that this length is potentially smaller than
    the actual size of the underlying buffer, as the {!sub} function can construct
    a smaller view. [t] needs at least read-capability {!rd}. *)

val split : ?start:int -> 'a t -> int -> 'a t * 'a t
(** [split ~start t len] is a tuple containing {!t}s extracted from [t] at
    offset [start] (default is [0]) of length [len] as first element, and the
    rest of [t] as second element.

    @raise Invalid_argument if [sart] exceeds the [t] length,
    or if there is a bounds violation of [t] via [len + start]. *)

val pp : Format.formatter -> 'a rd t -> unit
(** Pretty-printer of {!t}. {!t} needs at least read capability {!rd}. *)

module BE : sig
  (** {3 Big-endian Byte Order}

      The following operations assume a big-endian byte ordering of the
      cstruct. If the machine-native byte ordering differs, then the get
      operations will reorder the bytes so that they are in machine-native byte
      order before returning the result, and the set operations will reorder the
      bytes so that they are written out in the appropriate order.

      Network byte order is big-endian, so you may need these operations when
      dealing with raw frames, for example, in a userland networking stack. *)

  val get_uint16 : 'a rd t -> int -> uint16
  (** [get_uint16 t i] returns the two bytes in [t] starting at offset [i],
      interpreted as an {!uint16}.  Sign extension is not interpreted.

      @raise Invalid_argument if [t] is too small. *)

  val get_uint32 : 'a rd t -> int -> uint32
  (** [get_uint32 t i] returns the four bytes in [t] starting at offset [i].
      [t] needs at least read-capability {!rd}.

      @raise Invalid_argument if [t] is too small. *)

  val get_uint64 : 'a rd t -> int -> uint64
  (** [get_uint64 t i] returns the eight bytes in [t] starting at offset [i].
      [t] needs at least read-capability {!rd}.

      @raise Invalid_argument if [t] is too small. *)

  val set_uint16 : 'a wr t -> int -> uint16 -> unit
  (** [set_uint16 t i v] sets the two bytes in [t] starting at offset [i] to
      the value [v]. [t] needs at least write-capability {!wr}.

      @raise Invalid_argument if [t] is too small. *)

  val set_uint32 : 'a wr t -> int -> uint32 -> unit
  (** [set_uint32 t i v] sets the four bytes in [t] starting at offset [i] to
      the value [v]. [t] needs at least write-capability {!wr}.

      @raise Invalid_argument if [t] is too small. *)

  val set_uint64 : 'a wr t -> int -> uint64 -> unit
  (** [set_uint64 t i v] sets the eight bytes in [t] starting at offset [i] to
      the value [v]. [t] needs at least write-capability {!wr}.

      @raise Invalid_argument if [t] is too small. *)
end

module LE : sig
  (** {3 Little-endian Byte Order}

      The following operations assume a little-endian byte ordering of the
      cstruct. If the machine-native byte ordering differs, then the get
      operations will reorder the bytes so that they are in machine-native byte
      order before returning the result, and the set operations will reorder the
      bytes so that they are written out in the appropriate order.

      Most modern processor architectures are little-endian, so more likely than
      not, these operations will not do any byte reordering. *)

  val get_uint16 : 'a rd t -> int -> uint16
  (** [get_uint16 t i] returns the two bytes in [t] starting at offset [i],
      interpreted as an {!uint16}. Sign extension is not interpreted.

      @raise Invalid_argument if [t] is too small. *)

  val get_uint32 : 'a rd t -> int -> uint32
  (** [get_uint32 t i] returns the four bytes in [t] starting at offset [i].
      [t] needs at least read-capability {!rd}.

      @raise Invalid_argument if [t] is too small. *)

  val get_uint64 : 'a rd t -> int -> uint64
  (** [get_uint64 t i] returns the eight bytes in [t] starting at offset [i].
      [t] needs at least read-capability {!rd}.

      @raise Invalid_argument if [t] is too small. *)

  val set_uint16 : 'a wr t -> int -> uint16 -> unit
  (** [set_uint16 t i v] sets the two bytes in [t] starting at offset [i] to
      the value [v]. [t] needs at least write-capability {!wr}.

      @raise Invalid_argument if [t] is too small. *)

  val set_uint32 : 'a wr t -> int -> uint32 -> unit
  (** [set_uint32 t i v] sets the four bytes in [t] starting at offset [i] to
      the value [v]. [t] needs at least write-capability {!wr}.

      @raise Invalid_argument if [t] is too small. *)

  val set_uint64 : 'a wr t -> int -> uint64 -> unit
  (** [set_uint64 t i v] sets the eight bytes in [t] starting at offset [i] to
      the value [v]. [t] needs at least write-capability {!wr}.

      @raise Invalid_argument if [t] is too small. *)
end

val lenv : 'a rd t list -> int
(** [lenv vs] is the combined length of all {!t} in [vs].
    Each {!t} need at least read-capability {!rd}.

    @raise Invalid_argument if computing the sum overflows. *)

val copyv : 'a rd t list -> string
(** [copy vs] is the string representation of the concatenation of all {!t} in
    [vss]. Each {!t} need at least read-capability {!rd}.

    @raise Invalid_argument if the length of the result would exceed
    {!Sys.max_string_length}. *)

val fillv : src:'a rd t list -> dst:'b wr t -> int * 'a rd t list
(** [fillv ~src ~dst] copies from [src] to [dst] until [src] is exhausted or
    [dst] is full. It returns the number of bytes copied and the remaining data
    from [src], if any. This is useful if you want to {i bufferize} data into
    fixed-sized chunks. Each {!t} of [src] need at least read-capability {!rd}.
    [dst] needs at least write-capability {!wr}. Each {!t} of [src] and dst don't
    share capabilities. *)

type 'a iter = unit -> 'a option
(** Type of iterator. *)

val iter : ('a rd t -> int option) -> ('a rd t -> 'v) -> 'a rd t -> 'v iter
(** [iter lenf of_cstruct t] is an iterator over [t] that returns elements of
    size [lenf t] and type [of_cstruct t]. [t] needs at least read-capability {!rd} and
    [iter] keeps capabilities of [t] on [of_cstruct]. *)

val fold : ('acc -> 'x -> 'acc) -> 'x iter -> 'acc -> 'acc
(** [fold f iter acc] is [(f iterN accN ... (f iter acc)...)]. *)

val append : 'a rd t -> 'b rd t -> rdwr t
(** [append a b] create a fresh {!t} which is the concatenation of [a] and [b].
    [a] and [b] need at least read-capability {!rd}. Resulted {!t} has
    read-and-write capability. *)

val concat : 'a rd t list -> rdwr t
(** [concat vss] is the concatenation of all {!t} in [vss]. Each {!t} of [vss] need
    at least read-capability {!rd}.
    [concat] always creates a fresh {!t}. *)

val rev : 'a rd t -> rdwr t
(** [rev t] is [t] in reverse order. The return value is a freshly allocated
    {!t}, and [t] is not modified according {!rd} capability. *)

(** {2 Helpers to parse with capabilities.}

    As [Cstruct], capabilities interface provides helpers functions to help
   the user to parse contents. *)

val head : ?rev:bool -> 'a rd t -> char option
(** [head cs] is [Some (get cs h)] with [h = 0] if [rev = false] (default) or [h
   = length cs - 1] if [rev = true]. [None] is returned if [cs] is empty. *)

val tail : ?rev:bool -> 'a rd t -> 'a rd t
(** [tail cs] is [cs] without its first ([rev] is [false], default) or last
   ([rev] is [true]) byte or [cs] is empty. *)

val is_empty : 'a rd t -> bool
(** [is_empty cs] is [length cs = 0]. *)

val is_prefix : affix:'a rd t -> 'a rd t -> bool
(** [is_prefix ~affix cs] is [true] iff [affix.[zidx] = cs.[zidx]] for all
   indices [zidx] of [affix]. *)

val is_suffix : affix:'a rd t -> 'a rd t -> bool
(** [is_suffix ~affix cs] is [true] iff [affix.[n - zidx] = cs.[m - zidx]] for
   all indices [zidx] of [affix] with [n = length affix - 1] and [m = length cs
   - 1]. *)

val is_infix : affix:'a rd t -> 'a rd t -> bool
(** [is_infix ~affix cs] is [true] iff there exists an index [z] in [cs] such
   that for all indices [zidx] of [affix] we have [affix.[zidx] = cs.[z +
   zidx]]. *)

val for_all : (char -> bool) -> 'a rd t -> bool
(** [for_all p cs] is [true] iff for all indices [zidx] of [cs], [p cs.[zidx] =
   true]. *)

val exists : (char -> bool) -> 'a rd t -> bool
(** [exists p cs] is [true] iff there exists an index [zidx] of [cs] with [p
   cs.[zidx] = true]. *)

val start : 'a rd t -> 'a rd t
(** [start cs] is the empty sub-part at the start position of [cs]. *)

val stop : 'a rd t -> 'a rd t
(** [stop cs] is the empty sub-part at the stop position of [cs]. *)

val trim : ?drop:(char -> bool) -> 'a rd t -> 'a rd t
(** [trim ~drop cs] is [cs] with prefix and suffix bytes satisfying [drop] in
   [cs] removed. [drop] defaults to [function ' ' | '\r' .. '\t' -> true | _ ->
   false]. *)

val span : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> 'a rd t -> 'a rd t * 'a rd t
(** [span ~rev ~min ~max ~sat cs] is [(l, r)] where:

    {ul
    {- if [rev] is [false] (default), [l] is at least [min] and at most
       [max] consecutive [sat] satisfying initial bytes of [cs] or {!empty}
       if there are no such bytes. [r] are the remaining bytes of [cs].}
    {- if [rev] is [true], [r] is at least [min] and at most [max]
       consecutive [sat] satisfying final bytes of [cs] or {!empty}
       if there are no such bytes. [l] are the remaining bytes of [cs].}}

    If [max] is unspecified the span is unlimited. If [min] is unspecified
    it defaults to [0]. If [min > max] the condition can't be satisfied and
    the left or right span, depending on [rev], is always empty. [sat]
    defaults to [(fun _ -> true)].

    The invariant [l ^ r = s] holds.

    For instance, the {i ABNF} expression:

{v
  time := 1*10DIGIT
v}

    can be translated to:

    {[
      let (time, _) = span ~min:1 ~max:10 is_digit cs in
    ]}

    @raise Invalid_argument if [max] or [min] is negative. *)

val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> 'a rd t -> 'a rd t
(** [take ~rev ~min ~max ~sat cs] is the matching span of {!span} without the remaining one.
    In other words:

    {[(if rev then snd else fst) @@ span ~rev ~min ~max ~sat cs]} *)

val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> 'a rd t -> 'a rd t
(** [drop ~rev ~min ~max ~sat cs] is the remaining span of {!span} without the matching one.
    In other words:

    {[(if rev then fst else snd) @@ span ~rev ~min ~max ~sat cs]} *)

val cut : ?rev:bool -> sep:'a rd t -> 'a rd t -> ('a rd t * 'a rd t) option
(** [cut ~sep cs] is either the pair [Some (l, r)] of the two
    (possibly empty) sub-buffers of [cs] that are delimited by the first
    match of the non empty separator string [sep] or [None] if [sep] can't
    be matched in [cs]. Matching starts from the beginning of [cs] ([rev] is
    [false], default) or the end ([rev] is [true]).

    The invariant [l ^ sep ^ r = s] holds.

    For instance, the {i ABNF} expression:

{v
  field_name := *PRINT
  field_value := *ASCII
  field := field_name ":" field_value
v}

    can be translated to:

    {[
      match cut ~sep:":" value with
      | Some (field_name, field_value) -> ...
      | None -> invalid_arg "invalid field"
    ]}

    @raise Invalid_argument if [sep] is the empty buffer. *)

val cuts : ?rev:bool -> ?empty:bool -> sep:'a rd t -> 'a rd t -> 'a rd t list
(** [cuts ~sep cs] is the list of all sub-buffers of [cs] that are
    delimited by matches of the non empty separator [sep]. Empty sub-buffers are
    omitted in the list if [empty] is [false] (default to [true]).

    Matching separators in [cs] starts from the beginning of [cs]
    ([rev] is [false], default) or the end ([rev] is [true]). Once
    one is found, the separator is skipped and matching starts again,
    that is separator matches can't overlap. If there is no separator
    match in [cs], the list [[cs]] is returned.

    The following invariants hold:
    {ul
    {- [concat ~sep (cuts ~empty:true ~sep cs) = cs]}
    {- [cuts ~empty:true ~sep cs <> []]}}

    For instance, the {i ABNF} expression:

{v
  arg := *(ASCII / ",") ; any characters exclude ","
  args := arg *("," arg)
v}

    can be translated to:

    {[
      let args = cuts ~sep:"," buffer in
    ]}

    @raise Invalid_argument if [sep] is the empty buffer. *)

val fields : ?empty:bool -> ?is_sep:(char -> bool) -> 'a rd t -> 'a rd t list
(** [fields ~empty ~is_sep cs] is the list of (possibly empty)
    sub-buffers that are delimited by bytes for which [is_sep] is
    [true]. Empty sub-buffers are omitted in the list if [empty] is
    [false] (defaults to [true]). [is_sep c] if it's not define by the
    user is [true] iff [c] is an US-ASCII white space character,
    that is one of space [' '] ([0x20]), tab ['\t'] ([0x09]), newline
    ['\n'] ([0x0a]), vertical tab ([0x0b]), form feed ([0x0c]), carriage
    return ['\r'] ([0x0d]). *)

val find : ?rev:bool -> (char -> bool) -> 'a rd t -> 'a rd t option
(** [find ~rev sat cs] is the sub-buffer of [cs] (if any) that spans
    the first byte that satisfies [sat] in [cs] after position [start cs]
    ([rev] is [false], default) or before [stop cs] ([rev] is [true]).
    [None] is returned if there is no matching byte in [s]. *)

val find_sub : ?rev:bool -> sub:'a rd t -> 'a rd t -> 'a rd t option
(** [find_sub ~rev ~sub cs] is the sub-buffer of [cs] (if any) that spans
    the first match of [sub] in [cs] after position [start cs]
    ([rev] is [false], default) or before [stop cs] ([rev] is [true]).
    Only bytes are compared and [sub] can be on a different base buffer.
    [None] is returned if there is no match of [sub] in [s]. *)

val filter : (char -> bool) -> 'a rd t -> 'a rd t
(** [filter sat cs] is the buffer made of the bytes of [cs] that satisfy [sat],
    in the same order. *)

val filter_map : (char -> char option) -> 'a rd t -> rdwr t
(** [filter_map f cs] is the buffer made of the bytes of [cs] as mapped by
    [f], in the same order. *)

val map : (char -> char) -> 'a rd t -> rdwr t
(** [map f cs] is [cs'] with [cs'.[i] = f cs.[i]] for all indices [i]
    of [cs]. [f] is invoked in increasing index order. *)

val mapi : (int -> char -> char) -> 'a rd t -> rdwr t
(** [map f cs] is [cs'] with [cs'.[i] = f i cs.[i]] for all indices [i]
    of [cs]. [f] is invoked in increasing index order. *)
