(** {1 Helpers to parse.}

    [Cstruct] is used to manipulate {i payloads} which can be formatted
   according an {{:https://perdu.com/}RFC} or an user-defined format. In such context, this module
   provides utilities to be able to easily {i parse} {i payloads}.

    Due to the type {!Cstruct.t}, no copy are done when you use these utilities
   and you are able to extract your information without a big performance cost.

    More precisely, each values returned by these utilities will be located into
   the minor-heap where the base buffer will never be copied or relocated.

    For instance, to parse a Git tree object:

{v
  entry := perm ' ' name '\000' 20byte
  tree  := entry *
v}

    {[
      open Cstruct_parse

      let ( >>= ) = Option.bind

      let rec hash_of_name ~name payload =
        if is_empty payload then raise Not_found
        else
          cut ~sep:(v " ") payload >>= fun (_, payload) ->
          cut ~sep:(v "\000") payload >>= fun (name', payload) ->
          if name = name' then with_range ~len:20 payload
          else hash_of_name ~name (shift payload 20)
    ]}

    A [Cstruct] defines a possibly empty subsequence of bytes in a {e base}
   buffer (a {!Bigarray.Array1.t}).

    The positions of a buffer [b] of length [l] are the slits found
   before each byte and after the last byte of the buffer. They are
   labelled from left to right by increasing number in the range \[[0];[l]\].

{v
positions  0   1   2   3   4    l-1    l
           +---+---+---+---+     +-----+
  indices  | 0 | 1 | 2 | 3 | ... | l-1 |
           +---+---+---+---+     +-----+
v}

    The [i]th byte index is between positions [i] and [i+1].

    Formally we define a subbuffer of [b] as being a subsequence
   of bytes defined by a {e off} position and a {e len} number. When
   [len] is [0] the subbuffer is {e empty}. Note that for a given
   base buffer there are as many empty subbuffers as there are positions
   in the buffer.

    Like in strings, we index the bytes of a subbuffer using zero-based
   indices.
*)

open Cstruct

val empty : t
(** [empty] is the empty {!Cstruct.t}. *)

val get : t -> int -> char
(** [get cs zidx] is the byte of [cs] at its zero-based index [zidx].

    @raise Invalid_argument if [zidx] is not an index of [cs]. *)

val get_byte : t -> int -> int
(** [get_byte cs zidx] is [Char.code (get cs zidx)]. *)

val string : ?off:int -> ?len:int -> string -> t
(** [string ~off ~len str] is the subbuffer of [str] that starts at position [off]
   (defaults to [0]) and stops at position [off + len] (defaults to
   [String.length str]). [str] is fully-replaced by an fresh allocated
   {!Cstruct.buffer}.

    @raise Invalid_argument if [off] or [off + len] are not positions of [str].
   *)

val buffer : ?off:int -> ?len:int -> buffer -> t
(** [buffer ~off ~len buffer] is the sub-part of [buffer] that starts at
   position [off] (default to [0]) and stops at position [off + len] (default to
   [Bigarray.Array1.dim buffer]). [buffer] is used as the base buffer of the
   returned value (no major-heap allocation are performed).

    @raise Invalid_argument if [off] or [off + len] are not positions of
   [buffer]. *)

val start_pos : t -> int
(** [start_pos cs] is [cs]'s start position in the base {!Cstruct.buffer}. *)

val stop_pos : t -> int
(** [stop_pos cs] is [cs]'s stop position in the base {!Cstruct.buffer}. *)

val length : t -> int
(** [length cs] is the number of bytes in [cs]. *)

val head : ?rev:bool -> t -> char option
(** [head cs] is [Some (get cs h)] with [h = 0] if [rev = false] (default) or [h
   = length cs - 1] if [rev = true]. [None] is returned if [cs] is empty. *)

val tail : ?rev:bool -> t -> t
(** [tail cs] is [cs] without its first ([rev] is [false], default) or last
   ([rev] is [true]) byte or [cs] is empty. *)

val is_empty : t -> bool
(** [is_empty cs] is [length cs = 0]. *)

val is_prefix : affix:t -> t -> bool
(** [is_prefix ~affix cs] is [true] iff [affix.[zidx] = cs.[zidx]] for all
   indices [zidx] of [affix]. *)

val is_suffix : affix:t -> t -> bool
(** [is_suffix ~affix cs] is [true] iff [affix.[n - zidx] = cs.[m - zidx]] for
   all indices [zidx] of [affix] with [n = length affix - 1] and [m = length cs
   - 1]. *)

val is_infix : affix:t -> t -> bool
(** [is_infix ~affix cs] is [true] iff there exists an index [z] in [cs] such
   that for all indices [zidx] of [affix] we have [affix.[zidx] = cs.[z +
   zidx]]. *)

val for_all : (char -> bool) -> t -> bool
(** [for_all p cs] is [true] iff for all indices [zidx] of [cs], [p cs.[zidx] =
   true]. *)

val exists : (char -> bool) -> t -> bool
(** [exists p cs] is [true] iff there exists an index [zidx] of [cs] with [p
   cs.[zidx] = true]. *)

val start : t -> t
(** [start cs] is the empty sub-part at the start position of [cs]. *)

val stop : t -> t
(** [stop cs] is the empty sub-part at the stop position of [cs]. *)

val trim : ?drop:(char -> bool) -> t -> t
(** [trim ~drop cs] is [cs] with prefix and suffix bytes satisfying [drop] in
   [cs] removed. [drop] defaults to [function ' ' | '\r' .. '\t' -> true | _ ->
   false]. *)

val span : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t * t
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

val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
(** [take ~rev ~min ~max ~sat cs] is the matching span of {!span} without the remaining one.
    In other words:

    {[(if rev then snd else fst) @@ span ~rev ~min ~max ~sat cs]} *)

val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
(** [drop ~rev ~min ~max ~sat cs] is the remaining span of {!span} without the matching one.
    In other words:

    {[(if rev then fst else snd) @@ span ~rev ~min ~max ~sat cs]} *)

val cut : ?rev:bool -> sep:t -> t -> (t * t) option
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

val cuts : ?rev:bool -> ?empty:bool -> sep:t -> t -> t list
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

val fields : ?empty:bool -> ?is_sep:(char -> bool) -> t -> t list
(** [fields ~empty ~is_sep cs] is the list of (possibly empty)
    sub-buffers that are delimited by bytes for which [is_sep] is
    [true]. Empty sub-buffers are omitted in the list if [empty] is
    [false] (defaults to [true]). [is_sep c] if it's not define by the
    user is [true] iff [c] is an US-ASCII white space character,
    that is one of space [' '] ([0x20]), tab ['\t'] ([0x09]), newline
    ['\n'] ([0x0a]), vertical tab ([0x0b]), form feed ([0x0c]), carriage
    return ['\r'] ([0x0d]). *)

val find : ?rev:bool -> (char -> bool) -> t -> t option
(** [find ~rev sat cs] is the sub-buffer of [cs] (if any) that spans
    the first byte that satisfies [sat] in [cs] after position [start cs]
    ([rev] is [false], default) or before [stop cs] ([rev] is [true]).
    [None] is returned if there is no matching byte in [s]. *)

val find_sub : ?rev:bool -> sub:t -> t -> t option
(** [find_sub ~rev ~sub cs] is the sub-buffer of [cs] (if any) that spans
    the first match of [sub] in [cs] after position [start cs]
    ([rev] is [false], default) or before [stop cs] ([rev] is [true]).
    Only bytes are compared and [sub] can be on a different base buffer.
    [None] is returned if there is no match of [sub] in [s]. *)

val filter : (char -> bool) -> t -> t
(** [filter sat cs] is the buffer made of the bytes of [cs] that satisfy [sat],
    in the same order. *)

val filter_map : (char -> char option) -> t -> t
(** [filter_map f cs] is the buffer made of the bytes of [cs] as mapped by
    [f], in the same order. *)

val map : (char -> char) -> t -> t
(** [map f cs] is [cs'] with [cs'.[i] = f cs.[i]] for all indices [i]
    of [cs]. [f] is invoked in increasing index order. *)

val mapi : (int -> char -> char) -> t -> t
(** [map f cs] is [cs'] with [cs'.[i] = f i cs.[i]] for all indices [i]
    of [cs]. [f] is invoked in increasing index order. *)
