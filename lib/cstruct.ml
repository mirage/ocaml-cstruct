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

type buffer = (char, Bigarray_compat.int8_unsigned_elt, Bigarray_compat.c_layout) Bigarray_compat.Array1.t

(* Note:
 *
 * We try to maintain the property that no constructed [t] can ever point out of
 * its underlying buffer. This property is guarded by all of the constructing
 * functions and the fact that the type is private, and used by various
 * functions that would otherwise be completely unsafe.
 *
 * Furthermore, no operation on [t] is allowed to extend the view on the
 * underlying Bigarray structure, only narrowing is allowed. The deprecated
 * functions add_len and set_len violate this.
 *
 * All well-intended souls are kindly invited to cross-check that the code
 * indeed maintains this invariant.
 *)

(* Note that offset is stored in both bits and bytes! *)
type t = {
  buffer: buffer;
  off   : int;
  len   : int;
}

let pp_t ppf t =
  Format.fprintf ppf "[%d,%d](%d)" t.off t.len (Bigarray_compat.Array1.dim t.buffer)
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
  err "invalid bounds in Cstruct.%s %a off=%d len=%d" f pp_t [@@inline never]
let err_split t = err "Cstruct.split %a start=%d off=%d" pp_t t
let err_iter t = err "Cstruct.iter %a i=%d len=%d" pp_t t

let of_bigarray ?(off=0) ?len buffer =
  let dim = Bigarray_compat.Array1.dim buffer in
  let len =
    match len with
    | None     -> dim - off
    | Some len -> len in
  if off < 0 || len < 0 || off + len < 0 || off + len > dim then err_of_bigarray off len
  else { buffer; off; len }

let to_bigarray buffer =
  Bigarray_compat.Array1.sub buffer.buffer buffer.off buffer.len

let create_unsafe len =
  let buffer = Bigarray_compat.(Array1.create char c_layout len) in
  { buffer ; len ; off = 0 }

let check_bounds t len =
  len >= 0 && Bigarray_compat.Array1.dim t.buffer >= len

let empty = create_unsafe 0

external check_alignment_bigstring : buffer -> int -> int -> bool = "caml_check_alignment_bigstring"

let check_alignment t alignment =
  if alignment > 0 then
    check_alignment_bigstring t.buffer t.off alignment
  else invalid_arg "check_alignment must be positive integer"

type byte = char

let byte (i:int) : byte = Char.chr i
let byte_to_int (b:byte) = int_of_char b

type uint8 = int
type uint16 = int
type uint32 = int32
type uint64 = int64

let debug t =
  let max_len = Bigarray_compat.Array1.dim t.buffer in
  if t.off+t.len > max_len || t.len < 0 || t.off < 0 then (
    Format.printf "ERROR: t.off+t.len=%d %a\n%!" (t.off+t.len) pp_t t;
    assert false;
  ) else
    Format.asprintf "%a" pp_t t

let sub t off len =
  (* from https://github.com/mirage/ocaml-cstruct/pull/245

     Cstruct.sub should select what a programmer intuitively expects a
     sub-cstruct to be. I imagine holding out my hands, with the left
     representing the start offset and the right the end. I think of a
     sub-cstruct as any span within this range. If I move my left hand only to
     the right (new_start >= t.off), and my right hand only to the left
     (new_end <= old_end), and they don't cross (new_start <= new_end), then I
     feel sure the result will be a valid sub-cstruct. And if I violate any one
     of these constraints (e.g. moving my left hand further left), then I feel
     sure that the result wouldn't be something I'd consider to be a sub-cstruct.

     Wrapping considerations in modular arithmetic:

     Note that if x is non-negative, and x + y wraps, then x + y must be
     negative. This is easy to see with modular arithmetic because if y is
     negative then the two arguments will cancel to some degree the result
     cannot be further from zero than one of the arguments. If y is positive
     then x + y can wrap, but even max_int + max_int doesn't wrap all the way to
     zero.

     The three possibly-wrapping operations are:

     new_start = t.off + off. t.off is non-negative so if this wraps then
     new_start will be negative and will fail the new_start >= t.off test.

     new_end = new_start + len. The above test ensures that new_start is
     non-negative in any successful return. So if this wraps then new_end will
     be negative and will fail the new_start <= new_end test.

     old_end = t.off + t.len. This uses only the existing trusted values. It
     could only wrap if the underlying bigarray had a negative length!  *)
  let new_start = t.off + off in
  let new_end = new_start + len in
  let old_end = t.off + t.len in
  if new_start >= t.off && new_end <= old_end && new_start <= new_end then
    { t with off = new_start ; len }
  else
    err_sub t off len

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


external unsafe_blit_bigstring_to_bigstring : buffer -> int -> buffer -> int -> int -> unit = "caml_blit_bigstring_to_bigstring" [@@noalloc]

external unsafe_blit_string_to_bigstring : string -> int -> buffer -> int -> int -> unit = "caml_blit_string_to_bigstring" [@@noalloc]

external unsafe_blit_bytes_to_bigstring : Bytes.t -> int -> buffer -> int -> int -> unit = "caml_blit_string_to_bigstring" [@@noalloc]

external unsafe_blit_bigstring_to_bytes : buffer -> int -> Bytes.t -> int -> int -> unit = "caml_blit_bigstring_to_string" [@@noalloc]

external unsafe_compare_bigstring : buffer -> int -> buffer -> int -> int -> int = "caml_compare_bigstring" [@@noalloc]

external unsafe_fill_bigstring : buffer -> int -> int -> int -> unit = "caml_fill_bigstring" [@@noalloc]

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
  else if dstoff < 0 || dst.len - dstoff < len then
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
let memset t x = unsafe_fill_bigstring t.buffer (t.off/8) t.len x

let create len =
  let t = create_unsafe len in
  memset t 0;
  t

let set_uint8 t i c =
  if i >= t.len || i < 0 then err_invalid_bounds "set_uint8" t i 1
  else Bigarray_compat.Array1.set t.buffer (t.off+i) (Char.unsafe_chr c)

let set_char t i c =
  if i >= t.len || i < 0 then err_invalid_bounds "set_char" t i 1
  else Bigarray_compat.Array1.set t.buffer (t.off+i) c

let get_uint8 t i =
  if i >= t.len || i < 0 then err_invalid_bounds "get_uint8" t i 1
  else Char.code (Bigarray_compat.Array1.get t.buffer (t.off+i))

let get_char t i =
  if i >= t.len || i < 0 then err_invalid_bounds "get_char" t i 1
  else Bigarray_compat.Array1.get t.buffer (t.off+i)


external ba_set_int16 : buffer -> int -> uint16 -> unit = "%caml_bigstring_set16u"
external ba_set_int32 : buffer -> int -> uint32 -> unit = "%caml_bigstring_set32u"
external ba_set_int64 : buffer -> int -> uint64 -> unit = "%caml_bigstring_set64u"
external ba_get_int16 : buffer -> int -> uint16 = "%caml_bigstring_get16u"
external ba_get_int32 : buffer -> int -> uint32 = "%caml_bigstring_get32u"
external ba_get_int64 : buffer -> int -> uint64 = "%caml_bigstring_get64u"

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let set_uint16 swap p t i c =
  if i > t.len - 2 || i < 0 then err_invalid_bounds (p ^ ".set_uint16") t i 2
  else ba_set_int16 t.buffer (t.off+i) (if swap then swap16 c else c) [@@inline]

let set_uint32 swap p t i c =
  if i > t.len - 4 || i < 0 then err_invalid_bounds (p ^ ".set_uint32") t i 4
  else ba_set_int32 t.buffer (t.off+i) (if swap then swap32 c else c) [@@inline]

let set_uint64 swap p t i c =
  if i > t.len - 8 || i < 0 then err_invalid_bounds (p ^ ".set_uint64") t i 8
  else ba_set_int64 t.buffer (t.off+i) (if swap then swap64 c else c) [@@inline]

let get_uint16 swap p t i =
  if i > t.len - 2 || i < 0 then err_invalid_bounds (p ^ ".get_uint16") t i 2
  else
    let r = ba_get_int16 t.buffer (t.off+i) in
    if swap then swap16 r else r [@@inline]

let get_uint32 swap p t i =
  if i > t.len - 4 || i < 0 then err_invalid_bounds (p ^ ".get_uint32") t i 4
  else
    let r = ba_get_int32 t.buffer (t.off+i) in
    if swap then swap32 r else r [@@inline]

let get_uint64 swap p t i =
  if i > t.len - 8 || i < 0 then err_invalid_bounds (p ^ ".get_uint64") t i 8
  else
    let r = ba_get_int64 t.buffer (t.off+i) in
    if swap then swap64 r else r [@@inline]

(* o - bitoffset, i - count of bits *)
(* It assumes we use __attribute__((packed)) with alignment = 1 byte *)
(* Here we also assume there is no trailing padding at the end *)
(* The offset and alignment rules depend on the "parent" type *)
let align_bits = 8 (* bits, not bytes *)

(* Count the granulas required to read/write - little endian *)
let bitfield_granula_count_le align _ o i =
    if o mod align = 0 then (
        if i mod align = 0 then i / align
        else i / align + 1
    ) else (
        if (i + o) mod align = 0 then i / align
        else i / align + 1
    )

(* Count the granulas required to read/write - big endian *)
let bitfield_granula_count_be align t o i =
    let o' = t.len * 8 - o - i in
    if o' mod align = 0 then (
        if i mod align = 0 then i / align
        else i / align + 1
    ) else (
        if (i + o') mod align = 0 then i / align
        else i / align + 1
    )


(* These functions do not cross the boundary, and just add zeroes *)
let get_uint16_chop_boundary swap p t i =
    if i + 2 > t.len then
        let part = get_uint8 t i in
        if swap then part lsl 8 else part
    else
        get_uint16 swap p t i

let get_uint32_chop_boundary swap p t i =
    if i + 4 > t.len then
        if i + 3 > t.len then
            Int32.of_int (get_uint16_chop_boundary swap p t (i + 2))
        else
            let part1 = get_uint16 swap p t i in
            let part2 = get_uint8 t (i + 2) in
            let chunk =
                if swap then part1 lsl 8 + part2 else part2 lsl 16 + part1
            in
            Int32.of_int (chunk lsl 8)
    else
        get_uint32 swap p t i

let get_uint64_chop_boundary swap p t i =
    if i + 8 > t.len then
        if i + 7 > t.len then
            if i + 6 > t.len then
                if i + 5 > t.len then
                    Int64.of_int32 (get_uint32_chop_boundary swap p t (i + 4))
                else
                    let part1 = Int64.of_int32 (get_uint32 swap p t i) in
                    let part2 = Int64.of_int (get_uint8 t (i + 4)) in
                    let chunk =
                        if swap then
                            Int64.add (Int64.shift_left part1 8) part2
                        else
                            Int64.add (Int64.shift_left part2 32) part1
                    in
                    Int64.shift_left chunk 24
            else
                let part1 = Int64.of_int32 (get_uint32 swap p t i) in
                let part2 = Int64.of_int (get_uint16 swap p t (i + 4)) in
                let chunk =
                    if swap then
                        Int64.add (Int64.shift_left part1 16) part2
                    else
                        Int64.add (Int64.shift_left part2 32) part1
                in
                Int64.shift_left chunk 16
        else
            let part1 = Int64.of_int32 (get_uint32 swap p t i) in
            let part2 = Int64.of_int32 (get_uint32_chop_boundary swap p t (i + 4)) in
            let chunk =
                if swap then
                    Int64.add (Int64.shift_left part1 32) part2
                else
                    Int64.add (Int64.shift_left part2 32) part1
            in
            chunk
    else
        get_uint64 swap p t i

let get_bits_uint8_le_boundary_chop swap p t o i =
  let byteoff = o / align_bits in
  (* 6 cases total *)
  (* When they start at granularity boundary *)
  if o mod align_bits = 0 then
    (* First case - when they are located near the granularity boundary,
     * and lesser or equal than a granularity unit (byte) *)
    if i <= align_bits then
        let byte = get_uint8 t byteoff in
        let mask = (1 lsl i) - 1 in
        byte land mask
    (* Second case - when they are located near the granularity boundary,
     * and bigger than a granularity unit (byte) *)
    else
        (* Check if it crosses the boundary *)
        if i mod align_bits = 0 then
            (* TODO: Consider also the length of the buffer here *)
            get_uint16_chop_boundary swap p t byteoff
        else
            (* Here mask is tricky - what to read there?
             * Do we need to make swap before or after? *)
            let mask = (1 lsl i) - 1 in
            let raw = get_uint16_chop_boundary swap p t byteoff in
            raw land mask
  (* When they start not at granularity boundary *)
  else
      let bithead = o mod align_bits in
      (* Third case - when they are located not at a granularity boundary,
       * and lesser or equal than a tail of the granula (byte) *)
      if i + bithead < align_bits then
        let byte = get_uint8 t byteoff in
        let mask = ((1 lsl i) - 1) lsl bithead in
        (byte land mask) lsr bithead
      (* Fouth case - when they are located not at a granularity boundary,
       * and go further than next granularity boundary *)
      else
        (* Check if it crosses the boundary *)
        if (i + o) mod align_bits = 0 then
            (* Read these bytes, how to check the swap? *)
            let mask = ((1 lsl i) - 1) lsl bithead in
            let raw = get_uint16_chop_boundary swap p t byteoff in
            (raw land mask) lsr bithead
        else
            (* Read these bytes, how to check the swap? *)
            let mask = ((1 lsl i) - 1) lsl bithead in
            let raw = get_uint16_chop_boundary swap p t byteoff in
            (raw land mask) lsr bithead


(* It also should always start from the most significant bit for big endian and least significant bit on little endian machine
 * Basically on big endian machine we should read them in reversed order, bit by bit *)
(* Currently it always starts from the least significant bit
 * How to handle the byte-peeking then? *)
let get_bits_uint8_le_no_boundary_chop _ _ t o i =
  let byteoff = o / align_bits in
  (* 6 cases total *)
  (* When they start at granularity boundary *)
  if o mod align_bits = 0 then
    (* First case - when they are located near the granularity boundary,
     * and lesser or equal than a granularity unit (byte) *)
    if i <= align_bits then
        let byte = get_uint8 t byteoff in
        let mask = (1 lsl i) - 1 in
        byte land mask
    (* Second case - when they are located near the granularity boundary,
     * and bigger than a granularity unit (byte) *)
    else
        err_invalid_bounds "get_bits_uint8" t i 2
  (* When they start not at granularity boundary *)
  else
      let bithead = o mod align_bits in
      (* Third case - when they are located not at a granularity boundary,
       * and lesser or equal than a tail of the granula (byte) *)
      if i + bithead < align_bits then
        let byte = get_uint8 t byteoff in
        let mask = ((1 lsl i) - 1) lsl bithead in
        (byte land mask) lsr bithead
      (* Fouth case - when they are located not at a granularity boundary,
       * and go further than next granularity boundary *)
      else
        err_invalid_bounds "get_bits_uint8" t i 2

let get_bits_uint8_le swap p t o i =
  (* At first - check the boundary *)
  if ((o + i) / align_bits) > t.len || o < 0 || i > 8 then
    err_invalid_bounds "get_bits_uint8" t i 2
  else
    let count = bitfield_granula_count_le align_bits t o i in
    if count <= 2 then
        get_bits_uint8_le_boundary_chop swap p t o i
    else
        get_bits_uint8_le_no_boundary_chop swap p t o i

let get_bits_uint16_le_boundary_chop swap p t (o:int) (i:int) =
      let byteoff = o / align_bits in
      (* 6 cases total *)
      (* When they start at granularity boundary *)
      if o mod align_bits = 0 then
        (* First case - when they are located near the granularity boundary,
         * and lesser or equal than a granularity unit (byte) *)
        if i <= align_bits then
            let byte = get_uint16_chop_boundary swap p t byteoff in
            let mask = (1 lsl i) - 1 in
            byte land mask
        (* Second case - when they are located near the granularity boundary,
         * and bigger than a granularity unit (byte) *)
        else
            (* Check if it crosses the boundary *)
            if i mod align_bits = 0 then
                let mask = (1 lsl i) - 1 in
                let raw = get_uint16_chop_boundary swap p t byteoff in
                raw land mask
            else
                let open Int32 in
                let mask = sub (shift_left 1l i) 1l in
                let raw = of_int (get_uint16_chop_boundary swap p t byteoff) in
                Int32.to_int (logand raw mask)
      (* When they start not at granularity boundary *)
      else
          let bithead = o mod align_bits in
          (* Third case - when they are located not at a granularity boundary,
           * and lesser or equal than a tail of the granula (byte) *)
          if i + bithead < align_bits then
            let short = get_uint16_chop_boundary swap p t byteoff in
            let mask = ((1 lsl i) - 1) lsl bithead in
            (short land mask) lsr bithead
          (* Fouth case - when they are located not at a granularity boundary,
           * and go further than next granularity boundary *)
          else
            (* Read these bytes, how to check the swap? *)
            let mask = ((1 lsl i) - 1) lsl bithead in
            let raw = get_uint16_chop_boundary swap p t byteoff in
            (raw land mask) lsr bithead

let get_bits_uint16_le_no_boundary_chop swap p t (o:int) (i:int) =
      let byteoff = o / align_bits in
      (* 6 cases total *)
      (* When they start at granularity boundary *)
      if o mod align_bits = 0 then
        (* First case - when they are located near the granularity boundary,
         * and lesser or equal than a granularity unit (byte) *)
        if i <= align_bits then
            let byte = get_uint16_chop_boundary swap p t byteoff in
            let mask = (1 lsl i) - 1 in
            byte land mask
        (* Second case - when they are located near the granularity boundary,
         * and bigger than a granularity unit (byte) *)
        else
            (* Check if it crosses the boundary *)
            if i mod align_bits = 0 then
                let count = i / align_bits in
                if count <= 4 then
                    let open Int32 in
                    let mask = sub (shift_left 1l i) 1l in
                    let raw = get_uint32 swap p t byteoff in
                    let result = logand raw mask in
                    (* Downcast to uint16 *)
                    Int32.to_int result
                else
                    err_invalid_bounds (p ^ ".get_bits_uint16") t i 2
            else
                let count = i / align_bits + 1 in
                let open Int32 in
                let mask = sub (shift_left 1l i) 1l in
                let raw =
                    if count <= 4 then get_uint32 swap p t byteoff
                    else
                      err_invalid_bounds (p ^ ".get_bits_uint16") t i 2
                in
                Int32.to_int (logand raw mask)
      (* When they start not at granularity boundary *)
      else
          let bithead = o mod align_bits in
          (* Third case - when they are located not at a granularity boundary,
           * and lesser or equal than a tail of the granula (byte) *)
          if i + bithead < align_bits then
            let short = get_uint16_chop_boundary swap p t byteoff in
            let mask = ((1 lsl i) - 1) lsl bithead in
            (short land mask) lsr bithead
          (* Fouth case - when they are located not at a granularity boundary,
           * and go further than next granularity boundary *)
          else
            err_invalid_bounds (p ^ ".get_bits_uint16") t i 2

let get_bits_uint16_le swap p t o i =
  (* At first - check the boundary *)
  if ((o + i) / align_bits) > t.len || o < 0 || i > 16 then
    err_invalid_bounds (p ^ ".get_bits_uint16") t i 2
  else
    let count = bitfield_granula_count_le align_bits t o i in
    if count <= 2 then
        get_bits_uint16_le_boundary_chop swap p t o i
    else
        get_bits_uint16_le_no_boundary_chop swap p t o i

(* This function cannot read more than 32 bits *)
let get_bits_uint32_le swap p t o i =
  (* At first - check the boundary *)
  if ((o + i) / align_bits) > t.len || o < 0 || i > 32 then
    err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
  else
      let byteoff = o / align_bits in
      (* 6 cases total *)
      (* When they start at granularity boundary *)
      if o mod align_bits = 0 then
        (* First and second case - when they are located near the granularity boundary,
         * and lesser or equal than a granularity unit (byte) *)
        if i <= align_bits then
            let open Int32 in
            let byte = get_uint32 swap p t byteoff in
            let mask = sub (shift_left 1l i) 1l in
            logand byte mask
        (* Second case - when they are located near the granularity boundary,
         * and bigger than a granularity unit (byte) *)
        else
            (* Check if it crosses the boundary *)
            if i mod align_bits = 0 then
                let count = i / align_bits in
                let open Int32 in
                if count <= 2 then
                    let mask = sub (shift_left 1l i) 1l in
                    let raw = get_uint16 swap p t byteoff in
                    logand (of_int raw) mask
                else if count <= 4 then
                    let mask = sub (shift_left 1l i) 1l in
                    let raw = get_uint32 swap p t byteoff in
                    logand raw mask
                else if count <= 8 then
                    let open Int64 in
                    let mask = sub (shift_left 1L i) 1L in
                    let raw = get_uint64 swap p t byteoff in
                    let raw' = logand raw mask in
                    Int64.to_int32 raw'
                else
                    err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
            else
                let count = i / align_bits + 1 in
                let open Int64 in
                let raw =
                    if count <= 2 then Int64.of_int (get_uint16 swap p t byteoff)
                    else if count <= 4 then Int64.of_int32 (get_uint32 swap p t byteoff)
                    else if count <= 8 then get_uint64 swap p t o
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
                in
                let mask = sub (shift_left 1L i) 1L in
                let raw' = logand raw mask in
                Int64.to_int32 raw'
      (* When they start not at granularity boundary *)
      else
          let bithead = o mod align_bits in
          (* Third case - when they are located not at a granularity boundary,
           * and lesser or equal than a tail of the granula (byte) *)
          if i + bithead < align_bits then
            let open Int32 in
            let raw = get_uint32 swap p t byteoff in
            let mask = shift_left (sub (shift_left 1l i) 1l) bithead in
            shift_right_logical (logand raw mask) bithead
          (* Fouth case - when they are located not at a granularity boundary,
           * and go further than next granularity boundary *)
          else
            (* Check if it crosses the boundary *)
            if (i + o) mod align_bits = 0 then
                let count = i / align_bits in
                let open Int32 in
                let mask = shift_left (sub (shift_left 1l i) 1l) bithead in
                let raw =
                    if count <= 2 then of_int (get_uint16 swap p t byteoff)
                    else if count <= 4 then get_uint32 swap p t byteoff
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
                in
                shift_right_logical (logand raw mask) bithead
            else
                let count = i / align_bits + 1 in
                let open Int64 in
                let mask = shift_left (sub (shift_left 1L i) 1L) bithead in
                let raw =
                    if count <= 2 then of_int (get_uint16 swap p t byteoff)
                    else if count <= 4 then of_int32 (get_uint32 swap p t byteoff)
                    else if count <= 8 then get_uint64 swap p t o
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
                in
                let raw' = shift_right_logical (logand raw mask) bithead in
                Int64.to_int32 raw'

(* This function cannot read more than 64 bits *)
let get_bits_uint64_le swap p t o i =
  (* At first - check the boundary *)
  if ((o + i) / align_bits) > t.len || o < 0 || i > 64 then
    err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
  else
      let byteoff = o / align_bits in
      (* 6 cases total *)
      (* When they start at granularity boundary *)
      if o mod align_bits = 0 then
        (* First and second case - when they are located near the granularity boundary,
         * and lesser or equal than a granularity unit (byte) *)
        (* FIXME: Do not tie up itself to the byte as granularity unit *)
        if i <= align_bits then
            let open Int64 in
            let raw = get_uint64 swap p t byteoff in
            let mask = sub (shift_left 1L i) 1L in
            logand raw mask
        (* Second case - when they are located near the granularity boundary,
         * and bigger than a granularity unit (byte) *)
        else
            (* Check if it crosses the boundary *)
            if i mod align_bits = 0 then
                let count = i / align_bits in
                let open Int64 in
                if count <= 2 then
                    let mask = sub (shift_left 1L i) 1L in
                    let raw = get_uint16 swap p t byteoff in
                    logand (of_int raw) mask
                else if count <= 4 then
                    let mask = sub (shift_left 1L i) 1L in
                    let raw = get_uint32 swap p t byteoff in
                    logand (of_int32 raw) mask
                else if count <= 8 then
                    let mask = sub (shift_left 1L i) 1L in
                    let raw = get_uint64 swap p t byteoff in
                    logand raw mask
                else
                    err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
            else
                let count = i / align_bits + 1 in
                let open Int64 in
                let raw =
                    if count <= 2 then of_int (get_uint16 swap p t byteoff)
                    else if count <= 4 then of_int32 (get_uint32 swap p t byteoff)
                    else if count <= 8 then get_uint64 swap p t o
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
                in
                let mask = sub (shift_left 1L i) 1L in
                logand raw mask
      (* When they start not at granularity boundary *)
      else
          let bithead = o mod align_bits in
          (* Third case - when they are located not at a granularity boundary,
           * and lesser or equal than a tail of the granula (byte) *)
          if i + bithead < align_bits then
            let open Int64 in
            let raw = get_uint64 swap p t byteoff in
            let mask = shift_left (sub (shift_left 1L i) 1L) bithead in
            shift_right_logical (logand raw mask) bithead
          (* Fouth case - when they are located not at a granularity boundary,
           * and go further than next granularity boundary *)
          else
            (* Check if it crosses the boundary *)
            if (i + o) mod align_bits = 0 then
                let count = i / align_bits in
                let open Int64 in
                let mask = shift_left (sub (shift_left 1L i) 1L) bithead in
                let raw =
                    if count <= 2 then of_int (get_uint16 swap p t byteoff)
                    else if count <= 4 then of_int32 (get_uint32 swap p t byteoff)
                    else if count <= 8 then get_uint64 swap p t byteoff
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
                in
                shift_right_logical (logand raw mask) bithead
            else
                let count = i / align_bits + 1 in
                let open Int64 in
                let mask = shift_left (sub (shift_left 1L i) 1L) bithead in
                let raw =
                    if count <= 2 then of_int (get_uint16 swap p t byteoff)
                    else if count <= 4 then of_int32 (get_uint32 swap p t byteoff)
                    else if count <= 8 then get_uint64 swap p t o
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
                in
                shift_right_logical (logand raw mask) bithead

(* It also should always start from the most significant bit for big endian
 * Basically on big endian machine we should read them in reversed order, bit by bit
 * Currently it always starts from the least significant bit
 * How to handle the byte-peeking then? *)
let get_bits_uint8_be_boundary_chop swap p t o i =
  let o' = t.len * 8 - o - i in
  let byteoff = o' / align_bits in
  (* 6 cases total *)
  (* When they start at granularity boundary *)
  if o' mod align_bits = 0 then
    (* First case - when they are located near the granularity boundary,
     * and lesser or equal than a granularity unit (byte) *)
    if i <= align_bits then
      let byte = get_uint8 t byteoff in
      let mask = (1 lsl i) - 1 in
      byte land mask
    (* Second case - when they are located near the granularity boundary,
     * and bigger than a granularity unit (byte) *)
    else
      (* Check if it crosses the boundary *)
      if i mod align_bits = 0 then
        (* TODO: Consider also the length of the buffer here *)
        get_uint16_chop_boundary swap p t byteoff
      else
        (* Here mask is tricky - what to read there?
         * Do we need to make swap before or after? *)
        let mask = (1 lsl i) - 1 in
        let raw = get_uint16_chop_boundary swap p t byteoff in
        raw land mask
  (* When they start not at granularity boundary *)
  else
      let bithead = byteoff * 8 in
      (* Third case - when they are located not at a granularity boundary,
       * and lesser or equal than a tail of the granula (byte) *)
      if i + o' < align_bits then
        let byte = get_uint8 t byteoff in
        let mask = (((1 lsl i) - 1) lsl o') lsr  bithead in
        (byte land mask) lsr (bithead + 1)
      (* Fouth case - when they are located not at a granularity boundary,
       * and go further than next granularity boundary *)
      else
        (* Check if it crosses the boundary *)
        if (i + o') mod align_bits = 0 then
          (* Read these bytes, how to check the swap? *)
          let mask = ((1 lsl i) - 1) lsl bithead in
          let raw = get_uint16_chop_boundary swap p t byteoff in
          (raw land mask) lsr bithead
        else
          (* Read these bytes, how to check the swap? *)
          let mask = (((1 lsl i) - 1) lsl o') lsl bithead in
          let raw = get_uint16_chop_boundary swap p t byteoff in
          ((raw land mask) lsr o') lsr bithead

let get_bits_uint8_be_no_boundary_chop _ _ t o i =
  let o' = t.len * 8 - o - i in
  let byteoff = o' / align_bits in
  (* 6 cases total *)
  (* When they start at granularity boundary *)
  if o' mod align_bits = 0 then
    (* First case - when they are located near the granularity boundary,
     * and lesser or equal than a granularity unit (byte) *)
    if i <= align_bits then
      let byte = get_uint8 t byteoff in
      let mask = (1 lsl i) - 1 in
      byte land mask
    (* Second case - when they are located near the granularity boundary,
     * and bigger than a granularity unit (byte) *)
    else
      err_invalid_bounds "get_bits_uint8" t i 2
  (* When they start not at granularity boundary *)
  else
      let bithead = byteoff * 8 in
      (* Third case - when they are located not at a granularity boundary,
       * and lesser or equal than a tail of the granula (byte) *)
      if i + o' < align_bits then
        let byte = get_uint8 t byteoff in
        let mask = (((1 lsl i) - 1) lsl o') lsr  bithead in
        (byte land mask) lsr (bithead + 1)
      (* Fouth case - when they are located not at a granularity boundary,
       * and go further than next granularity boundary *)
      else
        err_invalid_bounds "get_bits_uint8" t i 2

let get_bits_uint8_be swap p t o i =
  (* At first - check the boundary *)
  if ((o + i) / align_bits) > t.len || o < 0 || i > 8 then
    err_invalid_bounds "get_bits_uint8" t i 2
  else
    let count = bitfield_granula_count_be align_bits t o i in
    if count <= 2 then
        get_bits_uint8_be_boundary_chop swap p t o i
    else
        get_bits_uint8_be_no_boundary_chop swap p t o i

let get_bits_uint16_be_boundary_chop swap p t (o:int) (i:int) =
  let o' = t.len * 8 - o - i in
  let byteoff = o / align_bits in
  let bithead = byteoff * 8 in
  (* 6 cases total *)
  (* When they start at granularity boundary *)
  if o' mod align_bits = 0 then
    (* First case - when they are located near the granularity boundary,
     * and lesser or equal than a granularity unit (byte) *)
    if i <= align_bits then
        let short = get_uint16_chop_boundary swap p t byteoff in
        let mask = (((1 lsl i) - 1) lsl o') lsl bithead in
        ((short land mask) lsr o') lsr bithead
    (* Second case - when they are located near the granularity boundary,
     * and bigger than a granularity unit (byte) *)
    else
        (* Check if it crosses the boundary *)
        if i mod align_bits = 0 then
          let mask = (1 lsl i) - 1 in
          let raw = get_uint16_chop_boundary swap p t byteoff in
          raw land mask
        else
            let open Int32 in
            let mask = sub (shift_left 1l i) 1l in
            let raw = of_int (get_uint16_chop_boundary swap p t byteoff) in
            Int32.to_int (logand raw mask)
  (* When they start not at granularity boundary *)
  else
      let bithead = byteoff * 8 in
      (* Third case - when they are located not at a granularity boundary,
       * and lesser or equal than a tail of the granula (byte) *)
      if i + o' < align_bits then
        let short = get_uint16_chop_boundary swap p t byteoff in
        let mask = (((1 lsl i) - 1) lsl o') lsl bithead in
        (short land mask) lsr (bithead + 1)
      (* Fouth case - when they are located not at a granularity boundary,
       * and go further than next granularity boundary *)
      else
        (* Check if it crosses the boundary *)
        if (i + o) mod align_bits = 0 then
            (* Read these bytes, how to check the swap? *)
            let mask = ((1 lsl i) - 1) lsl bithead in
            let raw = get_uint16_chop_boundary swap p t byteoff in
            (raw land mask) lsr bithead
        else
            (* Read these bytes, how to check the swap? *)
            let mask = (((1 lsl i) - 1) lsl o') lsl bithead in
            let raw = get_uint16_chop_boundary swap p t byteoff in
            ((raw land mask) lsr o') lsr bithead

let get_bits_uint16_be_no_boundary_chop swap p t (o:int) (i:int) =
  let o' = t.len * 8 - o - i in
  let byteoff = o / align_bits in
  let bithead = byteoff * 8 in
  (* 6 cases total *)
  (* When they start at granularity boundary *)
  if o' mod align_bits = 0 then
    (* First case - when they are located near the granularity boundary,
     * and lesser or equal than a granularity unit (byte) *)
    if i <= align_bits then
        let short = get_uint16_chop_boundary swap p t byteoff in
        let mask = (((1 lsl i) - 1) lsl o') lsl bithead in
        ((short land mask) lsr o') lsr bithead
    (* Second case - when they are located near the granularity boundary,
     * and bigger than a granularity unit (byte) *)
    else
        (* Check if it crosses the boundary *)
        if i mod align_bits = 0 then
            let count = i / align_bits in
            if count <= 4 then
                let open Int32 in
                let mask = sub (shift_left 1l i) 1l in
                let raw = get_uint32_chop_boundary swap p t byteoff in
                let result = logand raw mask in
                (* Downcast to uint16 *)
                Int32.to_int result
            else
                err_invalid_bounds (p ^ ".get_bits_uint16") t i 2
        else
            let count = i / align_bits + 1 in
            let open Int32 in
            let mask = sub (shift_left 1l i) 1l in
            let raw =
                if count <= 4 then get_uint32_chop_boundary swap p t byteoff
                else
                    err_invalid_bounds (p ^ ".get_bits_uint16") t i 2
            in
            Int32.to_int (logand raw mask)
  (* When they start not at granularity boundary *)
  else
      let bithead = byteoff * 8 in
      (* Third case - when they are located not at a granularity boundary,
       * and lesser or equal than a tail of the granula (byte) *)
      if i + o' < align_bits then
        let short = get_uint16_chop_boundary swap p t byteoff in
        let mask = (((1 lsl i) - 1) lsl o') lsl bithead in
        (short land mask) lsr (bithead + 1)
      (* Fouth case - when they are located not at a granularity boundary,
       * and go further than next granularity boundary *)
      else
        err_invalid_bounds (p ^ ".get_bits_uint16") t i 2

let get_bits_uint16_be swap p t o i =
  (* At first - check the boundary *)
  if ((o + i) / align_bits) > t.len || o < 0 || i > 16 then
    err_invalid_bounds (p ^ ".get_bits_uint16") t i 2
  else
    let count = bitfield_granula_count_be align_bits t o i in
    if count <= 2 then
        get_bits_uint16_be_boundary_chop swap p t o i
    else
        get_bits_uint16_be_no_boundary_chop swap p t o i

(* This function cannot read more than 32 bits *)
let get_bits_uint32_be swap p t o i =
  (* At first - check the boundary *)
  if ((o + i) / align_bits) > t.len || o < 0 || i > 32 then
    err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
  else
      let o' = t.len * 8 - o - i in
      let byteoff = o / align_bits in
      let bithead = byteoff * 8 in
      (* 6 cases total *)
      (* When they start at granularity boundary *)
      if o' mod align_bits = 0 then
        (* First and second case - when they are located near the granularity boundary,
         * and lesser or equal than a granularity unit (byte) *)
        if i <= align_bits then
            let open Int32 in
            let byte = get_uint32_chop_boundary swap p t byteoff in
            let mask = shift_left (shift_left (sub (shift_left 1l i) 1l) o') bithead in
            shift_right_logical (shift_right_logical (logand byte mask) o') bithead
        (* Second case - when they are located near the granularity boundary,
         * and bigger than a granularity unit (byte) *)
        else
            (* Check if it crosses the boundary *)
            if i mod align_bits = 0 then
                let count = i / align_bits in
                let open Int32 in
                if count <= 2 then
                    let mask = sub (shift_left 1l i) 1l in
                    let raw = get_uint16 swap p t byteoff in
                    logand (of_int raw) mask
                else if count <= 4 then
                    let mask = sub (shift_left 1l i) 1l in
                    let raw = get_uint32 swap p t byteoff in
                    logand raw mask
                else if count <= 8 then
                    let open Int64 in
                    let mask = sub (shift_left 1L i) 1L in
                    let raw = get_uint64 swap p t byteoff in
                    let raw' = logand raw mask in
                    Int64.to_int32 raw'
                else
                    err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
            else
                let count = i / align_bits + 1 in
                let open Int64 in
                let raw =
                    if count <= 2 then Int64.of_int (get_uint16 swap p t byteoff)
                    else if count <= 4 then Int64.of_int32 (get_uint32 swap p t byteoff)
                    else if count <= 8 then get_uint64 swap p t o
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
                in
                let mask = sub (shift_left 1L i) 1L in
                let raw' = logand raw mask in
                Int64.to_int32 raw'
      (* When they start not at granularity boundary *)
      else
          let bithead = byteoff * 8 in
          (* Third case - when they are located not at a granularity boundary,
           * and lesser or equal than a tail of the granula (byte) *)
          if i + o' < align_bits then
            let open Int32 in
            let raw = get_uint32_chop_boundary swap p t byteoff in
            let mask = shift_left (shift_left (sub (shift_left 1l i) 1l) o') bithead in
            shift_right_logical (logand raw mask) (bithead + 1)
          (* Fouth case - when they are located not at a granularity boundary,
           * and go further than next granularity boundary *)
          else
            (* Check if it crosses the boundary *)
            if (i + o) mod align_bits = 0 then
                let count = i / align_bits in
                let open Int32 in
                let mask = shift_left (sub (shift_left 1l i) 1l) bithead in
                let raw =
                    if count <= 2 then of_int (get_uint16_chop_boundary swap p t byteoff)
                    else if count <= 4 then get_uint32_chop_boundary swap p t byteoff
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
                in
                shift_right_logical (logand raw mask) bithead
            else
                let count = i / align_bits + 1 in
                let open Int64 in
                let mask = shift_left (sub (shift_left 1L i) 1L) bithead in
                let raw =
                    if count <= 2 then of_int (get_uint16_chop_boundary swap p t byteoff)
                    else if count <= 4 then of_int32 (get_uint32_chop_boundary swap p t byteoff)
                    else if count <= 8 then get_uint64_chop_boundary swap p t o
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint32") t i 2
                in
                let raw' = shift_right_logical
                    (shift_right_logical (logand raw mask) o') bithead in
                Int64.to_int32 raw'

(* This function cannot read more than 64 bits *)
let get_bits_uint64_be swap p t o i =
  (* At first - check the boundary *)
  if ((o + i) / align_bits) > t.len || o < 0 || i > 64 then
    err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
  else
      let o' = t.len * 8 - o - i in
      let byteoff = o / align_bits in
      let bithead = byteoff * 8 in
      (* 6 cases total *)
      (* When they start at granularity boundary *)
      if o' mod align_bits = 0 then
        (* First and second case - when they are located near the granularity boundary,
         * and lesser or equal than a granularity unit (byte) *)
        if i <= align_bits then
            let open Int64 in
            let raw = get_uint64_chop_boundary swap p t byteoff in
            let mask = shift_left (shift_left (sub (shift_left 1L i) 1L) o') bithead in
            shift_right_logical (shift_right_logical (logand raw mask) o') bithead
        (* Second case - when they are located near the granularity boundary,
         * and bigger than a granularity unit (byte) *)
        else
            (* Check if it crosses the boundary *)
            if i mod align_bits = 0 then
                let count = i / align_bits in
                let open Int64 in
                if count <= 2 then
                    let mask = sub (shift_left 1L i) 1L in
                    let raw = get_uint16_chop_boundary swap p t byteoff in
                    logand (of_int raw) mask
                else if count <= 4 then
                    let mask = sub (shift_left 1L i) 1L in
                    let raw = get_uint32_chop_boundary swap p t byteoff in
                    logand (of_int32 raw) mask
                else if count <= 8 then
                    let mask = sub (shift_left 1L i) 1L in
                    let raw = get_uint64_chop_boundary swap p t byteoff in
                    logand raw mask
                else
                    err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
            else
                let count = i / align_bits + 1 in
                let open Int64 in
                let raw =
                    if count <= 2 then of_int (get_uint16 swap p t byteoff)
                    else if count <= 4 then of_int32 (get_uint32 swap p t byteoff)
                    else if count <= 8 then get_uint64 swap p t o
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
                in
                let mask = sub (shift_left 1L i) 1L in
                logand raw mask
      (* When they start not at granularity boundary *)
      else
          let bithead = byteoff * 8 in
          (* Third case - when they are located not at a granularity boundary,
           * and lesser or equal than a tail of the granula (byte) *)
          if i + o' < align_bits then
            let open Int64 in
            let raw = get_uint64_chop_boundary swap p t byteoff in
            let mask = shift_left (shift_left (sub (shift_left 1L i) 1L) o') bithead in
            shift_right_logical (logand raw mask) (bithead + 1)
          (* Fouth case - when they are located not at a granularity boundary,
           * and go further than next granularity boundary *)
          else
            (* Check if it crosses the boundary *)
            if (i + o) mod align_bits = 0 then
                let count = i / align_bits in
                let open Int64 in
                let mask = shift_left (sub (shift_left 1L i) 1L) bithead in
                let raw =
                    if count <= 2 then of_int (get_uint16_chop_boundary swap p t byteoff)
                    else if count <= 4 then of_int32 (get_uint32_chop_boundary swap p t byteoff)
                    else if count <= 8 then get_uint64_chop_boundary swap p t byteoff
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
                in
                shift_right_logical (logand raw mask) bithead
            else
                let count = i / align_bits + 1 in
                let open Int64 in
                let mask = shift_left (shift_left (sub (shift_left 1L i) 1L) o') bithead in
                let raw =
                    if count <= 2 then of_int (get_uint16_chop_boundary swap p t byteoff)
                    else if count <= 4 then of_int32 (get_uint32_chop_boundary swap p t byteoff)
                    else if count <= 8 then get_uint64_chop_boundary swap p t o
                    else
                        err_invalid_bounds (p ^ ".get_bits_uint64") t i 2
                in
                shift_right_logical (shift_right_logical (logand raw mask) o') bithead

module BE = struct
  let set_uint16 t i c = set_uint16 (not Sys.big_endian) "BE" t i c [@@inline]
  let set_uint32 t i c = set_uint32 (not Sys.big_endian) "BE" t i c [@@inline]
  let set_uint64 t i c = set_uint64 (not Sys.big_endian) "BE" t i c [@@inline]
  let get_uint16 t i = get_uint16 (not Sys.big_endian) "BE" t i [@@inline]
  let get_uint32 t i = get_uint32 (not Sys.big_endian) "BE" t i [@@inline]
  let get_uint64 t i = get_uint64 (not Sys.big_endian) "BE" t i [@@inline]
  let set_bits_uint8 _t _o _i _c = ()
  let set_bits_uint16 _t _o _i _c = ()
  let set_bits_uint32 _t _o _i _c = ()
  let set_bits_uint64 _t _o _i _c = ()
  let get_bits_uint8 t o i = get_bits_uint8_be (not Sys.big_endian) "BE" t o i
  let get_bits_uint16 t o i = get_bits_uint16_be (not Sys.big_endian) "BE" t o i
  let get_bits_uint32 t o i = get_bits_uint32_be (not Sys.big_endian) "BE" t o i
  let get_bits_uint64 t o i = get_bits_uint64_be (not Sys.big_endian) "BE" t o i
end

module LE = struct
  let set_uint16 t i c = set_uint16 Sys.big_endian "LE" t i c [@@inline]
  let set_uint32 t i c = set_uint32 Sys.big_endian "LE" t i c [@@inline]
  let set_uint64 t i c = set_uint64 Sys.big_endian "LE" t i c [@@inline]
  let get_uint16 t i = get_uint16 Sys.big_endian "LE" t i [@@inline]
  let get_uint32 t i = get_uint32 Sys.big_endian "LE" t i [@@inline]
  let get_uint64 t i = get_uint64 Sys.big_endian "LE" t i [@@inline]
  let set_bits_uint8 _t _o _i _c = ()
  let set_bits_uint16 _t _o _i _c = ()
  let set_bits_uint32 _t _o _i _c = ()
  let set_bits_uint64 _t _o _i _c = ()
  let get_bits_uint8 t o i = get_bits_uint8_le Sys.big_endian "LE" t o i
  let get_bits_uint16 t o i = get_bits_uint16_le Sys.big_endian "LE" t o i
  let get_bits_uint32 t o i = get_bits_uint32_le Sys.big_endian "LE" t o i
  let get_bits_uint64 t o i = get_bits_uint64_le Sys.big_endian "LE" t o i
end

let len t =
  t.len

(** [sum_lengths ~caller acc l] is [acc] plus the sum of the lengths
    of the elements of [l].  Raises [Invalid_argument caller] if
    arithmetic overflows. *)
let rec sum_lengths_aux ~caller acc = function
  | [] -> acc
  | h :: t ->
     let sum = len h + acc in
     if sum < acc then invalid_arg caller
     else sum_lengths_aux ~caller sum t

let sum_lengths ~caller l = sum_lengths_aux ~caller 0 l

let lenv l = sum_lengths ~caller:"Cstruct.lenv" l

let copyv ts =
  let sz = sum_lengths ~caller:"Cstruct.copyv" ts in
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


let to_bytes t =
  let sz = len t in
  let b = Bytes.create sz in
  unsafe_blit_bigstring_to_bytes t.buffer t.off b 0 sz;
  b

let to_string t =
  (* The following call is safe, since this is the only reference to the
     freshly-created value built by [to_bytes t]. *)
  Bytes.unsafe_to_string (to_bytes t)

let of_data_abstract blitfun lenfun ?allocator ?(off=0) ?len buf =
  let buflen =
    match len with
    | None -> lenfun buf
    | Some len -> len in
  match allocator with
  | None ->
    let c = create_unsafe buflen in
    blitfun buf off c 0 buflen;
    c
  | Some fn ->
    let c = fn buflen in
    blitfun buf off c 0 buflen;
    set_len c buflen

let of_string ?allocator ?off ?len buf =
  of_data_abstract blit_from_string String.length ?allocator ?off ?len buf

let of_bytes ?allocator ?off ?len buf =
  of_data_abstract blit_from_bytes Bytes.length ?allocator ?off ?len buf

let of_hex str =
  let string_fold ~f ~z str =
    let st = ref z in
    ( String.iter (fun c -> st := f !st c) str  ; !st )
  in
  let hexdigit p = function
    | 'a' .. 'f' as x -> int_of_char x - 87
    | 'A' .. 'F' as x -> int_of_char x - 55
    | '0' .. '9' as x -> int_of_char x - 48
    | x ->
      Format.ksprintf invalid_arg "of_hex: invalid character at pos %d: %C" p x
  in
  let whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  match
    string_fold
      ~f:(fun (cs, i, p, acc) ->
          let p' = succ p in
          function
          | char when whitespace char -> (cs, i, p', acc)
          | char ->
            match acc, hexdigit p char with
            | (None  , x) -> (cs, i, p', Some (x lsl 4))
            | (Some y, x) -> set_uint8 cs i (x lor y) ; (cs, succ i, p', None))
      ~z:(create_unsafe (String.length str lsr 1), 0, 0, None)
      str
  with
  | _ , _, _, Some _ ->
    Format.ksprintf invalid_arg "of_hex: odd numbers of characters"
  | cs, i, _, _ -> sub cs 0 i

let hexdump_pp fmt t =
  let before fmt =
    function
    | 0 -> ()
    | 8 -> Format.fprintf fmt "  ";
    | _ -> Format.fprintf fmt " "
  in
  let after fmt =
    function
    | 15 -> Format.fprintf fmt "@;"
    |  _ -> ()
  in
  Format.pp_open_vbox fmt 0 ;
  for i = 0 to len t - 1 do
    let column = i mod 16 in
    let c = Char.code (Bigarray_compat.Array1.get t.buffer (t.off+i)) in
    Format.fprintf fmt "%a%.2x%a" before column c after column
  done ;
  Format.pp_close_box fmt ()

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
  let cs = create_unsafe (l1 + l2) in
  blit cs1 0 cs 0  l1 ;
  blit cs2 0 cs l1 l2 ;
  cs

let concat = function
  | []   -> create_unsafe 0
  | [cs] -> cs
  | css  ->
      let result = create_unsafe (sum_lengths ~caller:"Cstruct.concat" css) in
      let aux off cs =
        let n = len cs in
        blit cs 0 result off n ;
        off + n in
      ignore @@ List.fold_left aux 0 css ;
      result

let rev t =
  let n = len t in
  let out = create_unsafe n in
  for i_src = 0 to n - 1 do
    let byte = get_uint8 t i_src in
    let i_dst = n - 1 - i_src in
    set_uint8 out i_dst byte
  done;
  out
