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

cenum foo64 {
  ONE64;
  TWO64;
  THREE64
} as uint64_t

cenum foo32 {
  ONE32;
  TWO32 = 3;
  THREE32
} as uint32_t

cenum foo16 {
  ONE16;
  TWO16;
  THREE16
} as uint16_t

cenum foo8 {
  ONE8;
  TWO8;
  THREE8
} as uint8_t

let _ = 
  ignore(int_to_foo64 2L);
  ignore(int_to_foo32 1l);
  ignore(int_to_foo16 1);
  ignore(int_to_foo8 1);
  ignore(foo64_to_int ONE64);
  ignore(foo32_to_int ONE32);
  ignore(foo16_to_int ONE16);
  ignore(foo8_to_int ONE8);
  assert(foo32_to_int TWO32 = 3l);
  assert(foo32_to_int THREE32 = 1l);
  assert(int_to_foo32 3l = Some (TWO32));
  assert(int_to_foo32 1l = Some (THREE32));
  print_endline (foo8_to_string ONE8)
