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

cstruct foo {
  uint8_t a;
  uint16_t b;
  uint32_t c;
  uint8_t d[8]
} as big_endian

cstruct bar {
  uint8_t a;
  uint16_t b;
  uint32_t c;
  uint8_t d[8]
} as big_endian

let _ = 
  (* Test basic set/get functions *)
  let be = Bigarray.(Array1.create char c_layout sizeof_foo) in
  for i = 0 to 255 do
    set_bar_a be i;
    assert(get_bar_a be = i)
  done;
  let le = Bigarray.(Array1.create char c_layout sizeof_bar) in
  for i = 0 to 255 do
    set_foo_a le i;
    assert(get_foo_a le = i)
  done;
  let be = Bigarray.(Array1.create char c_layout sizeof_foo) in
  for i = 0 to 65535 do
    set_bar_b be i;
    assert(get_bar_b be = i)
  done;
  let le = Bigarray.(Array1.create char c_layout sizeof_bar) in
  for i = 0 to 65535 do
    set_foo_b le i;
    assert(get_foo_b le = i)
  done;
  let be = Bigarray.(Array1.create char c_layout sizeof_foo) in
  let rec fn = function
   |i when i < 0l -> ()
   |i ->
      set_bar_c be i;
      assert(get_bar_c be = i);
      fn (Int32.sub i 0x10l)
  in fn 0xffffffff_l;
  (* Get/set buffers and blits *)
  let le = Bigarray.(Array1.create char c_layout sizeof_bar) in
  let rec fn = function
   |i when i < 0l -> ()
   |i ->
      set_foo_c le i;
      assert(get_foo_c le = i);
      fn (Int32.sub i 0x10l)
  in fn 0xffffffff_l;
  let s1 = "deadbeef" in
  set_foo_d s1 0 be;
  assert(copy_foo_d be = s1);
  let sb1 = get_foo_d be in
  blit_bar_d sb1 0 le;
  assert(copy_bar_d le = s1);
  Printf.printf "%s %s\n" (copy_foo_d be) (copy_bar_d le);
  (* Create sub-view and shift it back *)
  let be = Bigarray.(Array1.create char c_layout sizeof_foo) in
  set_foo_a be 7;
  set_foo_b be 44;
  set_foo_c be 0xbeef_l;
  set_foo_d "abcdefgh" 0 be;
  (* shifting the base array should fail *)
  assert(not (Cstruct.shift_left be 1));
  (* get a subview *)
  let be2 = Cstruct.shift be 3 in
  assert(Cstruct.BE.get_uint32 be2 0 = 0xbeef_l);
  (* shift it back *)
  assert(Cstruct.shift_left be2 3);
  assert(Cstruct.BE.get_uint32 be2 3 = 0xbeef_l);
  assert(not (Cstruct.shift_left be2 1));
  assert(get_foo_b be2 = 44);
  assert(get_foo_a be2 = 7)
