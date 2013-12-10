(*
 * Copyright (c) 2013 Citrix Systems Inc
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
open OUnit

(* Check we can create and use an empty cstruct *)
let test_empty_cstruct () =
  let x = Cstruct.create 0 in
  assert_equal ~printer:string_of_int 0 (Cstruct.len x);
  let y = Cstruct.to_string x in
  assert_equal "" y

(* Check we can shift in the +ve direction *)
let test_positive_shift () =
  let x = Cstruct.create 1 in
  let y = Cstruct.shift x 1 in
  assert_equal ~printer:string_of_int 0 (Cstruct.len y)

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
  "Test bounds checks";

  let suite = "bounds" >::: [
    "test empty cstruct" >:: test_empty_cstruct;
    "test positive shift" >:: test_positive_shift;
  ] in
  run_test_tt ~verbose:!verbose suite

