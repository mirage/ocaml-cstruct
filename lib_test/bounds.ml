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

let to_string { Cstruct.buffer; off; len } =
  Printf.sprintf "buffer length = %d; off=%d; len=%d" (Bigarray.Array1.dim buffer) off len

(* Check we can create and use an empty cstruct *)
let test_empty_cstruct () =
  let x = Cstruct.create 0 in
  assert_equal ~printer:string_of_int 0 (Cstruct.len x);
  let y = Cstruct.to_string x in
  assert_equal "" y

(* Check that we can't create a cstruct with a negative length *)
let test_anti_cstruct () =
  try
    let x = Cstruct.create (-1) in
    failwith (Printf.sprintf "test_anti_cstruct: %s" (to_string x))
  with Invalid_argument _ ->
    ()

(* Check we can shift in the +ve direction *)
let test_positive_shift () =
  let x = Cstruct.create 1 in
  let y = Cstruct.shift x 1 in
  assert_equal ~printer:string_of_int 0 (Cstruct.len y)

(* Check we can shift in the -ve direction *)
let test_negative_shift () =
  let x = Cstruct.create 10 in
  let y = Cstruct.sub x 5 5 in
  let z = Cstruct.shift y (-5) in
  assert_equal ~printer:string_of_int 0 z.Cstruct.off;
  assert_equal ~printer:string_of_int 10 z.Cstruct.len

(* Check that an attempt to shift beyond the end of the buffer fails *)
let test_bad_positive_shift () =
  let x = Cstruct.create 10 in
  try
    let y = Cstruct.shift x 11 in
    failwith (Printf.sprintf "test_bad_positive_shift: %s" (to_string y))
  with Invalid_argument _ -> ()

(* Check that an attempt to shift before the start of the buffer fails *)
let test_bad_negative_shift () =
  let x = Cstruct.create 10 in
  try
    let y = Cstruct.shift x (-1) in
    failwith (Printf.sprintf "test_bad_negative_shift: %s" (to_string y))
  with Invalid_argument _ -> ()

(* Check that 'sub' works *)
let test_sub () =
  let x = Cstruct.create 100 in
  let y = Cstruct.sub x 10 80 in
  assert_equal ~printer:string_of_int 10 y.Cstruct.off;
  assert_equal ~printer:string_of_int 80 y.Cstruct.len;
  let z = Cstruct.sub y 10 60 in
  assert_equal ~printer:string_of_int 20 z.Cstruct.off;
  assert_equal ~printer:string_of_int 60 z.Cstruct.len

(* Check that 'sub' can't set 'len' too big *)
let test_sub_len_too_big () =
  let x = Cstruct.create 0 in
  try
    let y = Cstruct.sub x 0 1 in
    failwith (Printf.sprintf "test_sub_len_too_big: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_sub_len_too_small () =
  let x = Cstruct.create 0 in
  try
    let y = Cstruct.sub x 0 (-1) in
    failwith (Printf.sprintf "test_sub_len_too_small: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_sub_offset_too_big () =
  let x = Cstruct.create 10 in
  begin
    try
      let y = Cstruct.sub x 11 0 in
      failwith (Printf.sprintf "test_sub_offset_too_big: %s" (to_string y))
    with Invalid_argument _ -> ()
  end;
  let y = Cstruct.sub x 1 9 in
  begin
    try
      let z = Cstruct.sub y 10 0 in
      failwith (Printf.sprintf "test_sub_offset_too_big: %s" (to_string z))
    with Invalid_argument _ -> ()
  end

let test_sub_offset_too_small () =
  let x = Cstruct.create 0 in
  try
    let y = Cstruct.sub x (-1) 0 in
    failwith (Printf.sprintf "test_sub_offset_too_small: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_set_len_too_big () =
  let x = Cstruct.create 0 in
  try
    let y = Cstruct.set_len x 1 in
    failwith (Printf.sprintf "test_set_len_too_big: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_set_len_too_small () =
  let x = Cstruct.create 0 in
  try
    let y = Cstruct.set_len x (-1) in
    failwith (Printf.sprintf "test_set_len_too_small: %s" (to_string y))
  with Invalid_argument _ -> ()

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
  "Test bounds checks";

  let suite = "bounds" >::: [
    "test empty cstruct" >:: test_empty_cstruct;
    "test anti cstruct" >:: test_anti_cstruct;
    "test positive shift" >:: test_positive_shift;
    "test negative shift" >:: test_negative_shift;
    "test bad positive shift" >:: test_bad_positive_shift;
    "test bad negative shift" >:: test_bad_negative_shift;
    "test sub" >:: test_sub;
    "test sub len too big" >:: test_sub_len_too_big;
    "test sub len too small" >:: test_sub_len_too_small;
    "test sub offset too big" >:: test_sub_offset_too_big;
    "test sub offset too small" >:: test_sub_offset_too_small;
    "test set len too big" >:: test_set_len_too_big;
    "test set len too small" >:: test_set_len_too_small;
  ] in
  run_test_tt ~verbose:!verbose suite

