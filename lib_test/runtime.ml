let failed = ref false

let check name ~expected got =
  if String.equal expected got then Printf.printf "%s: ok\n" name
  else (
    failed := true;
    Printf.printf "%s: expected %S, got %S\n" name expected got)

let zeros n = Cstruct.create n

(* caml_blit_bigstring_to_bigstring *)
let () =
  let src = Cstruct.of_string "abcdefgh" and dst = zeros 8 in
  Cstruct.blit src 2 (Cstruct.shift dst 1) 1 3;
  check "blit" ~expected:"\000\000cde\000\000\000" (Cstruct.to_string dst);
  let t = Cstruct.of_string "abcdefgh" in
  Cstruct.blit t 0 t 2 4;
  check "blit overlapping" ~expected:"ababcdgh" (Cstruct.to_string t)

(* caml_blit_string_to_bigstring *)
let () =
  let dst = zeros 6 in
  Cstruct.blit_from_string "xyz" 1 dst 2 2;
  check "blit_from_string" ~expected:"\000\000yz\000\000" (Cstruct.to_string dst);
  let dst = zeros 6 in
  Cstruct.blit_from_bytes (Bytes.of_string "xyz") 0 dst 3 3;
  check "blit_from_bytes" ~expected:"\000\000\000xyz" (Cstruct.to_string dst)

(* caml_blit_bigstring_to_string *)
let () =
  let src = Cstruct.shift (Cstruct.of_string "-abcdef") 1 in
  let dst = Bytes.make 5 '.' in
  Cstruct.blit_to_bytes src 1 dst 2 3;
  check "blit_to_bytes" ~expected:"..bcd" (Bytes.to_string dst);
  check "to_string" ~expected:"cde" (Cstruct.to_string ~off:2 ~len:3 src)

(* caml_fill_bigstring *)
let () =
  let t = Cstruct.of_string "abcdefgh" in
  Cstruct.memset (Cstruct.sub t 2 3) (Char.code '*');
  check "memset" ~expected:"ab***fgh" (Cstruct.to_string t)

(* caml_compare_bigstring *)
let () =
  let compare a b = string_of_int (Cstruct.compare a b) in
  let s = Cstruct.of_string in
  check "compare lt" ~expected:"-1" (compare (s "abc") (s "abd"));
  check "compare gt" ~expected:"1" (compare (s "abd") (s "abc"));
  check "compare eq" ~expected:"0" (compare (Cstruct.shift (s "xabc") 1) (s "abc"));
  check "compare unsigned" ~expected:"1" (compare (s "\x80") (s "\x01"))

(* caml_check_alignment_bigstring *)
let () =
  check "check_alignment" ~expected:"true"
    (string_of_bool (Cstruct.check_alignment (Cstruct.create 8) 1))

let () = if !failed then exit 1
