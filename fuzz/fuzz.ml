open Crowbar

[@@@warning "-3"]

let create x =
  match Cstruct.create x with
  | c -> assert (x >= 0); c
  | exception Invalid_argument _ -> assert (x <= 0); bad_test ()

let create_sub x start len =
  try
    let c = Cstruct.create_unsafe x in
    for i = 0 to len - 1 do
      Cstruct.set_uint8 c i i
    done;
    Cstruct.sub c start len
  with Invalid_argument _ -> bad_test ()

let cstruct = choose [
    map [int8] create;
    map [range 0x10000; int; int] create_sub;
  ]

let bytes = map [bytes] Bytes.unsafe_of_string

let buffer = map [uint8] Bigarray.(Array1.create Char c_layout)

let pp_cstruct f c = Format.pp_print_string f (Cstruct.debug c)

let check_within ~base x =
  check Cstruct.(base.off <= x.off);
  check Cstruct.(base.off + base.len >= x.off + x.len);
  check Cstruct.(x.len >= 0 && x.len <= base.len)

let () =
  (* assert (Array.length Sys.argv = 2);   (* Prevent accidentally running in quickcheck mode *) *)
  add_test ~name:"blit" [cstruct; int; cstruct; int; int] (fun src srcoff dst dstoff len ->
      try Cstruct.blit src srcoff dst dstoff len
      with Invalid_argument _ ->
        check (srcoff < 0 || srcoff > Cstruct.length src ||
               dstoff < 0 || dstoff > Cstruct.length src ||
               len < 0 ||
               len > Cstruct.length src - srcoff ||
               len > Cstruct.length dst - dstoff)
    );
  add_test ~name:"sexp" [buffer] (fun b ->
      b |> Cstruct_sexp.sexp_of_buffer |> Cstruct_sexp.buffer_of_sexp
      |> check_eq
        ~cmp:(fun x y -> Cstruct.compare (Cstruct.of_bigarray x) (Cstruct.of_bigarray y))
        b
    );
  add_test ~name:"of_bigarray" [buffer; option int; option int] (fun b off len ->
      match Cstruct.of_bigarray b ?off ?len with
      | c -> check (Cstruct.length c <= Bigarray.Array1.dim b)
      | exception Invalid_argument _ -> ()
    );
  add_test ~name:"get_char" [cstruct; int] (fun c off ->
      let in_range = off >= 0 && off < Cstruct.length c in
      match Cstruct.get_char c off with
      | _ -> check in_range
      | exception Invalid_argument _ -> check (not in_range)
    );
  add_test ~name:"set_char" [cstruct; int] (fun c off ->
      let in_range = off >= 0 && off < Cstruct.length c in
      match Cstruct.set_char c off 'x' with
      | () -> check in_range
      | exception Invalid_argument _ -> check (not in_range)
    );
  add_test ~name:"sub" [cstruct; int; int] (fun base off len ->
      match Cstruct.sub base off len with
      | sub ->
        check_within ~base sub;
        check (Cstruct.length sub = len)
      | exception Invalid_argument _ ->
        check (off < 0 || len < 0 || off + len < 0 || off + len > Cstruct.length base)
    );
  add_test ~name:"shift" [cstruct; int] (fun base off ->
      match Cstruct.shift base off with
      | sub ->
        check_within ~base sub;
        check (Cstruct.length sub = max (Cstruct.length base - off) 0);
      | exception Invalid_argument _ -> check (off < 0 || off > Cstruct.length base)
    );
  add_test ~name:"shiftv" [list cstruct; int] (fun ts n ->
      match Cstruct.shiftv ts n with
      | exception Invalid_argument _ -> check (n < 0 || n > Cstruct.lengthv ts)
      | ts' ->
        assert (Cstruct.equal (Cstruct.concat ts') (Cstruct.shift (Cstruct.concat ts) n));
        assert ((Cstruct.lengthv ts = n) = (ts' = []));
        match ts' with
        | hd :: _ -> assert (not (Cstruct.is_empty hd))
        | [] -> ()
    );
  add_test ~name:"copy" [cstruct; int; int] (fun base off len ->
      match Cstruct.copy base off len with
      | x ->
        check (String.length x = len);
        check (String.equal x (Cstruct.sub base off len |> Cstruct.to_string))
      | exception Invalid_argument _ ->
        check (off < 0 || len < 0 || off + len < 0 || off + len > Cstruct.length base)
    );
  add_test ~name:"blit_from_bytes" [bytes; int; cstruct; int; int] (fun src srcoff dst dstoff len ->
      match Cstruct.blit_from_bytes src srcoff dst dstoff len with
      | () -> check (Cstruct.equal (Cstruct.sub (Cstruct.of_bytes src) srcoff len)
                                   (Cstruct.sub dst dstoff len))
      | exception Invalid_argument _ ->
        check (srcoff < 0 || srcoff > Bytes.length src ||
               dstoff < 0 || dstoff > Bytes.length src ||
               len < 0 ||
               len > Bytes.length src - srcoff ||
               len > Cstruct.length dst - dstoff)
    );
  add_test ~name:"blit_to_bytes" [cstruct; int; bytes; int; int] (fun src srcoff dst dstoff len ->
      match Cstruct.blit_to_bytes src srcoff dst dstoff len with
      | () -> check (Cstruct.equal (Cstruct.sub src srcoff len)
                                   (Cstruct.sub (Cstruct.of_bytes dst) dstoff len))
      | exception Invalid_argument _ ->
        check (srcoff < 0 || srcoff > Cstruct.length src ||
               dstoff < 0 || dstoff > Cstruct.length src ||
               len < 0 ||
               len > Cstruct.length src - srcoff ||
               len > Bytes.length dst - dstoff)
    );
  add_test ~name:"memset" [cstruct; int; int] (fun c x i ->
      guard (i >= 0 && i < Cstruct.length c);
      Cstruct.memset c x;
      check (Cstruct.get_uint8 c i = x land 0xff)
     );
  add_test ~name:"split" [cstruct; option int; int] (fun base start len ->
      match Cstruct.split ?start base len  with
      | c1, c2 ->
        check_within ~base c1;
        check_within ~base c2;
        let start = match start with None -> 0 | Some x -> x in
        check (Cstruct.equal (Cstruct.sub base start len) c1);
        check (Cstruct.equal (Cstruct.shift base (start + len)) c2)
      | exception Invalid_argument _ -> ()
    );
  add_test ~name:"BE.set_uint64" [cstruct; int] (fun c off ->
      let in_range = off >= 0 && off < Cstruct.length c - 7 in
      match Cstruct.BE.set_uint64 c off 42L with
      | () -> check in_range
      | exception Invalid_argument _ -> check (not in_range)
    );
  add_test ~name:"lengthv" [list cstruct] (fun cs ->
      check (Cstruct.lengthv cs >= 0)
    );
  add_test ~name:"copyv" [list cstruct] (fun cs ->
      check (String.equal (Cstruct.copyv cs) (Cstruct.concat cs |> Cstruct.to_string))
    );
  add_test ~name:"fillv" [list cstruct; cstruct] (fun src dst ->
      let copied, rest = Cstruct.fillv ~src ~dst in
      check (copied + Cstruct.lengthv rest = Cstruct.lengthv src);
      (* OCaml tends to underestimate how much space bigarrays are using: *)
      Gc.minor ()
    );
  add_test ~name:"concat" [list cstruct] (fun cs ->
      let x = Cstruct.concat cs in
      check (Cstruct.length x = Cstruct.lengthv cs)
    );
  add_test ~name:"span" [ cstruct; list char ] (fun cs p ->
      let sat chr = List.exists ((=) chr) p in
      let a, b = Cstruct.span ~sat cs in
      let r = Cstruct.concat [ a; b ] in
      check (Cstruct.to_string r = Cstruct.to_string cs));
  add_test ~name:"cut" [ cstruct; cstruct; ] (fun buf sep ->
      guard (Cstruct.length sep > 0);
      ( match Cstruct.cut ~sep buf with
        | Some (l, r) ->
          let r = Cstruct.concat [ l; sep; r; ] in
          check (Cstruct.to_string r = Cstruct.to_string buf)
        | None -> () ));
  add_test ~name:"cuts" [ cstruct; cstruct; ] (fun buf sep ->
      guard (Cstruct.length sep > 0);
      let lst = Cstruct.cuts ~sep buf in
      let lst = List.map Cstruct.to_string lst in
      let res = String.concat (Cstruct.to_string sep) lst in
      check (res = Cstruct.to_string buf));
