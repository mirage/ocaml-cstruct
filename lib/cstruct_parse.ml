(* (c) 2016 Daniel C. Bünzli
   (c) 2020 Romain Calascibetta *)

open Cstruct

let empty = Cstruct.create 0

external unsafe_blit_string_to_bigstring
  : string -> int -> buffer -> int -> int -> unit
  = "caml_blit_string_to_bigstring"
[@@noalloc]

let get { buffer; off; len; } zidx =
  if zidx < 0 || zidx >= len then invalid_arg "index out of bounds" ;
  Bigarray_compat.Array1.get buffer (off + zidx)

let get_byte { buffer; off; len; } zidx =
  if zidx < 0 || zidx >= len then invalid_arg "index out of bounds" ;
  Char.code (Bigarray_compat.Array1.get buffer (off + zidx))

let string ?(off= 0) ?len str =
  let str_len = String.length str in
  let len = match len with None -> str_len | Some len -> len in
  if off < 0 || len < 0 || off + len > str_len then invalid_arg "index out of bounds" ;
  let buffer = Bigarray_compat.(Array1.create char c_layout str_len) in
  unsafe_blit_string_to_bigstring str 0 buffer 0 str_len ;
  Cstruct.of_bigarray ~off ~len buffer

let buffer ?(off= 0) ?len buffer =
  let buffer_len = Bigarray_compat.Array1.dim buffer in
  let len = match len with None -> buffer_len - off | Some len -> len in
  if off < 0 || len < 0 || off + len > buffer_len then invalid_arg "index out of bounds" ;
  Cstruct.of_bigarray ~off ~len buffer

let length cs = Cstruct.len cs
let start_pos { off; _ } = off
let stop_pos { off; len; _ } = off + len

let head ?(rev= false) ({ len; _ } as cs) =
  if len = 0 then None
  else Some (Cstruct.get_char cs (if rev then len - 1 else 0))

let tail ?(rev= false) ({ buffer; off; len; } as cs) =
  if len = 0 then cs
  else if rev then Cstruct.of_bigarray ~off ~len:(len - 2) buffer
  else Cstruct.of_bigarray ~off:(off + 1) ~len:(len - 1) buffer

let is_empty { len; _ } = len = 0

let is_prefix ~affix:({ len= alen; _ } as affix)
    ({ len; _ } as cs) =
  if alen > len then false
  else
    let max_zidx = alen - 1 in
    let rec loop i =
      if i > max_zidx then true
      else if get_char affix i <> get_char cs i
      then false else loop (succ i) in
    loop 0

let is_infix ~affix:({ len= alen; _ } as affix)
    ({ len; _ } as cs) =
  if alen > len then false
  else
    let max_zidx_a = alen - 1 in
    let max_zidx_s = len - alen in
    let rec loop i k =
      if i > max_zidx_s then false
      else if k > max_zidx_a then true
      else if k > 0 then
        if get_char affix k = get_char cs (i + k)
        then loop i (succ k)
        else loop (succ i) 0
      else if get_char affix 0 = get_char cs i
      then loop i 1
      else loop (succ i) 0 in
    loop 0 0

let is_suffix ~affix:({ len= alen; _ } as affix)
    ({ len; _ } as cs) =
  if alen > len then false
  else
    let max_zidx = alen - 1 in
    let max_zidx_a = alen - 1 in
    let max_zidx_s = len - 1 in
    let rec loop i =
      if i > max_zidx then true
      else if get_char affix (max_zidx_a - i) <> get_char cs (max_zidx_s - i)
      then false else loop (succ i) in
    loop 0

let for_all sat cs =
  let rec go acc i =
    if i < len cs
    then go (sat (get_char cs i) && acc) (succ i)
    else acc in
  go true 0

let exists sat cs =
  let rec go acc i =
    if i < Cstruct.len cs
    then go (sat (get_char cs i) || acc) (succ i)
    else acc in
  go false 0

let start { buffer; off; _ } =
  Cstruct.of_bigarray buffer ~off ~len:0

let stop { buffer; off; len; } =
  Cstruct.of_bigarray buffer ~off:(off + len) ~len:0

let is_white = function ' ' | '\t' .. '\r' -> true | _ -> false

let trim ?(drop = is_white) ({ buffer; off; len; } as cs) =
  if len = 0 then cs
  else
    let max_zpos = len in
    let max_zidx = len - 1 in
    let rec left_pos i =
      if i > max_zidx then max_zpos
      else if drop (get_char cs i) then left_pos (succ i) else i in
    let rec right_pos i =
      if i < 0 then 0
      else if drop (get_char cs i) then right_pos (pred i) else succ i in
    let left = left_pos 0 in
    if left = max_zpos
    then Cstruct.of_bigarray buffer ~off:((off * 2 + len) / 2) ~len:0
    else
      let right = right_pos max_zidx in
      if left = 0 && right = max_zpos then cs
      else Cstruct.of_bigarray buffer ~off:(off + left) ~len:(right - left)

let fspan ~min ~max ~sat ({ buffer= v; off; len; } as cs) =
  if min < 0 then invalid_arg "span: negative min" ;
  if max < 0 then invalid_arg "span: negative max" ;
  if min > max || max = 0 then (buffer ~off:off ~len:0 v, cs)
  else
    let max_zidx = len - 1 in
    let max_zidx =
      let k = max - 1 in
      if k > max_zidx || k < 0 then max_zidx else k in
    let need_zidx = min in
    let rec loop i =
      if i <= max_zidx && sat (Cstruct.get_char cs i) then loop (i + 1)
      else if i < need_zidx || i = 0 then buffer ~off:off ~len:0 v, cs
      else if i = len then (cs, buffer ~off:(off + len) ~len:0 v)
      else buffer ~off:off ~len:i v, buffer ~off:(off + i) ~len:(len - i) v in
    loop 0

let rspan ~min ~max ~sat ({ buffer= v; off; len; } as cs) =
  if min < 0 then invalid_arg "span: negative min" ;
  if max < 0 then invalid_arg "span: negative max" ;
  if min > max || max = 0 then (cs, buffer ~off:(off + len) ~len:0 v)
  else
    let max_zidx = len - 1 in
    let min_zidx =
      let k = len - max in if k < 0 then 0 else k in
    let need_zidx = len - min - 1 in
    let rec loop i =
      if i >= min_zidx && sat (Cstruct.get_char cs i) then loop (i - 1)
      else if i > need_zidx || i = max_zidx then (cs, buffer ~off:(off + len) ~len:0 v)
      else if i < 0 then (buffer ~off:off ~len:0 v, cs)
      else (buffer ~off:off ~len:(i + 1) v, buffer ~off:(off + i + 1) ~len:(len - (i + 1)) v) in
    loop max_zidx

let span ?(rev= false) ?(min= 0) ?(max= max_int) ?(sat= fun _ -> true) cs =
  match rev with
  | true  -> rspan ~min ~max ~sat cs
  | false -> fspan ~min ~max ~sat cs

let take ?(rev= false) ?min ?max ?sat cs =
  (if rev then snd else fst) @@ span ~rev ?min ?max ?sat cs

let drop ?(rev= false) ?min ?max ?sat cs =
  (if rev then fst else snd) @@ span ~rev ?min ?max ?sat cs

let fcut ~sep:({ Cstruct.len= sep_len; _ } as sep)
    ({ Cstruct.buffer= v; off; len; } as cs) =
  if sep_len = 0 then invalid_arg "cut: empty separator" ;
  let max_sep_zidx = sep_len - 1 in
  let max_s_zidx = len - sep_len in
  let rec check_sep i k =
    if k > max_sep_zidx
    then Some (buffer ~off:off ~len:i v,
               buffer ~off:(off + i + sep_len) ~len:(len - i - sep_len) v)
    else if Cstruct.get_char cs (i + k) = Cstruct.get_char sep k
    then check_sep i (k + 1)
    else scan (i + 1)
  and scan i =
    if i > max_s_zidx then None
    else if Cstruct.get_char cs i = Cstruct.get_char sep 0
    then check_sep i 1
    else scan (i + 1) in
  scan 0

let rcut ~sep:({ Cstruct.len= sep_len; _ } as sep) ({ Cstruct.buffer= v; off; len; } as cs) =
  if sep_len = 0 then invalid_arg "cut: empty separator" ;
  let max_sep_zidx = sep_len - 1 in
  let max_s_zidx = len - 1 in
  let rec check_sep i k =
    if k > max_sep_zidx then Some (buffer ~off:off ~len:i v,
                                   buffer ~off:(off + i + sep_len) ~len:(len - i - sep_len) v)
    else if Cstruct.get_char cs (i + k) = Cstruct.get_char sep k
    then check_sep i (k + 1)
    else rscan (i - 1)
  and rscan i =
    if i < 0 then None
    else if Cstruct.get_char cs i = Cstruct.get_char sep 0
    then check_sep i 1
    else rscan (i - 1) in
  rscan (max_s_zidx - max_sep_zidx)

let cut ?(rev= false) ~sep cs = match rev with
  | true  -> rcut ~sep cs
  | false -> fcut ~sep cs

let add_sub ~no_empty buf ~off ~len acc =
  if len = 0
  then ( if no_empty then acc else buffer ~off ~len buf :: acc )
  else buffer ~off ~len buf :: acc

let fcuts ~no_empty ~sep:({ Cstruct.len= sep_len; _ } as sep)
      ({ Cstruct.buffer; off; len; } as cs) =
  if sep_len = 0 then invalid_arg "cuts: empty separator" ;
  let max_sep_zidx = sep_len - 1 in
  let max_s_zidx = len - sep_len in
  let rec check_sep zanchor i k acc =
    if k > max_sep_zidx
    then
      let new_start = i + sep_len in
      scan new_start new_start (add_sub ~no_empty buffer ~off:(off + zanchor) ~len:(i - zanchor) acc)
    else
      if Cstruct.get_char cs (i + k) = Cstruct.get_char sep k
      then check_sep zanchor i (k + 1) acc
      else scan zanchor (i + 1) acc
  and scan zanchor i acc =
    if i > max_s_zidx
    then
      if zanchor = 0 then (if no_empty && len = 0 then [] else [ cs ])
      else List.rev (add_sub ~no_empty buffer ~off:(off + zanchor) ~len:(len - zanchor) acc)
    else
      if Cstruct.get_char cs i = Cstruct.get_char sep 0
      then check_sep zanchor i 1 acc
      else scan zanchor (i + 1) acc in
  scan 0 0 []

let rcuts ~no_empty ~sep:({ Cstruct.len= sep_len; _ } as sep)
      ({ Cstruct.buffer; len; _ } as cs) =
  if sep_len = 0 then invalid_arg "cuts: empty separator" ;
  let s_len = len in
  let max_sep_zidx = sep_len - 1 in
  let max_s_zidx = len - 1 in
  let rec check_sep zanchor i k acc =
    if k > max_sep_zidx
    then let off = i + sep_len in
         rscan i (i - sep_len) (add_sub ~no_empty buffer ~off ~len:(zanchor - off) acc)
    else
      if Cstruct.get_char cs (i + k) = Cstruct.get_char cs k
      then check_sep zanchor i (k + 1) acc
      else rscan zanchor (i - 1) acc
  and rscan zanchor i acc =
    if i < 0 then
      if zanchor = s_len then ( if no_empty && s_len = 0 then [] else [ cs ])
      else add_sub ~no_empty buffer ~off:0 ~len:zanchor acc
    else
      if Cstruct.get_char cs i = Cstruct.get_char sep 0
      then check_sep zanchor i 1 acc
      else rscan zanchor (i - 1) acc in
  rscan s_len (max_s_zidx - max_sep_zidx) []

let cuts ?(rev= false) ?(empty= true) ~sep cs = match rev with
  | true  -> rcuts ~no_empty:(not empty) ~sep cs
  | false -> fcuts ~no_empty:(not empty) ~sep cs

let fields ?(empty= false) ?(is_sep= is_white) ({ Cstruct.buffer; off; len; } as cs) =
  let no_empty = not empty in
  let max_pos = len in
  let rec loop i end_pos acc =
    if i < 0 then begin
        if end_pos = len
        then ( if no_empty && len = 0 then [] else [ cs ])
        else add_sub ~no_empty buffer ~off:off ~len:(end_pos - (i + 1)) acc
      end else begin
        if not (is_sep (Cstruct.get_char cs i))
        then loop (i - 1) end_pos acc
        else loop (i - 1) i (add_sub ~no_empty buffer ~off:(off + i + 1) ~len:(end_pos - (i + 1)) acc)
      end in
  loop (max_pos - 1) max_pos []

let ffind sat ({ Cstruct.buffer= v; len; _ } as cs) =
  let max_idx = len - 1 in
  let rec loop i =
    if i > max_idx then None
    else if sat (Cstruct.get_char cs i)
    then Some (buffer ~off:i ~len:1 v)
    else loop (i + 1) in
  loop 0

let rfind sat ({ Cstruct.buffer= v; len; _ } as cs) =
  let rec loop i =
    if i < 0 then None
    else if sat (Cstruct.get_char cs i)
    then Some (buffer ~off:i ~len:1 v)
    else loop (i - 1) in
  loop (len - 1)

let find ?(rev= false) sat cs = match rev with
  | true  -> rfind sat cs
  | false -> ffind sat cs

let ffind_sub ~sub:({ Cstruct.len= sub_len; _ } as sub) ({ Cstruct.buffer= v; off; len; } as cs) =
  if sub_len > len then None
  else
    let max_zidx_sub = sub_len - 1 in
    let max_zidx_s = len - sub_len in
    let rec loop i k =
      if i > max_zidx_s then None
      else if k > max_zidx_sub then Some (buffer v ~off:(off + i) ~len:sub_len)
      else if k > 0
      then ( if Cstruct.get_char sub k = Cstruct.get_char cs (i + k)
             then loop i (k + 1)
             else loop (i + 1) 0 )
      else if Cstruct.get_char sub 0 = Cstruct.get_char cs i
      then loop i 1
      else loop (i + 1) 0 in
    loop 0 0

let rfind_sub ~sub:({ Cstruct.len= sub_len; _ } as sub) ({ Cstruct.buffer= v; len; _ } as cs) =
  if sub_len > len then None
  else
    let max_zidx_sub = sub_len - 1 in
    let rec loop i k =
      if i < 0 then None
      else if k > max_zidx_sub then Some (buffer v ~off:i ~len:sub_len)
      else if k > 0
      then ( if Cstruct.get_char sub k = Cstruct.get_char cs (i + k)
             then loop i (k + 1)
             else loop (i - 1) 0 )
      else if Cstruct.get_char sub 0 = Cstruct.get_char cs i
      then loop i 1
      else loop (i - 1) 0 in
    loop (len - sub_len) 0

let find_sub ?(rev= false) ~sub cs = match rev with
  | true  -> rfind_sub ~sub cs
  | false -> ffind_sub ~sub cs

let filter sat ({ Cstruct.len; _ } as cs) =
  if len = 0 then empty
  else
    let b = Cstruct.create len in
    let max_zidx = len - 1 in
    let rec loop b k i =
      if i > max_zidx
      then (if k = len then b else Cstruct.sub b 0 k)
      else
        let chr = Cstruct.get_char cs i in
        if sat chr then ( Cstruct.set_char b k chr ; loop b (k + 1) (i + 1))
        else loop b k (i + 1) in
    loop b 0 0

let filter_map f ({ Cstruct.len; _ } as cs) =
  if len = 0 then empty
  else
    let b = Cstruct.create len in
    let max_zidx = len - 1 in
    let rec loop b k i =
      if i > max_zidx
      then (if k = len then b else Cstruct.sub b 0 k)
      else match f (Cstruct.get_char cs i) with
           | Some chr ->
              Cstruct.set_char b i chr ;
              loop b (k + 1) (i + 1)
           | None ->
              loop b k (i + 1) in
    loop b 0 0

let map f ({ Cstruct.len; _ } as cs) =
  if len = 0 then empty
  else
    let b = Cstruct.create len in
    for i = 0 to len - 1 do
      Cstruct.set_char b i (f (Cstruct.get_char cs i))
    done ; b

let mapi f ({ Cstruct.len; _ } as cs) =
  if len = 0 then empty
  else
    let b = Cstruct.create len in
    for i = 0 to len - 1 do
      Cstruct.set_char b i (f i (Cstruct.get_char cs i))
    done ; b
