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

let of_fd fd =
  let buffer = Bigarray.(array1_of_genarray (Unix.map_file fd char c_layout false [|-1|])) in
  Cstruct.of_bigarray buffer

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* Returns 0 if there is no writev *)
external stub_iov_max: unit -> int = "stub_cstruct_iov_max"

external stub_write: Unix.file_descr -> (buffer * int * int) -> int = "stub_cstruct_write"

external stub_writev: Unix.file_descr -> (buffer * int * int) list -> int = "stub_cstruct_writev"

let iov_max = stub_iov_max ()

(* return the first n fragments, suitable for writev *)
let rec first n rev_acc = function
| [] -> List.rev rev_acc
| _ when n = 0 -> List.rev rev_acc
| x :: xs -> first (n - 1) (x :: rev_acc) xs

(* shift a list of fragments along by n bytes *)
let rec shift t x =
  if x = 0 then t else match t with
  | [] -> invalid_arg "foo"
  | y :: ys ->
    let y' = Cstruct.length y in
    if y' > x
    then Cstruct.shift y x :: ys
    else shift ys (x - y')

let rec write fd buf =
  if Cstruct.length buf > 0 then begin
    let n = stub_write fd (buf.Cstruct.buffer, buf.Cstruct.off, buf.Cstruct.len) in
    write fd @@ Cstruct.shift buf n
  end

let writev fd bufs =
  let rec use_writev = function
    | [] -> ()
    | remaining ->
      (* write at most iov_max at a time *)
      let to_send = first iov_max [] remaining in
      let n = stub_writev fd (List.map (fun x -> x.Cstruct.buffer, x.Cstruct.off, x.Cstruct.len) to_send) in
      let rest = shift remaining n in
      use_writev rest in
  let use_write_fallback = List.iter (write fd) in
  (if iov_max = 0 then use_write_fallback else use_writev) bufs

external stub_send: Unix.file_descr -> (buffer * int * int) -> int = "stub_cstruct_send"

external stub_recv: Unix.file_descr -> (buffer * int * int) -> int = "stub_cstruct_recv"

let send fd x = stub_send fd (x.Cstruct.buffer, x.Cstruct.off, x.Cstruct.len)

let recv fd x = stub_recv fd (x.Cstruct.buffer, x.Cstruct.off, x.Cstruct.len)

external stub_read: Unix.file_descr -> (buffer * int * int) -> int = "stub_cstruct_read"

let read fd x = stub_read fd (x.Cstruct.buffer, x.Cstruct.off, x.Cstruct.len)

external stub_recvfrom : Unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> int * Unix.sockaddr = "stub_cstruct_recvfrom"

let recvfrom fd x fl = stub_recvfrom fd x fl

external stub_sendto : Unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> Unix.sockaddr -> int = "stub_cstruct_sendto"

let sendto fd x fl a = stub_sendto fd x fl a
