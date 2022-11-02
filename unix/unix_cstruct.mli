(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2022 David Scott <dave@recoil.org>
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

(** Unix functions that operate on Cstruct buffers. *)

val of_fd : Unix.file_descr -> Cstruct.t
(** [of_fd fd] memory maps the [fd] and returns a cstruct *)

val read: Unix.file_descr -> Cstruct.t -> int
(** [read fd cs] reads from the file descriptor into the buffer, returning the number of bytes read.
    Similar to Unix.read. *)

val write: Unix.file_descr -> Cstruct.t -> unit
(** [write fd cs] writes the whole Cstruct to the file descriptor.
    Similar to Unix.write. *)

val writev: Unix.file_descr -> Cstruct.t list -> unit
(** [writev fd cs] writes the whole list of Cstructs to the file descriptor.
    Similar to Unix.write. *)

val send: Unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> int
(** [send fd c] Like Unix.send, but for Cstruct.
    If only a partial send is possible, the return argument is how many bytes were sent. *)

val recv: Unix.file_descr -> Cstruct.t -> int
(** [recv fd c] receives a message from a socket. This can be used to receive a datagram.
    If only a partial receive is possible, the return argument is now many bytes were received. *)

val recvfrom: Unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> int * Unix.sockaddr
(** [recvfrom fd c] Like Unix.recvfrom, but for Cstruct. *)

val sendto: Unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> Unix.sockaddr -> int
(** [sendto fd c a] Like Unix.sendto, but for Cstruct. *)
