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
    Like {! Unix.read}, but for Cstruct. *)

val write: Unix.file_descr -> Cstruct.t -> unit
(** [write fd cs] writes the whole Cstruct to the file descriptor. Like {! Unix.write}, but for Cstruct. *)

val writev: Unix.file_descr -> Cstruct.t list -> unit
(** [writev fd cs] writes the whole list of Cstructs to the file descriptor.
    Like {! Unix.write}, but for a list of Cstructs. *)

val send: Unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> int
(** [send fd c flags] sends the Cstruct to the file descriptor, returning the number of bytes written.
    Like {! Unix.send}, but for Cstruct. *)

val recv: Unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> int
(** [recv fd c flags] receives up to a Cstruct's worth of data from the file descriptor, returning the number of bytes written.
    Like {! Unix.recv}, but for Cstruct. *)

val recvfrom: Unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> int * Unix.sockaddr
(** [recvfrom fd c flags] receives up to a Cstruct's worth of data from the file descriptor, returning the number of bytes
    read and the address they were sent from. Like {! Unix.recvfrom}, but for Cstruct. *)

val sendto: Unix.file_descr -> Cstruct.t -> Unix.msg_flag list -> Unix.sockaddr -> int
(** [sendto fd c flags addr] sends a Cstruct's worth of data to the address via the file descriptor.
    Like {! Unix.sendto}, but for Cstruct. *)
