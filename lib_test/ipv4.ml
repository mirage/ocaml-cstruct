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

cstruct ipv4 {
   uint8_t        hlen_version;
   uint8_t        tos;
   uint16_t       len;
   uint16_t       id;
   uint16_t       off;
   uint8_t        ttl;
   uint8_t        proto;
   uint16_t       csum;
   uint32_t       src;
   uint32_t       dst
} as big_endian

cstruct pcap_header {
   uint32_t magic_number;   (* magic number *)
   uint16_t version_major;  (* major version number *)
   uint16_t version_minor;  (* minor version number *)
   uint32_t thiszone;       (* GMT to local correction *)
   uint32_t sigfigs;        (* accuracy of timestamps *)
   uint32_t snaplen;        (* max length of captured packets, in octets *)
   uint32_t network         (* data link type *)
} as big_endian

open Lwt
open Printf

let print_pcap_header buf =
  let magic = get_pcap_header_magic_number buf in
  let endian =
    match magic with
    |0xa1b2c3d4l -> "bigendian"
    |0xd4c3b2a1l -> "littlendian"
    |_ -> "not a pcap file"
  in
  printf "magic_number %lx (%s)\n%!" magic endian;
  printf "version %d %d\n" 
   (get_pcap_header_version_major buf) (get_pcap_header_version_minor buf);
  printf "timezone shift %lu\n" (get_pcap_header_thiszone buf);
  printf "timestamp accuracy %lu\n" (get_pcap_header_sigfigs buf);
  printf "snaplen %lu\n" (get_pcap_header_snaplen buf);
  printf "lltype %lx\n" (get_pcap_header_network buf)

let parse () =
  lwt lfd = Lwt_unix.(openfile "http.cap" [O_RDONLY] 0) in
  let fd = Lwt_unix.unix_file_descr lfd in
  let buf = Lwt_bytes.map_file ~shared:false ~fd () in
  print_pcap_header buf;
  return ()

let _ = Lwt_unix.run (parse ())
