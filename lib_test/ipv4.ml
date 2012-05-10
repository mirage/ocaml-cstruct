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

cstruct pcap_header {
   uint32_t magic_number;   (* magic number *)
   uint16_t version_major;  (* major version number *)
   uint16_t version_minor;  (* minor version number *)
   uint32_t thiszone;       (* GMT to local correction *)
   uint32_t sigfigs;        (* accuracy of timestamps *)
   uint32_t snaplen;        (* max length of captured packets, in octets *)
   uint32_t network         (* data link type *)
} as big_endian


cstruct pcap_packet {
  uint32_t ts_sec;         (* timestamp seconds *)
  uint32_t ts_usec;        (* timestamp microseconds *)
  uint32_t incl_len;       (* number of octets of packet saved in file *)
  uint32_t orig_len        (* actual length of packet *)
} as big_endian

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


open Lwt
open Printf

let num_packets = ref 0

let rec print_pcap_packet buf =
  incr num_packets;
  let hdr, rest = Cstruct.split buf sizeof_pcap_packet in
  printf "** %lu.%lu  bytes %lu (of %lu)\n" 
    (get_pcap_packet_ts_sec hdr)
    (get_pcap_packet_ts_usec hdr)
    (get_pcap_packet_incl_len hdr)
    (get_pcap_packet_orig_len hdr);
  let plen = Int32.to_int (get_pcap_packet_incl_len hdr)  in
  let body, rest = Cstruct.split rest plen in
  (* print_pcap_body body; *)
  if Cstruct.len rest > 0 then print_pcap_packet rest
  
let print_pcap_header buf =
  let magic = get_pcap_header_magic_number buf in
  let endian =
    match magic with
    |0xa1b2c3d4l -> "bigendian"
    |0xd4c3b2a1l -> "littlendian"
    |_ -> "not a pcap file"
  in
  printf "pcap_header (len %d)\n" sizeof_pcap_header;
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
  printf "total pcap file length %d\n" (Cstruct.len buf);
  let header, packets = Cstruct.split buf sizeof_pcap_header in
  print_pcap_header header;
  print_pcap_packet packets;
  printf "num_packets %d\n" !num_packets; 
  return ()

let _ = Lwt_unix.run (parse ())
