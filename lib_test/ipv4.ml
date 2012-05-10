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
} as little_endian

cstruct pcap_packet {
  uint32_t ts_sec;         (* timestamp seconds *)
  uint32_t ts_usec;        (* timestamp microseconds *)
  uint32_t incl_len;       (* number of octets of packet saved in file *)
  uint32_t orig_len        (* actual length of packet *)
} as little_endian

cstruct ethernet {
  uint8_t        dst[6];
  uint8_t        src[6];
  uint16_t       ethertype
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
  uint8_t        src[4];
  uint8_t        dst[4]
} as big_endian

cstruct tcpv4 {
  uint16_t       src_port;
  uint16_t       dst_port;
  uint32_t       seqnum;
  uint32_t       acknum;
  uint16_t       offset_flags;
  uint16_t       window;
  uint16_t       checksum;
  uint16_t       urg
} as big_endian

open Lwt
open Printf

let num_packets = ref 0

let mac_to_string buf =
  let i n = Cstruct.BE.get_uint8 buf n in
  sprintf "%.2x:%.2x:%.2x:%.2x:%.2x:%.2x"
    (i 0) (i 1) (i 2) (i 3) (i 4) (i 5)

let print_packet p =
  let dst_mac = mac_to_string (get_ethernet_dst p) in
  let src_mac = mac_to_string (get_ethernet_src p) in
  let ethertype = get_ethernet_ethertype p in
  printf "ether %s -> %s etype %x\n" src_mac dst_mac ethertype;
  match ethertype with
  |0x0800 -> begin
     let _,ip = Cstruct.split p sizeof_ethernet in
     let version = get_ipv4_hlen_version ip lsr 4 in
     let hlen = (get_ipv4_hlen_version ip land 0xf) * 4 in
     let ttl = get_ipv4_ttl ip in
     let proto = get_ipv4_proto ip in
     printf "ipv%d hlen %d ttl %d proto %d\n" version hlen ttl proto;
     match proto with 
     |6 -> begin (* tcp *)
       let _,tcp = Cstruct.split ip sizeof_ipv4 in
       let off = 0 in
       let flags = "?" in
       printf "tcpv4 port %d->%d seq %lu ack %lu win %d off %d flags %s\n"
         (get_tcpv4_src_port tcp) (get_tcpv4_dst_port tcp) (get_tcpv4_seqnum tcp)
         (get_tcpv4_acknum tcp) (get_tcpv4_window tcp) off flags;
     end
     |_ -> printf "unknown ip proto %d\n" proto
  end
  |_ -> printf "unknown body\n"
 
let rec print_pcap_packet buf =
  incr num_packets;
  let hdr, rest = Cstruct.split buf sizeof_pcap_packet in
  printf "\n** %lu.%lu  bytes %lu (of %lu)\n" 
    (get_pcap_packet_ts_sec hdr)
    (get_pcap_packet_ts_usec hdr)
    (get_pcap_packet_incl_len hdr)
    (get_pcap_packet_orig_len hdr);
  let plen = Int32.to_int (get_pcap_packet_incl_len hdr)  in
  let body, rest = Cstruct.split rest plen in
  print_packet body;
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
