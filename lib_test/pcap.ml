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

let num_packets = ref 0

let mac_to_string buf =
  let i n = Cstruct.get_uint8 buf n in
  Printf.sprintf "%.2x:%.2x:%.2x:%.2x:%.2x:%.2x"
    (i 0) (i 1) (i 2) (i 3) (i 4) (i 5)

let printf fmt =
  Printf.kprintf (fun _ -> ()) fmt

let print_packet p =
  let dst_mac = mac_to_string (get_ethernet_dst p) in
  let src_mac = mac_to_string (get_ethernet_src p) in
  let ethertype = get_ethernet_ethertype p in
  printf "ether %s -> %s etype %x\n" src_mac dst_mac ethertype;
  match ethertype with
  |0x0800 -> begin
     let ip = Cstruct.shift p sizeof_ethernet in
     let version = get_ipv4_hlen_version ip lsr 4 in
     let hlen = (get_ipv4_hlen_version ip land 0xf) * 4 in
     let ttl = get_ipv4_ttl ip in
     let proto = get_ipv4_proto ip in
     printf "ipv%d hlen %d ttl %d proto %d\n" version hlen ttl proto;
     match proto with
     |6 -> begin (* tcp *)
       let tcp = Cstruct.shift ip sizeof_ipv4 in
       let off = 0 in
       let x = get_tcpv4_offset_flags tcp in
       let data_offset = (x lsr 12) * 4 in
       let options =
         match data_offset - sizeof_tcpv4 with
         |0 -> 0
         |n -> n (* TODO parse *)
       in
       let payload = Cstruct.shift tcp data_offset in
       let fin = (x land 1) = 1 in
       let syn = (x land 2) = 2 in
       let flags = "?" in
       let src_port = get_tcpv4_src_port tcp in
       let dst_port = get_tcpv4_dst_port tcp in
       let seqnum = get_tcpv4_seqnum tcp in
       let acknum = get_tcpv4_acknum tcp in
       let window = get_tcpv4_window tcp in
       printf "tcpv4 port %d->%d seq %lu ack %lu win %d off %d flags %s opt %d fin %b syn %b payload_len=%d\n"
         src_port dst_port seqnum
         acknum window off flags options fin syn (Cstruct.len payload);
       ()
     end
     |_ -> printf "unknown ip proto %d\n" proto
  end
  |_ -> printf "unknown body\n"

let rec print_pcap_packet (hdr,pkt) =
  let ts_sec = get_pcap_packet_ts_sec hdr in
  let ts_usec = get_pcap_packet_ts_usec hdr in
  let incl_len = get_pcap_packet_incl_len hdr in
  let orig_len = get_pcap_packet_orig_len hdr in
  printf "\n** %lu.%lu  bytes %lu (of %lu)\n"
    ts_sec ts_usec incl_len orig_len;
  print_packet pkt

let print_pcap_header buf =
  let magic = get_pcap_header_magic_number buf in
  let endian =
    match magic with
    |0xa1b2c3d4l -> "bigendian"
    |0xd4c3b2a1l -> "littlendian"
    |_ -> "not a pcap file"
  in
  let version_major = get_pcap_header_version_major buf in
  let version_minor = get_pcap_header_version_minor buf in
  let thiszone = get_pcap_header_thiszone buf in
  let sigfis = get_pcap_header_sigfigs buf in
  let snaplen = get_pcap_header_snaplen buf in
  let header_network = get_pcap_header_network buf in
  printf "pcap_header (len %d)\n" sizeof_pcap_header;
  printf "magic_number %lx (%s)\n%!" magic endian;
  printf "version %d %d\n" version_major version_minor;
  printf "timezone shift %lu\n" thiszone;
  printf "timestamp accuracy %lu\n" sigfis;
  printf "snaplen %lu\n" snaplen;
  printf "lltype %lx\n" header_network

let parse () =
  printf "start parse\n%!";
  let fd = Unix.(openfile "http.cap" [O_RDONLY] 0) in
  let t = Unix_cstruct.of_fd fd in
  printf "total pcap file length %d\n%!" (Cstruct.len t);

  let header, body = Cstruct.split t sizeof_pcap_header in
  print_pcap_header header;

  let packets = Cstruct.iter
    (fun buf -> Some (sizeof_pcap_packet + Int32.to_int (get_pcap_packet_incl_len buf)))
    (fun buf -> buf, Cstruct.shift buf sizeof_pcap_packet)
    body
  in
  let num_packets = Cstruct.fold
    (fun a packet -> print_pcap_packet packet; (a+1))
    packets 0
  in
  printf "num_packets %d\n" num_packets

let _ = parse ()
