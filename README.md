Cstruct is a library and syntax extension to make it easier to access C-like
structures directly from OCaml.  It supports both reading and writing to these
structures, and they are accessed via the `Bigarray` module.

An example pcap description is:

```
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
```

This auto-generates generates functions of the form below in the `ml` file:

```
let sizeof_pcap_packet = 16
let get_pcap_packet_ts_sec v = Cstruct.LE.get_uint32 v 0
let set_pcap_packet_ts_sec v x = Cstruct.LE.set_uint32 v 0 x
let get_pcap_packet_ts_usec v = Cstruct.LE.get_uint32 v 4
let set_pcap_packet_ts_usec v x = Cstruct.LE.set_uint32 v 4 x
let get_pcap_packet_incl_len v = Cstruct.LE.get_uint32 v 8
let set_pcap_packet_incl_len v x = Cstruct.LE.set_uint32 v 8 x
let get_pcap_packet_orig_len v = Cstruct.LE.get_uint32 v 12
let set_pcap_packet_orig_len v x = Cstruct.LE.set_uint32 v 12 x

let sizeof_ethernet = 14
let get_ethernet_dst src = Cstruct.sub src 0 6
let copy_ethernet_dst src = Cstruct.copy src 0 6
let set_ethernet_dst src srcoff dst =
  Cstruct.blit_from_string src srcoff dst 0 6
let blit_ethernet_dst src srcoff dst = Cstruct.blit src srcoff dst 0 6
let get_ethernet_src src = Cstruct.sub src 6 6
let copy_ethernet_src src = Cstruct.copy src 6 6
let set_ethernet_src src srcoff dst =
  Cstruct.blit_from_string src srcoff dst 6 6
let blit_ethernet_src src srcoff dst = Cstruct.blit src srcoff dst 6 6
let get_ethernet_ethertype v = Cstruct.BE.get_uint16 v 12
let set_ethernet_ethertype v x = Cstruct.BE.set_uint16 v 12 x
```

The `mli` file will have signatures of this form:

```
val sizeof_pcap_packet : int
val get_pcap_packet_ts_sec : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_ts_sec : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_ts_usec : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_ts_usec : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_incl_len : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_incl_len : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_orig_len : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_orig_len : Cstruct.t -> Cstruct.uint32 -> unit
val hexdump_pcap_packet_to_buffer : Buffer.t -> pcap_packet -> unit
val hexdump_pcap_packet : Cstruct.t -> unit

val sizeof_ethernet : int
val get_ethernet_dst : Cstruct.t -> Cstruct.t
val copy_ethernet_dst : Cstruct.t -> string
val set_ethernet_dst : string -> int -> Cstruct.t -> unit
val blit_ethernet_dst : Cstruct.t -> int -> Cstruct.t -> unit
val get_ethernet_src : Cstruct.t -> Cstruct.t
val copy_ethernet_src : Cstruct.t -> string
val set_ethernet_src : string -> int -> Cstruct.t -> unit
val blit_ethernet_src : Cstruct.t -> int -> Cstruct.t -> unit
val get_ethernet_ethertype : Cstruct.t -> Cstruct.uint16
val set_ethernet_ethertype : Cstruct.t -> Cstruct.uint16 -> unit
val hexdump_ethernet_to_buffer : Buffer.t -> Cstruct.t -> unit
val hexdump_ethernet : Cstruct.t -> unit
```

The `hexdump` functions above are convenient pretty-printing functions
to help you debug, and aren't intended to be high performance.

You can also declare C-like enums:

```
cenum foo32 {
  ONE32;
  TWO32 = 0xfffffffel;
  THREE32
} as uint32_t

cenum bar16 {
  ONE = 1;
  TWO;
  FOUR = 4;
  FIVE
} as uint16_t
```

This generates signatures of the form:

```
type foo32 = | ONE32 | TWO32 | THREE32
val int_to_foo32 : int32 -> foo32 option
val foo32_to_int : foo32 -> int32
val foo32_to_string : foo32 -> string
val string_to_foo32 : string -> foo32 option
type bar16 = | ONE | TWO | FOUR | FIVE
val int_to_bar16 : int -> bar16 option
val bar16_to_int : bar16 -> int
val bar16_to_string : bar16 -> string
val string_to_bar16 : string -> bar16 option
```

You can also add a `(sexp)` decorator to output s-expression convertors
for use with the `sexplib` library.

```
cenum foo32 {
  ONE32;
  TWO32 = 0xfffffffel;
  THREE32
} as uint32_t(sexp)
```

And `sexp_of_foo64` and `foo64_of_sexp` functions will also be available.
The representation of the Sexp is the string representation of the enum.


Please see the `lib_test/` directory for more in-depth examples.

[![Build Status](https://travis-ci.org/mirage/ocaml-cstruct.png)](https://travis-ci.org/mirage/ocaml-cstruct)
