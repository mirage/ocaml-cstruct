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

You can also declare C-like enums:

```
cenum foo64 {
  ONE64;
  TWO64;
  THREE64
} as uint64_t
```

Please see the `lib_test/` directory for more in-depth examples.
