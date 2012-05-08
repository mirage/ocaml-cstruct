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
}

