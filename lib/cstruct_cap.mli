type 'a rd = < rd: unit; .. > as 'a
type 'a wr = < wr: unit; .. > as 'a

type 'a t
type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type rdwr =  < rd: unit; wr: unit; >
type ro = < rd: unit; >
type wo = < wr: unit; >

type uint8 = int
type uint16 = int
type uint32 = int32
type uint64 = int64

val create : int -> rdwr t
val create_unsafe : int -> rdwr t

val ro : 'a rd t -> ro t
val wo : 'a wr t -> wo t

val of_string : ?off:int -> ?len:int -> string -> rdwr t
val of_bytes : ?off:int -> ?len:int -> bytes -> rdwr t
val of_hex : string -> rdwr t

val to_bigarray : 'a t -> buffer

val equal : 'a rd t -> 'b rd t -> bool
val compare : 'a rd t -> 'b rd t -> int

val check_bounds : 'a rd t -> int -> bool
val check_alignment : 'a rd t -> int -> bool

val get_char : 'a rd t -> int -> char
val get_uint8 : 'a rd t -> int -> uint8
val set_char : 'a wr t -> int -> char -> unit
val set_uint8 : 'a wr t -> int -> uint8 -> unit

val sub : 'a rd t -> off:int -> len:int -> 'a rd t
val shift : 'a rd t -> int -> 'a rd t
val to_string : ?off:int -> ?len:int -> 'a rd t -> string
val to_bytes : ?off:int -> ?len:int -> 'a rd t -> bytes

val blit : 'a rd t -> src_off:int -> 'b wr t -> dst_off:int -> len:int -> unit
val blit_from_string : string -> src_off:int -> 'a wr t -> dst_off:int -> len:int -> unit
val blit_from_bytes : bytes -> src_off:int -> 'a wr t -> dst_off:int -> len:int -> unit
val blit_to_bytes : 'a rd t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit

val memset : 'a wr t -> int -> unit

val length : 'a rd t -> int

val split : ?start:int -> 'a t -> int -> 'a t * 'a t

val pp : Format.formatter -> 'a rd t -> unit

module BE : sig
  val get_uint16 : 'a rd t -> int -> uint16
  val get_uint32 : 'a rd t -> int -> uint32
  val get_uint64 : 'a rd t -> int -> uint64
  val set_uint16 : 'a wr t -> int -> uint16 -> unit
  val set_uint32 : 'a wr t -> int -> uint32 -> unit
  val set_uint64 : 'a wr t -> int -> uint64 -> unit
end

module LE : sig
  val get_uint16 : 'a rd t -> int -> uint16
  val get_uint32 : 'a rd t -> int -> uint32
  val get_uint64 : 'a rd t -> int -> uint64
  val set_uint16 : 'a wr t -> int -> uint16 -> unit
  val set_uint32 : 'a wr t -> int -> uint32 -> unit
  val set_uint64 : 'a wr t -> int -> uint64 -> unit
end

val lenv : 'a rd t list -> int
val copyv : 'a rd t list -> string
val fillv : src:'a rd t list -> dst:'b wr t -> int * 'a rd t list

type 'a iter = unit -> 'a option

val iter : ('a rd t -> int option) -> ('a rd t -> 'v) -> 'a rd t -> 'v iter
val fold : ('acc -> 'x -> 'acc) -> 'x iter -> 'acc -> 'acc
val append : 'a rd t -> 'b rd t -> rdwr t
val concat : 'a rd t list -> rdwr t
val rev : 'a rd t -> rdwr t
