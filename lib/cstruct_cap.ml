include (Cstruct_core : module type of Cstruct_core with type t := Cstruct_core.t)

type 'a rd = < rd: unit; .. > as 'a
type 'a wr = < wr: unit; .. > as 'a

type 'a t = Cstruct_core.t

type rdwr =  < rd: unit; wr: unit; >
type ro = < rd: unit; >
type wo = < wr: unit; >

external ro : 'a rd t -> ro t = "%identity"
external wo : 'a wr t -> wo t = "%identity"

let of_string ?off ?len x =
  Cstruct_core.of_string ?off ?len x
let of_bytes ?off ?len x =
  Cstruct_core.of_bytes ?off ?len x

(* XXX(dinosaure): discard [?allocator] arguments (see discusion in #237). *)

let to_string ?(off= 0) ?len t =
  let len = match len with
    | Some len -> len
    | None -> Cstruct_core.len t - off in
  Cstruct_core.copy t off len

let to_bytes ?(off= 0) ?len t =
  let len = match len with
    | Some len -> len
    | None -> Cstruct_core.len t - off in
  (* XXX(dinosaure): this is safe when [copy] allocates itself [bytes]
     and uses [Bytes.unsafe_to_string]. *)
  Bytes.unsafe_of_string (Cstruct_core.copy t off len)

let pp ppf t = Cstruct_core.hexdump_pp ppf t
