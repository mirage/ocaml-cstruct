include (Cstruct_core : module type of Cstruct_core with type t := Cstruct_core.t)

type 'a rd = < rd: unit; .. > as 'a
type 'a wr = < wr: unit; .. > as 'a
type 'a aligned = < aligned: unit; .. > as 'a

type 'a t = Cstruct_core.t

type rdwr =  < rd: unit; wr: unit; >
type ro = < rd: unit; >
type wo = < wr: unit; >

external ro : 'a rd t -> ro t = "%identity"
external wo : 'a wr t -> wo t = "%identity"
