# cstruct-async is deprecated

The `cstruct-async` package no longer installs an `Async_cstruct` library.
It remains only as an empty transitional package so that existing opam
installations do not break, and it will be removed in a future release.

## Migration

`Async_cstruct` was just a thin adapter between `Cstruct.t` and
`Bigsubstring.t`.  Drop the `cstruct-async` dependency and use `Cstruct`
directly alongside `Async`.  The entire old library is below, for your
convenience:

```ocaml
open Core
open Async

let to_bigsubstring (t : Cstruct.t) =
  Bigsubstring.create ~pos:t.Cstruct.off ~len:t.Cstruct.len t.Cstruct.buffer

let of_bigsubstring t =
  Cstruct.of_bigarray
    ~off:(Bigsubstring.pos t)
    ~len:(Bigsubstring.length t)
    (Bigsubstring.base t)

let read rd t = Reader.read_bigsubstring rd (to_bigsubstring t)

let schedule_write wr (t : Cstruct.t) =
  Writer.schedule_bigstring ~pos:t.Cstruct.off ~len:t.Cstruct.len wr t.Cstruct.buffer

module Pipe = struct
  let map_string rd wr =
    let rd = Pipe.map rd ~f:Cstruct.to_string in
    let rd', wr' = Pipe.create () in
    don't_wait_for (Pipe.transfer rd' wr ~f:Cstruct.of_string);
    (rd, wr')

  let map_bigsubstring rd wr =
    let rd = Pipe.map rd ~f:to_bigsubstring in
    let rd', wr' = Pipe.create () in
    don't_wait_for (Pipe.transfer rd' wr ~f:of_bigsubstring);
    (rd, wr')
end
```

The two conversion functions are zero-copy views over the same bigarray, so a
`Bigsubstring.t` obtained from a `Cstruct.t` aliases it in both directions.
