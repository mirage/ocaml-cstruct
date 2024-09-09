(* inspect output with [dune describe pp ppx_test/with-sexp-of/ppx_cstruct_and_sexp_of.ml] *)
[%%cenum
type enum =
  | One [@id 1]
  | Three [@id 3]
[@@uint8_t][@@sexp_of]]
