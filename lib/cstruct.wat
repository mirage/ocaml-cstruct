;; cstruct's C primitives for wasm_of_ocaml, as cstruct.js does for js_of_ocaml.
(module
   (import "env" "caml_bigstring_blit_ba_to_ba"
      (func $caml_blit_bigstring_to_bigstring
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_ba_to_bytes"
      (func $caml_blit_bigstring_to_string
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_string_to_ba"
      (func $caml_blit_string_to_bigstring
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_bytes_to_ba"
      (func $caml_blit_bytes_to_bigstring
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_memset"
      (func $caml_fill_bigstring
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "env" "caml_bigstring_memcmp"
      (func $caml_compare_bigstring
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))

   (export "caml_blit_bigstring_to_bigstring"
      (func $caml_blit_bigstring_to_bigstring))
   (export "caml_blit_bigstring_to_string" (func $caml_blit_bigstring_to_string))
   (export "caml_blit_string_to_bigstring" (func $caml_blit_string_to_bigstring))
   (export "caml_blit_bytes_to_bigstring" (func $caml_blit_bytes_to_bigstring))
   (export "caml_fill_bigstring" (func $caml_fill_bigstring))
   (export "caml_compare_bigstring" (func $caml_compare_bigstring))

   ;; Always aligned, as in cstruct.js.
   (func (export "caml_check_alignment_bigstring")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 1)))
)
