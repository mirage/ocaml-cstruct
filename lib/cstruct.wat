;; The Wasm runtime for cstruct's bigstring primitives -- the analog of
;; cstruct.js for wasm_of_ocaml. wasm_of_ocaml's runtime already provides these
;; operations under its own bigarray names, so each cstruct primitive simply
;; re-exports the matching runtime function; only caml_check_alignment_bigstring
;; is cstruct-specific.
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
   (export "caml_fill_bigstring" (func $caml_fill_bigstring))
   (export "caml_compare_bigstring" (func $caml_compare_bigstring))

   ;; No fixed buffer address on Wasm, as in cstruct.js: always aligned.
   (func (export "caml_check_alignment_bigstring")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 1)))
)
