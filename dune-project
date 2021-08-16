(lang dune 2.9)

(name broken)

(version "0.4.2-current")

(generate_opam_files true)

(package
 (name broken)
;  (synopsis "Broken testsuite framework")
;  (description "The Broken package is a simple testsuite framework.")
 (depends
  ;; General system dependencies
  (dune
   (>= 2.2))
  (ocaml
   (>= 4.10.0))
  (reason
   (and
    (>= 3.6.0)
    (< 4.0.0)))
  ;; Dev dependencies
  (ocaml-lsp-server :dev)))
