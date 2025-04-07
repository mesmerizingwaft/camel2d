(lang dune 3.17)

(name camel2d)

(generate_opam_files true)

(source
 (github mesmerizingwaft/camel2d))

(authors "mesmera <mesmera.waft@gmail.com>")

(maintainers "mesmera <mesmera.waft@gmail.com>")

(license MIT)

(documentation https://github.com/mesmerizingwaft/camel2d)

(package
 (name camel2d)
 (synopsis "A 2D browser game library written in OCaml")
 (description "A 2D browser game library written in OCaml")
 (depends
    ocaml
    uutf
    (js_of_ocaml (= 5.9.1))
    yojson
    ppx_deriving_yojson
    ppx_blob
    base64
    js_of_ocaml-ppx))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
