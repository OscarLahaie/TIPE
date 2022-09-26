(lang dune 3.4)

(name TIPE)

(generate_opam_files false)

(source
 (github OscarLahaie/TIPE))

(authors "Oscar Lahaie")

(maintainers "Oscar Lahaie")

(license LICENSE)

(documentation https://github.com/OscarLahaie/TIPE/blob/main/README.md)

(package
 (name TIPE)
 (synopsis
  "An algorithm to evaluate the presence of traffic signs in an image")
 (description "A longer description")
 (depends ocaml dune bimage)
 (tags
  (DOI "image processing" "traffic signs" TIPE)))
