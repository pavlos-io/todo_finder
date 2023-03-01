(lang dune 3.3)

(name todo_finder)

(generate_opam_files true)

(source
 (github pavlos-io/todo_finder))

(authors "Paul Maltsev")

(maintainers "Paul Maltsev")

(license LICENSE)

(documentation "https://github.com/pavlos-io/todo_finder")

(package
 (name todo_finder)
 (synopsis "Finds todos in codebase")
 (description "A simple tool that scans the provided dir/file and outputs the lines where a todo was found.")
 (depends ocaml dune)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
