(lang dune 3.10)
(name qbn)
(generate_opam_files true)
(using menhir 2.1)

(package
 (name qbn)
 (synopsis "Quantum Bayesian Networks compiler")
 (description "A compiler for Quantum Bayesian Networks based on the Geometry of Interaction")
 (depends menhir))
