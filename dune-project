(lang dune 3.11)
(using menhir 2.0)

(generate_opam_files true)

(license "BSD-2-Clause")
(maintainers "simon.cruanes.2.17@m4x.org")
(authors "Simon Cruanes" "Petar Vukmirovic" "Alexander Bentkamp" "Sophie Tourret" "Visa Nummelin")
(source (github sneeuwballen/zipperposition))
(bug_reports "https://github.com/sneeuwballen/zipperposition/issues")
(homepage "https://github.com/sneeuwballen/zipperposition")

(package
 (name logtk)
 (version "2.1")
 (synopsis "Core types and algorithms for logic")
 (tags (logic unification term))
 (depends
  (ocaml (>= "4.14"))
  base-bytes
  base-unix
  (zarith (>= "1.7"))
  (oseq (>= "0.5"))
  (containers (and (>= "3.0") (< "4.0")))
  (containers-data (and (>= "3.0") (< "4.0")))
  (iter (>= "1.2"))
  (menhir :build)
  (trace (>= 0.11))
  (alcotest :with-test)
  (qcheck-core (and :with-test (>= "0.9")))
  (qcheck-alcotest (and :with-test (>= "0.9"))))
 (depopts
  msat
  trace-tef
  tracy-client)
 (conflicts
  (msat (or (< "0.8.1") (>= "0.10")))
  (zarith (< "1.7"))))

(package
 (name libzipperposition)
 (version "2.1")
 (synopsis "Library for Zipperposition")
 (tags (logic unification term superposition prover))
 (depends
  (ocaml (>= "4.14"))
  base-bytes
  base-unix
  (logtk (= :version))
  (containers (and (>= "3.1") (< "4.0")))
  (containers-data (and (>= "3.1") (< "4.0")))
  (iter (>= "1.2"))
  oseq
  (menhir :build)
  (msat (and (>= "0.8.1") (< "0.10")))))

(package
 (name zipperposition)
 (version "2.1")
 (synopsis "A fully automatic theorem prover for typed higher-order and beyond")
 (tags (logic unification term superposition prover))
 (depends
  (ocaml (>= "4.14"))
  base-unix
  (logtk (= :version))
  (libzipperposition (= :version))
  (containers (and (>= "3.0") (< "4.0")))
  (iter (>= "1.2"))
  oseq
  (menhir :build)
  (msat (and (>= "0.8") (< "0.10")))))

(package
 (name zipperposition-tools)
 (version "2.1")
 (synopsis "Support tools for Zipperposition")
 (tags (logic unification term superposition prover))
 (depends
  (ocaml (>= "4.14"))
  (containers (and (>= "3.0") (< "4.0")))
  (iter (>= "1.2"))
  (oseq (>= "0.2"))
  (msat (and (>= "0.8") (< "0.10")))
  (logtk (= :version))
  (libzipperposition (= :version))))
