(define-module (myguix packages rust-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-audio)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-shell)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crates-database)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (ice-9 match))

(define-public rust-torch-sys-0.14
  (package
    (name "rust-torch-sys")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "torch-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08pgxjccfqrqmh7q2i0g1s5zlababcvvigrxa0bs2xxqkzw4cd40"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-zip" ,rust-zip-0.6))))
    (home-page "https://github.com/LaurentMazare/tch-rs")
    (synopsis "Low-level FFI bindings for the PyTorch C++ api (libtorch).")
    (description
     "Low-level FFI bindings for the @code{PyTorch} C++ api (libtorch).")
    (license (list license:expat license:asl2.0))))

(define-public rust-safetensors-0.3
  (package
    (name "rust-safetensors")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "safetensors" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pxi5w8q0lm2yr3rrc8plibzlg63rc3dsm481a16zrrxdfw7jcnr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/huggingface/safetensors")
    (synopsis
     "Provides functions to read and write safetensors which aim to be safer than
their PyTorch counterpart.
The format is 8 bytes which is an unsized int, being the size of a JSON header,
the JSON header refers the `dtype` the `shape` and `data_offsets` which are the offsets
for the values in the rest of the file.
")
    (description
     "This package provides functions to read and write safetensors which aim to be
safer than their @code{PyTorch} counterpart.  The format is 8 bytes which is an
unsized int, being the size of a JSON header, the JSON header refers the `dtype`
the `shape` and `data_offsets` which are the offsets for the values in the rest
of the file.")
    (license license:asl2.0)))

(define-public rust-memmap2-0.6
  (package
    (name "rust-memmap2")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memmap2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wm8avdjma6j3x5fjdqwxcj89h52pzmwanw46xkn9rnz9albna3d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-stable-deref-trait" ,rust-stable-deref-trait-1))))
    (home-page "https://github.com/RazrFalcon/memmap2-rs")
    (synopsis "Cross-platform Rust API for memory-mapped file IO")
    (description "Cross-platform Rust API for memory-mapped file IO")
    (license (list license:expat license:asl2.0))))

(define-public rust-python3-sys-0.7
  (package
    (name "rust-python3-sys")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "python3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lmb38pmzglqwx7f5spy7c4jrmxv36zyw0x4b9riac7vf86vby29"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-regex" ,rust-regex-1))))
    (home-page
     "https://github.com/dgrunwald/rust-cpython/tree/master/python3-sys")
    (synopsis "FFI Declarations for Python 3")
    (description "FFI Declarations for Python 3")
    (license license:expat)))

(define-public rust-python27-sys-0.7
  (package
    (name "rust-python27-sys")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "python27-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lfzyhbghlnb6a62ffng7nn5d7gwdfxlr1m5h7g0sc34w9a06rwl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-regex" ,rust-regex-1))))
    (home-page
     "https://github.com/dgrunwald/rust-cpython/tree/master/python27-sys")
    (synopsis "FFI Declarations for Python 2.7")
    (description "FFI Declarations for Python 2.7")
    (license license:expat)))

(define-public rust-cpython-0.7
  (package
    (name "rust-cpython")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cpython" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03i8jlj3s2g6hpm58xr8hsr36mrk50chqcf2gcir0wys55n10lih"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-python27-sys" ,rust-python27-sys-0.7)
                       ("rust-python3-sys" ,rust-python3-sys-0.7)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/dgrunwald/rust-cpython")
    (synopsis "Bindings to Python")
    (description "Bindings to Python")
    (license license:expat)))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.154")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0inkwrnwzrr1kw75x944ihdajrhhldkgg4irx1n19y9gp4w36x5f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustix-0.38
  (package
    (name "rust-rustix")
    (version "0.38.34")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03vkqa2ism7q56rkifyy8mns0wwqrk70f4i4fd53r97p8b05xp3h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-errno" ,rust-errno-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-linux-raw-sys" ,rust-linux-raw-sys-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/bytecodealliance/rustix")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls")
    (description
     "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls")
    (license (list license:asl2.0 license:asl2.0 license:expat))))

(define-public rust-tempfile-3
  (package
    (name "rust-tempfile")
    (version "3.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wdzz35ri168jn9al4s1g2rnsrr5ci91khgarc2rvpb3nappzdw5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-fastrand" ,rust-fastrand-2)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "A library for managing temporary files and directories.")
    (description
     "This package provides a library for managing temporary files and directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-segmentation-1
  (package
    (name "rust-unicode-segmentation")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-segmentation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00kjpwp1g8fqm45drmwivlacn3y9jx73bvs09n6s3x73nqi7vj6l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/unicode-rs/unicode-segmentation")
    (synopsis
     "This crate provides Grapheme Cluster, Word and Sentence boundaries
according to Unicode Standard Annex #29 rules.
")
    (description
     "This crate provides Grapheme Cluster, Word and Sentence boundaries according to
Unicode Standard Annex #29 rules.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-normalization-alignments-0.1
  (package
    (name "rust-unicode-normalization-alignments")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-normalization-alignments" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pk2f3arh3qvdsmrsiri0gr5y5vqpk2gv1yjin0njvh4zbj17xj3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/n1t0/unicode-normalization")
    (synopsis "This crate provides functions for normalization of
Unicode strings, including Canonical and Compatible
Decomposition and Recomposition, as described in
Unicode Standard Annex #15.
")
    (description
     "This crate provides functions for normalization of Unicode strings, including
Canonical and Compatible Decomposition and Recomposition, as described in
Unicode Standard Annex #15.")
    (license (list license:expat license:asl2.0))))

(define-public rust-spm-precompiled-0.1
  (package
    (name "rust-spm-precompiled")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "spm_precompiled" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09pkdk2abr8xf4pb9kq3rk80dgziq6vzfk7aywv3diik82f6jlaq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1))))
    (home-page "https://github.com/huggingface/spm_precompiled")
    (synopsis
     "This crate aims to emulate https://github.com/google/sentencepiece Dart::DoubleArray
struct and it's Normalizer.

This crate is highly specialized and not intended for general use.
")
    (description
     "This crate aims to emulate https://github.com/google/sentencepiece
Dart::@code{DoubleArray} struct and it's Normalizer.  This crate is highly
specialized and not intended for general use.")
    (license license:asl2.0)))

(define-public rust-rayon-cond-0.3
  (package
    (name "rust-rayon-cond")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon-cond" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ybxppq84p3q60h9rng9j3dm79f6970hn4wljyf31lpgan5m77q5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-itertools" ,rust-itertools-0.11)
                       ("rust-rayon" ,rust-rayon-1))))
    (home-page "https://github.com/cuviper/rayon-cond")
    (synopsis
     "Experimental iterator wrapper that is conditionally parallel or serial.")
    (description
     "Experimental iterator wrapper that is conditionally parallel or serial.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-wasm-sync-0.1
  (package
    (name "rust-wasm-sync")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm_sync" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1smhpgk5jiir089y5w6y2rgqq9aqzrbxlb4x1vzl3v3zvv561wyg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/DouglasDwyer/wasm_sync")
    (synopsis "Synchronization primitives for both web and native.
")
    (description "Synchronization primitives for both web and native.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rayon-core-1
  (package
    (name "rust-rayon-core")
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qpwim68ai5h0j7axa8ai8z0payaawv3id0lrgkqmapx7lx8fr8l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-wasm-sync" ,rust-wasm-sync-0.1))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Core APIs for Rayon")
    (description "Core APIs for Rayon")
    (license (list license:expat license:asl2.0))))

(define-public rust-rayon-1
  (package
    (name "rust-rayon")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ylgnzwgllajalr4v00y4kj22klq2jbwllm70aha232iah0sc65l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-rayon-core" ,rust-rayon-core-1)
                       ("rust-wasm-sync" ,rust-wasm-sync-0.1))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Simple work-stealing parallelism for Rust")
    (description "Simple work-stealing parallelism for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-onig-sys-69
  (package
    (name "rust-onig-sys")
    (version "69.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "onig_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rw6y2qkb765gzylmrydbbd90hdzhnqyvs2y65z4riwwgqyrx0kv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.59)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "http://github.com/iwillspeak/rust-onig")
    (synopsis "The `onig_sys` crate contains raw rust bindings to the
oniguruma library. This crate exposes a set of unsafe
functions which can then be used by other crates to
create safe wrappers around Oniguruma.

You probably don't want to link to this crate directly;
instead check out the `onig` crate.
")
    (description
     "The `onig_sys` crate contains raw rust bindings to the oniguruma library.  This
crate exposes a set of unsafe functions which can then be used by other crates
to create safe wrappers around Oniguruma.  You probably don't want to link to
this crate directly; instead check out the `onig` crate.")
    (license license:expat)))

(define-public rust-onig-6
  (package
    (name "rust-onig")
    (version "6.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "onig" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kyaz2fwa5dkr04rvk5ga2yv5jkqn1ymblvpdlf1gn9afb432jwc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-onig-sys" ,rust-onig-sys-69))))
    (home-page "http://github.com/iwillspeak/rust-onig")
    (synopsis "Rust-Onig is a set of Rust bindings for the
Oniguruma regular expression library. Oniguruma
is a modern regex library with support for
multiple character encodings and regex syntaxes.
")
    (description
     "Rust-Onig is a set of Rust bindings for the Oniguruma regular expression
library.  Oniguruma is a modern regex library with support for multiple
character encodings and regex syntaxes.")
    (license license:expat)))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.61")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j8zhf5mmd2l5niwhiniw5wcp9v6fbd4a61v6rbfhsm5rf6fv4y9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.82")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06qk88hbf6wg4v1i961zibhjz512873jwkz3myx1z82ip6dd9lwa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "A substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-monostate-impl-0.1
  (package
    (name "rust-monostate-impl")
    (version "0.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "monostate-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q3lxbfzpqcsy30gpyqkb2yppqzjj6ags6niflsi4kzdfnwn9km7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/monostate")
    (synopsis "Implementation detail of the monostate crate")
    (description "Implementation detail of the monostate crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-monostate-0.1
  (package
    (name "rust-monostate")
    (version "0.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "monostate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07hfvh2202477mx1ff47b6f04gihqcdrmdndv10x0b2msw3q880d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-monostate-impl" ,rust-monostate-impl-0.1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/dtolnay/monostate")
    (synopsis "Type that deserializes only from one specific value")
    (description "Type that deserializes only from one specific value")
    (license (list license:expat license:asl2.0))))

(define-public rust-macro-rules-attribute-proc-macro-0.2
  (package
    (name "rust-macro-rules-attribute-proc-macro")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "macro_rules_attribute-proc_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s45j4zm0a5d041g3vcbanvr76p331dfjb7gw9qdmh0w8mnqbpdq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page
     "https://github.com/danielhenrymantilla/macro_rules_attribute-rs")
    (synopsis "Use declarative macros as proc_macro attributes or derives")
    (description "Use declarative macros as proc_macro attributes or derives")
    (license license:expat)))

(define-public rust-macro-rules-attribute-0.2
  (package
    (name "rust-macro-rules-attribute")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "macro_rules_attribute" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04waa4qm28adwnxsxhx9135ki68mwkikr6m5pi5xhcy0gcgjg0la"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-macro-rules-attribute-proc-macro" ,rust-macro-rules-attribute-proc-macro-0.2)
                       ("rust-paste" ,rust-paste-1))))
    (home-page "https://crates.io/crates/macro_rules_attribute")
    (synopsis "Use declarative macros in attribute or derive position")
    (description "Use declarative macros in attribute or derive position")
    (license license:expat)))

(define-public rust-indicatif-0.17
  (package
    (name "rust-indicatif")
    (version "0.17.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indicatif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18xyqxw9i5x4sbpzckhfz3nm984iq9r7nbi2lk76nz888n7mlfkn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-console" ,rust-console-0.15)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-instant" ,rust-instant-0.1)
                       ("rust-number-prefix" ,rust-number-prefix-0.4)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-vt100" ,rust-vt100-0.15))))
    (home-page "https://github.com/console-rs/indicatif")
    (synopsis "A progress bar and cli reporting library for Rust")
    (description
     "This package provides a progress bar and cli reporting library for Rust")
    (license license:expat)))

(define-public rust-hf-hub-0.3
  (package
    (name "rust-hf-hub")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hf-hub" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cnpivy9fn62lm1fw85kmg3ryvrx8drq63c96vq94gabawshcy1b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dirs" ,rust-dirs-5)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-ureq" ,rust-ureq-2))))
    (home-page "https://github.com/huggingface/hf-hub")
    (synopsis
     "This crates aims ease the interaction with [huggingface](https://huggingface.co/)
It aims to be compatible with [huggingface_hub](https://github.com/huggingface/huggingface_hub/) python package, but only implements a smaller subset of functions.
")
    (description
     "This crates aims ease the interaction with
[huggingface](https://huggingface.co/) It aims to be compatible with
[huggingface_hub](https://github.com/huggingface/huggingface_hub/) python
package, but only implements a smaller subset of functions.")
    (license license:asl2.0)))

(define-public rust-fancy-regex-0.13
  (package
    (name "rust-fancy-regex")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fancy-regex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wjbqjsdj8fkq6z2i9llq25iaqzd9f208vxnwg8mdbr2ba1lc7jk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bit-set" ,rust-bit-set-0.5)
                       ("rust-regex-automata" ,rust-regex-automata-0.4)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8))))
    (home-page "https://github.com/fancy-regex/fancy-regex")
    (synopsis
     "An implementation of regexes, supporting a relatively rich set of features, including backreferences and look-around.")
    (description
     "An implementation of regexes, supporting a relatively rich set of features,
including backreferences and look-around.")
    (license license:expat)))

(define-public rust-esaxx-rs-0.1
  (package
    (name "rust-esaxx-rs")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "esaxx-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rm6vm5yr7s3n5ly7k9x9j6ra5p2l2ld151gnaya8x03qcwf05yq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/Narsil/esaxx-rs")
    (synopsis "Wrapping around sentencepiece's esaxxx library.")
    (description "Wrapping around sentencepiece's esaxxx library.")
    (license license:asl2.0)))

(define-public rust-darling-macro-0.20
  (package
    (name "rust-darling-macro")
    (version "0.20.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gwkz0cjfy3fgcc1zmm7azzhj5qpja34s0cklcria4l38sjyss56"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-darling-core" ,rust-darling-core-0.20)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis
     "Internal support for a proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.
")
    (description
     "Internal support for a proc-macro library for reading attributes into structs
when implementing custom derives.  Use https://crates.io/crates/darling in your
code.")
    (license license:expat)))

(define-public rust-darling-core-0.20
  (package
    (name "rust-darling-core")
    (version "0.20.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03x7s149p06xfwcq0lgkk4yxh6jf7jckny18nzp1yyk87b1g2b4w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fnv" ,rust-fnv-1)
                       ("rust-ident-case" ,rust-ident-case-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-strsim" ,rust-strsim-0.10)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis
     "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.
")
    (description
     "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives.  Use https://crates.io/crates/darling in your code.")
    (license license:expat)))

(define-public rust-darling-0.20
  (package
    (name "rust-darling")
    (version "0.20.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14a38qsi9104kvk1z11rqj0bnz1866dyhnvgvbgzz17d2g6nzqsl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-darling-core" ,rust-darling-core-0.20)
                       ("rust-darling-macro" ,rust-darling-macro-0.20))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis "A proc-macro library for reading attributes into structs when
implementing custom derives.
")
    (description
     "This package provides a proc-macro library for reading attributes into structs
when implementing custom derives.")
    (license license:expat)))

(define-public rust-derive-builder-core-0.20
  (package
    (name "rust-derive-builder-core")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_builder_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "078bralcmzwy3vi0w1rc65a3v4kk6jgccsir5mrm34c3gxwdm36l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-darling" ,rust-darling-0.20)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis "Internal helper library for the derive_builder crate.")
    (description "Internal helper library for the derive_builder crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-derive-builder-macro-0.20
  (package
    (name "rust-derive-builder-macro")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_builder_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yq9hnyayys16rzmiwjd6gfx1ysph7c9zh94w76cw9rg4jw6hs10"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-derive-builder-core" ,rust-derive-builder-core-0.20)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
     "Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (description
     "Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-derive-builder-0.20
  (package
    (name "rust-derive-builder")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mzz0njgbrzlhj97md03df5knfcp5svw1ifn2rcqlqii0g5val03"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-derive-builder-macro" ,rust-derive-builder-macro-0.20))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
     "Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (description
     "Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-memmap2-0.9
  (package
    (name "rust-memmap2")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memmap2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08hkmvri44j6h14lyq4yw5ipsp91a9jacgiww4bs9jm8whi18xgy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-stable-deref-trait" ,rust-stable-deref-trait-1))
       #:cargo-development-inputs (("rust-owning-ref" ,rust-owning-ref-0.4)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/RazrFalcon/memmap2-rs")
    (synopsis "Cross-platform Rust API for memory-mapped file IO")
    (description "Cross-platform Rust API for memory-mapped file IO")
    (license (list license:expat license:asl2.0))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.153")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gg7m1ils5dms5miq9fyllrcp0jxnbpgkx71chd2i0lafa8qy6cw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-macros-backend-0.21
  (package
    (name "rust-pyo3-macros-backend")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-macros-backend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fycsj0b8ajz2rnb002sjhm3dkcdi0mi5jqkcl1i0gamxgbvzywk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.21)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Code generation for PyO3 package")
    (description "Code generation for @code{PyO3} package")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-macros-0.21
  (package
    (name "rust-pyo3-macros")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ym6vg3mn4z199lm01swh883abfhcl74f396n7b5sqirsgik2jb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-pyo3-macros-backend" ,rust-pyo3-macros-backend-0.21)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Proc macros for PyO3 package")
    (description "Proc macros for @code{PyO3} package")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-ffi-0.21
  (package
    (name "rust-pyo3-ffi")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-ffi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ba6y12xq9hbp1viv5cbr75dwx443fgg4ncvj22212jaq27xm9q9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.21))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Python-API bindings for the PyO3 ecosystem")
    (description "Python-API bindings for the @code{PyO3} ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-build-config-0.21
  (package
    (name "rust-pyo3-build-config")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-build-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xxfyh7kihdsngmzl267mwb6hszbsszp279bn2ywvdk3shscl3b5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-python3-dll-a" ,rust-python3-dll-a-0.2)
                       ("rust-python3-dll-a" ,rust-python3-dll-a-0.2)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Build configuration for the PyO3 ecosystem")
    (description "Build configuration for the @code{PyO3} ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-0.21
  (package
    (name "rust-pyo3")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sx17rgkxf8v607nwibqyv2imggq7nhhirnmd0bpc66h1fcv3a57"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-tz" ,rust-chrono-tz-0.6)
                       ("rust-either" ,rust-either-1)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-hashbrown" ,rust-hashbrown-0.9)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-indoc" ,rust-indoc-2)
                       ("rust-inventory" ,rust-inventory-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memoffset" ,rust-memoffset-0.9)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-complex" ,rust-num-complex-0.2)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.21)
                       ("rust-pyo3-ffi" ,rust-pyo3-ffi-0.21)
                       ("rust-pyo3-macros" ,rust-pyo3-macros-0.21)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-unindent" ,rust-unindent-0.2))
       #:cargo-development-inputs (("rust-assert-approx-eq" ,rust-assert-approx-eq-1)
                                   ("rust-chrono" ,rust-chrono-0.4)
                                   ("rust-chrono-tz" ,rust-chrono-tz-0.6)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-send-wrapper" ,rust-send-wrapper-0.6)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Bindings to Python interpreter")
    (description "Bindings to Python interpreter")
    (license (list license:expat license:asl2.0))))

(define-public rust-cfg-if-1
  (package
    (name "rust-cfg-if")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cfg-if" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis
     "A macro to ergonomically define an item depending on a large number of #[cfg]
parameters. Structured like an if-else chain, the first matching branch is the
item that gets emitted.
")
    (description
     "This package provides a macro to ergonomically define an item depending on a
large number of #[cfg] parameters.  Structured like an if-else chain, the first
matching branch is the item that gets emitted.")
    (license (list license:expat license:asl2.0))))

(define-public rust-memoffset-0.9
  (package
    (name "rust-memoffset")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memoffset" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/Gilnaa/memoffset")
    (synopsis "offset_of functionality for Rust structs.")
    (description "offset_of functionality for Rust structs.")
    (license license:expat)))

(define-public rust-autocfg-1
  (package
    (name "rust-autocfg")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "autocfg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "102c77is3pii4rsqfsc5vrbk6qabjy0yqc0gwqzmjjb9fp3spzgi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags `("--" "--skip=test_wrappers")))
    (home-page "https://github.com/cuviper/autocfg")
    (synopsis "Automatic cfg for Rust compiler features")
    (description "Automatic cfg for Rust compiler features")
    (license (list license:asl2.0 license:expat))))

(define-public rust-parking-lot-0.12
  (package
    (name "rust-parking-lot")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "parking_lot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.9))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "More compact and efficient implementations of the standard synchronization primitives.")
    (description
     "More compact and efficient implementations of the standard synchronization
primitives.")
    (license (list license:expat license:asl2.0))))

(define-public rust-quote-1.0.35
  (package
    (name "rust-quote")
    (version "1.0.35")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quote" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vv8r2ncaz4pqdr78x7f138ka595sp2ncr1sa2plm4zxbsmwj7i9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1))
       #:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "This package provides Quasi-quoting macro quote!(...).")
    (license (list license:expat license:asl2.0))))

(define-public rust-lock-api-0.4
  (package
    (name "rust-lock-api")
    (version "0.4.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lock_api" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0iggx0h4jx63xm35861106af3jkxq06fpqhpkhgw0axi2n38y5iw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-owning-ref" ,rust-owning-ref-0.4)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis "Wrappers to create fully-featured Mutex and RwLock types")
    (description
     "This package provides wrappers to create fully-featured
@code{Mutex} and @code{RwLock} types.  It is compatible with @code{no_std}.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-scopeguard-1
  (package
    (name "rust-scopeguard")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "scopeguard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/scopeguard")
    (synopsis
     "A RAII scope guard that will run a given closure when it goes out of scope,
even if the code between panics (assuming unwinding panic).

Defines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as
shorthands for guards with one of the implemented strategies.
")
    (description
     "This package provides a RAII scope guard that will run a given closure when it
goes out of scope, even if the code between panics (assuming unwinding panic).
Defines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as
shorthands for guards with one of the implemented strategies.")
    (license (list license:expat license:asl2.0))))

(define-public rust-parking-lot-core-0.9
  (package
    (name "rust-parking-lot-core")
    (version "0.9.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "parking_lot_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13h0imw1aq86wj28gxkblhkzx6z1gk8q18n0v76qmmj6cliajhjc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-petgraph" ,rust-petgraph-0.6)
                       ("rust-redox-syscall" ,rust-redox-syscall-0.4)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thread-id" ,rust-thread-id-4)
                       ("rust-windows-targets" ,rust-windows-targets-0.48))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "An advanced API for creating custom synchronization primitives.")
    (description
     "An advanced API for creating custom synchronization primitives.")
    (license (list license:expat license:asl2.0))))

(define-public rust-smallvec-1
  (package
    (name "rust-smallvec")
    (version "1.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smallvec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-debugger-test" ,rust-debugger-test-0.1)
                                   ("rust-debugger-test-parser" ,rust-debugger-test-parser-0.1))))
    (home-page "https://github.com/servo/rust-smallvec")
    (synopsis
     "'Small vector' optimization: store up to a small number of items on the stack")
    (description
     "Small vector optimization: store up to a small number of items on the stack")
    (license (list license:expat license:asl2.0))))

(define-public rust-portable-atomic-1
  (package
    (name "rust-portable-atomic")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "portable-atomic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h77x9qx7pns0d66vdrmdbmwpi7586h7ysnkdnhrn5mwi2cyyw3i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-critical-section" ,rust-critical-section-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-build-context" ,rust-build-context-0.1)
                                   ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                                   ("rust-fastrand" ,rust-fastrand-2)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-sptr" ,rust-sptr-0.3)
                                   ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/taiki-e/portable-atomic")
    (synopsis
     "Portable atomic types including support for 128-bit atomics, atomic float, etc.")
    (description
     "Portable atomic types including support for 128-bit atomics, atomic float, etc.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-once-cell-1
  (package
    (name "rust-once-cell")
    (version "1.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "once_cell" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14kvw7px5z96dk4dwdm1r9cqhhy2cyj1l5n5b29mynbb8yr15nrz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-critical-section" ,rust-critical-section-1)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.9)
                       ("rust-portable-atomic" ,rust-portable-atomic-1))
       #:cargo-development-inputs (("rust-critical-section" ,rust-critical-section-1)
                                   ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/matklad/once_cell")
    (synopsis "Single assignment cells and lazy values.")
    (description "Single assignment cells and lazy values.")
    (license (list license:expat license:asl2.0))))

(define-public rust-target-lexicon-0.12
  (package
    (name "rust-target-lexicon")
    (version "0.12.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "target-lexicon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bzzr5cq1n56nmjp5fkf2h1g9a27lmkbld3qqfvwy6x2j4w41z71"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/bytecodealliance/target-lexicon")
    (synopsis "Targeting utilities for compilers and related tools")
    (description "Targeting utilities for compilers and related tools")
    (license (list license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.201")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r98v8h47s7zhml7gz0sl6wv82vyzh1hv27f1g0g35lp1f9hbr65"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.201")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g1nrz2s6l36na6gdbph8k07xf9h5p3s6f0s79sy8a8nxpmiq3vq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-bytemuck-1
  (package
    (name "rust-bytemuck")
    (version "1.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytemuck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05gxh5i8vhjhr8b7abzla1k74m3khsifr439320s18rmfb2nhvax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytemuck-derive" ,rust-bytemuck-derive-1))))
    (home-page "https://github.com/Lokathor/bytemuck")
    (synopsis "A crate for mucking around with piles of bytes.")
    (description
     "This package provides a crate for mucking around with piles of bytes.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-roaring-0.10
  (package
    (name "rust-roaring")
    (version "0.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "roaring" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l84m1s1lmqd9kcri0fyjpg40f696yxdcvfrpjhv7z04lqjlqvxj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/RoaringBitmap/roaring-rs")
    (synopsis "A better compressed bitset - pure Rust implementation")
    (description
     "This package provides a better compressed bitset - pure Rust implementation")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-ident-1
  (package
    (name "rust-unicode-ident")
    (version "1.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-ident" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jzf1znfpb2gx8nr8mvmyqs1crnv79l57nxnbiszc7xf7ynbjm1k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-fst" ,rust-fst-0.4)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-roaring" ,rust-roaring-0.10)
                                   ("rust-ucd-trie" ,rust-ucd-trie-0.1)
                                   ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://github.com/dtolnay/unicode-ident")
    (synopsis
     "Determine whether characters have the XID_Start or XID_Continue properties according to Unicode Standard Annex #31")
    (description
     "Determine whether characters have the XID_Start or XID_Continue properties
according to Unicode Standard Annex #31")
    (license (list license:asl2.0))))

(define-public rust-heck-0.4
  (package
    (name "rust-heck")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "heck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-unicode-segmentation" ,rust-unicode-segmentation-1))))
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description "heck is a case conversion library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-quote-1
  (package
    (name "rust-quote")
    (version "1.0.36")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quote" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19xcmh445bg6simirnnd4fvkmp6v2qiwxh5f6rw4a70h76pnm9qg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1))
       #:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (license (list license:expat license:asl2.0))))

(define-public rust-winreg-0.52
  (package
    (name "rust-winreg")
    (version "0.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19gh9vp7mp1ab84kc3ag48nm9y7xgjhh3xa4vxss1gylk1rsaxx2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/gentoo90/winreg-rs")
    (synopsis "Rust bindings to MS Windows Registry API")
    (description "Rust bindings to MS Windows Registry API")
    (license license:expat)))

(define-public rust-wasm-streams-0.4
  (package
    (name "rust-wasm-streams")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-streams" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ad17c59xb8fffsnbrqbyqz93hb66nzxhizpii31icb31g4w8pdn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/MattiasBuelens/wasm-streams/")
    (synopsis "Bridging between web streams and Rust streams using WebAssembly
")
    (description
     "Bridging between web streams and Rust streams using @code{WebAssembly}")
    (license (list license:expat license:asl2.0))))

(define-public rust-web-time-1
  (package
    (name "rust-web-time")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "web-time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/daxpedda/web-time")
    (synopsis "Drop-in replacement for std::time for Wasm in browsers")
    (description "Drop-in replacement for std::time for Wasm in browsers")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-pki-types-1
  (package
    (name "rust-rustls-pki-types")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pki-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0banlc9xzwqrx8n0h4bd0igmq3z5hc72rn941lf22cp3gkkraqlp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-web-time" ,rust-web-time-1))))
    (home-page "https://github.com/rustls/pki-types")
    (synopsis "Shared types for the rustls PKI ecosystem")
    (description "Shared types for the rustls PKI ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper-tls-0.6
  (package
    (name "rust-hyper-tls")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q36x2yps6hhvxq5r7mc8ph9zz6xlb573gx0x3yskb0fi736y83h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3))))
    (home-page "https://hyper.rs")
    (synopsis "Default TLS implementation for use with hyper")
    (description "Default TLS implementation for use with hyper")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper-util-0.1
  (package
    (name "rust-hyper-util")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1akngan7j0n2n0wd25c6952mvqbkj9gp1lcwzyxjc0d37l8yyf6a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://hyper.rs")
    (synopsis "hyper utilities")
    (description "hyper utilities")
    (license license:expat)))

(define-public rust-hyper-rustls-0.26
  (package
    (name "rust-hyper-rustls")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b4m1jvs147hxi8677n2dxxib663s7c31xmfni7b5qkanihsggm0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustls" ,rust-rustls-0.22)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/rustls/hyper-rustls")
    (synopsis "Rustls+hyper integration for pure rust HTTPS")
    (description "Rustls+hyper integration for pure rust HTTPS")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-hyper-1
  (package
    (name "rust-hyper")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0va9pjqshsr8zc07m9h4j2821hsmd9lw9j416yisjqh8gp8msmzy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.4)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-want" ,rust-want-0.3))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description "This package provides a fast and correct HTTP library.")
    (license license:expat)))

(define-public rust-http-body-util-0.1
  (package
    (name "rust-http-body-util")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07agldas2qgcfc05ckiarlmf9vzragbda823nqhrqrc6mjrghx84"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/hyperium/http-body")
    (synopsis "Combinators and adapters for HTTP request or response bodies.
")
    (description
     "Combinators and adapters for HTTP request or response bodies.")
    (license license:expat)))

(define-public rust-http-body-1
  (package
    (name "rust-http-body")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hyn8n3iadrbwq8y0p1rl1275s4nm49bllw5wji29g4aa3dqbb0w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-1))))
    (home-page "https://github.com/hyperium/http-body")
    (synopsis
     "Trait representing an asynchronous, streaming, HTTP request or response body.
")
    (description
     "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (license license:expat)))

(define-public rust-h3-quinn-0.0.5
  (package
    (name "rust-h3-quinn")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3-quinn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ii06bi5a19k4qfkppn5019nw8xca2wzfl66cax949jc1v66ny3k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-h3" ,rust-h3-0.0.4)
                       ("rust-quinn" ,rust-quinn-0.10)
                       ("rust-quinn-proto" ,rust-quinn-proto-0.10)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "QUIC transport implementation based on Quinn.")
    (description "QUIC transport implementation based on Quinn.")
    (license license:expat)))

(define-public rust-h3-0.0.4
  (package
    (name "rust-h3")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04clhh6b5iqlgnbppikbz4zpxl78g4vkyhyrjgnyg4vfkrmqij5i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fastrand" ,rust-fastrand-2)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "An async HTTP/3 implementation.")
    (description "An async HTTP/3 implementation.")
    (license license:expat)))

(define-public rust-h2-0.4
  (package
    (name "rust-h2")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sc0ymhiqp4hbz39d405cjbga77wnz2pprbgyc498xs58hlwfvl1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/hyperium/h2")
    (synopsis "An HTTP/2 client and server")
    (description "An HTTP/2 client and server")
    (license license:expat)))

(define-public rust-base64-0.22
  (package
    (name "rust-base64")
    (version "0.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base64" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/marshallpierce/rust-base64")
    (synopsis "encodes and decodes base64 as bytes or utf8")
    (description "encodes and decodes base64 as bytes or utf8")
    (license (list license:expat license:asl2.0))))

(define-public rust-reqwest-0.12
  (package
    (name "rust-reqwest")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "reqwest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "047aa0qnngnlnf9i0abrs6pgmz15vk81p5pvscwhk3l6jbfsyv2n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.4)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cookie" ,rust-cookie-0.17)
                       ("rust-cookie-store" ,rust-cookie-store-0.20)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.4)
                       ("rust-h3" ,rust-h3-0.0.4)
                       ("rust-h3-quinn" ,rust-h3-quinn-0.0.5)
                       ("rust-hickory-resolver" ,rust-hickory-resolver-0.24)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.26)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.6)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-quinn" ,rust-quinn-0.10)
                       ("rust-rustls" ,rust-rustls-0.22)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
                       ("rust-system-configuration" ,rust-system-configuration-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
                       ("rust-tokio-socks" ,rust-tokio-socks-0.5)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-wasm-streams" ,rust-wasm-streams-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26)
                       ("rust-winreg" ,rust-winreg-0.52))))
    (home-page "https://github.com/seanmonstar/reqwest")
    (synopsis "higher level HTTP client library")
    (description "higher level HTTP client library")
    (license (list license:expat license:asl2.0))))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.59")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nk1f98z027qdjwsn756hnshp0y4cka4pq729ig6awdhqzzk2raa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-automod" ,rust-automod-1)
                                   ("rust-flate2" ,rust-flate2-1)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-reqwest" ,rust-reqwest-0.12)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-syn-test-suite" ,rust-syn-test-suite-0.0.0)
                                   ("rust-tar" ,rust-tar-0.4)
                                   ("rust-termcolor" ,rust-termcolor-1)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-unindent-0.2
  (package
    (name "rust-unindent")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unindent" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1km2iy6fr6gsh2wvr1mxz86pm4wrlh3fjkinb35qfi3mw5rpvpn7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/indoc")
    (synopsis "Remove a column of leading whitespace from a string")
    (description "Remove a column of leading whitespace from a string")
    (license (list license:expat license:asl2.0))))

(define-public rust-safetensors-0.4
  (package
    (name "rust-safetensors")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "safetensors" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fbx56wikqcvqb4y0ym0cys68lj0v3cpanhsy5i13fkz5jr7dvcc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-memmap2" ,rust-memmap2-0.9)
                                   ("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/huggingface/safetensors")
    (synopsis
     "Provides functions to read and write safetensors which aim to be safer than
their PyTorch counterpart.
The format is 8 bytes which is an unsized int, being the size of a JSON header,
the JSON header refers the `dtype` the `shape` and `data_offsets` which are the offsets
for the values in the rest of the file.
")
    (description
     "This package provides functions to read and write safetensors which aim to be
safer than their @code{PyTorch} counterpart.  The format is 8 bytes which is an
unsized int, being the size of a JSON header, the JSON header refers the `dtype`
the `shape` and `data_offsets` which are the offsets for the values in the rest
of the file.")
    (license license:asl2.0)))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.197")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02v1x0sdv8qy06lpr6by4ar1n3jz3hmab15cgimpzhgd895v7c3y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.197")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qjcxqd3p4yh5cmmax9q4ics1zy34j5ij32cvjj5dc5rw5rwic9z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))
       #:cargo-development-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-indexmap-2
  (package
    (name "rust-indexmap")
    (version "2.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indexmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09hgwi2ig0wyj5rjziia76zmhgfj95k0jb4ic3iiawm4vlavg3qn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-borsh" ,rust-borsh-1)
                       ("rust-equivalent" ,rust-equivalent-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-rayon" ,rust-rustc-rayon-0.5)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/indexmap-rs/indexmap")
    (synopsis "A hash table with consistent order and fast iteration.")
    (description
     "This package provides a hash table with consistent order and fast iteration.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-indexmap-2
  (package
    (name "rust-indexmap")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indexmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lmnm1zbr5gq3wic3d8a76gpvampridzwckfl97ckd5m08mrk74c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-borsh" ,rust-borsh-1)
                       ("rust-equivalent" ,rust-equivalent-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.15)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-rayon" ,rust-rustc-rayon-0.5)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/indexmap-rs/indexmap")
    (synopsis "hash table with consistent order and fast iteration.")
    (description
     "This package provides a hash table with consistent order and fast iteration.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ordermap-0.5
  (package
    (name "rust-ordermap")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ordermap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q2iaiyhfw7367mmg0dxds5wj1byfsal6h3dcpijksdil92xypy5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-borsh" ,rust-borsh-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-fnv" ,rust-fnv-1)
                                   ("rust-itertools" ,rust-itertools-0.13)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/indexmap-rs/ordermap")
    (synopsis "hash table with consistent order and fast iteration.")
    (description
     "This package provides a hash table with consistent order and fast iteration.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.115")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pcpcik3jmfkw845irc61vd8f91zlpwnq6z7ypsgvsd8v935rp0j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-indoc" ,rust-indoc-2)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-stacker" ,rust-serde-stacker-0.1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-itoa-1
  (package
    (name "rust-itoa")
    (version "1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nv9cqjwzr3q58qz84dcz63ggc54yhf1yqar1m858m1kfd4g3wa9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-no-panic" ,rust-no-panic-0.1))))
    (home-page "https://github.com/dtolnay/itoa")
    (synopsis "Fast integer primitive to string conversion")
    (description "Fast integer primitive to string conversion")
    (license (list license:expat license:asl2.0))))

(define-public rust-ryu-1
  (package
    (name "rust-ryu")
    (version "1.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ryu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "188vrsh3zlnl5xl7lw0rp2sc0knpx8yaqpwvr648b6h12v4rfrp8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-no-panic" ,rust-no-panic-0.1))
       #:cargo-development-inputs (("rust-num-cpus" ,rust-num-cpus-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-xorshift" ,rust-rand-xorshift-0.3))))
    (home-page "https://github.com/dtolnay/ryu")
    (synopsis "Fast floating point to string conversion")
    (description "Fast floating point to string conversion")
    (license (list license:asl2.0 license:boost1.0))))

(define-public rust-value-bag-sval2-1
  (package
    (name "rust-value-bag-sval2")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "value-bag-sval2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nr8453w1357xc70ilbvwcg4qa6bgll44pci0fbznb82hvjbm18p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-sval" ,rust-sval-2)
                       ("rust-sval-buffer" ,rust-sval-buffer-2)
                       ("rust-sval-dynamic" ,rust-sval-dynamic-2)
                       ("rust-sval-fmt" ,rust-sval-fmt-2)
                       ("rust-sval-json" ,rust-sval-json-2)
                       ("rust-sval-ref" ,rust-sval-ref-2)
                       ("rust-sval-serde" ,rust-sval-serde-2)
                       ("rust-sval-test" ,rust-sval-test-2))))
    (home-page "")
    (synopsis "Implementation detail for value-bag")
    (description "Implementation detail for value-bag")
    (license (list license:asl2.0 license:expat))))

(define-public rust-value-bag-serde1-1
  (package
    (name "rust-value-bag-serde1")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "value-bag-serde1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12rjnrh2g3ijvjgfh78y7x1ilk3mw2svri93nymsjxxhbh6gbb6c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-erased-serde" ,rust-erased-serde-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-buf" ,rust-serde-buf-0.1)
                       ("rust-serde-fmt" ,rust-serde-fmt-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "")
    (synopsis "Implementation detail for value-bag")
    (description "Implementation detail for value-bag")
    (license (list license:asl2.0 license:expat))))

(define-public rust-value-bag-1
  (package
    (name "rust-value-bag")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "value-bag" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00aij8p1n7vcggkb9nxpwx9g5nqzclrf7prd1wpi9c3sscvw312s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-value-bag-serde1" ,rust-value-bag-serde1-1)
                       ("rust-value-bag-sval2" ,rust-value-bag-sval2-1))))
    (home-page "https://github.com/sval-rs/value-bag")
    (synopsis "Anonymous structured values")
    (description "Anonymous structured values")
    (license (list license:asl2.0 license:expat))))

(define-public rust-log-0.4
  (package
    (name "rust-log")
    (version "0.4.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rust-lang/log")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06xlvsi7kh1n5whzn2flm6nrfhmcqhamm00q6k2mp6w8ixyk6swl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sval" ,rust-sval-1)
                       ("rust-value-bag" ,rust-value-bag-1))
       #:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-sval" ,rust-sval-1)
                                   ("rust-value-bag" ,rust-value-bag-1))))
    (home-page "https://github.com/rust-lang/log")
    (synopsis "A lightweight logging facade for Rust
")
    (description "This package provides a lightweight logging facade for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-log-0.4
  (package
    (name "rust-log")
    (version "0.4.21")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "log" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "074hldq1q8rlzq2s2qa8f25hj4s3gpw71w64vdwzjd01a4g8rvch"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-sval" ,rust-sval-2)
                       ("rust-sval-ref" ,rust-sval-ref-2)
                       ("rust-value-bag" ,rust-value-bag-1))))
    (home-page "https://github.com/rust-lang/log")
    (synopsis "A lightweight logging facade for Rust
")
    (description "This package provides a lightweight logging facade for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-env-logger-0.11
  (package
    (name "rust-env-logger")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "env_logger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fa34dr082zfih5pw821d13kr6lcg18x6z08pa09d0aip8wmicrq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-env-filter" ,rust-env-filter-0.1)
                       ("rust-humantime" ,rust-humantime-2)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/rust-cli/env_logger")
    (synopsis
     "A logging implementation for `log` which is configured via an environment
variable.
")
    (description
     "This package provides a logging implementation for `log` which is configured via
an environment variable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstream-0.6
  (package
    (name "rust-anstream")
    (version "0.6.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yv2idkyf9mp9xwc684v0ywqiy86lwc9gvllwdishl7y6czx0syr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstyle" ,rust-anstyle-1)
                       ("rust-anstyle-parse" ,rust-anstyle-parse-0.2)
                       ("rust-anstyle-query" ,rust-anstyle-query-1)
                       ("rust-anstyle-wincon" ,rust-anstyle-wincon-3)
                       ("rust-colorchoice" ,rust-colorchoice-1)
                       ("rust-utf8parse" ,rust-utf8parse-0.2))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-lexopt" ,rust-lexopt-0.3)
                                   ("rust-owo-colors" ,rust-owo-colors-4)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis
     "A simple cross platform library for writing colored text to a terminal.")
    (description
     "This package provides a simple cross platform library for writing colored text
to a terminal.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-1
  (package
    (name "rust-anstyle")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g1ngvxrz9d6xsymxzzzg581jzyz1sn8d0jpjcwxks07cff2c0c9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-lexopt" ,rust-lexopt-0.3))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "ANSI text styling")
    (description "ANSI text styling")
    (license (list license:expat license:asl2.0))))

(define-public rust-codegenrs-3
  (package
    (name "rust-codegenrs")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "codegenrs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hjr5pp099aipm2apcd65a2fp2z6qsixnapsj3rbjgdiyixxzndx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-difference" ,rust-difference-2)
                       ("rust-normalize-line-endings" ,rust-normalize-line-endings-0.3))))
    (home-page "https://github.com/crate-ci/codegenrs")
    (synopsis "Moving code-gen our of build.rs")
    (description "Moving code-gen our of build.rs")
    (license (list license:expat license:asl2.0))))

(define-public rust-utf8parse-0.2
  (package
    (name "rust-utf8parse")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "utf8parse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02ip1a0az0qmc2786vxk2nqwsgcwf17d3a38fkf0q7hrmwh9c6vi"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alacritty/vte")
    (synopsis "Table-driven UTF-8 parser")
    (description "Table-driven UTF-8 parser")
    (license (list license:asl2.0 license:expat))))

(define-public rust-anstyle-query-1
  (package
    (name "rust-anstyle-query")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle-query" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j3na4b1nma39g4x7cwvj009awxckjf3z2vkwhldgka44hqj72g2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Look up colored console capabilities")
    (description "Look up colored console capabilities")
    (license (list license:expat license:asl2.0))))

(define-public rust-colorchoice-1
  (package
    (name "rust-colorchoice")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "colorchoice" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ix7w85kwvyybwi2jdkl3yva2r2bvdcc3ka2grjfzfgrapqimgxc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Global override of color control")
    (description "Global override of color control")
    (license (list license:expat license:asl2.0))))

(define-public rust-env-filter-0.1
  (package
    (name "rust-env-filter")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "env_filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1spp4jx0fissi0bg00d8nn4vnjwf6y3hr7d0vmcq65gb214al2d0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/rust-cli/env_logger")
    (synopsis "Filter log events using environment variables
")
    (description "Filter log events using environment variables")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-automata-0.4
  (package
    (name "rust-regex-automata")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex-automata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1spaq7y4im7s56d1gxa2hi4hzf6dwswb1bv8xyavzya7k25kpf46"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8))))
    (home-page "https://github.com/rust-lang/regex/tree/master/regex-automata")
    (synopsis "Automata construction and matching using regular expressions.")
    (description
     "Automata construction and matching using regular expressions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-1
  (package
    (name "rust-regex")
    (version "1.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k5sb0h2mkwf51ab0gvv3x38jp1q7wgxf63abfbhi0wwvvgxn5y1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-regex-automata" ,rust-regex-automata-0.4)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-env-logger" ,rust-env-logger-0.9)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-regex-test" ,rust-regex-test-0.1))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
     "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (description
     "An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-aho-corasick-1
  (package
    (name "rust-aho-corasick")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aho-corasick" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description "Fast multiple substring searching.")
    (license (list license:unlicense license:expat))))

(define-public rust-memchr-2
  (package
    (name "rust-memchr")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memchr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07bcqxb0vx4ji0648ny5xsicjnpma95x1n07v7mi7jrhsz2l11kc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-1))))
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis
     "Provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32) routines for
1, 2 or 3 byte search and single substring search.
")
    (description
     "This package provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32)
routines for 1, 2 or 3 byte search and single substring search.")
    (license (list license:unlicense license:expat))))

(define-public rust-regex-syntax-0.8
  (package
    (name "rust-regex-syntax")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex-syntax" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mhzkm1pkqg6y53xv056qciazlg47pq0czqs94cn302ckvi49bdd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1))))
    (home-page "https://github.com/rust-lang/regex/tree/master/regex-syntax")
    (synopsis "A regular expression parser.")
    (description "This package provides a regular expression parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-humantime-2
  (package
    (name "rust-humantime")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "humantime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-chrono" ,rust-chrono-0.4)
                                   ("rust-rand" ,rust-rand-0.6)
                                   ("rust-time" ,rust-time-0.1))))
    (home-page "https://github.com/tailhook/humantime")
    (synopsis
     "    A parser and formatter for std::time::{Duration, SystemTime}
")
    (description
     "This package provides a parser and formatter for std::time::{Duration,
@code{SystemTime}}")
    (license (list license:expat license:asl2.0))))

(define-public rust-itertools-0.12
  (package
    (name "rust-itertools")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itertools" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-either" ,rust-either-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-permutohedron" ,rust-permutohedron-0.2)
                                   ("rust-quickcheck" ,rust-quickcheck-0.9)
                                   ("rust-rand" ,rust-rand-0.7))))
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-itertools-0.14
  (package
    (name "rust-itertools")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itertools" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-either" ,rust-either-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-permutohedron" ,rust-permutohedron-0.2)
                                   ("rust-quickcheck" ,rust-quickcheck-0.9)
                                   ("rust-rand" ,rust-rand-0.7))))
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
     "Extra iterator adaptors, iterator methods, free functions, and macros")
    (description
     "This package provides Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-either-1
  (package
    (name "rust-either")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "either" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18l0cwyw18syl8b52syv6balql8mnwfyhihjqqllx5pms93iqz54"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/rayon-rs/either")
    (synopsis
     "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.
")
    (description
     "The enum `Either` with variants `Left` and `Right` is a general purpose sum type
with two cases.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ndarray-0.15
  (package
    (name "rust-ndarray")
    (version "0.15.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ndarray" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cpsm28hyk8qfjs4g9649dprv3hm53z12qqwyyjqbi3yjr72vcdd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-approx" ,rust-approx-0.4)
                       ("rust-approx" ,rust-approx-0.5)
                       ("rust-cblas-sys" ,rust-cblas-sys-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-matrixmultiply" ,rust-matrixmultiply-0.3)
                       ("rust-num-complex" ,rust-num-complex-0.4)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rawpointer" ,rust-rawpointer-0.2)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-approx" ,rust-approx-0.4)
                                   ("rust-defmac" ,rust-defmac-0.2)
                                   ("rust-itertools" ,rust-itertools-0.10)
                                   ("rust-quickcheck" ,rust-quickcheck-1))))
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis
     "An n-dimensional array for general elements and for numerics. Lightweight array views and slicing; views support chunking and splitting.")
    (description
     "An n-dimensional array for general elements and for numerics.  Lightweight array
views and slicing; views support chunking and splitting.")
    (license (list license:expat license:asl2.0))))

(define-public rust-matrixmultiply-0.3
  (package
    (name "rust-matrixmultiply")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "matrixmultiply" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1whgrp8ph7904aslqx87h9qm0ks4pxdj2nysffmrhiys6v7w2x3m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rawpointer" ,rust-rawpointer-0.2)
                       ("rust-thread-tree" ,rust-thread-tree-0.3))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-itertools" ,rust-itertools-0.8))))
    (home-page "https://github.com/bluss/matrixmultiply/")
    (synopsis
     "General matrix multiplication for f32 and f64 matrices. Operates on matrices with general layout (they can use arbitrary row and column stride). Detects and uses AVX or SSE2 on x86 platforms transparently for higher performance. Uses a microkernel strategy, so that the implementation is easy to parallelize and optimize.

Supports multithreading.")
    (description
     "General matrix multiplication for f32 and f64 matrices.  Operates on matrices
with general layout (they can use arbitrary row and column stride).  Detects and
uses AVX or SSE2 on x86 platforms transparently for higher performance.  Uses a
microkernel strategy, so that the implementation is easy to parallelize and
optimize.  Supports multithreading.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rawpointer-0.2
  (package
    (name "rust-rawpointer")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rawpointer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qy1qvj17yh957vhffnq6agq0brvylw27xgks171qrah75wmg8v0"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/rawpointer/")
    (synopsis
     "Extra methods for raw pointers and `NonNull<T>`.

For example `.post_inc()` and `.pre_dec()` (c.f. `ptr++` and `--ptr`),
`offset` and `add` for `NonNull<T>`, and the function `ptrdistance`.
")
    (description
     "Extra methods for raw pointers and `@code{NonNull<T>`}.  For example
`.post_inc()` and `.pre_dec()` (c.f. `ptr++` and `--ptr`), `offset` and `add`
for `@code{NonNull<T>`}, and the function `ptrdistance`.")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-complex-0.4
  (package
    (name "rust-num-complex")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-complex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19k89q0cd2jacd79z8ka95mmg0sx0fd1kpz01ycpr9clv8pn1ii3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytecheck" ,rust-bytecheck-0.6)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-num/num-complex")
    (synopsis "Complex numbers implementation for Rust")
    (description "Complex numbers implementation for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-traits-0.2
  (package
    (name "rust-num-traits")
    (version "0.2.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-traits" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yjib8p2p9kzmaz48xwhs69w5dh1wipph9jgnillzd2x33jz03fs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-libm" ,rust-libm-0.2))))
    (home-page "https://github.com/rust-num/num-traits")
    (synopsis "Numeric traits for generic mathematics")
    (description "Numeric traits for generic mathematics")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-integer-0.1
  (package
    (name "rust-num-integer")
    (version "0.1.46")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-integer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/rust-num/num-integer")
    (synopsis "Integer traits and functions")
    (description "Integer traits and functions")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-macros-backend-0.21
  (package
    (name "rust-pyo3-macros-backend")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-macros-backend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p58yp8ajlc8bq56wghw1syrjszmadasasdfpsjy3d9dychhf9h8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.21)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Code generation for PyO3 package")
    (description "Code generation for @code{PyO3} package")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-macros-0.21
  (package
    (name "rust-pyo3-macros")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g65z9yj2iffjrkrkzan9hwhhj7rrchh7lfv64dy30h6zill1cvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-pyo3-macros-backend" ,rust-pyo3-macros-backend-0.21)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Proc macros for PyO3 package")
    (description "Proc macros for @code{PyO3} package")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-ffi-0.21
  (package
    (name "rust-pyo3-ffi")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-ffi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00vlp4gmzn76gz250pz1r3fydd5r41nwl7dd9nmidfb0vi1migh1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.21))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Python-API bindings for the PyO3 ecosystem")
    (description "Python-API bindings for the @code{PyO3} ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-build-config-0.21
  (package
    (name "rust-pyo3-build-config")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-build-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l0vxvnqsbjrpdmysnwaljf4q3zcr1kb526qq23svzgs6mcdz0vq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-python3-dll-a" ,rust-python3-dll-a-0.2)
                       ("rust-python3-dll-a" ,rust-python3-dll-a-0.2)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Build configuration for the PyO3 ecosystem")
    (description "Build configuration for the @code{PyO3} ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-0.21
  (package
    (name "rust-pyo3")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n39y183jfqbyvimscqr0ysxxa6804pn46ivw048ww91lnb0pq55"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-tz" ,rust-chrono-tz-0.6)
                       ("rust-either" ,rust-either-1)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-hashbrown" ,rust-hashbrown-0.9)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-indoc" ,rust-indoc-2)
                       ("rust-inventory" ,rust-inventory-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memoffset" ,rust-memoffset-0.9)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-complex" ,rust-num-complex-0.2)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.21)
                       ("rust-pyo3-ffi" ,rust-pyo3-ffi-0.21)
                       ("rust-pyo3-macros" ,rust-pyo3-macros-0.21)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-unindent" ,rust-unindent-0.2))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Bindings to Python interpreter")
    (description "Bindings to Python interpreter")
    (license (list license:expat license:asl2.0))))

(define-public rust-numpy-0.21
  (package
    (name "rust-numpy")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "numpy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x1p5x7lwfc5nsccwj98sln5vx3g3n8sbgm5fmfmy5rpr8rhf5zc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-half" ,rust-half-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nalgebra" ,rust-nalgebra-0.32)
                       ("rust-ndarray" ,rust-ndarray-0.13)
                       ("rust-num-complex" ,rust-num-complex-0.2)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pyo3" ,rust-pyo3-0.21)
                       ("rust-rustc-hash" ,rust-rustc-hash-1))
       #:cargo-development-inputs (("rust-nalgebra" ,rust-nalgebra-0.32)
                                   ("rust-pyo3" ,rust-pyo3-0.21))))
    (native-inputs `(("python3" ,python-3)
                     ("numpy" ,python-numpy)))
    (home-page "https://github.com/PyO3/rust-numpy")
    (synopsis "PyO3-based Rust bindings of the NumPy C-API")
    (description "@code{PyO3-based} Rust bindings of the @code{NumPy} C-API")
    (license license:bsd-2)))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.79")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03mgcpad63z6ga7hx8hvi89bvvaf1aaf59csid0997m2n0bflvd5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-flate2" ,rust-flate2-1)
                                   ("rust-quote" ,rust-quote-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-tar" ,rust-tar-0.4))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "A substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.80")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03mgcpad63z6ga7hx8hvi89bvvaf1aaf59csid0997m2n0bflvd5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-flate2" ,rust-flate2-1)
                                   ("rust-quote" ,rust-quote-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-tar" ,rust-tar-0.4))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "A substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.81")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fiyxjg5x5nn4vnazz93dnirf0s3grdnbf63m44qyq94q2q9f59x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--lib")
       #:cargo-inputs (("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-flate2" ,rust-flate2-1)
                                   ("rust-quote" ,rust-quote-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-tar" ,rust-tar-0.4))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "A substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.60")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wr130gdrpgryyg6wz746w7rzbrmkn9g41xz3fgpg6qwgfy1i5ch"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-automod" ,rust-automod-1)
                                   ("rust-flate2" ,rust-flate2-1)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-reqwest" ,rust-reqwest-0.12)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-syn-test-suite" ,rust-syn-test-suite-0.0.0)
                                   ("rust-tar" ,rust-tar-0.4)
                                   ("rust-termcolor" ,rust-termcolor-1)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-hash-1
  (package
    (name "rust-rustc-hash")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustc-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/rustc-hash")
    (synopsis "speed, non-cryptographic hash used in rustc")
    (description "speed, non-cryptographic hash used in rustc")
    (license (list license:asl2.0 license:expat))))

(define-public rust-bitflags-1
  (package
    (name "rust-bitflags")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))
       (snippet #~(begin
                    (use-modules (guix build utils))
                    (substitute* "Cargo.toml"
                      (("^\\[dev-dependencies.serde\\].*$" all)
                       (string-append all "features = [\"derive\"]\n")))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags `("--" "--skip=fail")
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))
       #:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.")
    (description
     "This package provides a macro to generate structures which behave like bitflags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cc-1
  (package
    (name "rust-cc")
    (version "1.0.94")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rrm9yw419rwhnkdx9s31nlidqp2s5arf26ckwai3h4x48jf7xhp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-jobserver" ,rust-jobserver-0.1)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/rust-lang/cc-rs")
    (synopsis
     "A build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.
")
    (description
     "This package provides a build-time dependency for Cargo build scripts to assist
in invoking the native C compiler to compile native C code into a static archive
to be linked into Rust code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pkg-config-0.3
  (package
    (name "rust-pkg-config")
    (version "0.3.30")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkg-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v07557dj1sa0aly9c90wsygc0i8xv5vnmyv0g94lpkvj8qb4cfj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-lazy-static" ,rust-lazy-static-1))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'hardcode-pkg-config-location
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "src/lib.rs"
                        (("\"pkg-config\"")
                         (string-append "\""
                                        (assoc-ref inputs "pkg-config")
                                        "/bin/pkg-config\""))))))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/rust-lang/pkg-config-rs")
    (synopsis
     "A library to run the pkg-config system tool at build time in order to be used in
Cargo build scripts.
")
    (description
     "This package provides a library to run the pkg-config system tool at build time
in order to be used in Cargo build scripts.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-epoch-0.9
  (package
    (name "rust-crossbeam-epoch")
    (version "0.9.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-epoch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-loom" ,rust-loom-0.7))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
    (synopsis "Epoch-based garbage collection")
    (description "Epoch-based garbage collection")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-deque-0.8
  (package
    (name "rust-crossbeam-deque")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-deque" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03bp38ljx4wj6vvy4fbhx41q8f585zyqix6pncz1mkz93z08qgv1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description "Concurrent work-stealing deque")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-utils-0.8
  (package
    (name "rust-crossbeam-utils")
    (version "0.8.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0iakrb1b8fjqrag7wphl94d10irhbh2fw1g444xslsywqyn3p3i4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-loom" ,rust-loom-0.7))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description "Utilities for concurrent programming")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.198")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nb6hn1xpvhih00x6jkfm42na6pwz59h2zayj2x865xhd6wdm3p8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1.0.35)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.198")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k0z8mwkkl46bwfk16z7v8xidi5pwnj4a9fsf42k8cchjw6a8ilq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))
       #:cargo-development-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.116")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04r81f5myl41zrsyghnbmbl39c4n3azldb9zxfafnzyi4rqxn5ry"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-indoc" ,rust-indoc-2)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-stacker" ,rust-serde-stacker-0.1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-fnv-1
  (package
    (name "rust-fnv")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fnv" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/rust-fnv")
    (synopsis "FowlerâNollâVo hash function")
    (description "FowlerâNollâVo hash function")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ident-case-1
  (package
    (name "rust-ident-case")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ident_case" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/TedDriggs/ident_case")
    (synopsis "Utility for applying case rules to Rust identifiers.")
    (description "Utility for applying case rules to Rust identifiers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-strsim-0.10
  (package
    (name "rust-strsim")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strsim" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rapidfuzz/strsim-rs")
    (synopsis
     "Implementations of string similarity metrics. Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.
")
    (description
     "Implementations of string similarity metrics.  Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.")
    (license license:expat)))

(define-public rust-getrandom-0.2
  (package
    (name "rust-getrandom")
    (version "0.2.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "getrandom" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v18s6lpkvil6dkdfb86l84mwpqbpw6928qp0n0hj4dhxh32xcll"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--" "--skip=common::test_diff"
                            "--skip=common::test_huge"
                            "--skip=common::test_small"
                            "--skip=common::test_zero")
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))
       #:cargo-development-inputs (("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/rust-random/getrandom")
    (synopsis
     "A small cross-platform library for retrieving random data from system source")
    (description
     "This package provides a small cross-platform library for retrieving random data
from system source")
    (license (list license:expat license:asl2.0))))

(define-public rust-proptest-1
  (package
    (name "rust-proptest")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proptest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rdhjnf0xma5rmsq04d31n2vq1pgbm42pjc6jn3jsj8qgz09q38y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=num::test::contract_sanity::f32::range_from"
                            "--skip=num::test::contract_sanity::f32::range_to_inclusive"
                            "--skip=num::test::contract_sanity::f64::range_from"
                            "--skip=num::test::contract_sanity::f64::range_to_inclusive"
                            "--skip=string::test::askalono_0"
                            "--skip=string::test::askalono_9"
                            "--skip=string::test::comrak_0"
                            "--skip=string::test::linky_0"
                            "--skip=string::test::phone_number_0"
                            "--skip=string::test::phonenumber_5"
                            "--skip=string::test::spaceslugs_2"
                            "--skip=string::test::stache_0"
                            "--skip=string::test::fblog_0"
                            "--skip=string::test::fblog_1")
       #:cargo-inputs (("rust-bit-set" ,rust-bit-set-0.5)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-quick-error" ,rust-quick-error-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand-chacha" ,rust-rand-chacha-0.3)
                       ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.6)
                       ("rust-rusty-fork" ,rust-rusty-fork-0.3)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-x86" ,rust-x86-0.33))
       #:cargo-development-inputs (("rust-regex" ,rust-regex-1))))
    (home-page "https://proptest-rs.github.io/proptest/proptest/index.html")
    (synopsis "Hypothesis-like property-based testing and shrinking.")
    (description
     "This package provides Hypothesis-like property-based testing and shrinking.")
    (license (list license:expat license:asl2.0))))

(define-public rust-console-0.15
  (package
    (name "rust-console")
    (version "0.15.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "console" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sz4nl9nz8pkmapqni6py7jxzi7nzqjxzb3ya4kxvmkb0zy867qf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-encode-unicode" ,rust-encode-unicode-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-1)
                                   ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/console-rs/console")
    (synopsis "A terminal and console abstraction for Rust")
    (description
     "This package provides a terminal and console abstraction for Rust")
    (license license:expat)))

(define-public rust-lazy-static-1
  (package
    (name "rust-lazy-static")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lazy_static" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-spin" ,rust-spin-0.5))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/rust-lang-nursery/lazy-static.rs")
    (synopsis "A macro for declaring lazily evaluated statics in Rust.")
    (description
     "This package provides a macro for declaring lazily evaluated statics in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-width-0.1
  (package
    (name "rust-unicode-width")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-width" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11ds4ydhg8g7l06rlmh712q41qsrd0j0h00n1jm74kww3kqk65z5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-rustc-std-workspace-std" ,rust-rustc-std-workspace-std-1))))
    (home-page "https://github.com/unicode-rs/unicode-width")
    (synopsis "Determine displayed width of `char` and `str` types
according to Unicode Standard Annex #11 rules.
")
    (description
     "Determine displayed width of `char` and `str` types according to Unicode
Standard Annex #11 rules.")
    (license (list license:expat license:asl2.0))))

(define-public rust-monostate-impl-0.1
  (package
    (name "rust-monostate-impl")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "monostate-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19h5660c68v54g4y1k6mm964icvjwpzdm3f8rq89qykppnxpqc5z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/monostate")
    (synopsis "Implementation detail of the monostate crate")
    (description "Implementation detail of the monostate crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-monostate-0.1
  (package
    (name "rust-monostate")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "monostate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gzqi8gplj9nrgnz8pc9j0gv8iq182y1m9rnw0qrvim4ik6zy3x2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-monostate-impl" ,rust-monostate-impl-0.1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/dtolnay/monostate")
    (synopsis "Type that deserializes only from one specific value")
    (description "Type that deserializes only from one specific value")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-0.8
  (package
    (name "rust-rand")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rand" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-packed-simd-2" ,rust-packed-simd-2-0.3)
                       ("rust-rand-chacha" ,rust-rand-chacha-0.3)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-rand-pcg" ,rust-rand-pcg-0.3))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Random number generators and other randomness functionality.
")
    (description
     "Random number generators and other randomness functionality.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-chacha-0.3
  (package
    (name "rust-rand-chacha")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rand_chacha" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ppv-lite86" ,rust-ppv-lite86-0.2)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "ChaCha random number generator
")
    (description "@code{ChaCha} random number generator")
    (license (list license:expat license:asl2.0))))

(define-public rust-ppv-lite86-0.2
  (package
    (name "rust-ppv-lite86")
    (version "0.2.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ppv-lite86" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pp6g52aw970adv3x2310n7glqnji96z0a9wiamzw89ibf0ayh2v"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cryptocorrosion/cryptocorrosion")
    (synopsis "Implementation of the crypto-simd API for x86")
    (description "Implementation of the crypto-simd API for x86")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-core-0.6
  (package
    (name "rust-rand-core")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rand_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--lib")
       #:cargo-inputs (("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://rust-random.github.io/book")
    (synopsis
     "Core random number generator traits and tools for implementation.
")
    (description
     "Core random number generator traits and tools for implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokenizers-0.19
  (package
    (name "rust-tokenizers")
    (version "0.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokenizers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zg6ffpllygijb5bh227m9p4lrhf0pjkysky68kddwrsvp8zl075"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=lstrip_tokens"
                            "--skip=rstrip_tokens"
                            "--skip=overlapping_tokens"
                            "--skip=single_word_tokens"
                            "--skip=quicktour_slow_train"
                            "--skip=train_pipeline_bert"
                            "--skip=pipeline"
                            "--skip=pipeline_bert"
                            "--skip=quicktour"
                            "--skip=load_tokenizer"
                            "--skip=train_tokenizer"
                            "--skip=byte_level_pre_tokenized_sequence_with_trimming"
                            "--skip=byte_level_pre_tokenized_sequence"
                            "--skip=byte_level_basic"
                            "--skip=byte_level_double_sequence"
                            "--skip=byte_level_unicode"
                            "--skip=split_on_added_tokens_bert"
                            "--skip=bpe_serde"
                            "--skip=test_deserialize_long_file"
                            "--skip=wordlevel_serde"
                            "--skip=wordpiece_serde"
                            "--skip=bpe_values_after_training"
                            "--skip=bpe_continuing_subword_prefix_error"
                            "--skip=test_unigram_from_file"
                            "--skip=test_train_unigram_from_file")
       #:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-derive-builder" ,rust-derive-builder-0.20)
                       ("rust-esaxx-rs" ,rust-esaxx-rs-0.1)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-hf-hub" ,rust-hf-hub-0.3)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-macro-rules-attribute" ,rust-macro-rules-attribute-0.2)
                       ("rust-monostate" ,rust-monostate-0.1)
                       ("rust-onig" ,rust-onig-6)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rayon-cond" ,rust-rayon-cond-0.3)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-spm-precompiled" ,rust-spm-precompiled-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-normalization-alignments" ,rust-unicode-normalization-alignments-0.1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-categories" ,rust-unicode-categories-0.1))
       #:cargo-development-inputs (("rust-assert-approx-eq" ,rust-assert-approx-eq-1)
                                   ("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/huggingface/tokenizers")
    (synopsis "Provides an implementation of today's most used tokenizers,
with a focus on performances and versatility.
")
    (description
     "This package provides an implementation of today's most used tokenizers, with a
focus on performances and versatility.")
    (license license:asl2.0)))

(define-public rust-csv-1
  (package
    (name "rust-csv")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "csv" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q9nqn0qlamwl18v57p82c8yhxy43lkzf2z1mndmycsvqinkm092"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-0.2)
                       ("rust-csv-core" ,rust-csv-core-0.1)
                       ("rust-itoa" ,rust-itoa-0.4)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/BurntSushi/rust-csv")
    (synopsis "Fast CSV parsing with support for serde.")
    (description "Fast CSV parsing with support for serde.")
    (license (list license:unlicense license:expat))))

(define-public rust-once-cell-1
  (package
    (name "rust-once-cell")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "once_cell" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l4wgryqr9k6l59apms2g7kgcnx9c87ih02nh4s003ci3svlr86n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-parking-lot" ,rust-parking-lot-0.9))
       #:cargo-development-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/matklad/once_cell")
    (synopsis "Single assignment cells and lazy values.")
    (description "Single assignment cells and lazy values.")
    (license (list license:expat license:asl2.0))))

(define-public rust-self-cell-1
  (package
    (name "rust-self-cell")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Voultapher/self_cell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bd25higygacc00aay0pzgilv10vdsb31gqsk3vxxd6d9k6zlrk6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustversion" ,rust-rustversion-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/Voultapher/self_cell")
    (synopsis "Self-referential structs in stable Rust")
    (description
     "This package provides safe-to-use proc-macro-free self-referential structs
in stable Rust.")
    (license license:asl2.0)))

(define-public rust-indexmap-1
  (package
    (name "rust-indexmap")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indexmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cxx6d5zsh3hhp2c3hyv2xyqvgg4zx68jh0ragi68ygxd9v94qvc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-rayon" ,rust-rustc-rayon-0.4)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-fnv" ,rust-fnv-1)
                                   ("rust-fxhash" ,rust-fxhash-0.2)
                                   ("rust-itertools" ,rust-itertools-0.10)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/indexmap-rs/indexmap")
    (synopsis "A hash table with consistent order and fast iteration.")
    (description
     "This package provides a hash table with consistent order and fast iteration.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-memo-map-0.3
  (package
    (name "rust-memo-map")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memo-map" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10va9wc8jbc6vs0924ak0ac10rdw7i3h6c9jrga657pi5mdk6k1p"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mitsuhiko/memo-map")
    (synopsis "A crate implementing a synchronized map for memoization")
    (description
     "This package provides a crate implementing a synchronized map for memoization")
    (license license:asl2.0)))

(define-public rust-percent-encoding-2
  (package
    (name "rust-percent-encoding")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "percent-encoding" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13nrpp6r1f4k14viksga3094krcrxgv4b42kqbriy63k7ln5g327"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/rust-url/")
    (synopsis "Percent encoding and decoding")
    (description "Percent encoding and decoding")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.114")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q4saigxwkf8bw4y5kp6k33dnavlvvwa2q4zmag59vrjsqdrpw65"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itoa" ,rust-itoa-0.4)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-stacker" ,rust-serde-stacker-0.1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-stacker-0.1
  (package
    (name "rust-stacker")
    (version "0.1.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stacker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1klz4mk1iqn3jixhnls6ia4ql4fpinnfjibxabpx6pqmh12bv1n8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-psm" ,rust-psm-0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rust-lang/stacker")
    (synopsis
     "A stack growth library useful when implementing deeply recursive algorithms that
may accidentally blow the stack.
")
    (description
     "This package provides a stack growth library useful when implementing deeply
recursive algorithms that may accidentally blow the stack.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicase-2
  (package
    (name "rust-unicase")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicase" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xmlbink4ycgxrkjspp0mf7pghcx4m7vxq7fpfm04ikr2zk7pwsh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/seanmonstar/unicase")
    (synopsis "A case-insensitive wrapper around strings.")
    (description
     "This package provides a case-insensitive wrapper around strings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-ident-1
  (package
    (name "rust-unicode-ident")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-ident" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wznr6ax3jl09vxkvj4a62vip2avfgif13js9sflkjg4b6fv7skc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-fst" ,rust-fst-0.4)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-roaring" ,rust-roaring-0.10)
                                   ("rust-ucd-trie" ,rust-ucd-trie-0.1)
                                   ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://github.com/dtolnay/unicode-ident")
    (synopsis
     "Determine whether characters have the XID_Start or XID_Continue properties according to Unicode Standard Annex #31")
    (description
     "Determine whether characters have the XID_Start or XID_Continue properties
according to Unicode Standard Annex #31")
    (license (list license:expat license:expat license:expat))))

(define-public rust-v-htmlescape-0.15
  (package
    (name "rust-v-htmlescape")
    (version "0.15.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "v_htmlescape" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "135inp4x7cc32k0hzrymlz1baf0rj0ah5h82nrpa9w0hqpxmg0jf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-buf-min" ,rust-buf-min-0.7))))
    (home-page "https://github.com/botika/v_escape")
    (synopsis "The simd optimized HTML escaping code")
    (description "The simd optimized HTML escaping code")
    (license (list license:expat license:asl2.0))))

(define-public rust-insta-1
  (package
    (name "rust-insta")
    (version "1.35.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "insta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qpr5pndsbfc53nn1ppb8vlbz8l17m471r5diz2i7kwrxwdmr63w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=utils::test_format_rust_expression")
       #:cargo-inputs (("rust-console" ,rust-console-0.15)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-globset" ,rust-globset-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
                       ("rust-pest" ,rust-pest-2)
                       ("rust-pest-derive" ,rust-pest-derive-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-ron" ,rust-ron-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-similar" ,rust-similar-2)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-yaml-rust" ,rust-yaml-rust-0.4))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1)
                                   ("rust-similar-asserts" ,rust-similar-asserts-1))))
    (home-page "https://insta.rs/")
    (synopsis "A snapshot testing library for Rust")
    (description "This package provides a snapshot testing library for Rust")
    (license license:asl2.0)))

(define-public rust-insta-1
  (package
    (inherit rust-insta-1)
    (name "rust-insta")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "insta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k2kbpyw7wp1s9n7pw9wcg0rw5aii3lz023j0ck6bjjrivsp7ary"))))))

(define-public rust-insta-1
  (package
    (inherit rust-insta-1)
    (name "rust-insta")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "insta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xd64139vi0hrxs8l8k3r1xqf2x8b0x5chsh47lwkqj85l2fc2l1"))))
    (build-system cargo-build-system)))

(define-public rust-similar-asserts-1
  (package
    (name "rust-similar-asserts")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "similar-asserts" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03zwg4vy2c258v8sa13snfpz22akcqdxa49l467s3z0vgn1bnhg0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-console" ,rust-console-0.15)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-similar" ,rust-similar-2))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/mitsuhiko/similar-asserts")
    (synopsis "provides assert_eq! like macros with colorized diff output")
    (description "provides assert_eq! like macros with colorized diff output")
    (license license:asl2.0)))

(define-public rust-actix-web-codegen-4
  (package
    (name "rust-actix-web-codegen")
    (version "4.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xalrv1s7imzfgxyql6zii5bpxxkk11rlcc8n4ia3v1hpgmm07zb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-router" ,rust-actix-router-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://actix.rs")
    (synopsis "Routing and runtime macros for Actix Web")
    (description "Routing and runtime macros for Actix Web")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-server-2
  (package
    (name "rust-actix-server")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m62qbg7vl1wddr6mm8sd4rnvd3w5v3zcn8fmdpfl8q4xxz3xc9y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-uring" ,rust-tokio-uring-0.4)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://actix.rs")
    (synopsis "General purpose TCP server built for the Actix ecosystem")
    (description "General purpose TCP server built for the Actix ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-router-0.5
  (package
    (name "rust-actix-router")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-router" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04f5cdag2h9lbrgb0pzwznpfrl3ajbdxlsvb8a2kci1rcmcpa96j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytestring" ,rust-bytestring-0.1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/actix/actix-web")
    (synopsis "Resource path matching and router")
    (description "Resource path matching and router")
    (license (list license:expat license:asl2.0))))

(define-public rust-local-channel-0.1
  (package
    (name "rust-local-channel")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "local-channel" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j1ywn459kl4fdmjfyljm379k40qwwscd7mqp25lppxqd5gcijxn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-local-waker" ,rust-local-waker-0.1))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis
     "A non-threadsafe multi-producer, single-consumer, futures-aware, FIFO queue")
    (description
     "This package provides a non-threadsafe multi-producer, single-consumer,
futures-aware, FIFO queue")
    (license (list license:expat license:asl2.0))))

(define-public rust-h2-0.3
  (package
    (name "rust-h2")
    (version "0.3.26")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s7msnfv7xprzs6xzfj5sg6p8bjcdpcqcmjjbkd345cyi1x55zl1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/hyperium/h2")
    (synopsis "An HTTP/2 client and server")
    (description "An HTTP/2 client and server")
    (license license:expat)))

(define-public rust-bytestring-1
  (package
    (name "rust-bytestring")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytestring" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wpf0c5c72x3ycdb85vznkmcy8fy6ckzd512064dyabbx81h5n3l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://actix.rs")
    (synopsis "A UTF-8 encoded read-only string using `Bytes` as storage")
    (description
     "This package provides a UTF-8 encoded read-only string using `Bytes` as storage")
    (license (list license:expat license:asl2.0))))

(define-public rust-impl-more-0.1
  (package
    (name "rust-impl-more")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "impl-more" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bdv06br4p766rcgihhjwqyz8fcz31xyaq14rr53vfh3kifafv10"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/robjtede/impl-more")
    (synopsis "Concise, declarative trait implementation macros")
    (description "Concise, declarative trait implementation macros")
    (license (list license:expat license:asl2.0))))

(define-public rust-local-waker-0.1
  (package
    (name "rust-local-waker")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "local-waker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11vlcm8q6dhdf0srkgjnwca48dn9zcz820fq20hv82ffcxy3v1sd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/actix/actix-net")
    (synopsis "A synchronization primitive for thread-local task wakeup")
    (description
     "This package provides a synchronization primitive for thread-local task wakeup")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-utils-3
  (package
    (name "rust-actix-utils")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n05nzwdkx6jhmzr6f9qsh57a8hqlwv5rjz1i0j3qvj6y7gxr8c8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-local-waker" ,rust-local-waker-0.1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis "Various utilities used in the Actix ecosystem")
    (description "Various utilities used in the Actix ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-tls-3
  (package
    (name "rust-actix-tls")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pxyqpr5nangbl55gdc5zpc84viah7qhmpjw5v3pnirb5w5fdk6l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http" ,rust-http-1)
                       ("rust-impl-more" ,rust-impl-more-0.1)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/actix/actix-net.git")
    (synopsis "TLS acceptor and connector services for Actix ecosystem")
    (description "TLS acceptor and connector services for Actix ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-service-2
  (package
    (name "rust-actix-service")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-service" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fipjcc5kma7j47jfrw55qm09dakgvx617jbriydrkqqz10lk29v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis
     "Service trait and combinators for representing asynchronous request/response operations.")
    (description
     "Service trait and combinators for representing asynchronous request/response
operations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-http-3
  (package
    (name "rust-actix-http")
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hx7rjc1cwrrql5qmihl31hf7nblwyd6a4mvhcghvz41shzv28yj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-codec" ,rust-actix-codec-0.5)
                       ("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-tls" ,rust-actix-tls-3)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-ahash" ,rust-ahash-0.8)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-brotli" ,rust-brotli-3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-bytestring" ,rust-bytestring-1)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-language-tags" ,rust-language-tags-0.3)
                       ("rust-local-channel" ,rust-local-channel-0.1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-zstd" ,rust-zstd-0.13))))
    (home-page "https://actix.rs")
    (synopsis "HTTP types and services for the Actix ecosystem")
    (description "HTTP types and services for the Actix ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-codec-0.5
  (package
    (name "rust-actix-codec")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-codec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12m2jxysk2xpxi193340zv4w215cv9fyyna7rxvzh6wck0hhlysz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis "Codec utilities for working with framed protocols")
    (description "Codec utilities for working with framed protocols")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-web-4
  (package
    (name "rust-actix-web")
    (version "4.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-web" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1159grrp031zy9j97vr3c376w8pdawr8akbib0iqqqxvvrnmb9j3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-codec" ,rust-actix-codec-0.5)
                       ("rust-actix-http" ,rust-actix-http-3)
                       ("rust-actix-macros" ,rust-actix-macros-0.2)
                       ("rust-actix-router" ,rust-actix-router-0.5)
                       ("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-server" ,rust-actix-server-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-tls" ,rust-actix-tls-3)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-actix-web-codegen" ,rust-actix-web-codegen-4)
                       ("rust-ahash" ,rust-ahash-0.8)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-bytestring" ,rust-bytestring-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cookie" ,rust-cookie-0.16)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-language-tags" ,rust-language-tags-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://actix.rs")
    (synopsis
     "Actix Web is a powerful, pragmatic, and extremely fast web framework for Rust")
    (description
     "Actix Web is a powerful, pragmatic, and extremely fast web framework for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-notify-5
  (package
    (name "rust-notify")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "notify" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11l4r19z69mplx124hsvm2flf740ykykwkzs7vz46njmrbhn77vj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fsevent-sys" ,rust-fsevent-sys-4)
                       ("rust-inotify" ,rust-inotify-0.9)
                       ("rust-kqueue" ,rust-kqueue-1)
                       ("rust-kqueue" ,rust-kqueue-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))
       #:cargo-development-inputs (("rust-nix" ,rust-nix-0.23)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/notify-rs/notify")
    (synopsis "Cross-platform filesystem notification library")
    (description "Cross-platform filesystem notification library")
    (license (list license:cc0 license:artistic2.0))))

(define-public rust-notify-6
  (package
    (name "rust-notify")
    (version "6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "notify" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bad98r0ilkhhq2jg3zs11zcqasgbvxia8224wpasm74n65vs1b2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fsevent-sys" ,rust-fsevent-sys-4)
                       ("rust-inotify" ,rust-inotify-0.9)
                       ("rust-kqueue" ,rust-kqueue-1)
                       ("rust-kqueue" ,rust-kqueue-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-nix" ,rust-nix-0.23)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/notify-rs/notify")
    (synopsis "Cross-platform filesystem notification library")
    (description "Cross-platform filesystem notification library")
    (license license:cc0)))

(define-public rust-notify-types-2
  (package
    (name "rust-notify-types")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "notify-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pcjm3wnvb7pvzw6mn89csv64ip0xhx857kr8jic5vddi6ljc22y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-web-time" ,rust-web-time-1))))
    (home-page "https://github.com/notify-rs/notify")
    (synopsis "Types used by the notify crate")
    (description "This package provides Types used by the notify crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-notify-8
  (package
    (name "rust-notify")
    (version "8.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "notify" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hz9ab68gsbkidms6kgl4v7capfqjyl40vpfdarcfsnnnc1q9vig"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fsevent-sys" ,rust-fsevent-sys-4)
                       ("rust-inotify" ,rust-inotify-0.11)
                       ("rust-kqueue" ,rust-kqueue-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mio" ,rust-mio-1)
                       ("rust-notify-types" ,rust-notify-types-2)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))
       #:cargo-development-inputs (("rust-insta" ,rust-insta-1)
                                   ("rust-nix" ,rust-nix-0.29)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/notify-rs/notify")
    (synopsis "Cross-platform filesystem notification library")
    (description
     "This package provides Cross-platform filesystem notification library.")
    (license license:cc0)))

(define-public rust-time-macros-0.2
  (package
    (name "rust-time-macros")
    (version "0.2.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "time-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x3pahhk2751c6kqqq9dk6lz0gydbnxr44q01wpjlrz687ps78vv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-conv" ,rust-num-conv-0.1)
                       ("rust-time-core" ,rust-time-core-0.1))))
    (home-page "https://github.com/time-rs/time")
    (synopsis
     "    Procedural macros for the time crate.
    This crate is an implementation detail and should not be relied upon directly.
")
    (description
     "Procedural macros for the time crate.  This crate is an implementation detail
and should not be relied upon directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-conv-0.1
  (package
    (name "rust-num-conv")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-conv" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/jhpratt/num-conv")
    (synopsis
     "`num_conv` is a crate to convert between integer types without using `as` casts. This provides
better certainty when refactoring, makes the exact behavior of code more explicit, and allows using
turbofish syntax.
")
    (description
     "`num_conv` is a crate to convert between integer types without using `as` casts.
 This provides better certainty when refactoring, makes the exact behavior of
code more explicit, and allows using turbofish syntax.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-sys-0.32
  (package
    (name "rust-windows-sys")
    (version "0.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1imdvrsbivgrvzb9261aa77ws391l24s3r1b0wna34jz31vf9xix"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.32)
                       ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.32)
                       ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.32)
                       ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.32)
                       ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.32))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "Rust for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-tz-1
  (package
    (name "rust-time-tz")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "time-tz" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m69x2dc671r8wa8rrwvnv5br365bns01yjlglqq32phzmfzc8m4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-parse-zoneinfo" ,rust-parse-zoneinfo-0.3)
                       ("rust-phf" ,rust-phf-0.11)
                       ("rust-phf-codegen" ,rust-phf-codegen-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.5)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-windows-sys" ,rust-windows-sys-0.32))))
    (home-page "https://github.com/Yuri6037/time-tz")
    (synopsis "Implementation of tz database (IANA) for the time Rust crate.")
    (description
     "Implementation of tz database (IANA) for the time Rust crate.")
    (license license:bsd-3)))

(define-public rust-time-0.3
  (package
    (name "rust-time")
    (version "0.3.34")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jc7wgprzqjhzd0nqkbmdlnjwyddnswmjw86ni2vq55v45jqn968"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-deranged" ,rust-deranged-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-num-conv" ,rust-num-conv-0.1)
                       ("rust-num-threads" ,rust-num-threads-0.1)
                       ("rust-powerfmt" ,rust-powerfmt-0.2)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time-core" ,rust-time-core-0.1)
                       ("rust-time-macros" ,rust-time-macros-0.2))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-num-conv" ,rust-num-conv-0.1)
                                   ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rstest" ,rust-rstest-0.18)
                                   ("rust-rstest-reuse" ,rust-rstest-reuse-0.6)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-time-macros" ,rust-time-macros-0.2)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://time-rs.github.io")
    (synopsis
     "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
     "Date and time library.  Fully interoperable with the standard library.  Mostly
compatible with #![no_std].")
    (license (list license:expat license:asl2.0))))

(define-public rust-pure-rust-locales-0.5
  (package
    (name "rust-pure-rust-locales")
    (version "0.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pure-rust-locales" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n1jqy8g7ph9lvzncc8vy5jaqq2dljryp1agcnp5pwwi9zy4jp5l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/cecton/pure-rust-locales")
    (synopsis
     "Pure Rust locales imported directly from the GNU C Library. `LC_COLLATE` and `LC_CTYPE` are not yet supported.")
    (description
     "Pure Rust locales imported directly from the GNU C Library. `LC_COLLATE` and
`LC_CTYPE` are not yet supported.")
    (license (list license:expat license:asl2.0))))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.25")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chrono" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00zvdvkvvnpbs2ncxfpbvrscdwwv3nvl2fiz2zlbrrd9gp9kgg7x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=test_time_zone_from_posix_tz")
       #:cargo-inputs (("rust-android-tzdata" ,rust-android-tzdata-0.1)
                       ("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-criterion" ,rust-criterion-0.4)
                       ("rust-iana-time-zone" ,rust-iana-time-zone-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pure-rust-locales" ,rust-pure-rust-locales-0.5)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time" ,rust-time-0.1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-minijinja-2
  (package
    (name "rust-minijinja")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "minijinja" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f33jlf7wr6ak1r0dg20920k3n3na6lm8jsy57ajmm8693lx0rbi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-memo-map" ,rust-memo-map-0.3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-self-cell" ,rust-self-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-stacker" ,rust-stacker-0.1)
                       ("rust-unicase" ,rust-unicase-2)
                       ("rust-unicode-ident" ,rust-unicode-ident-1)
                       ("rust-v-htmlescape" ,rust-v-htmlescape-0.15))
       #:cargo-development-inputs (("rust-insta" ,rust-insta-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-similar-asserts" ,rust-similar-asserts-1))))
    (inputs (list rust-self-cell-1))
    (home-page "https://github.com/mitsuhiko/minijinja")
    (synopsis "a powerful template engine for Rust with minimal dependencies")
    (description
     "a powerful template engine for Rust with minimal dependencies")
    (license license:asl2.0)))

(define-public rust-minijinja-contrib-2
  (package
    (name "rust-minijinja-contrib")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "minijinja-contrib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07m9g58dd62yvx56fqy99b9x055j88m044ldmy5pq1sknm39m95j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-minijinja" ,rust-minijinja-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-time-tz" ,rust-time-tz-1)
                       ("rust-v-htmlescape" ,rust-v-htmlescape-0.15))
       #:cargo-development-inputs (("rust-minijinja" ,rust-minijinja-2)
                                   ("rust-similar-asserts" ,rust-similar-asserts-1)
                                   ("rust-chrono" ,rust-chrono-0.4))))
    (inputs (list rust-self-cell-1))
    (home-page "https://github.com/mitsuhiko/minijinja")
    (synopsis "Template engine for Rust")
    (description
     "@code{MiniJinja-Contrib} is a utility crate for @code{MiniJinja} that adds support for certain utilities that are too specific for the @code{MiniJinja} core. This is usually because they provide functionality that @{Jinja2} itself does not have.")
    (license license:asl2.0)))

(define-public rust-annotate-snippets-0.9
  (package
    (name "rust-annotate-snippets")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "annotate-snippets" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07p8r6jzb7nqydq0kr5pllckqcdxlyld2g275v425axnzffpxbyc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-yansi-term" ,rust-yansi-term-0.1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-difference" ,rust-difference-2)
                                   ("rust-glob" ,rust-glob-0.3)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-toml" ,rust-toml-0.5)
                                   ("rust-yansi-term" ,rust-yansi-term-0.1))))
    (home-page "https://github.com/rust-lang/annotate-snippets-rs")
    (synopsis "Library for building code annotations")
    (description "Library for building code annotations")
    (license (list license:asl2.0 license:expat))))

(define-public rust-annotate-snippets-0.11
  (package
    (name "rust-annotate-snippets")
    (version "0.11.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "annotate-snippets" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i1bmr5vy957l8fvivj9x1xs24np0k56rdgwj0bxqk45b2p8w3ki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstyle" ,rust-anstyle-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-unicode-width" ,rust-unicode-width-0.2))
       #:cargo-development-inputs (("rust-anstream" ,rust-anstream-0.6)
                                   ("rust-difference" ,rust-difference-2)
                                   ("rust-divan" ,rust-divan-0.1)
                                   ("rust-glob" ,rust-glob-0.3)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-snapbox" ,rust-snapbox-0.6)
                                   ("rust-toml" ,rust-toml-0.8)
                                   ("rust-tryfn" ,rust-tryfn-0.2))))
    (home-page "https://github.com/rust-lang/annotate-snippets-rs")
    (synopsis "Library for building code annotations")
    (description
     "This package provides Library for building code annotations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anyhow-1
  (package
    (name "rust-anyhow")
    (version "1.0.80")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anyhow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qdlk0mbf6mycr9rxyfc0ic9n8nn5v6pgh4qf07p6qa15vjjrlss"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-syn" ,rust-syn-2)
                                   ("rust-thiserror" ,rust-thiserror-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/anyhow")
    (synopsis "Flexible concrete Error type built on std::error::Error")
    (description "Flexible concrete Error type built on std::error::Error")
    (license (list license:expat license:asl2.0))))

(define-public rust-heck-0.5
  (package
    (name "rust-heck")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "heck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description "heck is a case conversion library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-4
  (package
    (name "rust-clap-derive")
    (version "4.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r2gs2p10pb435w52xzsgz2mmx5qd3qfkmk29y4mbz9ph11k30aj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_derive")
    (synopsis
     "Parse command line argument by defining a struct, derive crate.")
    (description
     "Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-strsim-0.11
  (package
    (name "rust-strsim")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strsim" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rapidfuzz/strsim-rs")
    (synopsis
     "Implementations of string similarity metrics. Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.
")
    (description
     "Implementations of string similarity metrics.  Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.")
    (license license:expat)))

(define-public rust-clap-lex-0.7
  (package
    (name "rust-clap-lex")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_lex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kh1sckgq71kay2rrr149pl9gbsrvyccsq6xm5xpnq0cxnyqzk4q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_lex")
    (synopsis "Minimal, flexible command line parser")
    (description "Minimal, flexible command line parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-builder-4
  (package
    (name "rust-clap-builder")
    (version "4.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d7p4hph4fyhaphkf0v5zv0kq4lz25a9jq2f901yrq3afqp9w4mf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-clap-lex" ,rust-clap-lex-0.7)
                       ("rust-strsim" ,rust-strsim-0.11)
                       ("rust-terminal-size" ,rust-terminal-size-0.3)
                       ("rust-unicase" ,rust-unicase-2)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-4
  (package
    (name "rust-clap")
    (version "4.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1828wm9qws5gh2xnimnvmp2vria6d6hsxnqmhnm84dwjcxm0dg4h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap-builder" ,rust-clap-builder-4)
                       ("rust-clap-derive" ,rust-clap-derive-4))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-winsplit-0.1
  (package
    (name "rust-winsplit")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winsplit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mnr78r4h9vsk1hiy8dvfl2n899p74k3alwsqcsjz9x65lsh7drs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/chipsenkbeil/winsplit-rs")
    (synopsis
     "Library to split string into command line arguments mirroring CommandLineToArgV with VC++ 2008 parsing rules")
    (description
     "Library to split string into command line arguments mirroring
@code{CommandLineToArgV} with VC++ 2008 parsing rules")
    (license (list license:expat license:asl2.0))))

(define-public rust-argfile-0.2
  (package
    (name "rust-argfile")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "argfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d2500nw7yv6q4iib3zlp91m0vvvv8pd2f90jfmgh3h833jciidp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fs-err" ,rust-fs-err-2)
                       ("rust-os-str-bytes" ,rust-os-str-bytes-6)
                       ("rust-winsplit" ,rust-winsplit-0.1))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-4)
                                   ("rust-wild" ,rust-wild-2))))
    (home-page "https://github.com/rust-cli/argfile")
    (synopsis "Load additional CLI args from file")
    (description "Load additional CLI args from file")
    (license (list license:expat license:asl2.0))))

(define-public rust-bincode-1
  (package
    (name "rust-bincode")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bincode" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bfw3mnwzx5g1465kiqllp5n4r10qrqy88kdlp3jfwnq2ya5xx5i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/bincode-org/bincode")
    (synopsis
     "A binary serialization / deserialization strategy for transforming structs into bytes and vice versa!")
    (description
     "This package provides a binary serialization / deserialization strategy for
transforming structs into bytes and vice versa!")
    (license license:expat)))

(define-public rust-bitflags-2
  (package
    (name "rust-bitflags")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h91vdx1il069vdiiissj8ymzj130rbiic0dbs77yxjgjim9sjyg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                                   ("rust-bytemuck" ,rust-bytemuck-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-zerocopy" ,rust-zerocopy-0.6))))
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.
")
    (description
     "This package provides a macro to generate structures which behave like bitflags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bitflags-2
  (package
    (name "rust-bitflags")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dixc6168i98652jxf0z9nbyn0zcis3g6hi6qdr7z5dbhcygas4g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "macro to generate structures which behave like bitflags.")
    (description
     "This package provides a macro to generate structures which behave like bitflags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bstr-1
  (package
    (name "rust-bstr")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bstr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01ipr5rncw3kf4dyc1p2g00njn1df2b0xpviwhb8830iv77wbvq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-memchr" ,rust-memchr-2)
                       ("rust-regex-automata" ,rust-regex-automata-0.4)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-ucd-parse" ,rust-ucd-parse-0.1)
                                   ("rust-unicode-segmentation" ,rust-unicode-segmentation-1))))
    (home-page "https://github.com/BurntSushi/bstr")
    (synopsis "A string type that is not required to be valid UTF-8.")
    (description
     "This package provides a string type that is not required to be valid UTF-8.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cachedir-0.3
  (package
    (name "rust-cachedir")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cachedir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wyqx30crm2qsq4ny57hhljyq6iw6j4qfg7fbfiqznvpf29z60s7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/jstasiak/cachedir")
    (synopsis
     "A library to help interacting with cache directories and CACHEDIR.TAG files.")
    (description
     "This package provides a library to help interacting with cache directories and
CACHEDIR.TAG files.")
    (license license:expat)))

(define-public rust-windows-metadata-0.54
  (package
    (name "rust-windows-metadata")
    (version "0.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hh4bpima19p18kr5a2ss46hgmgafjkqzyfzhm0dazvx6sw70hz4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows metadata reader")
    (description "Windows metadata reader")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-bindgen-0.54
  (package
    (name "rust-windows-bindgen")
    (version "0.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hid039rnygimc2kxkzfc892j6hcdjpza2490ggz35r8fjs7csfq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-windows-metadata" ,rust-windows-metadata-0.54))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows metadata compiler")
    (description "Windows metadata compiler")
    (license (list license:expat license:asl2.0))))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.35")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chrono" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16k3caxzip1ql827pz5rj7aqfqy7yhpxyxzb5wqkj2mwvh1mkbwf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-android-tzdata" ,rust-android-tzdata-0.1)
                       ("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-iana-time-zone" ,rust-iana-time-zone-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pure-rust-locales" ,rust-pure-rust-locales-0.8)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-windows-bindgen" ,rust-windows-bindgen-0.54))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-automod-1
  (package
    (name "rust-automod")
    (version "1.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "automod" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12rsa5barxi8v916hlvvpjyh43y5x2yjc2bg1xs6v960vccyxwzd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/automod")
    (synopsis "Pull in every source file in a directory as a module.")
    (description "Pull in every source file in a directory as a module.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trycmd-0.15
  (package
    (name "rust-trycmd")
    (version "0.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trycmd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06ab6gmkbbrqfj8ahvz3r5i7clk8fdlg1zx61icfznjlzw4jfqi9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-automod" ,rust-automod-1)
                       ("rust-escargot" ,rust-escargot-0.5)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-humantime" ,rust-humantime-2)
                       ("rust-humantime-serde" ,rust-humantime-serde-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-snapbox" ,rust-snapbox-0.5)
                       ("rust-toml-edit" ,rust-toml-edit-0.19))))
    (home-page "https://github.com/assert-rs/trycmd")
    (synopsis "Snapshot testing for a herd of CLI tests")
    (description "Snapshot testing for a herd of CLI tests")
    (license (list license:expat license:asl2.0))))

(define-public rust-snapbox-macros-0.3
  (package
    (name "rust-snapbox-macros")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "snapbox-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ka2av6cg4b5nxzi19c226jrcc909f9ir02ljb03ci3if93c3x5i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6))))
    (home-page "https://github.com/assert-rs/trycmd/tree/main/crates/snapbox")
    (synopsis "Snapshot testing toolbox")
    (description "Snapshot testing toolbox")
    (license (list license:expat license:asl2.0))))

(define-public rust-escape8259-0.5
  (package
    (name "rust-escape8259")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "escape8259" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vjpfnk9fyq6qcc18bq7yfk9ahi2r12lfywr4rwcsvv6wc8ljkxs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustversion" ,rust-rustversion-1))))
    (home-page "https://github.com/ericseppanen/escape8259")
    (synopsis "RFC8259-compliant string escaping and un-escaping")
    (description "RFC8259-compliant string escaping and un-escaping")
    (license license:expat)))

(define-public rust-libtest-mimic-0.7
  (package
    (name "rust-libtest-mimic")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libtest-mimic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n4vdf4wz4zglammhdzgwxqal9v1a8gbj6rc4q22jfjvxm2xl2yc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-escape8259" ,rust-escape8259-0.5)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-threadpool" ,rust-threadpool-1))))
    (home-page "https://github.com/LukasKalbertodt/libtest-mimic")
    (synopsis
     "Write your own test harness that looks and behaves like the built-in test harness used by `rustc --test`
")
    (description
     "Write your own test harness that looks and behaves like the built-in test
harness used by `rustc --test`")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-lossy-1
  (package
    (name "rust-anstyle-lossy")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle-lossy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "017hmj6dqmazvjlidifivn4c6zg6sv654l68cl0ipqh6kxczdkvg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstyle" ,rust-anstyle-1))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Lossy conversion between ANSI Color Codes")
    (description "Lossy conversion between ANSI Color Codes")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-svg-0.1
  (package
    (name "rust-anstyle-svg")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle-svg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aniq4hyb3d3g1szldch7cgibacdl26g4cj11c7h2qyn8zwhpgxv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-anstyle-lossy" ,rust-anstyle-lossy-1)
                       ("rust-html-escape" ,rust-html-escape-0.2)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Convert ANSI escape codes to SVG")
    (description "Convert ANSI escape codes to SVG")
    (license (list license:expat license:asl2.0))))

(define-public rust-snapbox-0.5
  (package
    (name "rust-snapbox")
    (version "0.5.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "snapbox" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pds90x19wam081mwwkgwg5v40yjyw1kd0zzh7aizk14qa793wbj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-anstyle-svg" ,rust-anstyle-svg-0.1)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-content-inspector" ,rust-content-inspector-0.2)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-escargot" ,rust-escargot-0.5)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-ignore" ,rust-ignore-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libtest-mimic" ,rust-libtest-mimic-0.7)
                       ("rust-normalize-line-endings" ,rust-normalize-line-endings-0.3)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-similar" ,rust-similar-2)
                       ("rust-snapbox-macros" ,rust-snapbox-macros-0.3)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-wait-timeout" ,rust-wait-timeout-0.2)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/assert-rs/trycmd/tree/main/crates/snapbox")
    (synopsis "Snapshot testing toolbox")
    (description "Snapshot testing toolbox")
    (license (list license:expat license:asl2.0))))

(define-public rust-shlex-1
  (package
    (name "rust-shlex")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "shlex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/comex/rust-shlex")
    (synopsis "Split a string into shell words, like Python's shlex.")
    (description "Split a string into shell words, like Python's shlex.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-4
  (package
    (name "rust-clap-derive")
    (version "4.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0byp6k5kyvi9jcbnjjbyw7ak7avn87f2s4ya154f3xc01h29l8wh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_derive")
    (synopsis
     "Parse command line argument by defining a struct, derive crate.")
    (description
     "Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-4
  (package
    (name "rust-clap")
    (version "4.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04w8fx68hzjzk45ir4b9jzwk4m7bki0k5afwns9zqgh61v82d5ll"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap-builder" ,rust-clap-builder-4)
                       ("rust-clap-derive" ,rust-clap-derive-4))
       #:cargo-development-inputs (("rust-humantime" ,rust-humantime-2)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-shlex" ,rust-shlex-1)
                                   ("rust-snapbox" ,rust-snapbox-0.5)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-trycmd" ,rust-trycmd-0.15))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-complete-command-0.5
  (package
    (name "rust-clap-complete-command")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_complete_command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gcsj6ls8y0jpjp5172gdqwx5zj6gm4wdgrqysglr3d73qvrad0q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-carapace-spec-clap" ,rust-carapace-spec-clap-0.1)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-complete" ,rust-clap-complete-4)
                       ("rust-clap-complete-fig" ,rust-clap-complete-fig-4)
                       ("rust-clap-complete-nushell" ,rust-clap-complete-nushell-0.1))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-4))))
    (home-page "https://github.com/nihaals/clap-complete-command")
    (synopsis
     "Reduces boilerplate for adding a shell completion command to Clap")
    (description
     "Reduces boilerplate for adding a shell completion command to Clap")
    (license license:expat)))

(define-public rust-winsafe-0.0.19
  (package
    (name "rust-winsafe")
    (version "0.0.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winsafe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rodrigocfd/winsafe")
    (synopsis "Windows API and GUI in safe, idiomatic Rust.")
    (description "Windows API and GUI in safe, idiomatic Rust.")
    (license license:expat)))

(define-public rust-home-0.5
  (package
    (name "rust-home")
    (version "0.5.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "home" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19grxyg35rqfd802pcc9ys1q3lafzlcjcv2pl2s5q8xpyr5kblg3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/rust-lang/cargo")
    (synopsis "Shared definitions of home directories.")
    (description "Shared definitions of home directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-which-6
  (package
    (name "rust-which")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "which" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mz0vijj9qvsmfqkjqw3wf8zqn19p2x0gg7gzfnhaa1bibsy84c2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-winsafe" ,rust-winsafe-0.0.19))))
    (home-page "https://github.com/harryfei/which-rs.git")
    (synopsis
     "A Rust equivalent of Unix command \"which\". Locate installed executable in cross platforms.")
    (description
     "This package provides a Rust equivalent of Unix command \"which\".  Locate
installed executable in cross platforms.")
    (license license:expat)))

(define-public rust-terminfo-0.8
  (package
    (name "rust-terminfo")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "terminfo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13s9920jzmnr0jidik8yn6gvkic9n39sl28440mx4x8pd2kd6v36"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dirs" ,rust-dirs-4)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-phf" ,rust-phf-0.11)
                       ("rust-phf-codegen" ,rust-phf-codegen-0.11))))
    (home-page "https://github.com/meh/rust-terminfo")
    (synopsis "Terminal information.")
    (description "Terminal information.")
    (license license:wtfpl2)))

(define-public rust-terminfo-0.9
  (package
    (name "rust-terminfo")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "terminfo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qp6rrzkxcg08vjzsim2bw7mid3vi29mizrg70dzbycj0q7q3snl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fnv" ,rust-fnv-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-phf" ,rust-phf-0.11)
                       ("rust-phf-codegen" ,rust-phf-codegen-0.11))))
    (home-page "https://github.com/meh/rust-terminfo")
    (synopsis "Terminal information")
    (description "This package provides Terminal information.")
    (license license:wtfpl2)))

(define-public rust-nix-0.28
  (package
    (name "rust-nix")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r0rylax4ycx3iqakwjvaa178jrrwiiwghcw95ndzy72zk25c8db"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cfg-aliases" ,rust-cfg-aliases-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memoffset" ,rust-memoffset-0.9)
                       ("rust-pin-utils" ,rust-pin-utils-0.1))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "Rust friendly bindings to *nix APIs")
    (license license:expat)))

(define-public rust-clearscreen-3
  (package
    (name "rust-clearscreen")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clearscreen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "132304x7ddim2lvwcw5c185a8cqhxw9lzqahfw60bjbpbzmr731g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nix" ,rust-nix-0.28)
                       ("rust-terminfo" ,rust-terminfo-0.8)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-which" ,rust-which-6)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/watchexec/clearscreen")
    (synopsis "Cross-platform terminal screen clearing")
    (description "Cross-platform terminal screen clearing")
    (license (list license:asl2.0 license:expat))))

(define-public rust-clearscreen-4
  (package
    (name "rust-clearscreen")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clearscreen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x72fipqn3xk44v13ycxf41m97rhh99gdfr4h9hf963vb91xqhcc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nix" ,rust-nix-0.29)
                       ("rust-terminfo" ,rust-terminfo-0.9)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-which" ,rust-which-7)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/watchexec/clearscreen")
    (synopsis "Cross-platform terminal screen clearing")
    (description
     "This package provides Cross-platform terminal screen clearing.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-codspeed-2
  (package
    (name "rust-codspeed")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "codspeed" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "129062464gv2c3kbfyjcvx1faqngbpavvz5k3s98n670934ll41s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-colored" ,rust-colored-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://codspeed.io")
    (synopsis "Core instrumentation library for CodSpeed")
    (description "Core instrumentation library for @code{CodSpeed}")
    (license (list license:expat license:asl2.0))))

(define-public rust-codspeed-criterion-compat-2
  (package
    (name "rust-codspeed-criterion-compat")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "codspeed-criterion-compat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1znp90j223cxhhyx3iyzjsjwf6pqg9if4v154z83d51dqsykcb3j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-codspeed" ,rust-codspeed-2)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-criterion" ,rust-criterion-0.5)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-smol" ,rust-smol-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://codspeed.io")
    (synopsis "Criterion.rs compatibility layer for CodSpeed")
    (description "Criterion.rs compatibility layer for @code{CodSpeed}")
    (license (list license:expat license:asl2.0))))

(define-public rust-console-error-panic-hook-0.1
  (package
    (name "rust-console-error-panic-hook")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "console_error_panic_hook" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g5v8s0ndycc10mdn6igy914k645pgpcl8vjpz6nvxkhyirynsm0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--" "--skip=cargo_readme_up_to_date")
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/rustwasm/console_error_panic_hook")
    (synopsis
     "A panic hook for `wasm32-unknown-unknown` that logs panics to `console.error`")
    (description
     "This package provides a panic hook for `wasm32-unknown-unknown` that logs panics
to `console.error`")
    (license (list license:asl2.0 license:expat))))

(define-public rust-console-log-1
  (package
    (name "rust-console-log")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "console_log" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03rwzvpg384y68j6hxm4h1bhzi7xcc5jdari8hxlvgzdwi0fv2my"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/iamcodemaker/console_log")
    (synopsis
     "A logging facility that routes Rust log messages to the browser's console.")
    (description
     "This package provides a logging facility that routes Rust log messages to the
browser's console.")
    (license (list license:expat license:asl2.0))))

(define-public rust-countme-3
  (package
    (name "rust-countme")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "countme" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dn62hhvgmwyxslh14r4nlbvz8h50cp5mnn1qhqsw63vs7yva13p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1))))
    (home-page "https://github.com/matklad/countme")
    (synopsis "Counts the number of live instances of types")
    (description "Counts the number of live instances of types")
    (license (list license:expat license:asl2.0))))

(define-public rust-tempfile-3
  (package
    (name "rust-tempfile")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "163rp254r3x7i5hisagrpxid2166pq94jvk511dpkmc1yf2fryxr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-fastrand" ,rust-fastrand-1)
                       ("rust-redox-syscall" ,rust-redox-syscall-0.3)
                       ("rust-rustix" ,rust-rustix-0.37)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "A library for managing temporary files and directories.")
    (description
     "This package provides a library for managing temporary files and directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-queue-0.3
  (package
    (name "rust-crossbeam-queue")
    (version "0.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-queue" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d8y8y3z48r9javzj67v3p2yfswd278myz1j9vzc4sp7snslc0yz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-queue")
    (synopsis "Concurrent queues")
    (description "Concurrent queues")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-0.8
  (package
    (name "rust-crossbeam")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a5c7yacnk723x0hfycdbl91ks2nxhwbwy46b8y5vyy0gxzcsdqi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.3)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/crossbeam-rs/crossbeam")
    (synopsis "Tools for concurrent programming")
    (description "This package provides tools for concurrent programming")
    (license (list license:expat license:asl2.0))))

(define-public rust-dashmap-5
  (package
    (name "rust-dashmap")
    (version "5.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dashmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.9)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/xacrimon/dashmap")
    (synopsis "Blazing fast concurrent HashMap for Rust.")
    (description "Blazing fast concurrent @code{HashMap} for Rust.")
    (license license:expat)))

(define-public rust-dirs-5
  (package
    (name "rust-dirs")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dirs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dirs-sys" ,rust-dirs-sys-0.4))))
    (home-page "https://github.com/soc/dirs-rs")
    (synopsis
     "tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
     "This package provides a tiny low-level library that provides platform-specific
standard locations of directories for config, cache and other data on Linux,
Windows, @code{macOS} and Redox by leveraging the mechanisms defined by the XDG
base/user directory specifications on Linux, the Known Folder API on Windows,
and the Standard Directory guidelines on @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-drop-bomb-0.1
  (package
    (name "rust-drop-bomb")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "drop_bomb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qc59a53ngwxpnbvl8xidp2cmwrl671dhbzw7zijmjjaq0hqxnlv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/matklad/drop_bomb")
    (synopsis "A runtime guard for implementing linear types.
")
    (description
     "This package provides a runtime guard for implementing linear types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-env-logger-0.11
  (package
    (name "rust-env-logger")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "env_logger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pbrv61fzmr11wz2mpb85gq2bfcv8n6f6jsis8pnd1fpg0k39swy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-env-filter" ,rust-env-filter-0.1)
                       ("rust-humantime" ,rust-humantime-2)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/rust-cli/env_logger")
    (synopsis
     "A logging implementation for `log` which is configured via an environment
variable.
")
    (description
     "This package provides a logging implementation for `log` which is configured via
an environment variable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fern-0.6
  (package
    (name "rust-fern")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fern" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ahys5fmc10vcgf6yyai0jiypl8pqwidydhqkbp7jph79447pp9v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-colored" ,rust-colored-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-reopen" ,rust-reopen-0.3)
                       ("rust-reopen" ,rust-reopen-1)
                       ("rust-syslog" ,rust-syslog-3)
                       ("rust-syslog" ,rust-syslog-4)
                       ("rust-syslog" ,rust-syslog-6))
       #:cargo-development-inputs (("rust-chrono" ,rust-chrono-0.4)
                                   ("rust-clap" ,rust-clap-2)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/daboross/fern")
    (synopsis "Simple, efficient logging")
    (description "Simple, efficient logging")
    (license license:expat)))

(define-public rust-fern-0.7
  (package
    (name "rust-fern")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fern" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a9v59vcq2fgd6bwgbfl7q6b0zzgxn85y6g384z728wvf1gih5j3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-reopen" ,rust-reopen-0.3)
                       ("rust-reopen" ,rust-reopen-1)
                       ("rust-syslog" ,rust-syslog-3)
                       ("rust-syslog" ,rust-syslog-4)
                       ("rust-syslog" ,rust-syslog-6)
                       ("rust-syslog" ,rust-syslog-7))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-2)
                                   ("rust-humantime" ,rust-humantime-2)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/daboross/fern")
    (synopsis "Simple, efficient logging")
    (description "This package provides Simple, efficient logging.")
    (license license:expat)))

(define-public rust-filetime-0.2
  (package
    (name "rust-filetime")
    (version "0.2.23")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "filetime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1za0sbq7fqidk8aaq9v7m9ms0sv8mmi49g6p5cphpan819q4gr0y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-redox-syscall" ,rust-redox-syscall-0.4)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/alexcrichton/filetime")
    (synopsis "Platform-agnostic accessors of timestamps in File metadata
")
    (description "Platform-agnostic accessors of timestamps in File metadata")
    (license (list license:expat license:asl2.0))))

(define-public rust-fs-err-2
  (package
    (name "rust-fs-err")
    (version "2.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fs-err" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hdajzh5sjvvdjg0n15j91mv8ydvb7ff6m909frvdmg1bw81z948"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/andrewhickman/fs-err")
    (synopsis
     "A drop-in replacement for std::fs with more helpful error messages.")
    (description
     "This package provides a drop-in replacement for std::fs with more helpful error
messages.")
    (license (list license:expat license:asl2.0))))

(define-public rust-globset-0.4
  (package
    (name "rust-globset")
    (version "0.4.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "globset" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qab0c1drpybgm4nc92lf8b46x0ap44c9y4k23rndgc5bfdkpnjp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-regex-automata" ,rust-regex-automata-0.4)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-glob" ,rust-glob-0.3)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page
     "https://github.com/BurntSushi/ripgrep/tree/master/crates/globset")
    (synopsis
     "Cross platform single glob and glob set matching. Glob set matching is the
process of matching one or more glob patterns against a single candidate path
simultaneously, and returning all of the globs that matched.
")
    (description
     "Cross platform single glob and glob set matching.  Glob set matching is the
process of matching one or more glob patterns against a single candidate path
simultaneously, and returning all of the globs that matched.")
    (license (list license:unlicense license:expat))))

(define-public rust-hashbrown-0.14
  (package
    (name "rust-hashbrown")
    (version "0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hashbrown" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "012nywlg0lj9kwanh69my5x67vjlfmzfi9a0rq4qvis2j8fil3r9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-allocator-api2" ,rust-allocator-api2-0.2)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-equivalent" ,rust-equivalent-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-fnv" ,rust-fnv-1)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-rkyv" ,rust-rkyv-0.7)
                                   ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "A Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's @code{SwissTable} hash map")
    (license (list license:expat license:asl2.0))))

(define-public rust-hexf-parse-0.2
  (package
    (name "rust-hexf-parse")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hexf-parse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pr3a3sk66ddxdyxdxac7q6qaqjcn28v0njy22ghdpfn78l8d9nz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/lifthrasiir/hexf")
    (synopsis "Parses hexadecimal floats (see also hexf)")
    (description "Parses hexadecimal floats (see also hexf)")
    (license license:cc0)))

(define-public rust-ignore-0.4
  (package
    (name "rust-ignore")
    (version "0.4.22")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wcaqpi6djqgi1brghrdyw4d5qgnwzhqrqyn4mar4vp677gi0s5l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-globset" ,rust-globset-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-regex-automata" ,rust-regex-automata-0.4)
                       ("rust-same-file" ,rust-same-file-1)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-winapi-util" ,rust-winapi-util-0.1))
       #:cargo-development-inputs (("rust-bstr" ,rust-bstr-1)
                                   ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5))))
    (home-page
     "https://github.com/BurntSushi/ripgrep/tree/master/crates/ignore")
    (synopsis
     "A fast library for efficiently matching ignore files such as `.gitignore`
against file paths.
")
    (description
     "This package provides a fast library for efficiently matching ignore files such
as `.gitignore` against file paths.")
    (license (list license:unlicense license:expat))))

(define-public rust-git-worktree-0.6
  (package
    (name "rust-git-worktree")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i018f73ffclslf6jnzp2sjj8rjalln91g9qj9pqm54s6003v5js"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-attributes" ,rust-git-attributes-0.5)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-glob" ,rust-git-glob-0.4)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-index" ,rust-git-index-0.6)
                       ("rust-git-object" ,rust-git-object-0.22)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-io-close" ,rust-io-close-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-revision-0.6
  (package
    (name "rust-git-revision")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "098vycrqy2ckxzh7yy8y9rzp370sqkp0jwgclpdvai9p7k333z8y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-date" ,rust-git-date-0.2)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-object" ,rust-git-object-0.22)
                       ("rust-hash-hasher" ,rust-hash-hasher-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-refspec-0.3
  (package
    (name "rust-git-refspec")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11s1y4rfxd5qah7h8lx67crqvhp6m787vzskl3yqrbiq6mvsz5wl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-revision" ,rust-git-revision-0.6)
                       ("rust-git-validate" ,rust-git-validate-0.6)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-packetline-0.13
  (package
    (name "rust-git-packetline")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-packetline" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kp3hjm75pijg5gbzg9h0x1an2n9iy36c1zjsiq9z177xnilna13"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-transport-0.21
  (package
    (name "rust-git-transport")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d9z2y30s4pxjczllb22xk4j1v2byi8jgcy47cjd62q4akzikw06"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-packetline" ,rust-git-packetline-0.13)
                       ("rust-git-sec" ,rust-git-sec-0.4)
                       ("rust-git-url" ,rust-git-url-0.10)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-protocol-0.21
  (package
    (name "rust-git-protocol")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pd0x9rqbnixjh5zc1hw4q1489690933qvq6q5f0h1pbv9m8bl0x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-git-credentials" ,rust-git-credentials-0.6)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-transport" ,rust-git-transport-0.21)
                       ("rust-maybe-async" ,rust-maybe-async-0.2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-chunk-0.3
  (package
    (name "rust-git-chunk")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-chunk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "127mk2xrs7fa5iy10ixp4k0s7435jjj896ip61pasq5n6lbbrch7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-pack-0.24
  (package
    (name "rust-git-pack")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07q9bmayvsrvkjg4nll3yw87h789q13pypdl9nilwmb1ba6qsbvb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytesize" ,rust-bytesize-1)
                       ("rust-clru" ,rust-clru-0.5)
                       ("rust-dashmap" ,rust-dashmap-5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-chunk" ,rust-git-chunk-0.3)
                       ("rust-git-diff" ,rust-git-diff-0.20)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-object" ,rust-git-object-0.22)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-git-tempfile" ,rust-git-tempfile-2)
                       ("rust-git-traverse" ,rust-git-traverse-0.18)
                       ("rust-hash-hasher" ,rust-hash-hasher-2)
                       ("rust-memmap2" ,rust-memmap2-0.5)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uluru" ,rust-uluru-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-odb-0.34
  (package
    (name "rust-git-odb")
    (version "0.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g7nb6f86d5jv8n3shbm10rbz623kazkwdsnfwp63wh2mm2azfs3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-object" ,rust-git-object-0.22)
                       ("rust-git-pack" ,rust-git-pack-0.24)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-git-quote" ,rust-git-quote-0.3)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-mailmap-0.5
  (package
    (name "rust-git-mailmap")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11q9dk56zpwwgax7s8iwsv25jgm6ngvhka1467mqlcmjhk78agxv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-actor" ,rust-git-actor-0.13)
                       ("rust-quick-error" ,rust-quick-error-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-traverse-0.18
  (package
    (name "rust-git-traverse")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12dg0l2sr37saxl4q40zg4lh2xzb90ykgs5c8d7jk7y6fgbls30d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-object" ,rust-git-object-0.22)
                       ("rust-hash-hasher" ,rust-hash-hasher-2)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-bitmap-0.1
  (package
    (name "rust-git-bitmap")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-bitmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w1qxjsf22xywhhaf42xfn6q5k3m8fyjsq3igs6jkbi7mnkrhw1j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quick-error" ,rust-quick-error-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-atoi-1
  (package
    (name "rust-atoi")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atoi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13mycnr954w17lcvvbpzr4rmhl1h13cg8hq63j0rrx9g6497vifp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/pacman82/atoi-rs")
    (synopsis "Parse integers directly from `[u8]` slices in safe code")
    (description "Parse integers directly from `[u8]` slices in safe code")
    (license license:expat)))

(define-public rust-git-index-0.6
  (package
    (name "rust-git-index")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hlfwd46bq8i0jblibc9lwwmyhjsbw8hzm5wgwims553w0h6hc9x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atoi" ,rust-atoi-1)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-git-bitmap" ,rust-git-bitmap-0.1)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-object" ,rust-git-object-0.22)
                       ("rust-git-traverse" ,rust-git-traverse-0.18)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memmap2" ,rust-memmap2-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-discover-0.6
  (package
    (name "rust-git-discover")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05cvw4qv5i1ni6vj70m25sbz4v5q8lh48nyfa9xxsndyzh1d0mpc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-git-ref" ,rust-git-ref-0.17)
                       ("rust-git-sec" ,rust-git-sec-0.4)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-diff-0.20
  (package
    (name "rust-git-diff")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16bihkq4c8zrbhpwdncr5snklr3i74b7sz39m937wgzld1975p1c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-object" ,rust-git-object-0.22)
                       ("rust-similar" ,rust-similar-2)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-url-0.10
  (package
    (name "rust-git-url")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "108z1ackzp7ficpm3g06v07i8v0xkhfhxdpm69whx14n64rgidr1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-prompt-0.1
  (package
    (name "rust-git-prompt")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-prompt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17yxmz45mfmr00igw7qcaxxlfy3rd3q3z25wfwi38w86bj9lfsgs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-git-command" ,rust-git-command-0.1)
                       ("rust-git-config-value" ,rust-git-config-value-0.8)
                       ("rust-nix" ,rust-nix-0.25)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-command-0.1
  (package
    (name "rust-git-command")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hnw25pwhf3ds3nrnr5akn33wk6hs1vj5w66v97malb5gfch2jwy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-credentials-0.6
  (package
    (name "rust-git-credentials")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ffk2y2snpg732js5h7ax9nfc623pw71h8xrl9dhgzanxa302m1z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-command" ,rust-git-command-0.1)
                       ("rust-git-config-value" ,rust-git-config-value-0.8)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-git-prompt" ,rust-git-prompt-0.1)
                       ("rust-git-sec" ,rust-git-sec-0.4)
                       ("rust-git-url" ,rust-git-url-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-msvc-0.40
  (package
    (name "rust-windows-x86-64-msvc")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a6szrdzslz4vw21yqgkks0rrv6qpm2fy70kdskvzlaxymnrq31a"))
       (snippet #~(delete-file "lib/windows.lib"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnullvm-0.40
  (package
    (name "rust-windows-x86-64-gnullvm")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lr8199gbmi3bkc6xl1lm813v5sqj4dqyj2zsx7w80ji4615nvh4"))
       (snippet #~(delete-file "lib/libwindows.a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnu-0.40
  (package
    (name "rust-windows-x86-64-gnu")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_gnu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ira2rf4nzm2piq417xfi037zqcq50c828lxnij81nkjvmzg63sj"))
       (snippet #~(delete-file "lib/libwindows.a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-msvc-0.40
  (package
    (name "rust-windows-i686-msvc")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xnqs15hknjqxm75qbjz2fgp9jw9fhyx9n4ml4glk60s3h1x86ia"))
       (snippet #~(delete-file "lib/windows.lib"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnu-0.40
  (package
    (name "rust-windows-i686-gnu")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_gnu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hgkx5g68qz2p3wirqryyqmmwhbyacjsbhiax9fzh38gszx0jnxa"))
       (snippet #~(delete-file "lib/libwindows.a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-msvc-0.40
  (package
    (name "rust-windows-aarch64-msvc")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d1shkdh4y9jygzvmimsavz45dp0fq311rx8maqhzigw5p37729j"))
       (snippet #~(delete-file "lib/windows.lib"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-gnullvm-0.40
  (package
    (name "rust-windows-aarch64-gnullvm")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "046a6yxd842x9kh94sis0f1qawq3fj0v19iw68abfqb5l6hs9jpk"))
       (snippet #~(delete-file "lib/libwindows.a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-interface-0.40
  (package
    (name "rust-windows-interface")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06q1n4335a0bjvcrk3dd7ynlzyk8abxz2vbicrjma53kihkiazdg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The interface macro for the windows crate")
    (description "The interface macro for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-implement-0.40
  (package
    (name "rust-windows-implement")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aiyh2hq1zn3r85v258cxr2k9040m59mfm0vhlr3kk0mwgqsyy1h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The implement macro for the windows crate")
    (description "The implement macro for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-0.40
  (package
    (name "rust-windows")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18fada51f8mj9plqbdz1af9frvpzamgwpcbjxh7i7ysji9qwq2p3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-implement" ,rust-windows-implement-0.40)
                       ("rust-windows-interface" ,rust-windows-interface-0.40)
                       ("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.40)
                       ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.40)
                       ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.40)
                       ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.40)
                       ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.40)
                       ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.40)
                       ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.40))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "Rust for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-sec-0.4
  (package
    (name "rust-git-sec")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-sec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14hqd9hjrbnzm17l3p8bwjvic424d33rawnvfh3lv0a6cngpcycc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-dirs" ,rust-dirs-4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows" ,rust-windows-0.40))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-validate-0.6
  (package
    (name "rust-git-validate")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-validate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gcc5h8mp0wbg2v9l6nikxqvqgp2m6bix9vlvpx8v0yyl1m9shxm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-object-0.22
  (package
    (name "rust-git-object")
    (version "0.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1czp4cvk310svlsk6wbl2vvxvd3ln5dlabm1fvp01dcb1p0ahscl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-actor" ,rust-git-actor-0.13)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-validate" ,rust-git-validate-0.6)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-tempfile-2
  (package
    (name "rust-git-tempfile")
    (version "2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "026p8c3k1g41ycwcr84zpm0nn32lnn2xdl69ws0vsg6y55hvq8rd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-lock-2
  (package
    (name "rust-git-lock")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y3wxg1i63rnv5ydqrykbrndid2gws2pgh5l6l9jpxk1z45y23sz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fastrand" ,rust-fastrand-1)
                       ("rust-git-tempfile" ,rust-git-tempfile-2)
                       ("rust-quick-error" ,rust-quick-error-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-ref-0.17
  (package
    (name "rust-git-ref")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i7wj0mhr6ddnpl0daljqj1bdz7406jwsdzxj61dzdz3qi6jh942"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-actor" ,rust-git-actor-0.13)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-lock" ,rust-git-lock-2)
                       ("rust-git-object" ,rust-git-object-0.22)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-git-tempfile" ,rust-git-tempfile-2)
                       ("rust-git-validate" ,rust-git-validate-0.6)
                       ("rust-memmap2" ,rust-memmap2-0.5)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-config-value-0.8
  (package
    (name "rust-git-config-value")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-config-value" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sy3dll54zpg8jxrpk5qyp6hcqh01zn1wbqi2m4l2sw0wnzpdwh5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-config-0.9
  (package
    (name "rust-git-config")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hydrnzs6fjjgb5ma1y52k4xw9fizbljh2a220b1xvr4p4l7qlqx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-config-value" ,rust-git-config-value-0.8)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-glob" ,rust-git-glob-0.4)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-git-ref" ,rust-git-ref-0.17)
                       ("rust-git-sec" ,rust-git-sec-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-bom-1
  (package
    (name "rust-unicode-bom")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-bom" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cpc54ssrlrwm3x8dm7z1dgwj9r9bxjls620ra1vfxfq87snkv33"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://gitlab.com/philbooth/unicode-bom")
    (synopsis "Unicode byte-order mark detection for files and byte arrays.")
    (description
     "Unicode byte-order mark detection for files and byte arrays.")
    (license license:asl2.0)))

(define-public rust-git-quote-0.3
  (package
    (name "rust-git-quote")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-quote" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z7chzgh4lsscmk82y23vf3flc1wy1gz9g8v6xzl9gkws0qpk8ay"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-quick-error" ,rust-quick-error-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-path-0.5
  (package
    (name "rust-git-path")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-path" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wi5h65g7svw6v6i9hsyc0ix614m9phbgr5xqpk17glh4q1c2pa2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-glob-0.4
  (package
    (name "rust-git-glob")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "145agpzf9109shqybxxi4x8h1c6dzmh6y8s9j34gh4ki4cq68xbd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-compact-str-0.6
  (package
    (name "rust-compact-str")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "compact_str" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fg1x55bdrlhd0jbbpi3ivs8ym3bfsgdqilnl3xpv7lljm9r8f2i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-castaway" ,rust-castaway-0.2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-markup" ,rust-markup-0.13)
                       ("rust-proptest" ,rust-proptest-1)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/ParkMyCar/compact_str")
    (synopsis
     "A memory efficient string type that transparently stores strings on the stack, when possible")
    (description
     "This package provides a memory efficient string type that transparently stores
strings on the stack, when possible")
    (license license:expat)))

(define-public rust-git-attributes-0.5
  (package
    (name "rust-git-attributes")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-attributes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0paxi5rsaq6mkr4bwjy5z6gxj0292dyy6fzsvg33jsrc0imfcqkc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-compact-str" ,rust-compact-str-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-glob" ,rust-git-glob-0.4)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-git-quote" ,rust-git-quote-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-tui-react-0.19
  (package
    (name "rust-tui-react")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tui-react" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kcgci93f8kxmvdydjkc9hcj80cbba2gg4b8m2pds0dgk8q3fb2l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-tui" ,rust-tui-0.19)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/Byron/tui-crates")
    (synopsis
     "TUI widgets using a react-like paradigm, allowing mutable component state and render properties.")
    (description
     "TUI widgets using a react-like paradigm, allowing mutable component state and
render properties.")
    (license license:expat)))

(define-public rust-crosstermion-0.10
  (package
    (name "rust-crosstermion")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crosstermion" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "017dmqfw6qngjac089nq8394pzx9ym3gn3zkjwkggmf20advvalr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ansi-term" ,rust-ansi-term-0.12)
                       ("rust-async-channel" ,rust-async-channel-1)
                       ("rust-crossterm" ,rust-crossterm-0.25)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-termion" ,rust-termion-1)
                       ("rust-tui" ,rust-tui-0.19)
                       ("rust-tui-react" ,rust-tui-react-0.19))))
    (home-page "https://github.com/Byron/tui-crates")
    (synopsis "utilities for `crossterm`, without ties to `termion`")
    (description "utilities for `crossterm`, without ties to `termion`")
    (license license:expat)))

(define-public rust-prodash-21
  (package
    (name "rust-prodash")
    (version "21.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prodash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rhiry5k0yw0i2qlw254vi7whf22qpfcx05pm2rhiayd72yxf4vy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-io" ,rust-async-io-1)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-compound-duration" ,rust-compound-duration-1)
                       ("rust-crosstermion" ,rust-crosstermion-0.10)
                       ("rust-ctrlc" ,rust-ctrlc-3)
                       ("rust-dashmap" ,rust-dashmap-5)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-human-format" ,rust-human-format-1)
                       ("rust-humantime" ,rust-humantime-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tui" ,rust-tui-0.19)
                       ("rust-tui-react" ,rust-tui-react-0.19)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/Byron/prodash")
    (synopsis
     "A dashboard for visualizing progress of asynchronous and possibly blocking tasks")
    (description
     "This package provides a dashboard for visualizing progress of asynchronous and
possibly blocking tasks")
    (license license:expat)))

(define-public rust-jwalk-0.6
  (package
    (name "rust-jwalk")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gd06fqyh5gjiiyi7s6rjgki943391rqnql9wmkw0vcbvdbxmg2x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam" ,rust-crossbeam-0.8)
                       ("rust-rayon" ,rust-rayon-1))))
    (home-page "https://github.com/byron/jwalk")
    (synopsis
     "Filesystem walk performed in parallel with streamed and sorted results.")
    (description
     "Filesystem walk performed in parallel with streamed and sorted results.")
    (license license:expat)))

(define-public rust-git-hash-0.9
  (package
    (name "rust-git-hash")
    (version "0.9.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m25ng3v75hylvz42d9m9ab01cv1cxm53gw7i91s938y5mn6xm0n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-features-0.23
  (package
    (name "rust-git-features")
    (version "0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "027d7f9i3lh003rgx7wapcsq2prky7swc5v51jrp2k376zl8ms2b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-jwalk" ,rust-jwalk-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-21)
                       ("rust-quick-error" ,rust-quick-error-2)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-date-0.2
  (package
    (name "rust-git-date")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-date" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g8gyb3zab8ii6rpaps2knixh7r5xq63lv912rfy2hfz4nbix21p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-git-actor-0.13
  (package
    (name "rust-git-actor")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09cyf72fp6mlkb1ycxvv01zl4zv6fwl97r808w21rix6q04wxm0q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-date" ,rust-git-date-0.2)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-quick-error" ,rust-quick-error-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Please use `gix-<thiscrate>` instead ('git' -> 'gix')")
    (description "Please use `gix-<thiscrate>` instead ('git -> gix')")
    (license (list license:expat license:asl2.0))))

(define-public rust-clru-0.5
  (package
    (name "rust-clru")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clru" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02hy88i8vagcb2gy7kxwjrsyvzg5q0rx477sfnll5r78vp9np391"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/marmeladema/clru-rs")
    (synopsis
     "An LRU cache implementation with constant time operations and weighted semantic")
    (description
     "An LRU cache implementation with constant time operations and weighted semantic")
    (license license:expat)))

(define-public rust-git-repository-0.25
  (package
    (name "rust-git-repository")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-repository" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ib3dhzdjdvg8qklb382a791lz2abia9c9ggjmv8p6xvn97caha0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-byte-unit" ,rust-byte-unit-4)
                       ("rust-clru" ,rust-clru-0.5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-git-actor" ,rust-git-actor-0.13)
                       ("rust-git-attributes" ,rust-git-attributes-0.5)
                       ("rust-git-config" ,rust-git-config-0.9)
                       ("rust-git-credentials" ,rust-git-credentials-0.6)
                       ("rust-git-date" ,rust-git-date-0.2)
                       ("rust-git-diff" ,rust-git-diff-0.20)
                       ("rust-git-discover" ,rust-git-discover-0.6)
                       ("rust-git-features" ,rust-git-features-0.23)
                       ("rust-git-glob" ,rust-git-glob-0.4)
                       ("rust-git-hash" ,rust-git-hash-0.9)
                       ("rust-git-index" ,rust-git-index-0.6)
                       ("rust-git-lock" ,rust-git-lock-2)
                       ("rust-git-mailmap" ,rust-git-mailmap-0.5)
                       ("rust-git-object" ,rust-git-object-0.22)
                       ("rust-git-odb" ,rust-git-odb-0.34)
                       ("rust-git-pack" ,rust-git-pack-0.24)
                       ("rust-git-path" ,rust-git-path-0.5)
                       ("rust-git-prompt" ,rust-git-prompt-0.1)
                       ("rust-git-protocol" ,rust-git-protocol-0.21)
                       ("rust-git-ref" ,rust-git-ref-0.17)
                       ("rust-git-refspec" ,rust-git-refspec-0.3)
                       ("rust-git-revision" ,rust-git-revision-0.6)
                       ("rust-git-sec" ,rust-git-sec-0.4)
                       ("rust-git-tempfile" ,rust-git-tempfile-2)
                       ("rust-git-transport" ,rust-git-transport-0.21)
                       ("rust-git-traverse" ,rust-git-traverse-0.18)
                       ("rust-git-url" ,rust-git-url-0.10)
                       ("rust-git-validate" ,rust-git-validate-0.6)
                       ("rust-git-worktree" ,rust-git-worktree-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "This crate is now named 'gix' and not available under this name anymore")
    (description
     "This crate is now named gix and not available under this name anymore")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-stemmers-1
  (package
    (name "rust-rust-stemmers")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rust-stemmers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m6acgdflrrcm17dj7lp7x4sfqqhga24qynv660qinwz04v20sp4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/CurrySoftware/rust-stemmers")
    (synopsis
     "A rust implementation of some popular snowball stemming algorithms")
    (description
     "This package provides a rust implementation of some popular snowball stemming
algorithms")
    (license (list license:expat license:bsd-3))))

(define-public rust-imperative-1
  (package
    (name "rust-imperative")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "imperative" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1633df2nvwxigzwprm0308nkpjkhp4klg5pibldry3gyy8xi54lz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-phf" ,rust-phf-0.11)
                       ("rust-rust-stemmers" ,rust-rust-stemmers-1))
       #:cargo-development-inputs (("rust-codegenrs" ,rust-codegenrs-2)
                                   ("rust-multimap" ,rust-multimap-0.8)
                                   ("rust-phf-codegen" ,rust-phf-codegen-0.11)
                                   ("rust-rust-stemmers" ,rust-rust-stemmers-1)
                                   ("rust-snapbox" ,rust-snapbox-0.4))))
    (home-page "https://github.com/crate-ci/imperative")
    (synopsis "Check for imperative mood in text")
    (description "Check for imperative mood in text")
    (license (list license:expat license:asl2.0))))

(define-public rust-indoc-2
  (package
    (name "rust-indoc")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indoc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n2z66b0y59rr6v4znpcijc2yd3yg6s40hpzv89yb140mvxnq60y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-unindent" ,rust-unindent-0.2))))
    (home-page "https://github.com/dtolnay/indoc")
    (synopsis "Indented document literals")
    (description "Indented document literals")
    (license (list license:expat license:asl2.0))))

(define-public rust-indoc-2
  (package
    (inherit rust-indoc-2)
    (name "rust-indoc")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indoc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dgjk49rkmx4kjy07k4b90qb5vl89smgb5rcw02n0q0x9ligaj5j"))))))

(define-public rust-insta-cmd-0.6
  (package
    (name "rust-insta-cmd")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "insta-cmd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rix5nmswns1p5p5f7pj5l9wvm69awzby0fbkkacwp4j4ylyzvpz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--" "--skip=test_basic")
       #:cargo-inputs (("rust-insta" ,rust-insta-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://insta.rs/")
    (synopsis "A command line extension to the insta testing library for Rust")
    (description
     "This package provides a command line extension to the insta testing library for
Rust")
    (license license:asl2.0)))

(define-public rust-is-macro-0.3
  (package
    (name "rust-is-macro")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "is-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13r84nzn5zqxf8pqjhpj2pr0vkjnam1iwnnbmimr05rpq6ymma2r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-inflector" ,rust-inflector-0.11)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dudykr/ddbase")
    (synopsis "Derive methods for using custom enums like Option / Result")
    (description "Derive methods for using custom enums like Option / Result")
    (license license:asl2.0)))

(define-public rust-is-docker-0.2
  (package
    (name "rust-is-docker")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "is-docker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cyibrv6817cqcpf391m327ss40xlbik8wxcv5h9pj9byhksx2wj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/TheLarkInn/is-docker")
    (synopsis "Checks if the process is running inside a Docker container.")
    (description "Checks if the process is running inside a Docker container.")
    (license license:expat)))

(define-public rust-is-wsl-0.4
  (package
    (name "rust-is-wsl")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "is-wsl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19bs5pq221d4bknnwiqqkqrnsx2in0fsk8fylxm1747iim4hjdhp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-is-docker" ,rust-is-docker-0.2)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/TheLarkInn/is-wsl")
    (synopsis
     "Checks if the process is running inside Windows Subsystem for Linux.")
    (description
     "Checks if the process is running inside Windows Subsystem for Linux.")
    (license license:expat)))

(define-public rust-web-sys-0.3
  (package
    (name "rust-web-sys")
    (version "0.3.69")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "web-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vqkxk935xa8zcnsi4bd88sb267ly2i24xl1yiq26d1n32hskbvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/web-sys/index.html")
    (synopsis
     "Bindings for all Web APIs, a procedurally generated crate from WebIDL
")
    (description
     "Bindings for all Web APIs, a procedurally generated crate from @code{WebIDL}")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-test-macro-0.3
  (package
    (name "rust-wasm-bindgen-test-macro")
    (version "0.3.42")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-test-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w3ypw6b0ffyyx0w83mlb4bw1jmjgza9kdxyjk5h6bhs6lwrgy5p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/rustwasm/wasm-bindgen")
    (synopsis "Internal testing macro for wasm-bindgen")
    (description "Internal testing macro for wasm-bindgen")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-test-0.3
  (package
    (name "rust-wasm-bindgen-test")
    (version "0.3.42")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yv6hyckfwnp3lkkm93di3jq62g4xqymhi10hlzaz007isjn5gyr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1)
                       ("rust-gg-alloc" ,rust-gg-alloc-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-scoped-tls" ,rust-scoped-tls-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-wasm-bindgen-test-macro" ,rust-wasm-bindgen-test-macro-0.3))))
    (home-page "https://github.com/rustwasm/wasm-bindgen")
    (synopsis "Internal testing crate for wasm-bindgen")
    (description "Internal testing crate for wasm-bindgen")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-futures-0.4
  (package
    (name "rust-wasm-bindgen-futures")
    (version "0.4.42")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-futures" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h322zjvpjllcpj7dahfxjsv6inkr6y0baw7nkdwivr1c4v19g3n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Bridging the gap between Rust Futures and JavaScript Promises")
    (description
     "Bridging the gap between Rust Futures and @code{JavaScript} Promises")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-shared-0.2
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-shared" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15kyavsrna2cvy30kg03va257fraf9x00ny554vxngvpyaa0q6dg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
    (description
     "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-backend-0.2
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-backend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nj7wxbi49f0rw9d44rjzms26xlw6r76b2mrggx8jfbdjrxphkb1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Backend code generation of the wasm-bindgen tool
")
    (description "Backend code generation of the wasm-bindgen tool")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-support-0.2
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-macro-support" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dqv2xs8zcyw4kjgzj84bknp2h76phmsb3n7j6hn396h4ssifkz9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-wasm-bindgen-backend" ,rust-wasm-bindgen-backend-0.2)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
    (description
     "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in
the shared backend crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-0.2
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09npa1srjjabd6nfph5yc03jb26sycjlxhy0c2a1pdrpx4yq5y51"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-wasm-bindgen-macro-support" ,rust-wasm-bindgen-macro-support-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
    (description
     "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-0.2
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a4mcw13nsk3fr8fxjzf9kk1wj88xkfsmnm0pjraw01ryqfm7qjb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen-macro" ,rust-wasm-bindgen-macro-0.2))))
    (home-page "https://rustwasm.github.io/")
    (synopsis "Easy support for interacting between JS and Rust.
")
    (description "Easy support for interacting between JS and Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-js-sys-0.3
  (package
    (name "rust-js-sys")
    (version "0.3.69")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "js-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v99rz97asnzapb0jsc3jjhvxpfxr7h7qd97yqyrf9i7viimbh99"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))
       #:cargo-development-inputs (("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
    (description
     "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-jod-thread-0.1
  (package
    (name "rust-jod-thread")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jod-thread" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bj7g6l59ybcf33znf80ccqbxvs1cmd8ynd4m8h7ywdqk473c8wb"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/matklad/jod-thread")
    (synopsis "std::thread which joins on drop by default.")
    (description "std::thread which joins on drop by default.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lexical-parse-float-0.8
  (package
    (name "rust-lexical-parse-float")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lexical-parse-float" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pz4b3gj2qk447g2mlmqlfgb7rym796c9b3cnhxs6ldq5k402fk7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-lexical-parse-integer" ,rust-lexical-parse-integer-0.8)
                       ("rust-lexical-util" ,rust-lexical-util-0.8)
                       ("rust-static-assertions" ,rust-static-assertions-1))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-0.10))))
    (home-page "https://github.com/Alexhuszagh/rust-lexical")
    (synopsis "Efficient parsing of floats from strings.")
    (description "Efficient parsing of floats from strings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libcst-derive-1
  (package
    (name "rust-libcst-derive")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libcst_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q0vc3fngqffgl8g09cawmv95vjxv2wh23m9lhadx4whspr12l2a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/Instagram/LibCST")
    (synopsis "Proc macro helpers for libcst.")
    (description "Proc macro helpers for libcst.")
    (license license:expat)))

(define-public rust-annotate-snippets-0.6
  (package
    (name "rust-annotate-snippets")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "annotate-snippets" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19x7ldklprdgf8pam8b3lfhrxqw5yldcvk5j0bw2agsajbj1q0n7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ansi-term" ,rust-ansi-term-0.11))))
    (home-page "https://github.com/rust-lang/annotate-snippets-rs")
    (synopsis "Library for building code annotations")
    (description "Library for building code annotations")
    (license (list license:asl2.0 license:expat))))

(define-public rust-chic-1
  (package
    (name "rust-chic")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "037pkdccj25gr4my8fq1qni9v87rydpyhfi2naf86mimkxhxpdd5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-annotate-snippets" ,rust-annotate-snippets-0.6))))
    (home-page "https://github.com/yoshuawuyts/chic")
    (synopsis "Pretty parser error reporting.")
    (description "Pretty parser error reporting.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libcst-1
  (package
    (name "rust-libcst")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libcst" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lx0fqhwr8539z5zhxn36z69gfwiaj41s64lyxxnbb6a03s2yp5x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags `("--" "--skip=roundtrip_fixtures")
       #:cargo-inputs (("rust-chic" ,rust-chic-1)
                       ("rust-libcst-derive" ,rust-libcst-derive-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-peg" ,rust-peg-0.8)
                       ("rust-pyo3" ,rust-pyo3-0.18)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-difference" ,rust-difference-2)
                                   ("rust-itertools" ,rust-itertools-0.11)
                                   ("rust-rayon" ,rust-rayon-1))))
    (inputs (list python))
    (home-page "https://github.com/Instagram/LibCST")
    (synopsis "A Python parser and Concrete Syntax Tree library.")
    (description
     "This package provides a Python parser and Concrete Syntax Tree library.")
    (license license:expat)))

(define-public rust-value-bag-1
  (package
    (name "rust-value-bag")
    (version "1.0.0-alpha.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "value-bag" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mgc2vlqikx16gabp4ghbm3fs773kxvwjmrn57rydxs92a6vf292"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ctor" ,rust-ctor-0.1)
                       ("rust-erased-serde" ,rust-erased-serde-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-fmt" ,rust-serde-fmt-1)
                       ("rust-sval" ,rust-sval-1)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/sval-rs/value-bag")
    (synopsis "Anonymous structured values")
    (description "Anonymous structured values")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sval-derive-1
  (package
    (name "rust-sval-derive")
    (version "1.0.0-alpha.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sval_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1clarb8fqxlffa2i6p70l5nr6l3pcp90p98xkvdn8f65xkc0hhkp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/sval-rs/sval")
    (synopsis "Derive support for sval")
    (description "Derive support for sval")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sval-1
  (package
    (name "rust-sval")
    (version "1.0.0-alpha.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sval" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "025sy290xnn56nl15qkrkq0whxcwlvb4bzp996azbjl7gdyfxxj5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sval-derive" ,rust-sval-derive-1))))
    (home-page "https://github.com/sval-rs/sval")
    (synopsis "Streaming, structured value serialization")
    (description "Streaming, structured value serialization")
    (license (list license:asl2.0 license:expat))))

(define-public rust-lsp-types-0.95
  (package
    (name "rust-lsp-types")
    (version "0.95.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lsp-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ra36fd4yr7lf5igfrdvwjx9g87z3a99mrjgzk9nq04viqxd6d4f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/gluon-lang/lsp-types")
    (synopsis
     "Types for interaction with a language server, using VSCode's Language Server Protocol")
    (description
     "Types for interaction with a language server, using VSCode's Language Server
Protocol")
    (license license:expat)))

(define-public rust-lsp-server-0.7
  (package
    (name "rust-lsp-server")
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lsp-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15bhdhkinhhw5fifrpmiiqdd4hwblac40jv0n7hxidbdiyvnb3r4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs (("rust-ctrlc" ,rust-ctrlc-3)
                                   ("rust-lsp-types" ,rust-lsp-types-0.95))))
    (home-page
     "https://github.com/rust-lang/rust-analyzer/tree/master/lib/lsp-server")
    (synopsis "Generic LSP server scaffold.")
    (description "Generic LSP server scaffold.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lsp-types-0.95
  (package
    (name "rust-lsp-types")
    (version "0.95.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lsp-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1139mg0hcxw45q12hzq0y73nbjw40h6b39idwj7kxxsf6l8ik30m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/gluon-lang/lsp-types")
    (synopsis
     "Types for interaction with a language server, using VSCode's Language Server Protocol")
    (description
     "Types for interaction with a language server, using VSCode's Language Server
Protocol")
    (license license:expat)))

(define-public rust-smartcow-0.2
  (package
    (name "rust-smartcow")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smartcow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18jxicfi9q2b666vxflyjk2mxpxgv23wwd116xald36a3wfcnvv5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-smartstring" ,rust-smartstring-1))))
    (home-page "https://github.com/jbr/smartcow")
    (synopsis "a cow for smartstrings")
    (description "a cow for smartstrings")
    (license (list license:expat license:asl2.0))))

(define-public rust-routefinder-0.5
  (package
    (name "rust-routefinder")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "routefinder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1db6w2bqix8gjpgs74hs73kr6xgs9bf2yy0dppb6fqisjk4d6w89"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-memchr" ,rust-memchr-2)
                       ("rust-smartcow" ,rust-smartcow-0.2)
                       ("rust-smartstring" ,rust-smartstring-1))))
    (home-page "https://github.com/jbr/routefinder")
    (synopsis "router")
    (description "router")
    (license (list license:expat license:asl2.0))))

(define-public rust-route-recognizer-0.3
  (package
    (name "rust-route-recognizer")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "route-recognizer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ikp3blbina00jdbifxw1c9whg6mljli24lq5pv82iar53xr9axg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rustasync/route-recognizer")
    (synopsis
     "Recognizes URL patterns with support for dynamic and wildcard segments")
    (description
     "Recognizes URL patterns with support for dynamic and wildcard segments")
    (license license:expat)))

(define-public rust-path-tree-0.2
  (package
    (name "rust-path-tree")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "path-tree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ank4ajbj4z2n5srdfx9qmjnbwfj5czm833md1x50h2n4sbn00sd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/viz-rs/path-tree")
    (synopsis
     "path-tree is a lightweight high performance HTTP request router for Rust")
    (description
     "path-tree is a lightweight high performance HTTP request router for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-gonzales-0.0.3
  (package
    (name "rust-gonzales")
    (version "0.0.3-beta")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gonzales" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01w0prwnqqp527qqb593fd25y4cnhgczk1fz9k8yygp4l3h3m9j7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/rust-darpi/darpi.git")
    (synopsis "This crate provides routing objects for darpi")
    (description "This crate provides routing objects for darpi")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-router-0.2
  (package
    (name "rust-actix-router")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "actix-router" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b258dplqmria44mv1zzjpmm2xrpdzwcqcz3jg41z7k4ffprklia"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytestring" ,rust-bytestring-0.1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/actix/actix-web")
    (synopsis "Resource path matching and router")
    (description "Resource path matching and router")
    (license (list license:expat license:asl2.0))))

(define-public rust-matchit-0.8
  (package
    (name "rust-matchit")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "matchit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "168l27mrdq6681nwqd1f7mc6znr83hy06qaqvcp4bbq4azab8z9m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-development-inputs (("rust-actix-router" ,rust-actix-router-0.2)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-gonzales" ,rust-gonzales-0.0.3)
                                   ("rust-hyper" ,rust-hyper-0.14)
                                   ("rust-path-tree" ,rust-path-tree-0.2)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-route-recognizer" ,rust-route-recognizer-0.3)
                                   ("rust-routefinder" ,rust-routefinder-0.5)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower" ,rust-tower-0.4))))
    (home-page "https://github.com/ibraheemdev/matchit")
    (synopsis "A high performance, zero-copy URL router.")
    (description
     "This package provides a high performance, zero-copy URL router.")
    (license (list license:expat license:bsd-3))))

(define-public rust-memchr-2
  (package
    (name "rust-memchr")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memchr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jf1kicqa4vs9lyzj4v4y1p90q0dh87hvhsdd5xvhnp527sw8gaj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-1))))
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis
     "Provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32) routines for
1, 2 or 3 byte search and single substring search.
")
    (description
     "This package provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32)
routines for 1, 2 or 3 byte search and single substring search.")
    (license (list license:unlicense license:expat))))

(define-public rust-natord-1
  (package
    (name "rust-natord")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "natord" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z75spwag3ch20841pvfwhh3892i2z2sli4pzp1jgizbipdrd39h"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/lifthrasiir/rust-natord")
    (synopsis "Natural ordering for Rust")
    (description "Natural ordering for Rust")
    (license license:expat)))

(define-public rust-concat-with-0.2
  (package
    (name "rust-concat-with")
    (version "0.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "concat-with" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0krfqby940vpza7df2r08gahk22r7a569xgmwwp46pgnrp4pylj5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://magiclen.org/concat-with")
    (synopsis "Extend the function of the `concat!` macro in `std`.")
    (description "Extend the function of the `concat!` macro in `std`.")
    (license license:expat)))

(define-public rust-slash-formatter-3
  (package
    (name "rust-slash-formatter")
    (version "3.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "slash-formatter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gzz6ggczm6s9ayz5wja20x0lr0ljmhzfx5k61107msrxglpyxhl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-concat-with" ,rust-concat-with-0.2))))
    (home-page "https://magiclen.org/slash-formatter")
    (synopsis
     "This crate provides functions to deal with slashes and backslashes in strings.")
    (description
     "This crate provides functions to deal with slashes and backslashes in strings.")
    (license license:expat)))

(define-public rust-path-dedot-3
  (package
    (name "rust-path-dedot")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "path-dedot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15wkx8q3vra34fslzlg1lkq7liyxwqrpbxiz44a28wa7w3bhmfh7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://magiclen.org/path-dedot")
    (synopsis
     "A library for extending `Path` and `PathBuf` in order to parse the path which contains dots.")
    (description
     "This package provides a library for extending `Path` and `@code{PathBuf`} in
order to parse the path which contains dots.")
    (license license:expat)))

(define-public rust-path-absolutize-3
  (package
    (name "rust-path-absolutize")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "path-absolutize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xc36c5lz187wy452qph3lrr41x8ffgxk1clj2s9b8czwwgkibz4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-path-dedot" ,rust-path-dedot-3))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-slash-formatter" ,rust-slash-formatter-3))))
    (home-page "https://magiclen.org/path-absolutize")
    (synopsis
     "A library for extending `Path` and `PathBuf` in order to get an absolute path and remove the containing dots.")
    (description
     "This package provides a library for extending `Path` and `@code{PathBuf`} in
order to get an absolute path and remove the containing dots.")
    (license license:expat)))

(define-public rust-path-slash-0.2
  (package
    (name "rust-path-slash")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "path-slash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hjgljv4vy97qqw9gxnwzqhhpysjss2yhdphfccy3c388afhk48y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-lazy-static" ,rust-lazy-static-1))))
    (home-page "https://github.com/rhysd/path-slash")
    (synopsis "Conversion to/from a file path from/to slash path")
    (description "Conversion to/from a file path from/to slash path")
    (license license:expat)))

(define-public rust-pathdiff-0.2
  (package
    (name "rust-pathdiff")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pathdiff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pa4dcmb7lwir4himg1mnl97a05b2z0svczg62l8940pbim12dc8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-camino" ,rust-camino-1))))
    (home-page "https://github.com/Manishearth/pathdiff")
    (synopsis "Library for diffing paths to obtain relative paths")
    (description "Library for diffing paths to obtain relative paths")
    (license (list license:expat license:asl2.0))))

(define-public rust-unscanny-0.1
  (package
    (name "rust-unscanny")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unscanny" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ivbipc1rnq15fhzgna41p1h01ncq4shycii72f3x5d7czq2mpz9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/typst/unscanny")
    (synopsis "Painless string scanning.")
    (description "Painless string scanning.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pubgrub-0.2
  (package
    (name "rust-pubgrub")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pubgrub" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z9qdzjfq4yqfzrh2rjx6m2a08k8y9vda41w68x78pazmm94blfd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/pubgrub-rs/pubgrub")
    (synopsis "PubGrub version solving algorithm")
    (description "@code{PubGrub} version solving algorithm")
    (license license:mpl2.0)))

(define-public rust-pretty-assertions-1
  (package
    (name "rust-pretty-assertions")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pretty_assertions" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mgp1ajl3fdc55h989ph48znnk86m41j9dqnpg80yy5a435rnpm2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ctor" ,rust-ctor-0.1)
                       ("rust-diff" ,rust-diff-0.1)
                       ("rust-output-vt100" ,rust-output-vt100-0.1)
                       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page
     "https://github.com/rust-pretty-assertions/rust-pretty-assertions")
    (synopsis
     "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding colorful diffs.")
    (description
     "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding
colorful diffs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pretty-assertions-1
  (package
    (inherit rust-pretty-assertions-1)
    (name "rust-pretty-assertions")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pretty_assertions" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rmsnqlpmpfjp5gyi31xgc48kdhc1kqn246bnc494nwadhdfwz5g"))))))

(define-public rust-pyo3-log-0.9
  (package
    (name "rust-pyo3-log")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-log" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gcqycjfjvr2j913xfppzrskisck4k1k1g14vfz06115wy78042c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pyo3" ,rust-pyo3-0.15))))
    (home-page "https://github.com/vorner/pyo3-log")
    (synopsis "Logging bridge from pyo3 native extension to python")
    (description "Logging bridge from pyo3 native extension to python")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pep508-rs-0.3
  (package
    (name "rust-pep508-rs")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pep508_rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07lbv11izhifkjcgd4l6x4fy0167x0h0y71j48qq6kqgx8xm234i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pep440-rs" ,rust-pep440-rs-0.4)
                       ("rust-pyo3" ,rust-pyo3-0.20)
                       ("rust-pyo3-log" ,rust-pyo3-log-0.9)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/konstin/pep508_rs")
    (synopsis
     "A library for python dependency specifiers, better known as PEP 508")
    (description
     "This package provides a library for python dependency specifiers, better known
as PEP 508")
    (license (list license:asl2.0 license:bsd-2))))

(define-public rust-pep440-rs-0.4
  (package
    (name "rust-pep440-rs")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pep440_rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "189nr65r9ck80di0b36y8c2bikz5w36dpmqc9r78ndyy8ff9zhp0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pyo3" ,rust-pyo3-0.20)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))
       #:cargo-development-inputs (("rust-indoc" ,rust-indoc-2))))
    (home-page "https://github.com/konstin/pep440-rs")
    (synopsis
     "A library for python version numbers and specifiers, implementing PEP 440")
    (description
     "This package provides a library for python version numbers and specifiers,
implementing PEP 440")
    (license (list license:asl2.0 license:bsd-2))))

(define-public rust-pep440-rs-0.6
  (package
    (inherit rust-pep440-rs-0.4)
    (name "rust-pep440-rs")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pep440_rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "028f986wzlr0p65pkcsl9c4hh97n07li8xn5r98145y9gq75f2na"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pubgrub" ,rust-pubgrub-0.2)
                       ("rust-pyo3" ,rust-pyo3-0.21)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-unscanny" ,rust-unscanny-0.1))
       #:cargo-development-inputs (("rust-indoc" ,rust-indoc-2))))))

(define-public rust-pyproject-toml-0.9
  (package
    (name "rust-pyproject-toml")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyproject-toml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ndb0bkn22ynhv69qx7xd8fjz36iyxchic3v9dakralrbxsdvhwm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-pep440-rs" ,rust-pep440-rs-0.4)
                       ("rust-pep508-rs" ,rust-pep508-rs-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-toml" ,rust-toml-0.8))))
    (home-page "https://github.com/PyO3/pyproject-toml-rs.git")
    (synopsis "pyproject.toml parser in Rust")
    (description "pyproject.toml parser in Rust")
    (license license:expat)))

(define-public rust-is-terminal-0.4
  (package
    (name "rust-is-terminal")
    (version "0.4.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "is-terminal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12vk6g0f94zlxl6mdh5gc4jdjb469n9k9s7y3vb0iml05gpzagzj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hermit-abi" ,rust-hermit-abi-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/sunfishcode/is-terminal")
    (synopsis "Test whether a given stream is a terminal")
    (description "Test whether a given stream is a terminal")
    (license license:expat)))

(define-public rust-yansi-1
  (package
    (name "rust-yansi")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yansi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-is-terminal" ,rust-is-terminal-0.4))))
    (home-page "https://github.com/SergioBenitez/yansi")
    (synopsis "A dead simple ANSI terminal color painting library.")
    (description
     "This package provides a dead simple ANSI terminal color painting library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-goldenfile-1
  (package
    (name "rust-goldenfile")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "goldenfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11y5wfsnhaghhg2crynpqgw2i3h6kz7b95wwl4765v5scm1c9md0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-similar-asserts" ,rust-similar-asserts-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-yansi" ,rust-yansi-1))))
    (home-page "https://github.com/calder/rust-goldenfile")
    (synopsis "Simple goldenfile testing library")
    (description "Simple goldenfile testing library")
    (license license:expat)))

(define-public rust-thiserror-impl-1
  (package
    (name "rust-thiserror-impl")
    (version "1.0.60")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0945q2hk1rqdzjz2zqakxbddwm4h26k5c0wdncdarhvfq10h0iz2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description "Implementation detail of the `thiserror` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-1
  (package
    (name "rust-thiserror")
    (version "1.0.60")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "067wi7pb1zn9jhhk82w0ppmvjwa00nwkp4m9j77rvpaqra1r17jp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "derive(Error)")
    (license (list license:expat license:asl2.0))))

(define-public rust-uuid-macro-internal-1
  (package
    (name "rust-uuid-macro-internal")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uuid-macro-internal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18n10d9himcn2a5lwc3hw8178j6hdk1pidxkk9nf71z6rfkvx0cq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "")
    (synopsis "Private implementation details of the uuid! macro.")
    (description "Private implementation details of the uuid! macro.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-uuid-1
  (package
    (name "rust-uuid")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uuid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h7wks153j08xmdk06wnza3is8pn6j37hihd3kfv95xsxrzwz0x1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-atomic" ,rust-atomic-0.5)
                       ("rust-borsh" ,rust-borsh-1)
                       ("rust-borsh-derive" ,rust-borsh-derive-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-slog" ,rust-slog-2)
                       ("rust-uuid-macro-internal" ,rust-uuid-macro-internal-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-zerocopy" ,rust-zerocopy-0.6))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
     "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-newtype-uuid-1
  (package
    (name "rust-newtype-uuid")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "newtype-uuid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vjaghfpf4q7ghdi5n5xnjsf732lbzwrfcmgpq0y8wh8crycn9im"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/oxidecomputer/newtype-uuid")
    (synopsis "Newtype wrapper around UUIDs")
    (description "Newtype wrapper around UUIDs")
    (license (list license:expat license:asl2.0))))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.38")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chrono" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "009l8vc5p8750vn02z30mblg4pv2qhkbfizhfwmzc6vpy5nr67x2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-tzdata" ,rust-android-tzdata-0.1)
                       ("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-iana-time-zone" ,rust-iana-time-zone-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pure-rust-locales" ,rust-pure-rust-locales-0.8)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-quick-junit-0.4
  (package
    (name "rust-quick-junit")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quick-junit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m642xj8zs7nsnz891dpaldk2mf7k1wm1y1dvw9lj4ba82jsdhfg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-newtype-uuid" ,rust-newtype-uuid-1)
                       ("rust-quick-xml" ,rust-quick-xml-0.31)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-goldenfile" ,rust-goldenfile-1)
                                   ("rust-owo-colors" ,rust-owo-colors-4))))
    (home-page "https://github.com/nextest-rs/quick-junit")
    (synopsis "Data model and serializer for JUnit/XUnit XML")
    (description "Data model and serializer for JUnit/XUnit XML")
    (license (list license:asl2.0 license:expat))))

(define-public rust-quote-1
  (package
    (name "rust-quote")
    (version "1.0.23")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quote" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ywwzw5xfwwgq62ihp4fbjbfdjb3ilss2vh3fka18ai59lvdhml8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1))
       #:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-1
  (package
    (name "rust-regex")
    (version "1.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hxkd814n4irind8im5c9am221ri6bprx49nc7yxv02ykhd9a2rq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-regex-automata" ,rust-regex-automata-0.4)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-env-logger" ,rust-env-logger-0.9)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-regex-test" ,rust-regex-test-0.1))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
     "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (description
     "An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pmutil-0.6
  (package
    (name "rust-pmutil")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pmutil" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ih9gbgcygfyrxqlgm3smffzqngzlnlpn5lb5l6h8n1c1k3hp92j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/kdy1/rust-pmutil")
    (synopsis "Utils for proc-macro")
    (description "Utils for proc-macro")
    (license (list license:asl2.0 license:expat))))

(define-public rust-result-like-derive-0.5
  (package
    (name "rust-result-like-derive")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "result-like-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r8g5pi0lgvbm4q5pdcgajdbrvcgsvjq3ingf1ixd5780965gmm8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pmutil" ,rust-pmutil-0.6)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/youknowone/result-like")
    (synopsis "derive macros for result-like")
    (description "derive macros for result-like")
    (license license:expat)))

(define-public rust-result-like-0.5
  (package
    (name "rust-result-like")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "result-like" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1px9d3n4mfpk5ycg95qnsmi7a9khcn16rgr6bimhazbaxwpigxxb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-result-like-derive" ,rust-result-like-derive-0.5))
       #:cargo-development-inputs (("rust-is-macro" ,rust-is-macro-0.3))))
    (home-page "https://github.com/youknowone/result-like")
    (synopsis "Option/Result-like monad interface for your own enum")
    (description "Option/Result-like monad interface for your own enum")
    (license license:expat)))

(define-public rust-schemars-derive-0.8
  (package
    (name "rust-schemars-derive")
    (version "0.8.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "schemars_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rq9sdcf5hyvsyj9v9nfy2jgjbjzaldjq4i6y2fcz72xlrpzsry7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-serde-derive-internals" ,rust-serde-derive-internals-0.26)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://graham.cool/schemars/")
    (synopsis "Macros for #[derive(JsonSchema)], for use with schemars")
    (description
     "Macros for #[derive(@code{JsonSchema})], for use with schemars")
    (license license:expat)))

(define-public rust-smol-str-0.2
  (package
    (name "rust-smol-str")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smol_str" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jca0hyrwnv428q5gxhn2s8jsvrrkyrb0fyla9x37056mmimb176"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags `("--" "--skip=check_code_formatting")
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/rust-analyzer/smol_str")
    (synopsis "small-string optimized string type with O(1) clone")
    (description "small-string optimized string type with O(1) clone")
    (license (list license:expat license:asl2.0))))

(define-public rust-schemars-0.8
  (package
    (name "rust-schemars")
    (version "0.8.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "schemars" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aadpjkaq7yl11b02pg4mwanylck328zg0q7w56dv6j89568z8j5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                       ("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.4)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-dyn-clone" ,rust-dyn-clone-1)
                       ("rust-either" ,rust-either-1)
                       ("rust-enumset" ,rust-enumset-1)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-schemars-derive" ,rust-schemars-derive-0.8)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-smol-str" ,rust-smol-str-0.2)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (inputs (list rust-smol-str-0.2))
    (home-page "https://graham.cool/schemars/")
    (synopsis "Generate JSON Schemas from Rust code")
    (description "Generate JSON Schemas from Rust code")
    (license license:expat)))

(define-public rust-seahash-4
  (package
    (name "rust-seahash")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "seahash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sxsb64np6bvnppjz5hg4rqpnkczhsl8w8kf2a5lr1c08xppn40w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-quickcheck" ,rust-quickcheck-0.9))))
    (home-page "https://gitlab.redox-os.org/redox-os/seahash")
    (synopsis
     "A blazingly fast, portable hash function with proven statistical guarantees.")
    (description
     "This package provides a blazingly fast, portable hash function with proven
statistical guarantees.")
    (license license:expat)))

(define-public rust-serde-wasm-bindgen-0.6
  (package
    (name "rust-serde-wasm-bindgen")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-wasm-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17zpasbscy4nk5xd4b41ld8356hq6nam2imcjxj9d2yf5c8k452c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-getrandom" ,rust-getrandom-0.2)
                                   ("rust-maplit" ,rust-maplit-1)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/RReverser/serde-wasm-bindgen")
    (synopsis "Native Serde adapter for wasm-bindgen")
    (description "Native Serde adapter for wasm-bindgen")
    (license license:expat)))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.113")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ycaiff7ar4qx5sy9kvi1kv9rnnfl15kcfmhxiiwknn3n5q1p039"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-indoc" ,rust-indoc-2)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-stacker" ,rust-serde-stacker-0.1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-test-1
  (package
    (name "rust-serde-test")
    (version "1.0.152")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17pdigm0w1wvch7vpnk13199wn3gmkb0883l0hr53qv75l6j249n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags `("--" "--skip=assert::assert_de_tokens"
                            "--skip=assert::assert_de_tokens_error"
                            "--skip=assert::assert_ser_tokens"
                            "--skip=assert::assert_ser_tokens_error"
                            "--skip=assert::assert_tokens"
                            "--skip=token::Token::Enum"
                            "--skip=token::Token::NewtypeStruct"
                            "--skip=token::Token::NewtypeVariant"
                            "--skip=token::Token::Struct"
                            "--skip=token::Token::StructVariant"
                            "--skip=token::Token::TupleStruct"
                            "--skip=token::Token::TupleVariant"
                            "--skip=token::Token::UnitStruct"
                            "--skip=token::Token::UnitVariant")
       #:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/serde-rs/test")
    (synopsis "Token De/Serializer for testing De/Serialize implementations")
    (description
     "Token De/Serializer for testing De/Serialize implementations")
    (license (list license:expat license:asl2.0))))

(define-public rust-xml-rs-0.8
  (package
    (name "rust-xml-rs")
    (version "0.8.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xml-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "072lwmk6y84gha04rr13z44y9daq3yfh6kgs7vv8wfh8274rv0sj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://lib.rs/crates/xml-rs")
    (synopsis "An XML library in pure Rust")
    (description "An XML library in pure Rust")
    (license license:expat)))

(define-public rust-num-cmp-0.1
  (package
    (name "rust-num-cmp")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-cmp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1alavi36shn32b3cwbmkncj1wal3y3cwzkm21bxy5yil5hp5ncv3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/lifthrasiir/num-cmp")
    (synopsis "Comparison between differently typed numbers")
    (description "Comparison between differently typed numbers")
    (license (list license:expat license:asl2.0))))

(define-public rust-iso8601-0.6
  (package
    (name "rust-iso8601")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "iso8601" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lqif1zp19fjmrbhcdjx0ydnljax3090san5zq8r1x98x9rmsklj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/badboy/iso8601")
    (synopsis "Parsing ISO8601 dates using nom")
    (description "Parsing ISO8601 dates using nom")
    (license license:expat)))

(define-public rust-juniper-codegen-0.15
  (package
    (name "rust-juniper-codegen")
    (version "0.15.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "juniper_codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nvigsc1yrfv09wx1yv830dd60ay556haz87p80h7m8s0rqpdsdf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page
     "https://github.com/graphql-rust/juniper/tree/master/juniper_codegen")
    (synopsis "Code generation for `juniper` crate.")
    (description "Code generation for `juniper` crate.")
    (license license:bsd-2)))

(define-public rust-graphql-parser-0.3
  (package
    (name "rust-graphql-parser")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "graphql-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wp6vnrhgi6q3b942zkc6p4mi104gbw71pnc0d5c1ps7ab7d9ayi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-combine" ,rust-combine-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/graphql-rust/graphql-parser")
    (synopsis
     "    A parser, AST and serializer for graphql query language and scheme
    definition language (sometimes called IDL).
")
    (description
     "This package provides a parser, AST and serializer for graphql query language
and scheme definition language (sometimes called IDL).")
    (license (list license:expat license:asl2.0))))

(define-public rust-derive-utils-0.11
  (package
    (name "rust-derive-utils")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gx7giwn8x427d5f8c92n9h0hhcqdsasvz7i8iq2rqffvhalqask"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/taiki-e/derive_utils")
    (synopsis
     "A procedural macro helper for easily writing derive macros for enums.
")
    (description
     "This package provides a procedural macro helper for easily writing derive macros
for enums.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-futures-enum-0.1
  (package
    (name "rust-futures-enum")
    (version "0.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-enum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07kvpnr21qalhw4hw44h3335wi0lgrdf02n1vglm4flhwx6x28il"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-derive-utils" ,rust-derive-utils-0.11)
                       ("rust-find-crate" ,rust-find-crate-0.6)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/taiki-e/futures-enum")
    (synopsis
     "#[derive(Future, Stream, Sink, AsyncRead, AsyncWrite, AsyncSeek, AsyncBufRead)] for enums.
")
    (description
     "#[derive(Future, Stream, Sink, @code{AsyncRead}, @code{AsyncWrite},
@code{AsyncSeek}, @code{AsyncBufRead})] for enums.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-bson-1
  (package
    (name "rust-bson")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bson" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cbc9mbjm4imzcv95y8r6rgl6kgv7ka5ifhhlxaqi4sv0dwaa2ny"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-decimal" ,rust-decimal-2)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-uuid" ,rust-uuid-0.8))))
    (home-page "https://github.com/mongodb/bson-rust")
    (synopsis "Encoding and decoding support for BSON in Rust")
    (description "Encoding and decoding support for BSON in Rust")
    (license license:expat)))

(define-public rust-juniper-0.15
  (package
    (name "rust-juniper")
    (version "0.15.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "juniper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dg1b5msr8k4kwmam2h0f64z7aamk4799vdh3cg55c881idclpc7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bson" ,rust-bson-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-tz" ,rust-chrono-tz-0.5)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-enum" ,rust-futures-enum-0.1)
                       ("rust-graphql-parser" ,rust-graphql-parser-0.3)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-juniper-codegen" ,rust-juniper-codegen-0.15)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smartstring" ,rust-smartstring-1)
                       ("rust-static-assertions" ,rust-static-assertions-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8))))
    (home-page "https://graphql-rust.github.io/juniper")
    (synopsis "GraphQL server library.")
    (description "@code{GraphQL} server library.")
    (license license:bsd-2)))

(define-public rust-fraction-0.13
  (package
    (name "rust-fraction")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fraction" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y6nh9qyfidm6hsp85wf1kv7l7nc9anzvj214bnln6ylz0fsw9rh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-juniper" ,rust-juniper-0.15)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num" ,rust-num-0.4)
                       ("rust-postgres-types" ,rust-postgres-types-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/dnsl48/fraction.git")
    (synopsis "Lossless fractions and decimals; drop-in float replacement")
    (description "Lossless fractions and decimals; drop-in float replacement")
    (license (list license:expat license:asl2.0))))

(define-public rust-jsonschema-0.17
  (package
    (name "rust-jsonschema")
    (version "0.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jsonschema" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y397fmb7qkah166lq5q39p9hizj9sls09xnvwc936pwgr7iy1ra"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bytecount" ,rust-bytecount-0.6)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.11)
                       ("rust-fraction" ,rust-fraction-0.13)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-iso8601" ,rust-iso8601-0.6)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-num-cmp" ,rust-num-cmp-0.1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/Stranger6667/jsonschema-rs")
    (synopsis "A crate for performing JSON schema validation")
    (description
     "This package provides a crate for performing JSON schema validation")
    (license license:expat)))

(define-public rust-serde-with-macros-3
  (package
    (name "rust-serde-with-macros")
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_with_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05gvlsjrwq2fbah05w6di0kngwr2w106gmkcffgqfyzl1vzpg1an"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-darling" ,rust-darling-0.20)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/jonasbb/serde_with/")
    (synopsis "proc-macro library for serde_with")
    (description "proc-macro library for serde_with")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-internals-0.29
  (package
    (name "rust-serde-derive-internals")
    (version "0.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive_internals" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04g7macx819vbnxhi52cx0nhxi56xlhrybgwybyy7fb9m4h6mlhq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://serde.rs")
    (synopsis "AST representation used by Serde derive macros. Unstable.")
    (description "AST representation used by Serde derive macros.  Unstable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-schemars-derive-0.8
  (package
    (name "rust-schemars-derive")
    (version "0.8.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "schemars_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j9znm9110a8agfbf16d5fynn9chd3hxv41p8n742bg0lxx2npqq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-serde-derive-internals" ,rust-serde-derive-internals-0.29)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://graham.cool/schemars/")
    (synopsis "Macros for #[derive(JsonSchema)], for use with schemars")
    (description
     "Macros for #[derive(@code{JsonSchema})], for use with schemars")
    (license license:expat)))

(define-public rust-textwrap-0.16
  (package
    (name "rust-textwrap")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "textwrap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fgqn3mg9gdbjxwfxl76fg0qiq53w3mk4hdh1x40jylnz39k9m13"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-hyphenation" ,rust-hyphenation-0.8)
                       ("rust-smawk" ,rust-smawk-0.3)
                       ("rust-terminal-size" ,rust-terminal-size-0.2)
                       ("rust-unicode-linebreak" ,rust-unicode-linebreak-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))
       #:cargo-development-inputs (("rust-termion" ,rust-termion-2)
                                   ("rust-unic-emoji-char" ,rust-unic-emoji-char-0.9)
                                   ("rust-version-sync" ,rust-version-sync-0.9))))
    (home-page "https://github.com/mgeisler/textwrap")
    (synopsis
     "Library for word wrapping, indenting, and dedenting strings. Has optional support for Unicode and emojis as well as machine hyphenation.")
    (description
     "Library for word wrapping, indenting, and dedenting strings.  Has optional
support for Unicode and emojis as well as machine hyphenation.")
    (license license:expat)))

(define-public rust-schemars-0.8
  (package
    (name "rust-schemars")
    (version "0.8.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "schemars" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vr1dwpx900b3d824f309lmzc6cj61a1dzq1zc3nbd4wj7b7wvpw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                       ("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.4)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-dyn-clone" ,rust-dyn-clone-1)
                       ("rust-either" ,rust-either-1)
                       ("rust-enumset" ,rust-enumset-1)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-schemars-derive" ,rust-schemars-derive-0.8)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-smol-str" ,rust-smol-str-0.2)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8)
                       ("rust-uuid" ,rust-uuid-1))))
    (inputs (list rust-smol-str-0.2))
    (home-page "https://graham.cool/schemars/")
    (synopsis "Generate JSON Schemas from Rust code")
    (description "Generate JSON Schemas from Rust code")
    (license license:expat)))

(define-public rust-serde-with-3
  (package
    (name "rust-serde-with")
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_with" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10g9jnv1x5jw2q1nh207fbrsyd72n3g1hvdp8m7ncs2s5ikd23hv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=time::test_timestamp_seconds_systemtime"
                            "--skip=time::test_timestamp_seconds_with_frac_systemtime"
                            "--skip=test_docs_rs_url_point_to_current_version"
                            "--skip=test_serde_with_macros_dependency")
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-doc-comment" ,rust-doc-comment-0.3)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-with-macros" ,rust-serde-with-macros-3)
                       ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs (("rust-expect-test" ,rust-expect-test-1)
                                   ("rust-fnv" ,rust-fnv-1)
                                   ("rust-glob" ,rust-glob-0.3)
                                   ("rust-jsonschema" ,rust-jsonschema-0.17)
                                   ("rust-mime" ,rust-mime-0.3)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-rmp-serde" ,rust-rmp-serde-1)
                                   ("rust-ron" ,rust-ron-0.8)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-schemars" ,rust-schemars-0.8)
                                   ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.6)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                                   ("rust-version-sync" ,rust-version-sync-0.9)
                                   ("rust-xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://github.com/jonasbb/serde_with/")
    (synopsis "Custom de/serialization functions for Rust's serde")
    (description "Custom de/serialization functions for Rust's serde")
    (license (list license:expat license:asl2.0))))

(define-public rust-shellexpand-3
  (package
    (name "rust-shellexpand")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "shellexpand" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qc0d1zsaha7hphmyp0323zbndby5c6hrz9r9i44sab5lvg7s76x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dirs" ,rust-dirs-4)
                       ("rust-os-str-bytes" ,rust-os-str-bytes-6))))
    (home-page "https://gitlab.com/ijackson/rust-shellexpand")
    (synopsis "Shell-like expansions in strings")
    (description "Shell-like expansions in strings")
    (license (list license:expat license:asl2.0))))

(define-public rust-similar-2
  (package
    (name "rust-similar")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "similar" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08gspd5a2w21k9s641z6fxvrzj34611rcjbjr5685vh9r8da9zij"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1))
       #:cargo-development-inputs (("rust-console" ,rust-console-0.15)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/mitsuhiko/similar")
    (synopsis "A diff library for Rust")
    (description "This package provides a diff library for Rust")
    (license license:asl2.0)))

(define-public rust-static-assertions-1
  (package
    (name "rust-static-assertions")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "static_assertions" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nvzqz/static-assertions-rs")
    (synopsis "Compile-time assertions to ensure that invariants are met.")
    (description "Compile-time assertions to ensure that invariants are met.")
    (license (list license:expat license:asl2.0))))

(define-public rust-strum-macros-0.26
  (package
    (name "rust-strum-macros")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strum_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "057y4cpbqknwfh0amrmm407y3ki25fyjb7a96gshgqps871ak4by"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--" "--skip=enum_discriminants"
                            "--skip=static_variants_array")
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-strum" ,rust-strum-0.25))))
    (home-page "https://github.com/Peternator7/strum")
    (synopsis "Helpful macros for working with enums and strings")
    (description "Helpful macros for working with enums and strings")
    (license license:expat)))

(define-public rust-strum-macros-0.26
  (package
    (inherit rust-strum-macros-0.26)
    (name "rust-strum-macros")
    (version "0.26.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strum_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0in9jvbb3g16x8fj7lf91vwzj98319hj3z8lpaaa9h42ybd5kky6"))))
    (arguments
     `(#:cargo-test-flags '("--release" "--" "--skip=enum_discriminants"
                            "--skip=static_variants_array")
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-strum" ,rust-strum-0.26))))))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.55")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w5qxgrqp6vyrk4347dajjwvf2xbfzrx0m96qfpxyzlnpwyinah0"))
       (snippet #~(begin
                    (use-modules (guix build utils))
                    (substitute* "Cargo.toml"
                      (("test = \\[\"syn-test-suite/all-features\"\\]")
                       ""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-automod" ,rust-automod-1)
                                   ("rust-flate2" ,rust-flate2-1)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-reqwest" ,rust-reqwest-0.12)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-syn-test-suite" ,rust-syn-test-suite-0.0.0)
                                   ("rust-tar" ,rust-tar-0.4)
                                   ("rust-termcolor" ,rust-termcolor-1)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.59")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nk1f98z027qdjwsn756hnshp0y4cka4pq729ig6awdhqzzk2raa"))
       (snippet #~(begin
                    (use-modules (guix build utils))
                    (substitute* "Cargo.toml"
                      (("test = \\[\"syn-test-suite/all-features\"\\]")
                       ""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-automod" ,rust-automod-1)
                                   ("rust-flate2" ,rust-flate2-1)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-reqwest" ,rust-reqwest-0.12)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-syn-test-suite" ,rust-syn-test-suite-0.0.0)
                                   ("rust-tar" ,rust-tar-0.4)
                                   ("rust-termcolor" ,rust-termcolor-1)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "This package provides Parser for Rust source code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tempfile-3
  (package
    (name "rust-tempfile")
    (version "3.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ypkl7rvv57n16q28psxpb61rnyhmfaif12ascdnsyljm90l3kh1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-fastrand" ,rust-fastrand-2)
                       ("rust-redox-syscall" ,rust-redox-syscall-0.4)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "A library for managing temporary files and directories.")
    (description
     "This package provides a library for managing temporary files and directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-test-case-3
  (package
    (name "rust-test-case")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "test-case" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a380yzm6787737cw7s09jqmkn9035hghahradl2ikdg2gfm09gb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-regex" ,rust-regex-1)
                       ("rust-test-case-macros" ,rust-test-case-macros-3))
       #:cargo-development-inputs (("rust-insta" ,rust-insta-1)
                                   ("rust-itertools" ,rust-itertools-0.11)
                                   ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/frondeus/test-case")
    (synopsis
     "Provides #[test_case(...)] procedural macro attribute for generating parametrized test cases easily")
    (description
     "This package provides #[test_case(...)] procedural macro attribute for
generating parametrized test cases easily")
    (license license:expat)))

(define-public rust-thiserror-impl-1
  (package
    (name "rust-thiserror-impl")
    (version "1.0.58")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xylyqcb8rv5yh2yf97hg4n4kg27qccc0ijafr1zqklrhahkn7y6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description "Implementation detail of the `thiserror` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-1
  (package
    (name "rust-thiserror")
    (version "1.0.58")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15rjgd1abi2mzjgzfhrvmsxf9h65n95h6sp8f4s52q4i00wqhih3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "derive(Error)")
    (license (list license:expat license:asl2.0))))

(define-public rust-tikv-jemalloc-sys-0.5
  (package
    (name "rust-tikv-jemalloc-sys")
    (version "0.5.4+5.3.0-patched")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tikv-jemalloc-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lc5vm1p9dqdvd3mn3264zddnd7z6i95ch3y69prnjgxp0y480ll"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "jemalloc")))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/tikv/jemallocator")
    (synopsis "Rust FFI bindings to jemalloc
")
    (description "This package provides a Rust FFI bindings to jemalloc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tikv-jemallocator-0.5
  (package
    (name "rust-tikv-jemallocator")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tikv-jemallocator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yb9sw5jr382x1jnbxj5d7hng8w585lm6ff8gvahcv1sl6w2sq90"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-tikv-jemalloc-sys" ,rust-tikv-jemalloc-sys-0.5))
       #:cargo-development-inputs (("rust-paste" ,rust-paste-1))))
    (home-page "https://github.com/tikv/jemallocator")
    (synopsis "A Rust allocator backed by jemalloc
")
    (description "This package provides a Rust allocator backed by jemalloc")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-test-data-1
  (package
    (name "rust-toml-test-data")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml-test-data" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "085w6r0c3kzdcw7y3k3wq9nk3a5pjkicv41qldzzk5nz5anzls33"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-include-dir" ,rust-include-dir-0.7))))
    (home-page "https://github.com/epage/toml-test-rs")
    (synopsis "TOML test cases")
    (description "TOML test cases")
    (license (list license:expat license:asl2.0))))

(define-public rust-winnow-0.6
  (package
    (name "rust-winnow")
    (version "0.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winnow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13b1kxlgqglp4787nrn4p4bpz4xfxn096v437sr73056jyf2xif3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.3)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-is-terminal" ,rust-is-terminal-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-terminal-size" ,rust-terminal-size-0.2))))
    (home-page "https://github.com/winnow-rs/winnow")
    (synopsis "A byte-oriented, zero-copy, parser combinators library")
    (description
     "This package provides a byte-oriented, zero-copy, parser combinators library")
    (license license:expat)))

(define-public rust-toml-datetime-0.6
  (package
    (name "rust-toml-datetime")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_datetime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1grcrr3gh7id3cy3j700kczwwfbn04p5ncrrj369prjaj9bgvbab"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "A TOML-compatible datetime type")
    (description "This package provides a TOML-compatible datetime type")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-edit-0.22
  (package
    (name "rust-toml-edit")
    (version "0.22.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_edit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v5rkld3cl628dygbngr1gk1cxm4pxmawclpshv0ihp8a1c7h9y1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-kstring" ,rust-kstring-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-spanned" ,rust-serde-spanned-0.6)
                       ("rust-toml-datetime" ,rust-toml-datetime-0.6)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Yet another format-preserving TOML parser.")
    (description "Yet another format-preserving TOML parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-spanned-0.6
  (package
    (name "rust-serde-spanned")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_spanned" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1839b6m5p9ijjmcwamiya2r612ks2vg6w2pp95yg76lr3zh79rkr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Serde-compatible spanned Value")
    (description "Serde-compatible spanned Value")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-0.8
  (package
    (name "rust-toml")
    (version "0.8.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13n4i0gka2m06r7z4sma4ra1bj1jf7axyqydq7lkb1yjc5jna1mg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-spanned" ,rust-serde-spanned-0.6)
                       ("rust-toml-datetime" ,rust-toml-datetime-0.6)
                       ("rust-toml-edit" ,rust-toml-edit-0.22))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-snapbox" ,rust-snapbox-0.4)
                                   ("rust-toml-test-data" ,rust-toml-test-data-1)
                                   ("rust-toml-test-harness" ,rust-toml-test-harness-0.4))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis
     "A native Rust encoder and decoder of TOML-formatted files and streams. Provides
implementations of the standard Serialize/Deserialize traits for TOML data to
facilitate deserializing and serializing Rust structures.
")
    (description
     "This package provides a native Rust encoder and decoder of TOML-formatted files
and streams.  Provides implementations of the standard Serialize/Deserialize
traits for TOML data to facilitate deserializing and serializing Rust
structures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tracing-indicatif-0.3
  (package
    (name "rust-tracing-indicatif")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-indicatif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07cmn4ilw8hdfzc1mirccwkgl160k3x9fhgg7xydj4gy9r181586"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-core" ,rust-tracing-core-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))
       #:cargo-development-inputs (("rust-console" ,rust-console-0.15)
                                   ("rust-dialoguer" ,rust-dialoguer-0.11)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/emersonford/tracing-indicatif")
    (synopsis
     "Tracing layer that automatically creates and manages progress bars for active spans.")
    (description
     "Tracing layer that automatically creates and manages progress bars for active
spans.")
    (license license:expat)))

(define-public rust-nu-ansi-term-0.46
  (package
    (name "rust-nu-ansi-term")
    (version "0.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-ansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-overload" ,rust-overload-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/nushell/nu-ansi-term")
    (synopsis "Library for ANSI terminal colors and styles (bold, underline)")
    (description
     "Library for ANSI terminal colors and styles (bold, underline)")
    (license license:expat)))

(define-public rust-nu-ansi-term-0.49
  (package
    (name "rust-nu-ansi-term")
    (version "0.49.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-ansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2svfnircd9jp06wk55qcbb9v5cadkfcjfg99vm21qdjg0x6wy0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/nushell/nu-ansi-term")
    (synopsis "Library for ANSI terminal colors and styles (bold, underline)")
    (description
     "Library for ANSI terminal colors and styles (bold, underline)")
    (license license:expat)))

(define-public rust-tracing-tree-0.3
  (package
    (name "rust-tracing-tree")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-tree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xb6csq7hpjjr9x7qx1h6r3ra7p2mxvirh4vp71q8r1z5k6rw4v5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.49)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing-core" ,rust-tracing-core-0.1)
                       ("rust-tracing-log" ,rust-tracing-log-0.2)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-log" ,rust-log-0.4)
                                   ("rust-tracing" ,rust-tracing-0.1)
                                   ("rust-ui-test" ,rust-ui-test-0.7))))
    (home-page "https://github.com/davidbarsky/tracing-tree")
    (synopsis "A Tracing Layer which prints a tree of spans and events.")
    (description
     "This package provides a Tracing Layer which prints a tree of spans and events.")
    (license (list license:expat license:asl2.0))))

(define-public rust-typed-arena-2
  (package
    (name "rust-typed-arena")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typed-arena" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0shj0jpmglhgw2f1i4b33ycdzwd1z205pbs1rd5wx7ks2qhaxxka"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/SimonSapin/rust-typed-arena")
    (synopsis "The arena, a fast but limited type of allocator")
    (description "The arena, a fast but limited type of allocator")
    (license license:expat)))

(define-public rust-unic-ucd-category-0.9
  (package
    (name "rust-unic-ucd-category")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unic-ucd-category" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h4ixzplc2s441vc8mc4zxliw6qfqh1ziaiv8pa1pzpwyn8lb38v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-matches" ,rust-matches-0.1)
                       ("rust-unic-char-property" ,rust-unic-char-property-0.9)
                       ("rust-unic-char-range" ,rust-unic-char-range-0.9)
                       ("rust-unic-ucd-version" ,rust-unic-ucd-version-0.9))))
    (home-page "https://github.com/open-i18n/rust-unic/")
    (synopsis "UNIC â Unicode Character Database â General Category")
    (description "UNIC â Unicode Character Database â General Category")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-names2-macros-1
  (package
    (name "rust-unicode-names2-macros")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode_names2_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ib3wyw5flvjipr44d6igj90gb5jqg2dyjd1ajbyypqa7sv3ysyf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-regex" ,rust-regex-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-unicode-names2" ,rust-unicode-names2-1))))
    (home-page "https://github.com/progval/unicode_names2")
    (synopsis "Support macros for `unicode_names2`.")
    (description "Support macros for `unicode_names2`.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-names2-generator-1
  (package
    (name "rust-unicode-names2-generator")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode_names2_generator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h16hhz6zxy4m1jmqncdq36c4b9zc1fa7b7za493rzj2l2xvhi7l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-getopts" ,rust-getopts-0.2)
                       ("rust-log" ,rust-log-0.3)
                       ("rust-phf-codegen" ,rust-phf-codegen-0.11)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/progval/unicode_names2")
    (synopsis "Generates the perfect-hash function used by `unicode_names2`.
")
    (description
     "Generates the perfect-hash function used by `unicode_names2`.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-names2-1
  (package
    (name "rust-unicode-names2")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode_names2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xvnf1zpaqmbmw4bzcrjrjcymg5vgsr9ywjg2shj4yfzjkrfppmd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-phf" ,rust-phf-0.11)
                       ("rust-unicode-names2-generator" ,rust-unicode-names2-generator-1))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
                                   ("rust-unicode-names2-macros" ,rust-unicode-names2-macros-1))))
    (home-page "https://github.com/progval/unicode_names2")
    (synopsis
     "Map characters to and from their name given in the Unicode standard.
This goes to great lengths to be as efficient as possible in both time
and space, with the full bidirectional tables weighing barely 500 KB
but still offering O(1)* look-up in both directions. (*more precisely,
O(length of name).)
")
    (description
     "Map characters to and from their name given in the Unicode standard.  This goes
to great lengths to be as efficient as possible in both time and space, with the
full bidirectional tables weighing barely 500 KB but still offering O(1)*
look-up in both directions. (*more precisely, O(length of name).)")
    (license (list license:expat))))

(define-public rust-unicode-normalization-0.1
  (package
    (name "rust-unicode-normalization")
    (version "0.1.23")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-normalization" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x81a50h2zxigj74b9bqjsirxxbyhmis54kg600xj213vf31cvd5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-tinyvec" ,rust-tinyvec-1))))
    (home-page "https://github.com/unicode-rs/unicode-normalization")
    (synopsis "This crate provides functions for normalization of
Unicode strings, including Canonical and Compatible
Decomposition and Recomposition, as described in
Unicode Standard Annex #15.
")
    (description
     "This crate provides functions for normalization of Unicode strings, including
Canonical and Compatible Decomposition and Recomposition, as described in
Unicode Standard Annex #15.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hoot-0.1
  (package
    (name "rust-hoot")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hoot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mjfrn3yxhd2ll8kk5jhgasn8m2rbhb7va7s6dihin15afvf7spw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/algesten/hoot")
    (synopsis "no_std, allocation free http 1.1 library")
    (description "no_std, allocation free http 1.1 library")
    (license (list license:expat license:asl2.0))))

(define-public rust-hootbin-0.1
  (package
    (name "rust-hootbin")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hootbin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f616q6z7z97p1ylns8hdbikcpbazyad0370mfihkq8sj4brxkzb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fastrand" ,rust-fastrand-2)
                       ("rust-hoot" ,rust-hoot-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/algesten/hoot")
    (synopsis "hoot based library to emulate httpbin")
    (description "hoot based library to emulate httpbin")
    (license (list license:expat license:asl2.0))))

(define-public rust-cookie-store-0.21
  (package
    (name "rust-cookie-store")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cookie_store" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1akbrsgvb66zmbi5kzbanmh10mpqg8khv5anxyv4i4a1x2vycd29"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cookie" ,rust-cookie-0.18)
                       ("rust-idna" ,rust-idna-0.5)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-publicsuffix" ,rust-publicsuffix-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/pfernie/cookie_store")
    (synopsis "Implementation of Cookie storage and retrieval")
    (description "Implementation of Cookie storage and retrieval")
    (license (list license:expat license:asl2.0))))

(define-public rust-ureq-2
  (package
    (name "rust-ureq")
    (version "2.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ureq" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dfw99r7lxizkz7dpyyqz1f2hnrzxn369ank9vlcpcnq33719whi"))
       (snippet #~(begin
                    (use-modules (guix build utils))
                    (substitute* "src/lib.rs"
                      (("```rust")
                       "```no_run"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags `("--"
                            "--skip=error::tests::status_code_error_redirect"
                            "--skip=test::range::read_range_rustls"
                            "--skip=tests::connect_http_google"
                            "--skip=tests::connect_https_google_rustls"
                            "--skip=agent::Agent"
                            "--skip=agent::Agent::request"
                            "--skip=agent::Agent::request_url"
                            "--skip=error::Error"
                            "--skip=error::Error::kind"
                            "--skip=request"
                            "--skip=request_url"
                            "--skip=request::Request"
                            "--skip=request::Request::call"
                            "--skip=request::Request::query"
                            "--skip=request::Request::query_pairs"
                            "--skip=request::Request::send"
                            "--skip=request::Request::send_bytes"
                            "--skip=request::Request::send_form"
                            "--skip=request::Request::send_string"
                            "--skip=request::Request::set"
                            "--skip=response::Response"
                            "--skip=response::Response::charset"
                            "--skip=response::Response::content_type"
                            "--skip=response::Response::into_reader"
                            "--skip=response::Response::into_string")
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-brotli-decompressor" ,rust-brotli-decompressor-2)
                       ("rust-cookie" ,rust-cookie-0.18)
                       ("rust-cookie-store" ,rust-cookie-store-0.21)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hootbin" ,rust-hootbin-0.1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http" ,rust-http-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustls" ,rust-rustls-0.22)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.102)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-socks" ,rust-socks-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.3)
                                   ("rust-rustls" ,rust-rustls-0.22)
                                   ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                                   ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/algesten/ureq")
    (synopsis "Simple, safe HTTP client")
    (description "Simple, safe HTTP client")
    (license (list license:expat license:asl2.0))))

(define-public rust-url-2
  (package
    (name "rust-url")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cs65961miawncdg2z20171w0vqrmraswv2ihdpd8lxp7cp31rii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-idna" ,rust-idna-0.5)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/servo/rust-url")
    (synopsis "URL library for Rust, based on the WHATWG URL Standard")
    (description "URL library for Rust, based on the WHATWG URL Standard")
    (license (list license:expat license:asl2.0))))

(define-public rust-uuid-1
  (package
    (name "rust-uuid")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uuid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q45jxahvysldn3iy04m8xmr8hgig80855y9gq9di8x72v7myfay"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-atomic" ,rust-atomic-0.5)
                       ("rust-borsh" ,rust-borsh-0.10)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-slog" ,rust-slog-2)
                       ("rust-uuid-macro-internal" ,rust-uuid-macro-internal-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-zerocopy" ,rust-zerocopy-0.6))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
     "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-uuid-1
  (package
    (inherit rust-uuid-1)
    (name "rust-uuid")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uuid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h7wks153j08xmdk06wnza3is8pn6j37hihd3kfv95xsxrzwz0x1"))))))

(define-public rust-walkdir-2
  (package
    (name "rust-walkdir")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "walkdir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mnszy33685v8y9js8mw6x2p3iddqs8vfj7n2dhqddnlbirz5340"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-same-file" ,rust-same-file-1)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-winapi-util" ,rust-winapi-util-0.1))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/BurntSushi/walkdir")
    (synopsis "Recursively walk a directory.")
    (description "Recursively walk a directory.")
    (license (list license:unlicense license:expat))))

(define-public rust-ctrlc-3
  (package
    (name "rust-ctrlc")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ctrlc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0iak582cfv3jcprd9apsy6q9glsx7n4ahiv518wcc6yw6yp6a937"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nix" ,rust-nix-0.28)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-signal-hook" ,rust-signal-hook-0.3)
                                   ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/Detegr/rust-ctrlc")
    (synopsis "Easy Ctrl-C handler for Rust projects")
    (description "Easy Ctrl-C handler for Rust projects")
    (license (list license:expat license:asl2.0))))

(define-public rust-version-3
  (package
    (name "rust-version")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "version" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rg0ihhbwkn5j5mv12yjks1cixhh2mn3wsim071gq574zrj90i1s"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/nulldatamap/version")
    (synopsis
     "A very simple library who's job is to return the version of your crate if you're building with Cargo.")
    (description
     "This package provides a very simple library who's job is to return the version
of your crate if you're building with Cargo.")
    (license (list license:expat license:asl2.0))))

(define-public rust-statrs-0.16
  (package
    (name "rust-statrs")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "statrs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08bp7n3rwk41r11ynwl5x7xdc9cv85zw4r7ww117mhfsp8nhcnmk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-approx" ,rust-approx-0.5)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-nalgebra" ,rust-nalgebra-0.29)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/statrs-dev/statrs")
    (synopsis "Statistical computing library for Rust")
    (description
     "This package provides Statistical computing library for Rust.")
    (license license:expat)))

(define-public rust-fastset-0.2
  (package
    (name "rust-fastset")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fastset" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03kgmw0705vranbcngldgdfkx54n7cfn05hgaixqgmlnrj9jjhal"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nanorand" ,rust-nanorand-0.7)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-hashbrown" ,rust-hashbrown-0.14))))
    (home-page "https://github.com/b-vitamins/fastset")
    (synopsis
     "Fast set implementation for dense, bounded integer collections, optimized for quick updates and access")
    (description
     "This package provides Fast set implementation for dense, bounded integer collections, optimized for
quick updates and access.")
    (license license:gpl3)))

(define-public rust-fastset-0.4
  (package
    (inherit rust-fastset-0.2)
    (name "rust-fastset")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fastset" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vilsx2mwd7xys6ivss5k48q3f4xpyalsgsz0254lqh3xq8n7pkq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nanorand" ,rust-nanorand-0.7)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-hashbrown" ,rust-hashbrown-0.14)
                                   ("rust-statrs" ,rust-statrs-0.16))))))

(define-public rust-signvec-0.3
  (package
    (name "rust-signvec")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "signvec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dgh735a6xsz05qkr374rri3vz579dmv11v8xympyk90nnq1q2qn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fastset" ,rust-fastset-0.2)
                       ("rust-nanorand" ,rust-nanorand-0.7)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4))))
    (home-page "https://github.com/b-vitamins/signvec")
    (synopsis
     "Vector implementation for fast, sign-based manipulation of dynamic collections")
    (description
     "This package provides Vector implementation for fast, sign-based manipulation of dynamic collections.")
    (license license:expat)))

(define-public rust-colored-3
  (package
    (name "rust-colored")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "colored" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0plizddhxc4vgkzdbzky5zggyaqfrmyim2d0n6sb7py9j3nf1q7x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-windows-sys" ,rust-windows-sys-0.59))
       #:cargo-development-inputs (("rust-ansi-term" ,rust-ansi-term-0.12)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-rspec" ,rust-rspec-1))))
    (home-page "https://github.com/mackwic/colored")
    (synopsis "The most simple way to add colors in your terminal")
    (description
     "This package provides The most simple way to add colors in your terminal.")
    (license license:mpl2.0)))

(define-public rust-dir-test-macros-0.4
  (package
    (name "rust-dir-test-macros")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dir-test-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zns8frffpsxkks24lrphmsphdd3s4sqwcsvzq029g56nkbm8byl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glob" ,rust-glob-0.3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/fe-lang/dir-test")
    (synopsis "Provides a procedural macro for `dir-test`")
    (description "This package provides a procedural macro for `dir-test`.")
    (license license:asl2.0)))

(define-public rust-dir-test-0.4
  (package
    (name "rust-dir-test")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dir-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yaav4f0r6bsmz8shg7i0db4z9x73xn44dizb7jg6r2qhbz17h32"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dir-test-macros" ,rust-dir-test-macros-0.4))
       #:cargo-development-inputs (("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/fe-lang/dir-test")
    (synopsis "Provides a macro to generate tests from files in a directory")
    (description
     "This package provides a macro to generate tests from files in a directory.")
    (license license:asl2.0)))

(define-public rust-version-ranges-0.1
  (package
    (name "rust-version-ranges")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "version-ranges" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bgl8agnz3k3wsnydiq9qgahf4s0zvdbmbamqczyhazbbi0pkl7q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proptest" ,rust-proptest-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/pubgrub-rs/pubgrub")
    (synopsis
     "Performance-optimized type for generic version ranges and operations on them")
    (description
     "This package provides Performance-optimized type for generic version ranges and operations on them.")
    (license license:mpl2.0)))

(define-public rust-smol-str-0.3
  (package
    (name "rust-smol-str")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smol_str" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "039mj6lc1vkljj17ndlzzkak8kvlmw8ppi6yjdxsh433snfbhxln"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-borsh" ,rust-borsh-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-analyzer/smol_str")
    (synopsis "small-string optimized string type with O(1) clone")
    (description
     "This package provides small-string optimized string type with O(1) clone.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rkyv-derive-0.8
  (package
    (name "rust-rkyv-derive")
    (version "0.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rkyv_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ymv3al6d3qza3lpqdhp7v2lclkdxzl05f14s5swdxls32n40sr4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/rkyv/rkyv")
    (synopsis "Derive macro for rkyv")
    (description "This package provides Derive macro for rkyv.")
    (license license:expat)))

(define-public rust-rend-0.5
  (package
    (name "rust-rend")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05gjxzzsajl61sgif4h0lvagmbry5rm2xak6782j3lccy9mqlpm3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytecheck" ,rust-bytecheck-0.8)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-zerocopy" ,rust-zerocopy-0.8)
                       ("rust-zerocopy-derive" ,rust-zerocopy-derive-0.8))))
    (home-page "https://github.com/djkoloski/rend")
    (synopsis "Cross-platform, endian-aware primitives for Rust")
    (description
     "This package provides Cross-platform, endian-aware primitives for Rust.")
    (license license:expat)))

(define-public rust-munge-macro-0.4
  (package
    (name "rust-munge-macro")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "munge_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pifls5cmx8561wh4hv2way838grybga1v5yrk8gf4sg33cc3d8v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/djkoloski/munge")
    (synopsis "Macro for custom destructuring")
    (description "This package provides Macro for custom destructuring.")
    (license license:expat)))

(define-public rust-munge-0.4
  (package
    (name "rust-munge")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "munge" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pqrlhq0l29mcmqd86xill3465yj1bc9pzq6pw5gdbabr0w2s534"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-munge-macro" ,rust-munge-macro-0.4))))
    (home-page "https://github.com/djkoloski/munge")
    (synopsis "Macro for custom destructuring")
    (description "This package provides Macro for custom destructuring.")
    (license license:expat)))

(define-public rust-rancor-0.1
  (package
    (name "rust-rancor")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rancor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0iyr19x1aryadcyc2zwjbwmskkkjqfbvrjp4l37d3f9434bggxfa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ptr-meta" ,rust-ptr-meta-0.3))))
    (home-page "https://github.com/rkyv/rancor")
    (synopsis "Scalable and efficient error handling without type composition")
    (description
     "This package provides Scalable and efficient error handling without type composition.")
    (license license:expat)))

(define-public rust-ptr-meta-derive-0.3
  (package
    (name "rust-ptr-meta-derive")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ptr_meta_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l9jznaz85cchixyp07v6sxcvjadsyq6lmhjbh98sk0v2pdlwhfa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/rkyv/ptr_meta")
    (synopsis "Proc macros for ptr_meta")
    (description "This package provides Proc macros for ptr_meta.")
    (license license:expat)))

(define-public rust-ptr-meta-0.3
  (package
    (name "rust-ptr-meta")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ptr_meta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "147a6z4qz35gipj9k0d2yh4wygmibhaqsna59vs0d5izdpv7d7py"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ptr-meta-derive" ,rust-ptr-meta-derive-0.3))))
    (home-page "https://github.com/rkyv/ptr_meta")
    (synopsis "radioactive stabilization of the ptr_meta rfc")
    (description
     "This package provides a radioactive stabilization of the ptr_meta rfc.")
    (license license:expat)))

(define-public rust-bytecheck-derive-0.8
  (package
    (name "rust-bytecheck-derive")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytecheck_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zwgvgm7d849av8xdbin93xv1hrs205m1pzg2n1bcjfzw75n6csj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/rkyv/bytecheck")
    (synopsis "Derive macro for bytecheck")
    (description "This package provides Derive macro for bytecheck.")
    (license license:expat)))

(define-public rust-bytecheck-0.8
  (package
    (name "rust-bytecheck")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytecheck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vxw00k85v13x9gl5gmc7svjjanq4aygqqais55ba8sbfhqg9j2h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytecheck-derive" ,rust-bytecheck-derive-0.8)
                       ("rust-ptr-meta" ,rust-ptr-meta-0.3)
                       ("rust-rancor" ,rust-rancor-0.1)
                       ("rust-simdutf8" ,rust-simdutf8-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/rkyv/bytecheck")
    (synopsis "Memory validation framework for Rust")
    (description "This package provides Memory validation framework for Rust.")
    (license license:expat)))

(define-public rust-rkyv-0.8
  (package
    (name "rust-rkyv")
    (version "0.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rkyv" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rbvkcm1ia5rafajf9hlqcw882slm123jj6vzkif2lsmqxqp650y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bytecheck" ,rust-bytecheck-0.8)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-hashbrown" ,rust-hashbrown-0.15)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-munge" ,rust-munge-0.4)
                       ("rust-ptr-meta" ,rust-ptr-meta-0.3)
                       ("rust-rancor" ,rust-rancor-0.1)
                       ("rust-rend" ,rust-rend-0.5)
                       ("rust-rkyv-derive" ,rust-rkyv-derive-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-smol-str" ,rust-smol-str-0.3)
                       ("rust-smol-str" ,rust-smol-str-0.2)
                       ("rust-thin-vec" ,rust-thin-vec-0.2)
                       ("rust-tinyvec" ,rust-tinyvec-1)
                       ("rust-triomphe" ,rust-triomphe-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/rkyv/rkyv")
    (synopsis "Zero-copy deserialization framework for Rust")
    (description
     "This package provides Zero-copy deserialization framework for Rust.")
    (license license:expat)))

(define-public rust-pep440-rs-0.7
  (package
    (name "rust-pep440-rs")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pep440_rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "177vv3fvdsp80x9hi2wigw3hkg7pxq6v4hjzfhrdxqwnyfhmq29i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rkyv" ,rust-rkyv-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.2)
                       ("rust-unscanny" ,rust-unscanny-0.1)
                       ("rust-version-ranges" ,rust-version-ranges-0.1))
       #:cargo-development-inputs (("rust-indoc" ,rust-indoc-2))))
    (home-page "https://github.com/konstin/pep440-rs")
    (synopsis
     "library for python version numbers and specifiers, implementing PEP 440")
    (description
     "This package provides a library for python version numbers and specifiers,
implementing PEP 440.")
    (license (list license:asl2.0 license:bsd-2))))

(define-public rust-boxcar-0.2
  (package
    (name "rust-boxcar")
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "boxcar" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hpbh4w6jd6r3vbwkbb7bxa5yxpbxlzrcp8j0zkggrzhlv2w6897"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ibraheemdev/boxcar")
    (synopsis "concurrent, append-only vector")
    (description "This package provides a concurrent, append-only vector.")
    (license license:expat)))

(define-public rust-pep508-rs-0.9
  (package
    (name "rust-pep508-rs")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pep508_rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01fcbf9vq8ya3shlsmx04fyz5n7h4vm8ixrgrnnzq8a10qkp5vps"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-boxcar" ,rust-boxcar-0.2)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itertools" ,rust-itertools-0.13)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pep440-rs" ,rust-pep440-rs-0.7)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-2)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.2)
                       ("rust-url" ,rust-url-2)
                       ("rust-urlencoding" ,rust-urlencoding-2)
                       ("rust-version-ranges" ,rust-version-ranges-0.1))))
    (home-page "https://github.com/konstin/pep508_rs")
    (synopsis
     "library for python dependency specifiers, better known as PEP 508")
    (description
     "This package provides a library for python dependency specifiers, better known
as PEP 508.")
    (license (list license:asl2.0 license:bsd-2))))

(define-public rust-pep440-rs-0.7
  (package
    (name "rust-pep440-rs")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pep440_rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "177vv3fvdsp80x9hi2wigw3hkg7pxq6v4hjzfhrdxqwnyfhmq29i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rkyv" ,rust-rkyv-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.2)
                       ("rust-unscanny" ,rust-unscanny-0.1)
                       ("rust-version-ranges" ,rust-version-ranges-0.1))))
    (home-page "https://github.com/konstin/pep440-rs")
    (synopsis
     "library for python version numbers and specifiers, implementing PEP 440")
    (description
     "This package provides a library for python version numbers and specifiers,
implementing PEP 440.")
    (license (list license:asl2.0 license:bsd-2))))

(define-public rust-pyproject-toml-0.13
  (package
    (name "rust-pyproject-toml")
    (version "0.13.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyproject-toml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dfqb1yb05rc7m6azyzbm4478bq9v0kjg5rynnl91fin7xygafk4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glob" ,rust-glob-0.3)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-pep440-rs" ,rust-pep440-rs-0.7)
                       ("rust-pep508-rs" ,rust-pep508-rs-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-toml" ,rust-toml-0.8))
       #:cargo-development-inputs (("rust-insta" ,rust-insta-1))))
    (home-page "https://github.com/PyO3/pyproject-toml-rs.git")
    (synopsis "pyproject.toml parser in Rust")
    (description "This package provides pyproject.toml parser in Rust.")
    (license license:expat)))

(define-public rust-structmeta-derive-0.3
  (package
    (name "rust-structmeta-derive")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "structmeta-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z12r4v2d3272hxqxclnr1kn2kp07qsy5aswm4ynrzwhlmjhnahm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/frozenlib/structmeta")
    (synopsis "derive macro for structmeta crate")
    (description "This package provides derive macro for structmeta crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-structmeta-0.3
  (package
    (name "rust-structmeta")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "structmeta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0afk0s9paazsvyvsirxvbnqp3blhdck3fmfhdw7xf209skc7a59f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-structmeta-derive" ,rust-structmeta-derive-0.3)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/frozenlib/structmeta")
    (synopsis "Parse Rust's attribute arguments by defining a struct")
    (description
     "This package provides Parse Rust's attribute arguments by defining a struct.")
    (license (list license:expat license:asl2.0))))

(define-public rust-test-strategy-0.4
  (package
    (name "rust-test-strategy")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "test-strategy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06z0slp3ckxfsynq3772jy1dlasv3pa2kmii90ccqm1zbvs1mx1b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-structmeta" ,rust-structmeta-0.3)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/frozenlib/test-strategy")
    (synopsis
     "Procedural macro to easily write higher-order strategies in proptest")
    (description
     "This package provides Procedural macro to easily write higher-order strategies in proptest.")
    (license (list license:expat license:asl2.0))))

(define-public rust-goldenfile-1
  (package
    (name "rust-goldenfile")
    (version "1.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "goldenfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a74vvxyrkj0gyv5x01cmsmz5hrggskqmkk543wz6z2ky31g2bv7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-similar-asserts" ,rust-similar-asserts-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-yansi" ,rust-yansi-1))))
    (home-page "https://github.com/calder/rust-goldenfile")
    (synopsis "Simple goldenfile testing library")
    (description "This package provides Simple goldenfile testing library.")
    (license license:expat)))

(define-public rust-newtype-uuid-1
  (package
    (name "rust-newtype-uuid")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "newtype-uuid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mlh7imm4r79bn1fx6a3b735a3xrxhz9rvvpphg2lz5yx3q28cpf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proptest" ,rust-proptest-1)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/oxidecomputer/newtype-uuid")
    (synopsis "Newtype wrapper around UUIDs")
    (description "This package provides Newtype wrapper around UUIDs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-quick-junit-0.5
  (package
    (name "rust-quick-junit")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quick-junit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mxczpzhcnj3gkd22q448339lln6i1md0fhhaxr325hs769sdl9y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-newtype-uuid" ,rust-newtype-uuid-1)
                       ("rust-quick-xml" ,rust-quick-xml-0.37)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-goldenfile" ,rust-goldenfile-1)
                                   ("rust-owo-colors" ,rust-owo-colors-4)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-test-strategy" ,rust-test-strategy-0.4))))
    (home-page "https://github.com/nextest-rs/quick-junit")
    (synopsis "Data model and serializer for JUnit/XUnit XML")
    (description
     "This package provides Data model and serializer for JUnit/XUnit XML.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-tracing-flame-0.2
  (package
    (name "rust-tracing-flame")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-flame" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ad34bhy9gsj0ijn56jsvizydash6zcybbls29g1i2a7w5z13bhb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://tokio.rs")
    (synopsis "Tracing layer for creating flamegraphs from span timings")
    (description
     "This package provides Tracing layer for creating flamegraphs from span timings.")
    (license license:expat)))

(define-public rust-tracing-tree-0.4
  (package
    (name "rust-tracing-tree")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-tree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "175lqyfp6zq7jbj8m026xdp8p765pzgfdzfxahfggmdhy5wwlngl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing-core" ,rust-tracing-core-0.1)
                       ("rust-tracing-log" ,rust-tracing-log-0.2)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-log" ,rust-log-0.4)
                                   ("rust-tracing" ,rust-tracing-0.1)
                                   ("rust-ui-test" ,rust-ui-test-0.7))))
    (home-page "https://github.com/davidbarsky/tracing-tree")
    (synopsis "Tracing Layer which prints a tree of spans and events.")
    (description
     "This package provides a Tracing Layer which prints a tree of spans and events.")
    (license (list license:expat license:asl2.0))))

(define-public rust-scaling-0.1
  (package
    (name "rust-scaling")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "scaling" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bxyq4y69i89asg82749br3n7zsi55l1nrk737189s0whl03dbmx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/droundy/easybench-rs")
    (synopsis
     "lightweight benchmarking library that measures scaling behavior")
    (description
     "This package provides a lightweight benchmarking library that measures scaling
behavior.")
    (license (list license:expat license:asl2.0))))

(define-public rust-salsa-macros-0.17
  (package
    (name "rust-salsa-macros")
    (version "0.17.0-pre.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "salsa-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xrgb38l5fhv3lqx2lwnqc3s2zrgxmj63cd7kl0vyl7m5lsjwv5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/salsa-rs/salsa")
    (synopsis "Procedural macros for the salsa crate")
    (description
     "This package provides Procedural macros for the salsa crate.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-salsa-0.17
  (package
    (name "rust-salsa")
    (version "0.17.0-pre.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "salsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ypiig0p9969nkb7k0ydxm5bnc3jva815dfh8hqpackcnk63s8lv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-oorandom" ,rust-oorandom-11)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-salsa-macros" ,rust-salsa-macros-0.17)
                       ("rust-smallvec" ,rust-smallvec-1))
       #:cargo-development-inputs (("rust-diff" ,rust-diff-0.1)
                                   ("rust-env-logger" ,rust-env-logger-0.7)
                                   ("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
                                   ("rust-rand" ,rust-rand-0.7)
                                   ("rust-rand-distr" ,rust-rand-distr-0.2))))
    (home-page "https://github.com/salsa-rs/salsa")
    (synopsis
     "generic framework for on-demand, incrementalized computation (experimental)")
    (description
     "This package provides a generic framework for on-demand, incrementalized
computation (experimental).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-append-only-vec-0.1
  (package
    (name "rust-append-only-vec")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "append-only-vec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wg596rw1dhw8wjgd5dvd4cx7sx2jpabycfxj9lykkrmq1g0i4kr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-parking-lot" ,rust-parking-lot-0.12)
                                   ("rust-scaling" ,rust-scaling-0.1))))
    (home-page "https://github.com/droundy/append-only-vec")
    (synopsis "Append-only, concurrent vector")
    (description "This package provides Append-only, concurrent vector.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-1
  (package
    (name "rust-anstyle")
    (version "1.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-lexopt" ,rust-lexopt-0.3))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "ANSI text styling")
    (description "This package provides ANSI text styling.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fluent-uri-0.1
  (package
    (name "rust-fluent-uri")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fluent-uri" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03ah2qajw5l1zbc81kh1n8g7n24mfxbg6vqyv9ixipg1vglh9iqp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1))))
    (home-page "https://github.com/yescallop/fluent-uri-rs")
    (synopsis "generic URI/IRI handling library compliant with RFC 3986/3987.")
    (description
     "This package provides a generic URI/IRI handling library compliant with RFC
3986/3987.")
    (license license:expat)))

(define-public rust-anstyle-1
  (package
    (name "rust-anstyle")
    (version "1.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "ANSI text styling")
    (description "This package provides ANSI text styling.")
    (license (list license:expat license:asl2.0))))

(define-public rust-assert-fs-1
  (package
    (name "rust-assert-fs")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "assert_fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x3nj817l5kbpmr42habqv5i49rpxdpncmr86ix840knnkyv3zby"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-doc-comment" ,rust-doc-comment-0.3)
                       ("rust-globwalk" ,rust-globwalk-0.9)
                       ("rust-predicates" ,rust-predicates-3)
                       ("rust-predicates-core" ,rust-predicates-core-1)
                       ("rust-predicates-tree" ,rust-predicates-tree-1)
                       ("rust-tempfile" ,rust-tempfile-3))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1))))
    (home-page "https://github.com/assert-rs/assert_fs")
    (synopsis "Filesystem fixtures and assertions for testing")
    (description
     "This package provides Filesystem fixtures and assertions for testing.")
    (license (list license:expat license:asl2.0))))

(define-public rust-boolinator-2
  (package
    (name "rust-boolinator")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "boolinator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nccxzb1dfkjfrgzqaw1a90p26zlvv6nah5ckcpj6bn9a4zqga6g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/DanielKeep/rust-boolinator")
    (synopsis
     "Provides the Boolinator trait, which lets you use Option and Result-style combinators with bools")
    (description
     "This package provides the Boolinator trait, which lets you use Option and
Result-style combinators with bools.")
    (license (list license:expat license:asl2.0))))

(define-public rust-yew-macro-0.21
  (package
    (name "rust-yew-macro")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yew-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qix6k8f8gzxb750icxvxknm3xrg8g7a4035g6gyasbd2sjqrz82"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-boolinator" ,rust-boolinator-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/yewstack/yew")
    (synopsis "framework for making client-side single-page apps")
    (description
     "This package provides a framework for making client-side single-page apps.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anymap2-0.13
  (package
    (name "rust-anymap2")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anymap2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "031kw3bp0zh2pn9fcayaw0w0gydgpgfhm08pg4yz5cml9jwv60fk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/azriel91/anymap2")
    (synopsis "safe and convenient store for one value of each type")
    (description
     "This package provides a safe and convenient store for one value of each type.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-worker-0.2
  (package
    (name "rust-gloo-worker")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-worker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sjiw13069i7bpiyb03w3kyddn3q07fmj4vd60l1l1kqva21aiqk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anymap2" ,rust-anymap2-0.13)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-gloo-console" ,rust-gloo-console-0.2)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with Web Workers")
    (description
     "This package provides Convenience crate for working with Web Workers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-storage-0.2
  (package
    (name "rust-gloo-storage")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-storage" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1074j754a6c21sbmqws5qwaha0a13fikv17ps476zzfvyl5vcsjx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis
     "Convenience crate for working with local and session storage in browser")
    (description
     "This package provides Convenience crate for working with local and session storage in browser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-render-0.1
  (package
    (name "rust-gloo-render")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-render" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r3pxj22l489ldakj6521a0f0n1r9v8xrai3k12d9kv7xxm31n9g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis
     "Convenience crate for working with browser's requestAnimationFrame")
    (description
     "This package provides Convenience crate for working with browser's @code{requestAnimationFrame}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-net-0.3
  (package
    (name "rust-gloo-net")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-net" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0866ih3bff7dwxdfc813pk5nwz2ayyqwi5vbzlax7n4ygly4wsx6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "HTTP requests library for WASM Apps")
    (description "This package provides HTTP requests library for WASM Apps.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-wasm-bindgen-0.5
  (package
    (name "rust-serde-wasm-bindgen")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-wasm-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03m01y4l2kqz63pb1bip52j8bqilzlhhsa7asfdanmrwhgi47cgk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/RReverser/serde-wasm-bindgen")
    (synopsis "Native Serde adapter for wasm-bindgen")
    (description
     "This package provides Native Serde adapter for wasm-bindgen.")
    (license license:expat)))

(define-public rust-gloo-history-0.1
  (package
    (name "rust-gloo-history")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-history" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zsy3m5bgah8hyd95sc9b68afn1nhs7g43lkndip1m0fpy85swl5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-events" ,rust-gloo-events-0.1)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-wasm-bindgen" ,rust-serde-wasm-bindgen-0.5)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Universal Session History")
    (description "This package provides Universal Session History.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-file-0.2
  (package
    (name "rust-gloo-file")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-file" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mxnd7l8gglv5yqhah6ny329hc0c98vn7h5xg0yv8f0aax75dmd8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-gloo-events" ,rust-gloo-events-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with JavaScript files and blobs")
    (description
     "This package provides Convenience crate for working with @code{JavaScript} files and blobs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-events-0.1
  (package
    (name "rust-gloo-events")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-events" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z4j14r2lim77s0jm1dpk306jyycmx2kirid33j0b0gdmgw0gcb8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with DOM event listeners")
    (description
     "This package provides Convenience crate for working with DOM event listeners.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-dialogs-0.1
  (package
    (name "rust-gloo-dialogs")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-dialogs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rh2j0l8rbj8pbypxqy99qi2x3hq52sclijs8h47zlkjmij261k7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with dialogs in browser")
    (description
     "This package provides Convenience crate for working with dialogs in browser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-utils-0.1
  (package
    (name "rust-gloo-utils")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13m59g36spynspvhx0xsaahbkdshn1v03gcjf87s7cvc443wnzq3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for common `web_sys` features")
    (description
     "This package provides Convenience crate for common `web_sys` features.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-console-0.2
  (package
    (name "rust-gloo-console")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-console" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gqd35vn0i5y6hzfrsb2i032p1j832c08sar6dr19gny0lycxdw2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with browser's console")
    (description
     "This package provides Convenience crate for working with browser's console.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-0.8
  (package
    (name "rust-gloo")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kdr8ahxl77fby89fvfwq13kqqyyw63pnjpv6gynz4gnbvd9r698"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-console" ,rust-gloo-console-0.2)
                       ("rust-gloo-dialogs" ,rust-gloo-dialogs-0.1)
                       ("rust-gloo-events" ,rust-gloo-events-0.1)
                       ("rust-gloo-file" ,rust-gloo-file-0.2)
                       ("rust-gloo-history" ,rust-gloo-history-0.1)
                       ("rust-gloo-net" ,rust-gloo-net-0.3)
                       ("rust-gloo-render" ,rust-gloo-render-0.1)
                       ("rust-gloo-storage" ,rust-gloo-storage-0.2)
                       ("rust-gloo-timers" ,rust-gloo-timers-0.2)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-gloo-worker" ,rust-gloo-worker-0.2))))
    (home-page "https://gloo-rs.web.app/")
    (synopsis "modular toolkit for Rust and WebAssembly")
    (description
     "This package provides a modular toolkit for Rust and @code{WebAssembly}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-prokio-0.1
  (package
    (name "rust-prokio")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prokio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "127l9k5076xwlaf0b64hw3l14wqjss2krldb2ddgm4apdq85xd83"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-gloo" ,rust-gloo-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-pinned" ,rust-pinned-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4))))
    (home-page "https://github.com/futursolo/prokio")
    (synopsis
     "An asynchronous runtime compatible with WebAssembly and non-WebAssembly targets")
    (description
     "This package provides An asynchronous runtime compatible with @code{WebAssembly} and
non-@code{WebAssembly} targets.")
    (license (list license:expat license:asl2.0))))

(define-public rust-implicit-clone-derive-0.1
  (package
    (name "rust-implicit-clone-derive")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "implicit-clone-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fsfj6n56mg92f3899gcdck1dqlsmgyd52k0n2xhhj53p5g6h4ck"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/yewstack/implicit-clone")
    (synopsis "Immutable types and ImplicitClone trait similar to Copy")
    (description
     "This package provides Immutable types and @code{ImplicitClone} trait similar to Copy.")
    (license (list license:expat license:asl2.0))))

(define-public rust-implicit-clone-0.4
  (package
    (name "rust-implicit-clone")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "implicit-clone" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "113agd9bqk7c0s2rqgarzkfp3wgbzl3q59mp6sv72nkv3iwsmagq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-implicit-clone-derive" ,rust-implicit-clone-derive-0.1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/yewstack/implicit-clone")
    (synopsis "Immutable types and ImplicitClone trait similar to Copy")
    (description
     "This package provides Immutable types and @code{ImplicitClone} trait similar to Copy.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pinned-0.1
  (package
    (name "rust-pinned")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pinned" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nsrxs49dhjjz1gvg0pvac2rcidnwwd8l99y7vhwym2yv5xh4ad8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/futursolo/pinned")
    (synopsis "Synchronisation primitives for !Send tasks")
    (description
     "This package provides Synchronisation primitives for !Send tasks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-worker-macros-0.1
  (package
    (name "rust-gloo-worker-macros")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-worker-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rs0f6b34mkhlmpmhqi747c34000sd5mxma92yacjyw5sicalv4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with Web Workers")
    (description
     "This package provides Convenience crate for working with Web Workers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-worker-0.4
  (package
    (name "rust-gloo-worker")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-worker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00744js1jcwdndzf22c8gzml7aqql4ymjfpsd2i1vrbxv0ymsjbn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bincode" ,rust-bincode-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.2)
                       ("rust-gloo-worker-macros" ,rust-gloo-worker-macros-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-pinned" ,rust-pinned-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with Web Workers")
    (description
     "This package provides Convenience crate for working with Web Workers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-storage-0.3
  (package
    (name "rust-gloo-storage")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-storage" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yi7740iza6nyg6n8sxzzhy6yg6xpbxhig7r2bwqlxcjihg07j7v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-utils" ,rust-gloo-utils-0.2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis
     "Convenience crate for working with local and session storage in browser")
    (description
     "This package provides Convenience crate for working with local and session storage in browser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-render-0.2
  (package
    (name "rust-gloo-render")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-render" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cwqcka7l5p29idq174c6mi5cgal0rywngdck26qwfki8ikqn02n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis
     "Convenience crate for working with browser's requestAnimationFrame")
    (description
     "This package provides Convenience crate for working with browser's @code{requestAnimationFrame}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-net-0.4
  (package
    (name "rust-gloo-net")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-net" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i3x5fvp07valrxjsa25ycq1b2p3pxqaqmw6kzx35ip2i8lfijca"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "HTTP requests library for WASM Apps")
    (description "This package provides HTTP requests library for WASM Apps.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-history-0.2
  (package
    (name "rust-gloo-history")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-history" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mhphqywgbqj4agpi4zyc4hah12nys7085jymiz44d5swlml6gwh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gloo-events" ,rust-gloo-events-0.2)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-wasm-bindgen" ,rust-serde-wasm-bindgen-0.6)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Universal Session History")
    (description "This package provides Universal Session History.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-file-0.3
  (package
    (name "rust-gloo-file")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-file" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07xxainnnrg6l3ccw2bvqiz4m76ih557aklp5r5q5cizhrqksmlp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-gloo-events" ,rust-gloo-events-0.2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with JavaScript files and blobs")
    (description
     "This package provides Convenience crate for working with @code{JavaScript} files and blobs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-events-0.2
  (package
    (name "rust-gloo-events")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-events" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h8yr4n1pvwp4rr87835w14kjdkycyn8gypmh2lmnf3wbys6zhi7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with DOM event listeners")
    (description
     "This package provides Convenience crate for working with DOM event listeners.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-dialogs-0.2
  (package
    (name "rust-gloo-dialogs")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-dialogs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pqmg2z3x4c3id25jd0p8rjwy5qjbc4k1x8gflsi9c1207hlhixz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with dialogs in browser")
    (description
     "This package provides Convenience crate for working with dialogs in browser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-utils-0.2
  (package
    (name "rust-gloo-utils")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1am31cd6889shb7158bg9zzsjcpvyzxrhfhxgia8rc8k84smam8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for common `web_sys` features")
    (description
     "This package provides Convenience crate for common `web_sys` features.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-console-0.3
  (package
    (name "rust-gloo-console")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-console" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qg24wbvql0bsr980hbrm0pi11c3jmlwpj0pgdklz8mlas7qc5ra"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-utils" ,rust-gloo-utils-0.2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with browser's console")
    (description
     "This package provides Convenience crate for working with browser's console.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-0.10
  (package
    (name "rust-gloo")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j82hwfn6ibqla1q849vhsmxnxw6wmnjkmmffzdw2mfc51n54dfd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-console" ,rust-gloo-console-0.3)
                       ("rust-gloo-dialogs" ,rust-gloo-dialogs-0.2)
                       ("rust-gloo-events" ,rust-gloo-events-0.2)
                       ("rust-gloo-file" ,rust-gloo-file-0.3)
                       ("rust-gloo-history" ,rust-gloo-history-0.2)
                       ("rust-gloo-net" ,rust-gloo-net-0.4)
                       ("rust-gloo-render" ,rust-gloo-render-0.2)
                       ("rust-gloo-storage" ,rust-gloo-storage-0.3)
                       ("rust-gloo-timers" ,rust-gloo-timers-0.3)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.2)
                       ("rust-gloo-worker" ,rust-gloo-worker-0.4))))
    (home-page "https://gloo-rs.web.app/")
    (synopsis "modular toolkit for Rust and WebAssembly")
    (description
     "This package provides a modular toolkit for Rust and @code{WebAssembly}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-yew-0.21
  (package
    (name "rust-yew")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yew" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b0ccqqlyyhcrk0l6d8jch2xwbhl5wliabn6x6ipl367apr066jz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-gloo" ,rust-gloo-0.10)
                       ("rust-html-escape" ,rust-html-escape-0.2)
                       ("rust-implicit-clone" ,rust-implicit-clone-0.4)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-prokio" ,rust-prokio-0.1)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-yew-macro" ,rust-yew-macro-0.21))))
    (home-page "https://yew.rs")
    (synopsis "framework for creating reliable and efficient web applications")
    (description
     "This package provides a framework for creating reliable and efficient web
applications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-await-test-macro-0.3
  (package
    (name "rust-futures-await-test-macro")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-await-test-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ff8g0rlgxh7shd9v2pm685klmnwwh00wm9l0d8009l3wrsh25ag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/ngg/futures-await-test")
    (synopsis "Helper crate for futures-await-test.")
    (description "This package provides Helper crate for futures-await-test.")
    (license license:expat)))

(define-public rust-futures-await-test-0.3
  (package
    (name "rust-futures-await-test")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-await-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s6790zdmah6yikkm70na20w2r01yakz9r41llmmffp7ph2ks3df"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-await-test-macro" ,rust-futures-await-test-macro-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3))))
    (home-page "https://github.com/ngg/futures-await-test")
    (synopsis
     "Async test support for Rust through a procedural macro. This crate defines the
`#[async_test]` attribute to make writing tests that use async/await easier.")
    (description
     "This package provides Async test support for Rust through a procedural macro.  This crate defines the
`#[async_test]` attribute to make writing tests that use async/await easier.")
    (license license:expat)))

(define-public rust-big-s-1
  (package
    (name "rust-big-s")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "big_s" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a2igv4pddakvshmp69jyxxrhzcbpjhfc8i41jqq64k3j1xxp7hr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/brson/big_s")
    (synopsis "Rust's missing `String` literal")
    (description "This package provides Rust's missing `String` literal.")
    (license (list license:expat license:asl2.0 license:cc0))))

(define-public rust-yaup-0.3
  (package
    (name "rust-yaup")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yaup" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1immam40n4dp6vsr4cj68csb8c6rxmsdl90hn9n896d12qd4y55h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/meilisearch/yaup")
    (synopsis "URL parameters serialization")
    (description "This package provides URL parameters serialization.")
    (license (list license:expat license:asl2.0))))

(define-public rust-meilisearch-index-setting-macro-0.27
  (package
    (name "rust-meilisearch-index-setting-macro")
    (version "0.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "meilisearch-index-setting-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mbympkini51y6pd9p2mg7bvr9vy0ghw1ng04mjwr0dga838qvh5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-convert-case" ,rust-convert-case-0.6)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-structmeta" ,rust-structmeta-0.3)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/meilisearch/meilisearch-rust")
    (synopsis "Helper tool to generate settings of a Meilisearch index")
    (description
     "This package provides Helper tool to generate settings of a Meilisearch index.")
    (license license:expat)))

(define-public rust-meilisearch-sdk-0.27
  (package
    (name "rust-meilisearch-sdk")
    (version "0.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "meilisearch-sdk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "014kc558pkmzj55mcpzidybxqh8vcsl7gqzc5m7jnwcdhxaq55b6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-either" ,rust-either-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-iso8601" ,rust-iso8601-0.6)
                       ("rust-jsonwebtoken" ,rust-jsonwebtoken-9)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-meilisearch-index-setting-macro" ,rust-meilisearch-index-setting-macro-0.27)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-yaup" ,rust-yaup-0.3))
       #:cargo-development-inputs (("rust-big-s" ,rust-big-s-1)
                                   ("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-futures-await-test" ,rust-futures-await-test-0.3)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-mockito" ,rust-mockito-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                                   ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                                   ("rust-web-sys" ,rust-web-sys-0.3)
                                   ("rust-yew" ,rust-yew-0.21))))
    (home-page "https://github.com/meilisearch/meilisearch-sdk")
    (synopsis
     "Rust wrapper for the Meilisearch API. Meilisearch is a powerful, fast, open-source, easy to use and deploy search engine")
    (description
     "This package provides Rust wrapper for the Meilisearch API. Meilisearch is a powerful, fast,
open-source, easy to use and deploy search engine.")
    (license license:expat)))

(define-public rust-snowflake-1
  (package
    (name "rust-snowflake")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "snowflake" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wadr7bxdxbmkbqkqsvzan6q1h3mxqpxningi3ss3v9jaav7n817"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/Stebalien/snowflake")
    (synopsis "module for generating guaranteed process unique IDs.")
    (description
     "This package provides a module for generating guaranteed process unique IDs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zookeeper-async-5
  (package
    (name "rust-zookeeper-async")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zookeeper-async" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d62zi7f8f06pl9vravcj99ry7mfgzgm8f1dh206k6zy9hf47b01"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num-enum" ,rust-num-enum-0.7)
                       ("rust-snowflake" ,rust-snowflake-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/krojew/rust-zookeeper")
    (synopsis "An async ZooKeeper client")
    (description "This package provides An async @code{ZooKeeper} client.")
    (license license:expat)))

(define-public rust-solrstice-0.6
  (package
    (name "rust-solrstice")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "solrstice" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cbw8ka4954m8plha1389g1l4bfq0r5cngyv9yz996hh6gg354q2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-dyn-clone" ,rust-dyn-clone-1)
                       ("rust-fastrand" ,rust-fastrand-2)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-zip" ,rust-zip-2)
                       ("rust-zookeeper-async" ,rust-zookeeper-async-5))
       #:cargo-development-inputs (("rust-dotenv" ,rust-dotenv-0.15)
                                   ("rust-env-logger" ,rust-env-logger-0.11)
                                   ("rust-serial-test" ,rust-serial-test-3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/Sh1nku/solrstice")
    (synopsis "Solr 8+ client")
    (description "This package provides a Solr 8+ client.")
    (license (list license:expat license:asl2.0))))

(define-public rust-logtest-2
  (package
    (name "rust-logtest")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logtest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09ihwkq6z7xm6wdwxmc9mz74lsl20g5bi7fcdm8n87bwcnl46gpb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/yoshuawuyts/logtest")
    (synopsis "Test and assert log statements")
    (description "This package provides Test and assert log statements.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cassandra-cpp-sys-1
  (package
    (name "rust-cassandra-cpp-sys")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cassandra-cpp-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kg2nwpw6z9da1bbgv5cxzc98a9rrakxsicrv1slfv6nkn2vqhb1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/cassandra-rs/cassandra-sys-rs")
    (synopsis
     "bindgen-generated Rust wrapper around the DataStax Cassandra C++ driver plus working examples. You probably want to use the \"cassandra-cpp\" crate which provides a safe wrapper.")
    (description
     "This package provides a bindgen-generated Rust wrapper around the
@code{DataStax} Cassandra C++ driver plus working examples.  You probably want
to use the \"cassandra-cpp\" crate which provides a safe wrapper.")
    (license license:asl2.0)))

(define-public rust-cassandra-cpp-3
  (package
    (name "rust-cassandra-cpp")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cassandra-cpp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1asfa9vwkinwy2jvj03zglnmlfzldpsnmrrn4nkks9waqmxv8hfk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bigdecimal" ,rust-bigdecimal-0.4)
                       ("rust-cassandra-cpp-sys" ,rust-cassandra-cpp-sys-1)
                       ("rust-error-chain" ,rust-error-chain-0.12)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-slog" ,rust-slog-2)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-logtest" ,rust-logtest-2)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/cassandra-rs/cassandra-rs")
    (synopsis
     "Cassandra CQL driver, built on top of the DataStax C++ driver for performance and functionality.")
    (description
     "This package provides a Cassandra CQL driver, built on top of the
@code{DataStax} C++ driver for performance and functionality.")
    (license license:asl2.0)))

(define-public rust-mobc-0.7
  (package
    (name "rust-mobc")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mobc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zpk3xzzlv5a8wb0gkajxa9jda3ld819zc5jsfl01fywwbrd4xkz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-rt" ,rust-actix-rt-1)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-timer" ,rust-futures-timer-3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/importcjj/mobc")
    (synopsis "generic connection pool with async/await support")
    (description
     "This package provides a generic connection pool with async/await support.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gremlin-derive-0.1
  (package
    (name "rust-gremlin-derive")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gremlin-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06925paqw25j5hnsp0k60894r1lvwi5rcva2w4zn407x5wq287fn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/wolf4ood/gremlin-rs")
    (synopsis "Proc macro for gremlin-rsâ¢")
    (description "This package provides Proc macro for gremlin-rsâ¢.")
    (license license:asl2.0)))

(define-public rust-async-native-tls-0.5
  (package
    (name "rust-async-native-tls")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v2v82crqm4fgj1s32gik56m7cwx0ygqjdqc5pw9zrq7rxddqhwk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://docs.rs/crate/async-native-tls/")
    (synopsis "Native TLS using futures")
    (description "This package provides Native TLS using futures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-tungstenite-0.23
  (package
    (name "rust-async-tungstenite")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p20alhqi6agmgn1hw2x4bv9q7k21fd0b0xrkyhs0bb12jzfzsd1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-native-tls" ,rust-async-native-tls-0.5)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-async-tls" ,rust-async-tls-0.12)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tungstenite" ,rust-tungstenite-0.20)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25))))
    (home-page "https://github.com/sdroege/async-tungstenite")
    (synopsis
     "Async binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Async binding for Tungstenite, the Lightweight stream-based @code{WebSocket}
implementation.")
    (license license:expat)))

(define-public rust-async-tls-0.12
  (package
    (name "rust-async-tls")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15a7vaig0adwa7bsymw20dvm2ahw80lb5mjgscxvpjwpl86gvvng"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/async-std/async-tls")
    (synopsis "Asynchronous TLS/SSL streams using Rustls")
    (description
     "This package provides Asynchronous TLS/SSL streams using Rustls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gremlin-client-0.8
  (package
    (name "rust-gremlin-client")
    (version "0.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gremlin-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rcjcpyawmks8dzkviydk3zwymy4ssh78njzr9d772kxhsd7zri3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-tls" ,rust-async-tls-0.12)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-async-tungstenite" ,rust-async-tungstenite-0.23)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-gremlin-derive" ,rust-gremlin-derive-0.1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-mobc" ,rust-mobc-0.7)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-r2d2" ,rust-r2d2-0.8)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tungstenite" ,rust-tungstenite-0.20)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-webpki" ,rust-webpki-0.22))))
    (home-page "https://github.com/wolf4ood/gremlin-rs")
    (synopsis "Rust client for Apache TinkerPopâ¢")
    (description
     "This package provides a Rust client for Apache @code{TinkerPopâ¢}.")
    (license license:asl2.0)))
