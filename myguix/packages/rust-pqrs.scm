(define-module (myguix packages rust-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages ninja)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
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

(define-public rust-tch-0.14
  (package
    (name "rust-tch")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nh3xj5acz4nxhff4c99cwqrz91f6xp16rxmbyzr4a41ngddvm8f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-cpython" ,rust-cpython-0.7)
                       ("rust-half" ,rust-half-2)
                       ("rust-image" ,rust-image-0.24)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memmap2" ,rust-memmap2-0.6)
                       ("rust-ndarray" ,rust-ndarray-0.15)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-safetensors" ,rust-safetensors-0.3)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-torch-sys" ,rust-torch-sys-0.14)
                       ("rust-zip" ,rust-zip-0.6))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("python3" ,python))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'enable-unstable-features
                    (lambda _
                      (setenv "LIBTORCH_USE_PYTORCH" "1") #t)))))
    (home-page "https://github.com/LaurentMazare/tch-rs")
    (synopsis "Rust wrappers for the PyTorch C++ api (libtorch).")
    (description "Rust wrappers for the @code{PyTorch} C++ api (libtorch).")
    (license (list license:expat license:asl2.0))))

(define-public libtorch
  (package
    (name "libtorch")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pytorch/pytorch.git")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "069fsnf5hw2q3hx91kj86r5rgxk1l3drl7a4lx8fd8xl1y2vsxp0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_SHARED_LIBS:BOOL=ON"
                               "-DCMAKE_BUILD_TYPE:STRING=Release"
                               "-DPYTHON_EXECUTABLE:PATH=python3")
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (invoke "cmake" "--build" ".")))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (invoke "cmake" "--install" "." "--prefix" out)) #t)))))
    (native-inputs `(("python3" ,python-3)
                     ("pyyaml" ,python-pyyaml)
                     ("cmake" ,cmake)
                     ("ninja" ,ninja)))
    (synopsis "The core library of PyTorch in C++")
    (description
     "libtorch is the core library of PyTorch without Python bindings. It 
      provides the tensor and neural network backends for PyTorch in C++.")
    (license license:bsd-3)
    (home-page "https://github.com/pytorch/pytorch")))

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

(define-public rust-indoc-2
  (package
    (name "rust-indoc")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indoc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dgjk49rkmx4kjy07k4b90qb5vl89smgb5rcw02n0q0x9ligaj5j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-unindent" ,rust-unindent-0.2))))
    (home-page "https://github.com/dtolnay/indoc")
    (synopsis "Indented document literals")
    (description "Indented document literals")
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
    (synopsis
     "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
     "Wrappers to create fully-featured Mutex and @code{RwLock} types.  Compatible
with no_std.")
    (license (list license:expat license:asl2.0))))

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
     `(#:cargo-inputs (("rust-critical-section" ,rust-critical-section-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-build-context" ,rust-build-context-0.1)
                                   ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                                   ("rust-fastrand" ,rust-fastrand-2)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-sptr" ,rust-sptr-0.3)
                                   ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/taiki-e/portable-atomic")
    (synopsis
     "Portable atomic types including support for 128-bit atomics, atomic float, etc.
")
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

(define-public rust-anstyle-parse-0.2
  (package
    (name "rust-anstyle-parse")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle-parse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "134jhzrz89labrdwxxnjxqjdg06qvaflj1wkfnmyapwyldfwcnn7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-utf8parse" ,rust-utf8parse-0.2))
       #:cargo-development-inputs (("rust-codegenrs" ,rust-codegenrs-3)
                                   ("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-snapbox" ,rust-snapbox-0.4)
                                   ("rust-vte-generate-state-changes" ,rust-vte-generate-state-changes-0.1))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Parse ANSI Style Escapes")
    (description "Parse ANSI Style Escapes")
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
    (home-page "https://github.com/PyO3/rust-numpy")
    (synopsis "PyO3-based Rust bindings of the NumPy C-API")
    (description "@code{PyO3-based} Rust bindings of the @code{NumPy} C-API")
    (license license:bsd-2)))

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
        (base32 "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))
       #:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.
")
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
     `(#:cargo-development-inputs (("rust-lazy-static" ,rust-lazy-static-1))))
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
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
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

(define-public rust-number-prefix-0.4
  (package
    (name "rust-number-prefix")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "number_prefix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wvh13wvlajqxkb1filsfzbrnq0vrmrw298v2j3sy82z1rm282w3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/ogham/rust-number-prefix")
    (synopsis "Library for numeric prefixes (kilo, giga, kibi).")
    (description "Library for numeric prefixes (kilo, giga, kibi).")
    (license license:expat)))

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

(define-public rust-paste-1
  (package
    (name "rust-paste")
    (version "1.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "paste" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k7d54zz8zrz0623l3xhvws61z5q2wd3hkwim6gylk8212placfy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-paste-test-suite" ,rust-paste-test-suite-0.0.0)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/paste")
    (synopsis "Macros for all your token pasting needs")
    (description "Macros for all your token pasting needs")
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
     `(#:cargo-inputs (("rust-getrandom" ,rust-getrandom-0.2)
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

(define-public rust-insta-1
  (package
    (name "rust-insta")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "insta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xd64139vi0hrxs8l8k3r1xqf2x8b0x5chsh47lwkqj85l2fc2l1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
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
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://insta.rs/")
    (synopsis "A snapshot testing library for Rust")
    (description "This package provides a snapshot testing library for Rust")
    (license license:asl2.0)))

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
     `(#:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
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

(define-public rust-insta-1
  (package
    (name "rust-insta")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "insta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k2kbpyw7wp1s9n7pw9wcg0rw5aii3lz023j0ck6bjjrivsp7ary"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=test_format_rust_expression")
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
                       ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-rustc-version" ,rust-rustc-version-0.4)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-similar-asserts" ,rust-similar-asserts-1))))
    (home-page "https://insta.rs/")
    (synopsis "A snapshot testing library for Rust")
    (description "This package provides a snapshot testing library for Rust")
    (license license:asl2.0)))

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
     `(#:tests? #f ;Not all files included
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

