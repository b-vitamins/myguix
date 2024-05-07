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
