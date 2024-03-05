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
