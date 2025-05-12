(define-module (myguix packages python-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-windows)
  #:use-module ((gnu packages crates-io)
                #:hide (rust-annotate-snippets-0.11 rust-wasm-bindgen-test-0.3
                        rust-wasm-bindgen-0.2
                        rust-walkdir-2
                        rust-uuid-1
                        rust-url-2
                        rust-ureq-2
                        rust-unicode-normalization-0.1
                        rust-unicode-ident-1
                        rust-unic-ucd-category-0.9
                        rust-typed-arena-2
                        rust-toml-0.8
                        rust-test-case-3
                        rust-syn-2
                        rust-strum-macros-0.26
                        rust-static-assertions-1
                        rust-smallvec-1
                        rust-similar-2
                        rust-shellexpand-3
                        rust-serde-with-3
                        rust-serde-test-1
                        rust-seahash-4
                        rust-schemars-0.8
                        rust-regex-1
                        rust-rand-0.8
                        rust-quote-1
                        rust-proc-macro2-1
                        rust-pretty-assertions-1
                        rust-pkg-config-0.3
                        rust-pathdiff-0.2
                        rust-path-slash-0.2
                        rust-parking-lot-0.12
                        rust-natord-1
                        rust-memchr-2
                        rust-lsp-types-0.95
                        rust-lsp-server-0.7
                        rust-log-0.4
                        rust-libcst-1
                        rust-js-sys-0.3
                        rust-is-wsl-0.4
                        rust-insta-1
                        rust-indoc-2
                        rust-indicatif-0.17
                        rust-indexmap-2
                        rust-ignore-0.4
                        rust-filetime-0.2
                        rust-crossbeam-0.8
                        rust-console-log-1
                        rust-globset-0.4
                        rust-console-error-panic-hook-0.1
                        rust-codspeed-criterion-compat-2
                        rust-clap-4
                        rust-chrono-0.4
                        rust-cc-1
                        rust-cachedir-0.3
                        rust-bstr-1
                        rust-bitflags-2
                        rust-bitflags-1
                        rust-bincode-1
                        rust-assert-fs-1
                        rust-anyhow-1
                        rust-anstyle-1
                        rust-anstream-0.6
                        rust-aho-corasick-1
                        rust-num-traits-0.2))
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages image)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module ((gnu packages python-xyz)
                #:hide (python-pillow-simd))
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages web)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages video)
  #:use-module (myguix packages machine-learning)
  #:use-module (myguix packages java-pqrs)
  #:use-module ((myguix packages rust-pqrs)
                #:hide (rust-lexical-parse-float-0.8))
  #:use-module (myguix packages nvidia))

(define-public python-grobid-client-python
  (package
    (name "python-grobid-client-python")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grobid_client_python" version))
       (sha256
        (base32 "1qxwkp0brqrfxwm853f48jzraff8934b89nm0h0vb7mpsgv1p0xw"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-requests))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/kermitt2/grobid_client_python")
    (synopsis "Simple python client for GROBID REST services")
    (description "Simple python client for GROBID REST services")
    (license license:asl2.0)))

(define-public python-openalexapi
  (package
    (name "python-openalexapi")
    (version "0.0.1a0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openalexapi" version))
       (sha256
        (base32 "02bwnqr0kbjgjqcaknwnzm92nczxvc03av93ngmkin40cb99bxk0"))))
    (arguments
     (list
      #:tests? #f))
    (build-system pyproject-build-system)
    (inputs (list python-requests python-pydantic python-purl))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/dpriskorn/OpenAlexAPI")
    (synopsis "Libray for accessing the OpenAlex API")
    (description "Libray for accessing the @code{OpenAlex} API")
    (license license:gpl3+)))

(define-public python-pyalex
  (package
    (name "python-pyalex")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyalex" version))
       (sha256
        (base32 "0n29hl0m137jyx65lry7mh50lwcnba1j8zpap252rckli0gbfffh"))))
    (arguments
     '(#:tests? #f)) ;Requires network access
    (build-system pyproject-build-system)
    (propagated-inputs (list python-requests python-urllib3))
    (native-inputs (list python-pytest python-pytest-xdist python-setuptools
                         python-wheel))
    (home-page "")
    (synopsis "Python interface to the OpenAlex database")
    (description "Python interface to the @code{OpenAlex} database")
    (license license:expat)))

(define-public python-decord
  (package
    (name "python-decord")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmlc/decord")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wh9bg3m1bnxqzqmm1pg3hlfhlvb3xgcxyf0qw8i9j7mv6z4xqqr"))))
    (arguments
     '(#:tests? #f ;Requires MXNet
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'libdecord
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((libdecord (string-append (assoc-ref inputs
                                                                 "decord")
                                                      "/lib/libdecord.so")))
                        (install-file libdecord "python/decord")
                        (chdir "python")))))))
    (build-system python-build-system)
    (inputs (list python-numpy))
    (native-inputs (list decord))
    (home-page "https://github.com/dmlc/decord")
    (synopsis
     "@code{Decord} is a reverse procedure of @code{Record}. It provides convenient video slicing methods based on a thin wrapper on top of hardware accelerated video decoders, e.g. 1) FFMPEG/LibAV, 2) NVIDEA Codecs, 3) Intel Codecs")
    (description
     "@code{Decord} was designed to handle awkward video shuffling experience in order to provide smooth experiences similar to random image loader for deep learning.

@code{Decord} is also able to decode audio from both video and audio files. One can slice video and audio together to get a synchronized result; hence providing a one-stop solution for both video and audio decoding.")
    (license license:asl2.0)))

(define-public python-lsprotocol
  (package
    (name "python-lsprotocol")
    (version "2023.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lsprotocol" version))
       (sha256
        (base32 "07c6476y1mla347pskswl0c30b94a6g36123ff5w20r41l9iap6c"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core python-flit-core python-cattrs
                         python-attrs))
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/microsoft/lsprotocol")
    (synopsis "Python implementation of the Language Server Protocol.")
    (description "Python implementation of the Language Server Protocol.")
    (license license:expat)))

(define-public python-cattrs
  (package
    (name "python-cattrs")
    (version "23.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cattrs" version))
       (sha256
        (base32 "17rbcx8rvbdisb3vqac59yqv9q4rhqx7wddc3n8rxambjl6hjd59"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme
                         python-hatch-vcs))
    (propagated-inputs (list python-attrs python-exceptiongroup
                             python-typing-extensions))
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/python-attrs/cattrs")
    (synopsis "Python library for structuring and unstructuring data")
    (description
     "@code{cattrs} is an Python library for structuring and
unstructuring data.  @code{cattrs} works best with @code{attrs} classes,
@code{dataclasses} and the usual Python collections, but other kinds of
classes can also be supported by manually registering converters.")
    (license license:expat)))

(define-public python-pygls
  (package
    (name "python-pygls")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pygls" version))
       (sha256
        (base32 "062hlmkd4lqc6f689bhpjbyx59s25a4pqm1kqnryk80dzbpdq3hl"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core python-lsprotocol python-cattrs))
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/openlawlibrary/pygls")
    (synopsis
     "A pythonic generic language server (pronounced like 'pie glass')")
    (description
     "This package provides a pythonic generic language server (pronounced like pie
glass')")
    (license license:asl2.0)))

(define-public python-docstring-to-markdown
  (package
    (name "python-docstring-to-markdown")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docstring-to-markdown" version))
       (sha256
        (base32 "0gdpabnyl1kyy0cjrnph6xl4fyhgim50a1amsaqq3hahki6i2ip1"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/python-lsp/docstring-to-markdown")
    (synopsis "On the fly conversion of Python docstrings to markdown")
    (description "On the fly conversion of Python docstrings to markdown")
    (license license:lgpl2.1)))

(define-public python-jedi-language-server
  (package
    (name "python-jedi-language-server")
    (version "0.41.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jedi_language_server" version))
       (sha256
        (base32 "12lzgb4yqxg3dc15kpifp2bd5gl4acv5yjvhpgpwxnpsjlmw4ghi"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core
                         python-pygls
                         python-lsprotocol
                         python-jedi
                         python-docstring-to-markdown
                         python-cattrs))
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/pappasam/jedi-language-server")
    (synopsis "A language server for Jedi!")
    (description "This package provides a language server for Jedi!")
    (license license:expat)))

(define-public python-free-proxy
  (package
    (name "python-free-proxy")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "free_proxy" version))
       (sha256
        (base32 "1ywrd27a05ajq6dyx6qaxy7xp7ac5w9igvic5622kd3j763fn81b"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-lxml python-requests))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/jundymek/free-proxy")
    (synopsis "Proxy scraper for further use")
    (description "Proxy scraper for further use")
    (license license:expat)))

(define-public python-fake-useragent
  (package
    (name "python-fake-useragent")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fake-useragent" version))
       (sha256
        (base32 "0jaw5xv8wshf8rr0xicm3id891jlyx95i4yqgsxbb5i1bagjd1v3"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-importlib-metadata
                             python-importlib-resources))
    (home-page "")
    (synopsis "Up-to-date simple useragent faker with real world database")
    (description "Up-to-date simple useragent faker with real world database")
    (license license:asl2.0)))

(define-public python-bibtexparser-1
  (package
    (name "python-bibtexparser-1")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bibtexparser" version))
       (sha256
        (base32 "12fpi0ajh2c50minz9big2xrrjjwnmxv6cs31f781i3n8vi2j3p0"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pyparsing))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/sciunto-org/python-bibtexparser")
    (synopsis "Bibtex parser for python 3")
    (description "Bibtex parser for python 3.")
    (license license:expat)))

(define-public python-types-tqdm
  (package
    (name "python-types-tqdm")
    (version "4.66.0.20240417")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "types-tqdm" version))
       (sha256
        (base32 "0c8s8ahz3w4b2mz7mvnf2gyfwrhiibfq93av9w7d9a1fabpykp0n"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/python/typeshed")
    (synopsis "Typing stubs for tqdm")
    (description "Typing stubs for tqdm")
    (license license:asl2.0)))

(define-public python-types-simplejson
  (package
    (name "python-types-simplejson")
    (version "3.19.0.20240310")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "types-simplejson" version))
       (sha256
        (base32 "005mq8q7mvmzdl41sqiqzdamsmw9r7vxyy6rqcr5imfmf1pkcc98"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/python/typeshed")
    (synopsis "Typing stubs for simplejson")
    (description "Typing stubs for simplejson")
    (license license:asl2.0)))

(define-public python-types-pyyaml
  (package
    (name "python-types-pyyaml")
    (version "6.0.12.20240311")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "types-PyYAML" version))
       (sha256
        (base32 "0hjk1b5khpzckbg7kbviga4jmjh41plix98w1jdp6df8ipwg1q59"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/python/typeshed")
    (synopsis "Typing stubs for PyYAML")
    (description "Typing stubs for @code{PyYAML}")
    (license license:asl2.0)))

(define-public python-pfzy
  (package
    (name "python-pfzy")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pfzy" version))
       (sha256
        (base32 "1wdkjkmwwg920ybmr46m1jry06fqxyc2v2r9wwc3ddhhvmjsfzki"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-poetry-core python-myst-parser
                             python-sphinx python-sphinx-autobuild
                             python-sphinx-copybutton))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/kazhala/pfzy")
    (synopsis "Python port of the fzy fuzzy string matching algorithm")
    (description "Python port of the fzy fuzzy string matching algorithm")
    (license license:expat)))

(define-public python-inquirerpy
  (package
    (name "python-inquirerpy")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kazhala/InquirerPy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01s1wpsfsjxd1vpvhrz9b5314fml8kg11a3fiqnrzqqlf5j33782"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list python-poetry-core python-pytest
                         python-prompt-toolkit python-pfzy))
    (home-page "https://github.com/kazhala/InquirerPy")
    (synopsis
     "Python port of Inquirer.js (A collection of common interactive command-line user interfaces)")
    (description
     "Python port of Inquirer.js (A collection of common interactive command-line user interfaces)")
    (license license:expat)))

(define-public python-ffmpy
  (package
    (name "python-ffmpy")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ffmpy" version))
       (sha256
        (base32 "13x74ydb8sa769z65h3qxv9mdgxr5p6xnjckjs6inrj423zvypj7"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/Ch00k/ffmpy")
    (synopsis "A simple Python wrapper for ffmpeg")
    (description "This package provides a simple Python wrapper for ffmpeg")
    (license license:expat)))

(define-public python-chardet
  (package
    (name "python-chardet")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "chardet" version))
       (sha256
        (base32 "1bpalpia6r5x1kknbk11p1fzph56fmmnp405ds8icksd3knr5aw4"))))
    (arguments
     (list
      #:tests? #f))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/chardet/chardet")
    (synopsis "Universal encoding detector for Python 3")
    (description "Universal encoding detector for Python 3.")
    (license license:lgpl2.1)))

(define-public python-deprecated
  (package
    (name "python-deprecated")
    (version "1.2.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Deprecated" version))
       (sha256
        (base32 "1cq17pavjw291hmzyrbhl9ssfln84b1zkiidb31cr3a56swkwcp5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-wrapt))
    (native-inputs (list python-bump2version
                         python-pytest
                         python-pytest-cov
                         python-sphinx
                         python-tox
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/tantale/deprecated")
    (synopsis
     "Python @deprecated decorator to deprecate old python classes, functions or methods.")
    (description
     "Python @@deprecated decorator to deprecate old python classes, functions or
methods.")
    (license license:expat)))

(define-public python-future
  (package
    (name "python-future")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "future" version))
       (sha256
        (base32 "01bvq2a5vgxffq8555rvwhxw161m9y54z2j5w7d1x1h7jcq6hadx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://python-future.org")
    (synopsis "Clean single-source support for Python 3 and 2")
    (description "Clean single-source support for Python 3 and 2.")
    (license license:expat)))

(define-public python-distro
  (package
    (name "python-distro")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "distro" version))
       (sha256
        (base32 "1vfvkgfvrjpxpb48pf8rs2l5wfxij0plmffnw5p123wlv1ppr9rg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/nir0s/distro")
    (synopsis "OS platform information API")
    (description
     "@code{distro} provides information about the OS distribution it runs on,
such as a reliable machine-readable ID, or version information.

It is the recommended replacement for Python's original
`platform.linux_distribution` function (which will be removed in Python 3.8).
@code{distro} also provides a command-line interface to output the platform
information in various formats.")
    (license license:asl2.0)))

(define-public python-openai-next
  (package
    (name "python-openai")
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openai" version))
       (sha256
        (base32 "1cv2zc0z0jrkn1lnqk66096jf7fhiqs1znxxdklni138h14yyji5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-anyio
                             python-distro
                             python-httpx
                             python-jiter
                             python-pydantic
                             python-sniffio
                             python-tqdm
                             python-typing-extensions))
    (native-inputs (list python-hatch-fancy-pypi-readme python-hatchling))
    (home-page "https://github.com/openai/openai-python")
    (synopsis "The official Python library for the openai API")
    (description "The official Python library for the openai API.")
    (license license:expat)))

(define-public python-bibtexparser
  (package
    (name "python-bibtexparser")
    (version "2.0.0b7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sciunto-org/python-bibtexparser")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y76hfm6yddp61wb2m2yavlvl9cmi2drh7d5r6irk1bxnhr4g08b"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-pyparsing python-pylatexenc))
    (native-inputs (list python-future))
    (home-page "https://github.com/sciunto-org/python-bibtexparser")
    (synopsis "Python library to parse BibTeX files")
    (description "BibtexParser is a Python library to parse BibTeX files.")
    (license (list license:bsd-3 license:lgpl3))))

(define-public python-pysbd
  (package
    (name "python-pysbd")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nipunsadvilkar/pySBD")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12p7qm237z56hw4zr03n8rycgfymhki2m9c4w3ib0mvqq122a5dp"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-pyparsing python-pylatexenc))
    (native-inputs (list python-future))
    (home-page "https://github.com/nipunsadvilkar/pySBD")
    (synopsis
     "pySBD - python Sentence Boundary Disambiguation (SBD) - is a rule-based sentence boundary detection module that works out-of-the-box.")
    (description
     "pySBD - python Sentence Boundary Disambiguation (SBD) - is a rule-based sentence boundary detection module that works out-of-the-box.")
    (license (list license:bsd-3 license:lgpl3))))

(define-public python-pybind11
  (package
    (name "python-pybind11")
    (version "2.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pybind11" version))
       (sha256
        (base32 "1c67pfycghy9gl2hsmzjxdmr2x2n6xjqwl6immhn2ldc3j5lkgk5"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/pybind/pybind11")
    (synopsis "Seamless operability between C++11 and Python")
    (description "Seamless operability between C++11 and Python.")
    (license license:bsd-3)))

(define-public python-camel-converter
  (package
    (name "python-camel-converter")
    (version "3.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sanders41/camel-converter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f8cw4habpfkyz9gjjmj548ipjk18c32yvl57qkgbh9p2aaxz5h8"))))
    (build-system pyproject-build-system)
    (inputs (list python-poetry-core python-pytest python-pytest-cov
                  python-pydantic-2))
    (home-page "https://github.com/sanders41/camel-converter")
    (synopsis
     "Converts a string from snake case to camel case or camel case to snake case")
    (description
     "Converts a string from snake case to camel case or camel case to snake case.")
    (license license:expat)))

(define-public python-meilisearch
  (package
    (name "python-meilisearch")
    (version "0.34.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/meilisearch/meilisearch-python")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0408yjq8wh5ljr3map2zl1l6hxwkyrqvkyg8fpfkril98h1qd26b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (inputs (list python-poetry-core python-pytest pre-commit
                  python-pytest-cov))
    (propagated-inputs (list python-camel-converter python-requests
                             python-pydantic-2))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/meilisearch/meilisearch-python")
    (synopsis "The python client for Meilisearch API.")
    (description "The python client for Meilisearch API.")
    (license license:expat)))

(define-public python-mss
  (package
    (name "python-mss")
    (version "9.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mss" version))
       (sha256
        (base32 "07m2jc0fzg1v24ljx1yi01lviy0qmb7z6zn05fr7vni46b3lwsn9"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling python-wheel python-mypy))
    (propagated-inputs (list python-numpy
                             python-pillow
                             python-pytest
                             python-pytest-cov
                             python-pytest-rerunfailures
                             python-pyvirtualdisplay))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-invalid-classifier
            (lambda _
              (substitute* "pyproject.toml"
                (("Programming Language :: Python :: 3.13")
                 "Programming Language :: Python :: 3.12")))))))
    (home-page "https://github.com/BoboTiG/python-mss")
    (synopsis
     "An ultra fast cross-platform multiple screenshots module in pure python using ctypes.")
    (description
     "An ultra fast cross-platform multiple screenshots module in pure python using
ctypes.")
    (license license:expat)))

(define-public python-nodriver
  (let ((websockets (package
                      (name "python-websockets")
                      (version "14.2")
                      (source
                       (origin
                         (method git-fetch)
                         (uri (git-reference
                               (url "https://github.com/aaugustin/websockets")
                               (commit version)))
                         (file-name (git-file-name name version))
                         (sha256
                          (base32
                           "0j8x1xn3m1jcghwy42y6ibspr9kwazcgdz1c90i0jxdgj50xxbiz"))))
                      (build-system pyproject-build-system)
                      (arguments
                       (list
                        #:phases
                        #~(modify-phases %standard-phases
                            (add-before 'check 'extend-test-timeout
                              (lambda _
                                (setenv "WEBSOCKETS_TESTS_TIMEOUT_FACTOR" "10"))))))
                      (native-inputs (list python-setuptools python-wheel))
                      (home-page "https://github.com/aaugustin/websockets")
                      (synopsis
                       "Python implementation of the WebSocket Protocol (RFC 6455 & 7692)")
                      (description
                       "@code{websockets} is a library for building WebSocket servers and clients
in Python with a focus on correctness and simplicity.

Built on top of @code{asyncio}, Python's standard asynchronous I/O framework,
it provides an elegant coroutine-based API.")
                      (license license:bsd-3))))
    (package
      (name "python-nodriver")
      (version "0.39")
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "nodriver" version))
         (sha256
          (base32 "1vs848zh0a8z40fjkizl31hizqv8a9hygj4mdwb78z472migg15g"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:tests? #f))
      (propagated-inputs (list python-setuptools
                               python-wheel
                               python-deprecated
                               python-mss
                               websockets
                               python-black
                               python-sphinx
                               python-pygments))
      (home-page "https://github.com/ultrafunkamsterdam/nodriver")
      (synopsis "Browser automation without webdriver based Selenium")
      (description
       "This package provides next level webscraping and browser automation using a relatively simple interface.
@itemize
@item This is the official successor of the Undetected-Chromedriver python package.
@item No more webdriver, no more selenium
@end itemize
Direct communication provides even better resistance against web applicatinon firewalls (WAF’s), while performance gets a massive boost. This module is, contrary to undetected-chromedriver, fully asynchronous.
What makes this package different from other known packages, is the optimization to stay undetected for most anti-bot solutions.
Another focus point is usability and quick prototyping, so expect a lot to work -as is- , with most method parameters having best practice defaults. Using 1 or 2 lines, this is up and running, providing best practice config by default.
While usability and convenience is important. It’s also easy to fully customizable everything using the entire array of CDP domains, methods and events available.
Some features:
@itemize
@item A blazing fast undetected chrome (-ish) automation library
@item No chromedriver binary or Selenium dependency
@item This equals bizarre performance increase and less detections!
@item Up and running in 1 line of code*
@item uses fresh profile on each run, cleans up on exit
@item save and load cookies to file to not repeat tedious login steps
@item smart element lookup, by selector or text, including iframe content. this could also be used as wait condition for a element to appear, since it will retry for the duration of until found. single element lookup by text using tab.find(), accepts a best_match flag, which will not naively return the first match, but will match candidates by closest matching text length.
@item descriptive __repr__ for elements, which represent the element as html
@item utility function to convert a running undetected_chromedriver.Chrome instance to a nodriver.Browser instance and contintue from there
@item packed with helpers and utility methods for most used and important operations
@end itemize")
      (license license:agpl3))))

(define-public python-pyemd
  (package
    (name "python-pyemd")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyemd" version))
       (sha256
        (base32 "0w3yw014760ncm09ymbh0wnw4wwz7ph773dvvxcyaww5dw8w50gw"))))
    (build-system python-build-system)
    (propagated-inputs (list python-numpy))
    (home-page "http://github.com/wmayner/pyemd")
    (synopsis
     "Wrapper for Pele and Werman's implementation of the Earth Mover's Distance")
    (description
     "This package provides a Python wrapper for Ofir Pele and Michael Werman's
implementation of the Earth Mover's Distance.")
    (license license:expat)))

(define-public python-pyemd
  (package
    (name "python-pyemd")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyemd" version))
       (sha256
        (base32 "0w3yw014760ncm09ymbh0wnw4wwz7ph773dvvxcyaww5dw8w50gw"))))
    (build-system python-build-system)
    (propagated-inputs (list python-numpy))
    (home-page "http://github.com/wmayner/pyemd")
    (synopsis
     "Wrapper for Pele and Werman's implementation of the Earth Mover's Distance")
    (description
     "This package provides a Python wrapper for Ofir Pele and Michael Werman's
implementation of the Earth Mover's Distance.")
    (license license:expat)))

(define-public python-morfessor
  (package
    (name "python-morfessor")
    (version "2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Morfessor" version))
       (sha256
        (base32 "1cmsxyd7ymlqlgam9a6va0x3fqhz0w1mixj0yv2j85rl6k1flfxv"))))
    (build-system python-build-system)
    (home-page "http://morpho.aalto.fi")
    (synopsis "Morfessor")
    (description "Morfessor")
    (license license:bsd-3)))

(define-public pybind11-2.6.1
  (package
    (inherit pybind11)
    (name "pybind11")
    (version "2.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pybind/pybind11")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1wh5b1xnywzxwxkyac2wvyqwzmy1qxs341jjk820r7b825wn6yad"))
       (file-name (git-file-name name version))))
    (arguments
     (substitute-keyword-arguments (package-arguments pybind11)
       ((#:phases phases
         #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'skip-failing-test
              (lambda _
                (substitute* "tests/test_exceptions.py"
                  ;; This test fails with Python 3.10; skip it.
                  (("^def test_python_alreadyset_in_destructor(.*)" _ rest)
                   (string-append "def test_python_alreadyset_in_destructor"
                                  rest "\n" "    return\n")))))))))))

(define-public python-nmslib
  (package
    (name "python-nmslib")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nmslib" version))
       (sha256
        (base32 "084wl5kl2grr2yi3bibc6i2ak5s7vanwi21wssbwd4bgfskr84lp"))))
    (build-system python-build-system)
    (propagated-inputs (list python-numpy python-psutil pybind11-2.6.1))
    (home-page "https://github.com/nmslib/nmslib")
    (synopsis "Non-Metric Space Library (NMSLIB)")
    (description "Non-Metric Space Library (NMSLIB)")
    (license license:asl2.0)))

;; This package bundles 'multibuild'.
(define-public python-gensim
  (package
    (name "python-gensim")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gensim" version))
       (sha256
        (base32 "1wgf6kzm3jc3i39kcrdhw89bzqj75whi1b5a030lf7d3f0lvsplr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This fails for unknown reasons when trying to launch
      ;; visdom.server.
      '(list "-k" "not test_callback_update_graph"
             ;; This needs access to the internet
             "--ignore=gensim/test/test_api.py")
      #:phases '(modify-phases %standard-phases
                  (add-after 'unpack 'patch-build-system
                    (lambda _
                      (substitute* "setup.py"
                        (("__builtins__.__NUMPY_SETUP__.*")
                         ""))))
                  (add-before 'check 'build-extensions
                    (lambda _
                      ;; Cython extensions have to be built before running the tests.
                      (invoke "python" "setup.py" "build_ext" "--inplace")
                      ;; Needed for some of the tests
                      (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list python-pyemd python-numpy python-nmslib
                             python-scipy python-smart-open))
    (native-inputs (list python-cython
                         python-mock
                         python-pytest
                         python-pytest-cov
                         python-testfixtures
                         python-visdom))
    (home-page "https://radimrehurek.com/gensim/")
    (synopsis "Python framework for fast vector space modelling")
    (description
     "This package provides a Python library for topic modelling,
document indexing and similarity retrieval with large corpora.  The target
audience is the natural language processing and information retrieval
community.")
    (license license:lgpl2.1)))

(define-public python-pyclibrary
  (package
    (name "python-pyclibrary")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyclibrary" version))
       (sha256
        (base32 "1xvm6l5fr14pdnz94s2w7jv85lsdxjal3ak2mdbnzf0v6vzgy0lr"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))
    (propagated-inputs (list python-pyparsing))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "http://github.com/MatthieuDartiailh/pyclibrary")
    (synopsis "C binding automation")
    (description "C binding automation.")
    (license license:expat)))

(define-public python-scihub-py
  (let ((commit "82532fc4fe9b405e1286f60676b776696f4bc844")
        (version "20200619"))
    (package
      (name "python-scihub-py")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zaytoun/scihub.py")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1p8dy1y8sihdjzqyjc7clvm41m3mmz9ybkxmqg1q8vb6yxkpr837"))))
      (build-system pyproject-build-system)
      (arguments
       '(#:tests? #f))
      (propagated-inputs (list python-beautifulsoup4 python-requests
                               python-retrying))
      (native-inputs (list python-setuptools python-wheel))
      (home-page "https://github.com/zaytoun/scihub.py")
      (synopsis "Unofficial scihub API")
      (description
       "@code{scihub.py} is an unofficial API for Sci-hub. @code{scihub.py} can search for papers on Google Scholars and download papers from Sci-hub.
It can be imported independently or used from the command-line. If you believe in open access to scientific papers, please donate to Sci-Hub. Features:
@itemize
@item Download specific articles directly or via Sci-hub.
@item Download a collection of articles by passing in file of article identifiers.
@item Search for articles on Google Scholars and download them
@end itemize
Note: A known limitation of scihub.py is that captchas show up every now and then, blocking any searches or downloads.")
      (license license:expat))))

(define-public python-jose
  (package
    (name "python-jose")
    (version "3.3.0")
    (home-page "https://github.com/mpdavis/python-jose")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18whsdpllg8574ma4r0qawkgw4nam6lsf63pi6761j38rvl84lg9"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (native-inputs (list ;All native inputs are for tests.
                         python-pyasn1 python-pytest python-pytest-cov
                         python-pytest-runner))
    (propagated-inputs (list python-cryptography python-ecdsa python-rsa
                             python-six))
    (synopsis "JOSE implementation in Python")
    (description
     "The @dfn{JavaScript Object Signing and Encryption} (JOSE) technologies
- JSON Web Signature (JWS), JSON Web Encryption (JWE), JSON Web Key (JWK), and
JSON Web Algorithms (JWA) - collectively can be used to encrypt and/or sign
content using a variety of algorithms.")
    (license license:expat)))

(define-public python-pillow-simd
  (package
    (inherit python-pillow)
    (name "python-pillow-simd")
    (version "9.3.0")
    ;; The PyPI tarball does not include test files.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uploadcare/pillow-simd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qnvpwzlx4rfz17qmsipr5iwzmh8xgmzvc79spnrmqibk3s18vyi"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; This test fails because it cannot find the zlib version string
      ;; "1.3.1".
      #:tests? #f
      #:phases '(modify-phases %standard-phases
                  (add-after 'unpack 'patch-ldconfig
                    (lambda _
                      (substitute* "setup.py"
                        (("\\['/sbin/ldconfig', '-p'\\]")
                         "['true']")))))))
    (inputs (modify-inputs (package-inputs python-pillow)
              (prepend libraqm libimagequant)))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/uploadcare/pillow-simd")
    (synopsis "Fork of the Python Imaging Library (Pillow)")
    (description "This package is a fork of Pillow which adds support for SIMD
parallelism.")))

(define-public python-ruff
  (package
    (name "python-ruff")
    (version "0.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astral-sh/ruff")
             (commit version)))
       (file-name (git-file-name name version))
       (patches (parameterize ((%patch-path (map (lambda (directory)
                                                   (string-append directory
                                                    "/myguix/patches"))
                                                 %load-path)))
                  (search-patches "ruff-embed-salsa-and-lsp-types.patch")))
       (sha256
        (base32 "1785vq9xx1nxipjwkp97fbvzyp8qg1zrmyv0amhs80v7ir94ckjp"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system)
                   #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'install)
          (add-after 'unpack 'override-jemalloc
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((jemalloc (assoc-ref inputs "jemalloc")))
                ;; This flag is needed when not using the bundled jemalloc.
                ;; https://github.com/tikv/jemallocator/issues/19
                (setenv
                 "CARGO_FEATURE_UNPREFIXED_MALLOC_ON_SUPPORTED_PLATFORMS" "1")
                (setenv "JEMALLOC_OVERRIDE"
                        (string-append jemalloc "/lib/libjemalloc.so")))))
          (add-after 'build 'build-python-module
            (assoc-ref py:%standard-phases
                       'build))
          (add-after 'build-python-module 'install-python-module
            (lambda* (#:key outputs #:allow-other-keys)
              ;; We'll do a manual ‘pip install’ to prefix=$out and trust that
              ;; it installs the compiled `ruff` binary without trying to parse it.
              (let* ((out (assoc-ref outputs "out"))
                     (wheel (car (find-files "target/wheels" "\\.whl$"))))
                (invoke "pip"
                        "--no-cache-dir"
                        "--no-input"
                        "install"
                        "--no-deps"
                        "--prefix"
                        out
                        wheel)))))
      #:cargo-inputs `(("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-append-only-vec" ,rust-append-only-vec-0.1)
                       ("rust-argfile" ,rust-argfile-0.2)
                       ("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-assert-fs" ,rust-assert-fs-1)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-cachedir" ,rust-cachedir-0.3)
                       ("rust-camino" ,rust-camino-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-complete-command" ,rust-clap-complete-command-0.6)
                       ("rust-clearscreen" ,rust-clearscreen-4)
                       ("rust-codspeed-criterion-compat" ,rust-codspeed-criterion-compat-2)
                       ("rust-colored" ,rust-colored-3)
                       ("rust-compact-str" ,rust-compact-str-0.8)
                       ("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1)
                       ("rust-console-log" ,rust-console-log-1)
                       ("rust-countme" ,rust-countme-3)
                       ("rust-criterion" ,rust-criterion-0.5)
                       ("rust-crossbeam" ,rust-crossbeam-0.8)
                       ("rust-dashmap" ,rust-dashmap-6)
                       ("rust-dir-test" ,rust-dir-test-0.4)
                       ("rust-drop-bomb" ,rust-drop-bomb-0.1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-env-logger" ,rust-env-logger-0.11)
                       ("rust-etcetera" ,rust-etcetera-0.8)
                       ("rust-fern" ,rust-fern-0.7)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-globset" ,rust-globset-0.4)
                       ("rust-globwalk" ,rust-globwalk-0.9)
                       ("rust-hashbrown" ,rust-hashbrown-0.15)
                       ("rust-hashlink" ,rust-hashlink-0.9)
                       ("rust-ignore" ,rust-ignore-0.4)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-imperative" ,rust-imperative-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-indoc" ,rust-indoc-2)
                       ("rust-insta" ,rust-insta-1)
                       ("rust-insta-cmd" ,rust-insta-cmd-0.6)
                       ("rust-is-macro" ,rust-is-macro-0.3)
                       ("rust-is-wsl" ,rust-is-wsl-0.4)
                       ("rust-itertools" ,rust-itertools-0.14)
                       ("rust-jemallocator" ,rust-jemallocator-0.5)
                       ("rust-jod-thread" ,rust-jod-thread-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libcst" ,rust-libcst-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lsp-server" ,rust-lsp-server-0.7)
                       ("rust-lsp-types" ,rust-lsp-types-0.95)
                       ("rust-matchit" ,rust-matchit-0.8)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mimalloc" ,rust-mimalloc-0.1)
                       ("rust-natord" ,rust-natord-1)
                       ("rust-notify" ,rust-notify-8)
                       ("rust-ordermap" ,rust-ordermap-0.5)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-path-absolutize" ,rust-path-absolutize-3)
                       ("rust-path-slash" ,rust-path-slash-0.2)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-pep440-rs" ,rust-pep440-rs-0.7)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-pyproject-toml" ,rust-pyproject-toml-0.13)
                       ("rust-quick-junit" ,rust-quick-junit-0.5)
                       ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-2)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-seahash" ,rust-seahash-4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-serde-test" ,rust-serde-test-1)
                       ("rust-serde-wasm-bindgen" ,rust-serde-wasm-bindgen-0.6)
                       ("rust-serde-with" ,rust-serde-with-3)
                       ("rust-shellexpand" ,rust-shellexpand-3)
                       ("rust-similar" ,rust-similar-2)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-snapbox" ,rust-snapbox-0.6)
                       ("rust-static-assertions" ,rust-static-assertions-1)
                       ("rust-strum" ,rust-strum-0.26)
                       ("rust-strum-macros" ,rust-strum-macros-0.26)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-test-case" ,rust-test-case-3)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-tikv-jemalloc-sys" ,rust-tikv-jemalloc-sys-0.6)
                       ("rust-tikv-jemallocator" ,rust-tikv-jemallocator-0.6)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-flame" ,rust-tracing-flame-0.2)
                       ("rust-tracing-indicatif" ,rust-tracing-indicatif-0.3)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-tracing-tree" ,rust-tracing-tree-0.4)
                       ("rust-tryfn" ,rust-tryfn-0.2)
                       ("rust-typed-arena" ,rust-typed-arena-2)
                       ("rust-unic-ucd-category" ,rust-unic-ucd-category-0.9)
                       ("rust-unicode-ident" ,rust-unicode-ident-1)
                       ("rust-unicode-names2" ,rust-unicode-names2-1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.2)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                       ("rust-wild" ,rust-wild-2)
                       ("rust-zip" ,rust-zip-0.6))
      #:cargo-development-inputs `(("rust-annotate-snippets" ,rust-annotate-snippets-0.11)
                                   ("rust-codspeed-criterion-compat" ,rust-codspeed-criterion-compat-2)
                                   ("rust-derive-new" ,rust-derive-new-0.6)
                                   ("rust-expect-test" ,rust-expect-test-1)
                                   ("rust-eyre" ,rust-eyre-0.6)
                                   ("rust-notify-debouncer-mini" ,rust-notify-debouncer-mini-0.4)
                                   ("rust-ordered-float" ,rust-ordered-float-4)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-test-log" ,rust-test-log-0.2)
                                   ("rust-trybuild" ,rust-trybuild-1))
      #:cargo-test-flags `(list "--release"
                           "--"
                           "--skip=rules::flake8_executable::tests::rules::path_new_exe001_1_py_expects"
                           "--skip=rules::flake8_executable::tests::rules::path_new_exe003_py_expects"
                           "--skip=rules::isort::tests::required_import::path_new_comment_py_expects"
                           "--skip=rules::isort::tests::required_import_with_alias::path_new_comment_py_expects"
                           "--skip=rules::pycodestyle::tests::max_doc_length"
                           "--skip=rules::pycodestyle::tests::max_doc_length_with_utf_8"
                           "--skip=rules::pycodestyle::tests::shebang"
                           "--skip=rules::pyupgrade::tests::rules::rule_utf8encodingdeclaration_path_new_up009_1_py_expects"
                           "--skip=black_compatibility")
      #:install-source? #f))
    (inputs (list (list zstd "lib") jemalloc))
    (native-inputs (list maturin python-pypa-build python-typing-extensions
                         python-wrapper pkg-config))
    (home-page "https://docs.astral.sh/ruff")
    (synopsis
     "An extremely fast Python linter and code formatter, written in Rust.")
    (description
     "An extremely fast Python linter and code formatter, written in Rust.")
    (license license:expat)))

(define-public python-cassandra-driver
  (let ((geomet (package
                  (name "python-geomet")
                  (version "0.2.1")
                  (source
                   (origin
                     (method url-fetch)
                     (uri (pypi-uri "geomet" version))
                     (sha256
                      (base32
                       "15q27jg6ca9f7gcsl3m4xjijgsvzvgk1rik7kx8cqv0binngxymd"))))
                  (build-system pyproject-build-system)
                  (arguments
                   (list
                    #:tests? #f))
                  (propagated-inputs (list python-click python-six))
                  (native-inputs (list python-setuptools python-wheel))
                  (home-page "")
                  (synopsis
                   "Pure Python conversion library for common geospatial data formats")
                  (description
                   "Pure Python conversion library for common geospatial data formats.")
                  (license license:asl2.0))))
    (package
      (name "python-cassandra-driver")
      (version "3.29.2")
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "cassandra-driver" version))
         (sha256
          (base32 "0z4val3glqmwgw4m6arn848lk32q07sqx781zdiimxap0iyhlcf4"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:tests? #f))
      (propagated-inputs (list geomet))
      (native-inputs (list python-setuptools python-wheel))
      (home-page "http://github.com/datastax/python-driver")
      (synopsis "DataStax Driver for Apache Cassandra")
      (description "@code{DataStax} Driver for Apache Cassandra.")
      (license license:asl2.0))))

(define-public python-gremlinpython
  (package
    (name "python-gremlinpython")
    (version "3.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gremlinpython" version))
       (sha256
        (base32 "1x1yp3v1zz7nk7vra112cnha3aggc3i1b6pzvrrg1p4z178ln568"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-aenum python-aiohttp python-async-timeout
                             python-isodate python-nest-asyncio))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://tinkerpop.apache.org")
    (synopsis "Gremlin-Python for Apache TinkerPop")
    (description "Gremlin-Python for Apache @code{TinkerPop}.")
    (license license:asl2.0)))

(define-public python-questionary
  (package
    (name "python-questionary")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "questionary" version))
       (sha256
        (base32 "11smz75cm5ixfiz2ignrmb8zr2skx5s4fqz6yvc6g5mi8pbcs0k3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-prompt-toolkit))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/tmbo/questionary")
    (synopsis "Python library to build pretty command line user prompts â­ï¸")
    (description
     "Python library to build pretty command line user prompts â­ï¸.")
    (license license:expat)))

(define-public python-hatch-regex-commit
  (package
    (name "python-hatch-regex-commit")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hatch_regex_commit" version))
       (sha256
        (base32 "0f4v1vmim33nwi1r6v5sbf62f3aq2rvkbrl63j86gmnq3li4nvps"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-hatchling))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/frankie567/hatch-regex-commit")
    (synopsis "Hatch plugin to create a commit and tag when bumping version")
    (description
     "Hatch plugin to create a commit and tag when bumping version.")
    (license license:expat)))

(define-public python-fief-client
  (package
    (name "python-fief-client")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fief_client" version))
       (sha256
        (base32 "1mrlqjxzn1pccgic9hfmynv5ylq88ym47j65x8n419f40dnr1yyv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-httpx python-jwcrypto))
    (native-inputs (list python-hatch-regex-commit python-hatchling))
    (home-page "https://github.com/fief-dev/fief-python")
    (synopsis "Fief Client for Python")
    (description "Fief Client for Python.")
    (license license:expat)))

(define-public python-codecarbon
  (package
    (name "python-codecarbon")
    (version "2.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "codecarbon" version))
       (sha256
        (base32 "0s8ji43pfipglwsgzjfgxn39g10c3rix3v4kz1a03xn5l6pxaz83"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-arrow
                             python-click
                             python-fief-client
                             python-pandas
                             python-prometheus-client
                             python-psutil
                             python-py-cpuinfo
                             python-py3nvml
                             python-questionary
                             python-rapidfuzz
                             python-requests
                             python-rich
                             python-typer))
    (native-inputs (list python-hatchling))
    (home-page "https://mlco2.github.io/codecarbon")
    (synopsis
     "Estimate and track carbon emissions from your computer, quantify and analyze their impact.")
    (description
     "CodeCarbon started with a quite simple question: What is the carbon emission impact of my computer program? We found some global data like computing currently represents roughly 0.5% of the world’s energy consumption but nothing on our individual/organisation level impact. At CodeCarbon, we believe, along with Niels Bohr, that Nothing exists until it is measured. So we found a way to estimate how much CO2 we produce while running our code. How? We created a Python package that estimates your hardware electricity power consumption (GPU + CPU + RAM) and we apply to it the carbon intensity of the region where the computing is done.")
    (license license:expat)))

(define-public python-jiter
  (package
    (name "python-jiter")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pydantic/jiter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02fnjgiw9avjqxzrx3bgy828l5g7ngp4nysgx9yb1k3zmd1z0lz8"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (substitute* "crates/jiter/Cargo.toml"
                     (("version = \\{ workspace = true \\}")
                      "version = \"0.8.2\""))))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system)
                   #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'install)
          (add-after 'build 'build-python-module
            (assoc-ref py:%standard-phases
                       'build))
          (add-before 'build-python-module 'chdir
            (lambda* _
              (chdir "crates/jiter-python")))
          (add-after 'build-python-module 'install-python-module
            (assoc-ref py:%standard-phases
                       'install)))
      #:cargo-inputs `(("rust-ahash" ,rust-ahash-0.8)
                       ("rust-bitvec" ,rust-bitvec-1)
                       ("rust-lexical-parse-float" ,rust-lexical-parse-float-0.8)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pyo3" ,rust-pyo3-0.23)
                       ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.23)
                       ("rust-smallvec" ,rust-smallvec-1))
      #:cargo-development-inputs `(("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-codspeed-bencher-compat" ,rust-codspeed-bencher-compat-2)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-pyo3" ,rust-pyo3-0.23)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1))
      #:install-source? #f))
    (inputs (list maturin))
    (native-inputs (list python-wrapper))
    (home-page "https://github.com/pydantic/jiter/")
    (synopsis "Fast Iterable JSON parser")
    (description "This package provides Fast Iterable JSON parser.")
    (license license:expat)))

(define-public python-alexify
  (package
    (name "python-alexify")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/b-vitamins/alexify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v8cd5vjw6zk3q0wa02aigi3mzjr2pb3s52y5w69r8rp3kvldch4"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-bibtexparser-1 python-fuzzywuzzy
                             python-pyalex python-requests))
    (native-inputs (list python-poetry-core python-pytest))
    (home-page "https://github.com/b-vitamins/alexify")
    (synopsis
     "CLI tool and Python library for adding OpenAlex metadata to BibTeX files.")
    (description
     "@code{alexify} is a command-line tool and Python library that helps you enrich your BibTeX files with metadata from @url{https://openalex.org/,OpenAlex}. It automates the process of matching entries by Title and/or DOI, retrieving corresponding OpenAlex IDs, and optionally fetching detailed JSON metadata about those works.")
    (license license:asl2.0)))

(define-public python-isosurfaces
  (package
    (name "python-isosurfaces")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "isosurfaces" version))
       (sha256
        (base32 "08bfimqq12rvgssd9a4z865sbma1dbfpzqihd2r5b4zacklfnlgs"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/jared-hughes/isosurfaces")
    (synopsis
     "Construct isolines/isosurfaces over a 2D/3D scalar field defined by a function (not a uniform grid)")
    (description
     "Construct isolines/isosurfaces over a 2D/3D scalar field defined by a function
(not a uniform grid).")
    (license license:expat)))

(define-public python-neo4j
  (package
    (name "python-neo4j")
    (version "5.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "neo4j" version))
       (sha256
        (base32 "0572k090q657xg86irvvmjx9j876rjr5khsvqxi902cmv2hkg3mf"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pytz))
    (native-inputs (list python-setuptools python-tomlkit python-wheel))
    (home-page "https://github.com/neo4j/neo4j-python-driver")
    (synopsis "Neo4j Bolt driver for Python")
    (description "Neo4j Bolt driver for Python.")
    (license license:asl2.0)))

(define-public python-graphdatascience
  (let* ((python-pyarrow-flight (package
                                  (inherit python-pyarrow)
                                  (name "python-pyarrow-flight")
                                  (propagated-inputs (modify-inputs (package-propagated-inputs
                                                                     python-pyarrow)
                                                       (replace "apache-arrow"
                                                        apache-arrow-flight)))

                                  ;; Ensure the C++ libs are available while building wheels
                                  (native-inputs (modify-inputs (package-native-inputs
                                                                 python-pyarrow)
                                                   (prepend
                                                    apache-arrow-flight)))
                                  (arguments
                                   (substitute-keyword-arguments (package-arguments
                                                                  python-pyarrow)
                                     ((#:phases phases)
                                      #~(modify-phases #$phases
                                          (replace 'set-pyarrow-build-options
                                            (lambda _
                                              (setenv
                                               "PYARROW_BUNDLE_ARROW_CPP_HEADERS"
                                               "0")
                                              (setenv "PYARROW_CMAKE_OPTIONS"
                                                      (string-append
                                                       "-DCMAKE_INSTALL_RPATH="
                                                       #$output))
                                              (setenv "PYARROW_PARALLEL"
                                                      (number->string (parallel-job-count)))
                                              (setenv "PYARROW_WITH_DATASET"
                                                      "1")
                                              (setenv "PYARROW_WITH_HDFS" "1")
                                              (setenv "PYARROW_WITH_ORC" "1")
                                              (setenv "PYARROW_WITH_PARQUET"
                                                      "1")
                                              (setenv "PYARROW_WITH_FLIGHT"
                                                      "1"))))))))))
    (package
      (name "python-graphdatascience")
      (version "1.15")
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "graphdatascience" version))
         (sha256
          (base32 "0fbkj1s1scx64w2sqcwm7rgx1852m898wjxj4f0w471m29rz5w3p"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:tests? #f))
      (propagated-inputs (list python-multimethod
                               python-neo4j
                               python-numpy
                               python-pandas
                               python-pyarrow-flight
                               python-requests
                               python-tenacity
                               python-textdistance
                               python-tqdm
                               python-typing-extensions))
      (native-inputs (list python-setuptools python-wheel))
      (home-page "https://neo4j.com/product/graph-data-science/")
      (synopsis "Python client for the Neo4j Graph Data Science library")
      (description "Python interface for Neo4j Graph Data Science.")
      (license license:asl2.0))))

(define-public python-qdrant-client
  (package
    (name "python-qdrant-client")
    (version "1.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qdrant_client" version))
       (sha256
        (base32 "03dgjcdm04zqsjjqz7cgidpxg2zwmr2hvwxn609rs2bx6r6snp6s"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-httpx ;+ http2 extra already handled by PyPI wheel
                             python-numpy
                             python-pydantic-2
                             python-grpcio
                             python-protobuf
                             python-urllib3
                             python-portalocker))
    (native-inputs (list python-pytest python-poetry-core))
    (arguments
     (list
      ;; Skip the test subset that needs FastEmbed/ONNXRuntime
      #:test-flags
      #~(list "-m" "not fastembed")
      ;; A few tests reach for the network; disarm them.
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'offline
            (lambda _
              (setenv "HTTPX_OFFLINE" "1"))))))
    (home-page "https://github.com/qdrant/qdrant-client")
    (synopsis "Python client SDK for the Qdrant vector search engine")
    (description "Typed, sync+async SDK with local-mode support for Qdrant.")
    (license license:asl2.0)))

(define-public python-apache-airflow-client
  (package
    (name "python-apache-airflow-client")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "apache_airflow_client" version))
       (sha256
        (base32 "18v989453hcj11x9kv5jicfplqdv0x30rlggcagqvx7jxi3ilr28"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-dateutil python-urllib3 python-pytest
                             python-pytest-cov))
    (native-inputs (list python-hatchling))
    (home-page "https://pypi.org/project/apache-airflow-client")
    (synopsis "Apache Airflow API (Stable)")
    (description "Apache Airflow API (Stable).")
    (license license:asl2.0)))

(define-public python-ruff-lsp
  (package
    (name "python-ruff-lsp")
    (version "0.0.62")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruff_lsp" version))
       (sha256
        (base32 "03cc6fymxy5i4psvrbmpvj8y3q93gv1qsg2dqqbcnglpfn9s7ckd"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-lsprotocol python-packaging python-pygls
                             python-ruff python-typing-extensions))
    (native-inputs (list python-hatchling
                         python-mypy
                         python-pytest
                         python-pytest-asyncio
                         python-lsp-jsonrpc
                         python-cattrs))
    (home-page "https://github.com/python-lsp/python-lsp-ruff")
    (synopsis "A Language Server Protocol implementation for Ruff.")
    (description
     "This package provides a Language Server Protocol implementation for Ruff.")
    (license license:expat)))

(define-public python-pylsp-ruff
  (package
    (name "python-pylsp-ruff")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python_lsp_ruff" version))
       (sha256
        (base32 "1b0ckzzxsinryydhywvk2xvkgk181gvcz8cn8mi29vm8njqbv01z"))))
    (arguments
     (list
      #:tests? #f))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cattrs python-lsprotocol python-lsp-server
                             python-ruff python-tomli))
    (native-inputs (list python-pre-commit python-pytest python-setuptools
                         python-wheel))
    (home-page "https://github.com/python-lsp/python-lsp-ruff")
    (synopsis "Ruff linting plugin for pylsp")
    (description "Ruff linting plugin for pylsp.")
    (license license:expat)))

(define-public python-scipy-next
  (let* ((new-version "1.15.1")
         (new-hash "1xmx5qqz07z8drmi6dv4abfzvk47vyhn7058jq69fqqlmpfpafh3"))
    (package
      (inherit python-scipy)
      (name "python-scipy-next")
      (version new-version)
      (source
       (origin
         (inherit (package-source python-scipy))
         (uri (pypi-uri "scipy" new-version))
         (sha256
          (base32 new-hash))))
      (propagated-inputs (modify-inputs (package-propagated-inputs
                                         python-scipy)
                           (delete "python-numpy")
                           (prepend python-numpy-2)))
      (inputs (modify-inputs (package-inputs python-scipy)
                (delete "pybind11-2.10")
                (prepend pybind11)))
      (native-inputs (modify-inputs (package-native-inputs python-scipy)
                       (delete "python-cython-0.29.35")
                       (prepend python-cython-3))))))
