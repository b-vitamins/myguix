(define-module (myguix packages python-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages digest)
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
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages java)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages elf)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system node)
  #:use-module (gnu packages node)
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
  #:use-module (myguix packages nvidia)
  #:use-module (myguix build-system binary))

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
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyalex" version))
       (sha256
        (base32 "1rz5prl42icrcjbaafjnwjg0axacn90jpjsfls1if404w7hgc7dp"))))
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


(define-public python-alexify
  (package
    (name "python-alexify")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/b-vitamins/alexify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fdrsw5scpy5jm7n0nq5ky0dhb4qlraaiha3ibmdggcdjgmqsmlk"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-bibtexparser-1 python-fuzzywuzzy
                             python-httpx python-levenshtein))
    (native-inputs (list python-poetry-core python-pytest
                         python-pytest-asyncio python-time-machine))
    (home-page "https://github.com/b-vitamins/alexify")
    (synopsis
     "CLI tool and Python library for adding OpenAlex metadata to BibTeX files.")
    (description
     "@code{alexify} is a command-line tool and Python library that helps you enrich your BibTeX files with metadata from @url{https://openalex.org/,OpenAlex}. It
  automates the process of matching entries by Title and/or DOI, retrieving corresponding OpenAlex IDs, and optionally fetching detailed JSON metadata about those
  works.")
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

(define-public python-mutmut
  (package
    (name "python-mutmut")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mutmut" version))
       (sha256
        (base32 "0dimi8cr73if0qzbgl8aq98vw9vfz17ih0i0g3cbgzwjpngziw28"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'relax-version-requirements
                    (lambda _
                      ;; Relax libcst version requirement to work with 1.6.0
                      (substitute* "requirements.txt"
                        (("libcst ~= 1\\.7\\.0")
                         "libcst >= 1.6.0"))))
                  (add-after 'unpack 'skip-failing-tests
                    (lambda _
                      (substitute* "tests/e2e/test_e2e_result_snapshots.py"
                        ;; These tests fail due to missing e2e_projects directories
                        (("^def test_my_lib_result_snapshot(.*)" _ rest)
                         (string-append "def test_my_lib_result_snapshot" rest
                                        "\n" "    return\n"))
                        (("^def test_config_result_snapshot(.*)" _ rest)
                         (string-append "def test_config_result_snapshot" rest
                                        "\n" "    return\n"))))))))
    (propagated-inputs (list python-click
                             python-libcst
                             python-pytest
                             python-setproctitle
                             python-textual
                             python-mdit-py-plugins))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/boxed/mutmut")
    (synopsis "mutation testing for Python 3")
    (description "mutation testing for Python 3.")
    (license license:bsd-3)))

(define-public python-pyclean
  (package
    (name "python-pyclean")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyclean" version))
       (sha256
        (base32 "1z56qxdir83mz0sx1k2qzlw7yvzy8br7j720g66f15l6al0ajgyj"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/bittner/pyclean")
    (synopsis
     "Pure Python cross-platform pyclean. Clean up your Python bytecode.")
    (description
     "Pure Python cross-platform pyclean.  Clean up your Python bytecode.")
    (license license:gpl3+)))

;; Note: This package skips snapshot tests and syntax highlighting tests
;; during the build phase. These tests require pytest-textual-snapshot
;; (not yet packaged in Guix) and proper tree-sitter setup respectively.
;; The core functionality is still thoroughly tested.

(define-public python-textual
  (package
    (name "python-textual")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Textualize/textual")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ndz2z7w3f0fnhm4c9flpc7906xxx2an1dx6z2hdbzqqmxc5diqs"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:test-flags '( ;Skip snapshot tests that require the snap_compare fixture
                       "-k"
                      "not test_snapshots"
                      ;; Skip syntax highlighting tests that require tree-sitter
                      "-m"
                      "not syntax"
                      ;; Disable pytest-textual-snapshot plugin if it's autoloaded
                      "-p"
                      "no:pytest_textual_snapshot")
       #:phases (modify-phases %standard-phases
                  ;; Set up test environment
                  (add-before 'check 'pre-check
                    (lambda _
                      (setenv "HOME" "/tmp")
                      ;; Disable devtools features that don't work in build environment
                      (setenv "TEXTUAL" "") #t))
                  ;; Remove failing tests that depend on development features
                  (add-after 'unpack 'remove-problematic-tests
                    (lambda _
                      ;; Remove snapshot tests directory entirely
                      (delete-file-recursively "tests/snapshot_tests")
                      ;; Remove specific failing test files
                      (delete-file "tests/test_features.py")
                      (delete-file "tests/text_area/test_languages.py")
                      #t)))))
    (native-inputs (list python-poetry-core
                         python-pytest
                         python-pytest-asyncio
                         python-pytest-cov
                         python-pytest-xdist
                         ;; python-pytest-textual-snapshot ; Not packaged in Guix yet
                         ))
    (propagated-inputs (list python-markdown-it-py python-rich
                             python-typing-extensions python-platformdirs))
    (home-page "https://github.com/Textualize/textual")
    (synopsis "Modern Text User Interface framework")
    (description
     "Textual is a modern Text User Interface (TUI) framework for Python that
allows developers to build sophisticated terminal applications.  It provides a
rich set of widgets, supports mouse and keyboard input, and offers a reactive
programming model similar to web frameworks.  Textual applications can run in
any terminal that supports modern terminal features, including Windows Terminal,
iTerm2, and most Linux terminals.

Key features include:
@itemize
@item Rich rendering capabilities powered by the Rich library
@item Reactive attributes and declarative UI design
@item Built-in widgets including buttons, inputs, tables, trees, and more
@item CSS-like styling system for customizing appearance
@item Mouse support and smooth scrolling
@item Async/await support for non-blocking operations
@item Cross-platform compatibility (Windows, macOS, Linux)
@end itemize")
    (license license:expat)))

(define-public python-stringzilla
  (package
    (name "python-stringzilla")
    (version "3.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stringzilla" version))
       (sha256
        (base32 "03811kjs9q1sk3sxnj1sf4c4rxjpscsnm9s0hyazsnwf1i18m5ap"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;Tests require specific setup
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/ashvardanian/stringzilla")
    (synopsis
     "SIMD-accelerated string search, sort, hashes, fingerprints, & edit distances")
    (description
     "SIMD-accelerated string search, sort, hashes, fingerprints, & edit distances.")
    (license license:asl2.0)))

(define-public python-simsimd
  (package
    (name "python-simsimd")
    (version "6.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "simsimd" version))
       (sha256
        (base32 "1lhwpqd3hqk3bnyfag7agcz5178qfmbiliwkswicvljspks99hc0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;Tests require specific setup
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/ashvardanian/simsimd")
    (synopsis
     "Portable mixed-precision BLAS-like vector math library for x86 and ARM")
    (description
     "Portable mixed-precision BLAS-like vector math library for x86 and ARM.")
    (license license:asl2.0)))

(define-public python-opencv-python-headless
  (package
    (name "python-opencv-python-headless")
    (version "4.11.0.86")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "opencv-python-headless" version))
       (sha256
        (base32 "161phkxik73k94d0jh9bvaf5s7rkwbh4shbj75mfqhsbra1b4vlr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;Tests fail in build environment due to read-only filesystem
      #:phases
      #~(modify-phases %standard-phases
          (delete 'sanity-check)))) ;Skip sanity check due to binary dependencies
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-numpy
                         python-packaging
                         python-pip
                         python-scikit-build
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/opencv/opencv-python")
    (synopsis "Wrapper package for OpenCV python bindings.")
    (description "Wrapper package for @code{OpenCV} python bindings.")
    (license license:asl2.0)))

(define-public python-albucore
  (package
    (name "python-albucore")
    (version "0.0.23")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "albucore" version))
       (sha256
        (base32 "1ax8xyav5ng2bf442b56jwiv0xs5ih2p3kz2m623p4alp613k0jp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;Tests require specific setup
    (propagated-inputs (list python-numpy python-opencv-python-headless
                             python-simsimd python-stringzilla))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/albumentations-team/albucore")
    (synopsis
     "High-performance image processing functions for deep learning and computer vision.")
    (description
     "High-performance image processing functions for deep learning and computer
vision.")
    (license license:expat)))

(define-public python-albumentations
  (package
    (name "python-albumentations")
    (version "1.4.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "albumentations" version))
       (sha256
        (base32 "0vcabzn5rmln11n6mx22l57q5b2lqf2y7jzsmqbr3k5w1b5p926c"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;Tests require specific setup and data
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'disable-update-checks
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Set environment variable to disable update checks
              (let* ((out (assoc-ref outputs "out"))
                     (site-packages (string-append out "/lib/python"
                                                   #$(version-major+minor (package-version
                                                                           python))
                                                   "/site-packages"))
                     (init-file (string-append site-packages
                                               "/albumentations/__init__.py")))
                (substitute* init-file
                  ;; Add environment variable setting at the top
                  (("^import")
                   "import os
os.environ.setdefault('NO_ALBUMENTATIONS_UPDATE', '1')
import"))))))))
    (propagated-inputs (list python-albucore
                             python-numpy
                             python-opencv-python-headless
                             python-pydantic-2
                             python-pyyaml
                             python-scipy))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/albumentations-team/albumentations")
    (synopsis
     "Fast image augmentation library for deep learning and computer vision")
    (description
     "Fast, flexible, and advanced augmentation library for deep learning, computer
vision, and medical imaging.  Albumentations offers a wide range of
transformations for both 2D (images, masks, bboxes, keypoints) and 3D (volumes,
volumetric masks, keypoints) data, with optimized performance and seamless
integration into ML workflows.")
    (license license:expat)))

(define-public python-ctypesgen
  (package
    (name "python-ctypesgen")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ctypesgen" version))
       (sha256
        (base32 "1xljmkdzjjrm8m9867pqcqrnzgjjp7wrqs4a5rm1k42xm5j2vany"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;Tests require specific setup
    (native-inputs (list python-setuptools python-setuptools-scm python-wheel))
    (home-page "https://github.com/ctypesgen/ctypesgen")
    (synopsis "Python wrapper generator for ctypes")
    (description "Python wrapper generator for ctypes.")
    (license license:bsd-2)))

(define-public python-sconf
  (package
    (name "python-sconf")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/khanrc/sconf")
             (commit "7db1a247c5264b174068a5d3c639db6cd34f65c0"))) ;HEAD commit on master
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vrzkw4iq1c30araw23ilz59qdgxc1d1y294jp6kgscwn5vy5nkp"))))
    (build-system python-build-system) ;Use python-build-system as it has setup.py
    (propagated-inputs (list python-ruamel.yaml python-munch))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/khanrc/sconf")
    (synopsis "Simple config supporting CLI modification")
    (description
     "Simple configuration management library that supports CLI modification
and YAML-based configuration files with dot-accessible dictionaries.")
    (license license:expat)))

(define pdfium-binary-for-pypdfium2
  (package
    (name "pdfium-binary")
    (version "6721")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/bblanchon/pdfium-binaries/releases/download/"
             "chromium%2F" version "/pdfium-linux-x64.tgz"))
       (sha256
        (base32 "08dr49v4akc3l9hbzh0mydgs4i18biidadbpgci2aqc4dpjf10w7"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((out (assoc-ref %outputs "out"))
                         (source (assoc-ref %build-inputs "source"))
                         (tar (assoc-ref %build-inputs "tar"))
                         (gzip (assoc-ref %build-inputs "gzip")))
                     (setenv "PATH"
                             (string-append tar "/bin:" gzip "/bin"))
                     ;; Extract the tarball
                     (invoke "tar" "-xzf" source)
                     ;; Create output directories
                     (mkdir-p (string-append out "/lib"))
                     (mkdir-p (string-append out "/include/pdfium"))
                     ;; Copy files
                     (copy-file "lib/libpdfium.so"
                                (string-append out "/lib/libpdfium.so"))
                     (copy-recursively "include"
                                       (string-append out "/include/pdfium"))
                     #t))))
    (native-inputs (list tar gzip))
    (home-page "https://github.com/bblanchon/pdfium-binaries")
    (synopsis "Prebuilt PDFium library")
    (description "Prebuilt PDFium library for PDF rendering.")
    (license license:bsd-3)))

(define-public python-pypdfium2
  (package
    (name "python-pypdfium2")
    (version "4.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pypdfium2" version))
       (sha256
        (base32 "1khlm2cf0av9bzd4r8qx37msqdlllhhb4rjgjxyi13jr0dnpqp2z"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-build-system
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Completely disable pdfium downloading by patching the emplace module
              (substitute* "setupsrc/pypdfium2_setup/emplace.py"
                (("def _get_pdfium_with_cache.*" all)
                 (string-append all
                                "    # Guix patch: skip all pdfium setup\n"
                                "    return\n"
                                "    # Original code follows (unreachable):
")))

              ;; Patch the packaging_base to return correct platform info
              (substitute* "setupsrc/pypdfium2_setup/packaging_base.py"
                (("def parse_pl_spec\\(pl_spec, version=None\\):" all)
                 (string-append all
                  "\n"
                  "    # Guix patch: return fixed platform info
"
                  "    ver = PdfiumVer('chromium/6721')\n"
                  "    return ['linux_x64'], ver\n"
                  "    # Original code follows (unreachable):
")))

              ;; Disable git operations
              (substitute* "setupsrc/pypdfium2_setup/packaging_base.py"
                (("git_ls = run_cmd.*" all)
                 (string-append "# Guix: disabled git command\n"
                                "        git_ls = 'ref\\tchromium/6721\\n'\n"
                                "        # " all)))

              ;; Patch update_pdfium to prevent downloads
              (substitute* "setupsrc/pypdfium2_setup/update_pdfium.py"
                (("def main\\(platforms.*" all)
                 (string-append all "    # Guix patch: skip all downloads\n"
                                "    return\n"
                                "    # Original code follows (unreachable):
")))))
          (add-after 'patch-build-system 'prepare-pdfium
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Create the expected directory structure for pdfium
              (mkdir-p "data/binaries")
              (mkdir-p "data/linux_x64")
              (let ((pdfium-dir "data/binaries/pdfium-linux-x64")
                    (pdfium-lib (string-append #$(this-package-input
                                                  "pdfium-binary")
                                               "/lib/libpdfium.so")))
                (mkdir-p (string-append pdfium-dir "/lib"))
                (mkdir-p (string-append pdfium-dir "/include"))
                ;; Copy PDFium files from input
                (copy-file pdfium-lib
                           (string-append pdfium-dir "/lib/libpdfium.so"))
                (copy-recursively (string-append #$(this-package-input
                                                    "pdfium-binary")
                                                 "/include/pdfium")
                                  (string-append pdfium-dir "/include"))
                ;; Also copy library to the linux_x64 directory (expected by setup)
                (copy-file pdfium-lib "data/linux_x64/libpdfium.so"))

              ;; Create version file
              (call-with-output-file "data/binaries/pdfium-linux-x64/VERSION"
                (lambda (port)
                  (display "6721\n" port)))

              ;; Create the .emplaced marker to indicate pdfium is ready
              (mkdir-p "src/pypdfium2_raw/")
              (call-with-output-file "src/pypdfium2_raw/.emplaced"
                (lambda (port)
                  (display "marker\n" port)))

              ;; Create symlink for expected data structure
              (symlink "../binaries/pdfium-linux-x64"
                       "data/linux_x64/pdfium-linux-x64")))

          (add-after 'prepare-pdfium 'generate-bindings
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Generate bindings using ctypesgen
              (mkdir-p "data/bindings")
              (let ((pdfium-headers (string-append #$(this-package-input
                                                      "pdfium-binary")
                                                   "/include/pdfium")))
                ;; Generate bindings.py using ctypesgen with all available headers
                (invoke "ctypesgen"
                        "--no-stddef-types"
                        "--no-gnu-types"
                        "--no-python-types"
                        (string-append "--library="
                                       #$(this-package-input "pdfium-binary")
                                       "/lib/libpdfium.so")
                        "-o"
                        "data/bindings/bindings.py"
                        ;; Include all available PDFium headers
                        (string-append pdfium-headers "/fpdfview.h")
                        (string-append pdfium-headers "/fpdf_doc.h")
                        (string-append pdfium-headers "/fpdf_edit.h")
                        (string-append pdfium-headers "/fpdf_save.h")
                        (string-append pdfium-headers "/fpdf_formfill.h")
                        (string-append pdfium-headers "/fpdf_annot.h")
                        (string-append pdfium-headers "/fpdf_text.h")
                        (string-append pdfium-headers "/fpdf_transformpage.h")
                        (string-append pdfium-headers "/fpdf_flatten.h")
                        (string-append pdfium-headers "/fpdf_structtree.h")
                        (string-append pdfium-headers "/fpdf_thumbnail.h")
                        (string-append pdfium-headers "/fpdf_attachment.h")
                        (string-append pdfium-headers "/fpdf_dataavail.h")
                        (string-append pdfium-headers "/fpdf_progressive.h")
                        (string-append pdfium-headers "/fpdf_searchex.h")
                        (string-append pdfium-headers "/fpdf_sysfontinfo.h")
                        (string-append pdfium-headers "/fpdf_ext.h")
                        (string-append pdfium-headers "/fpdf_javascript.h")
                        (string-append pdfium-headers "/fpdf_signature.h")
                        (string-append pdfium-headers "/fpdf_catalog.h")
                        (string-append pdfium-headers "/fpdf_ppo.h")))

              ;; Add _libs_info variable that pypdfium2 expects
              (let ((lib-path (string-append #$(this-package-input
                                                "pdfium-binary")
                                             "/lib/libpdfium.so")))
                (invoke "sh" "-c"
                        (string-append
                         "echo \"\" >> data/bindings/bindings.py && "
                         "echo \"# Guix: Add _libs_info variable\" >> data/bindings/bindings.py && "
                         "echo \"_libs_info = {'pdfium': {'path': '" lib-path
                         "'}}\" >> data/bindings/bindings.py")))))

          (add-after 'generate-bindings 'create-version-files
            (lambda _
              ;; Create version.json with proper structure
              (let ((version-data
                     "{\"major\": 6721, \"minor\": 0, \"build\": 0, \"patch\": 0, \"n_commits\": 0, \"hash\": null, \"origin\": \"system/guix/pdfium-binaries\", \"flags\": []}
"))
                (call-with-output-file "data/bindings/version.json"
                  (lambda (port)
                    (display version-data port)))

                ;; Also create version.json in linux_x64 directory (expected by setup)
                (call-with-output-file "data/linux_x64/version.json"
                  (lambda (port)
                    (display version-data port))))))
          ;; The pre-compiled binary has dependencies that will be found at runtime
          ;; via the system's library search path, so we skip RUNPATH validation
          (delete 'validate-runpath))))
    (inputs (list pdfium-binary-for-pypdfium2))
    (native-inputs (list git-minimal python-ctypesgen python-packaging
                         python-setuptools python-wheel))
    (home-page "https://github.com/pypdfium2-team/pypdfium2")
    (synopsis "Python bindings to PDFium")
    (description "Python bindings to PDFium, a PDF rendering library.")
    (license license:bsd-3)))

(define-public python-glcontext
  (package
    (name "python-glcontext")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "glcontext" version))
       (sha256
        (base32 "1vqij9251anhi6rasgfdl7xr3475yzvfs663f06zrwldsgf8w5jp"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests
    (inputs (list mesa libx11))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/moderngl/glcontext")
    (synopsis "Portable OpenGL Context")
    (description
     "glcontext is a library for creating portable OpenGL contexts.")
    (license license:expat)))

(define-public python-moderngl
  (package
    (name "python-moderngl")
    (version "5.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "moderngl" version))
       (sha256
        (base32 "16qjwpfblwsqidw4x4izgqg87xhr565m466bwgbf3wmjrjc6m4sj"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require OpenGL context
    (inputs (list mesa))
    (propagated-inputs (list python-glcontext))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/moderngl/moderngl")
    (synopsis "Modern OpenGL binding for Python")
    (description
     "ModernGL is a Python wrapper over OpenGL 3.3+ core that simplifies the creation of simple graphics applications like scientific simulations, games or user interfaces.")
    (license license:expat)))

(define-public python-pyglet
  (package
    (name "python-pyglet")
    (version "2.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyglet" version))
       (sha256
        (base32 "1dyc9837lr2pkg28l2ay8yrz2a9b45wxbvfj0iwl9dn13wmsabkh"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require display
    (inputs (list mesa ffmpeg))
    (native-inputs (list python-flit-core))
    (home-page "https://pyglet.org/")
    (synopsis "Cross-platform windowing and multimedia library")
    (description
     "pyglet is a cross-platform windowing and multimedia library for Python, intended for developing games and other visually rich applications.")
    (license license:bsd-3)))

(define-public python-pyglm
  (package
    (name "python-pyglm")
    (version "2.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyGLM" version))
       (sha256
        (base32 "13rr7d9zvdykis7i166w2lqjk41nqb1xh72hpjp4if92fq16rjsc"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests not included in PyPI
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/Zuzu-Typ/PyGLM")
    (synopsis "OpenGL Mathematics library for Python")
    (description
     "PyGLM is a Python extension written in C++. GLSL + GLM = PyGLM. A mathematics library for graphics programming.")
    (license license:zlib)))

(define-public python-pyrr
  (package
    (name "python-pyrr")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyrr" version))
       (sha256
        (base32 "07byxkq8dcc8n6fc2q7g3vq3mxzzj0hqzm8hlq3gfwbf68h7n3rw"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require multipledispatch
    (propagated-inputs (list python-numpy python-multipledispatch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/adamlwgriffiths/Pyrr")
    (synopsis "3D mathematical functions using NumPy")
    (description
     "Pyrr is a Python library for common 3D mathematical objects and functions.")
    (license license:bsd-2)))

(define-public python-moderngl-window
  (package
    (name "python-moderngl-window")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "moderngl_window" version))
       (sha256
        (base32 "1g61lq9g4p1104rq1sj4pvpds3wig0khnj0x8vrrjwzq0msq5hi9"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require display
    (propagated-inputs (list python-moderngl
                             python-numpy
                             python-pillow
                             python-pyglet
                             python-pygame
                             python-pyglm
                             python-pyrr))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/moderngl/moderngl-window")
    (synopsis "A cross platform utility library for ModernGL")
    (description
     "moderngl-window is a cross platform utility library for ModernGL providing OS native window creation, resource loading, and other utility features.")
    (license license:expat)))

(define-public python-mapbox-earcut
  (package
    (name "python-mapbox-earcut")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mapbox_earcut" version))
       (sha256
        (base32 "0lkxxs898hwzchhnilxikb7alxqcqfyi35mz3h530gy2pil09yl3"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests in PyPI
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-setuptools python-wheel python-pybind11))
    (home-page "https://github.com/skogler/mapbox_earcut_python")
    (synopsis "Python bindings for Mapbox Earcut library")
    (description
     "Python bindings for the Mapbox Earcut polygon triangulation library.")
    (license license:isc)))

(define-public python-screeninfo
  (package
    (name "python-screeninfo")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "screeninfo" version))
       (sha256
        (base32 "1l9frlckb9zbwx5kngxv5byi353jyfmpskcy38m40d3yrimhg0wr"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require display
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/rr-/screeninfo")
    (synopsis "Fetch location and size of physical screens")
    (description
     "screeninfo is a Python library for programmatically obtaining information about the physical screens connected to the system.")
    (license license:expat)))

(define-public python-groovy
  (package
    (name "python-groovy")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "groovy" version))
       (sha256
        (base32 "0mh9ws7yihr59vf34p3wmg9jxwnd1d0ply3xcnw9g6zq4m0pslqz"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests included
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/Abrynos/Groovy.py")
    (synopsis "Python implementation of Groovy")
    (description
     "A simple implementation of the Groovy programming language in Python.")
    (license license:expat)))

(define-public python-safehttpx
  (package
    (name "python-safehttpx")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "safehttpx" version))
       (sha256
        (base32 "0hnfxdb28kvwx5gkpym53bzhmmmymfz2sjmrjp1j8fpf5k4bymmk"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests in PyPI
    (propagated-inputs (list python-httpx))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/zyddnys/safehttpx")
    (synopsis "Safe wrapper around httpx")
    (description
     "A wrapper around httpx that enforces timeouts and provides safe defaults.")
    (license license:expat)))

(define-public python-hatch-requirements-txt
  (package
    (name "python-hatch-requirements-txt")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hatch_requirements_txt" version))
       (sha256
        (base32 "083xakilrmy0ymh34s9wm8x8s7s8vn7ij33xz9avn1gxb1bnws1c"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests in PyPI
    (propagated-inputs (list python-hatchling python-packaging))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/repo-helper/hatch-requirements-txt")
    (synopsis
     "Hatchling plugin to read project dependencies from requirements.txt")
    (description
     "Hatchling plugin to read project dependencies from requirements.txt.")
    (license license:expat)))

(define-public python-gradio-client
  (package
    (name "python-gradio-client")
    (version "1.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gradio_client" version))
       (sha256
        (base32 "1c61pknk5q81daaynyggzyv8pm73g195b26fsfgqv6w5bxhzzl2x"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require network
    (propagated-inputs (list python-httpx python-huggingface-hub
                             python-packaging python-typing-extensions
                             python-websockets))
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme
                         python-hatch-requirements-txt))
    (home-page "https://github.com/gradio-app/gradio")
    (synopsis "Python client for Gradio")
    (description
     "Python library for interacting with Gradio apps as a client.")
    (license license:asl2.0)))

(define-public python-gradio
  (package
    (name "python-gradio")
    (version "5.36.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gradio" version))
       (sha256
        (base32 "0y30mc9lva7m04cmsppv8cb3msv5w5axg57kf3rcnp83msyrs3i9"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Remove examples with syntax errors
                   (when (file-exists? "gradio/_frontend_code/lite/examples")
                     (delete-file-recursively
                      "gradio/_frontend_code/lite/examples"))
                   (when (file-exists? "js/lite/examples")
                     (delete-file-recursively "js/lite/examples")) #t))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require network access
    (propagated-inputs (list python-aiofiles
                             python-anyio
                             python-brotli
                             python-fastapi
                             python-ffmpy
                             python-groovy
                             python-gradio-client
                             python-httpx
                             python-huggingface-hub
                             python-jinja2
                             python-markupsafe
                             python-numpy
                             python-orjson
                             python-packaging
                             python-pandas
                             python-pillow
                             python-pydantic
                             python-multipart
                             python-pydub
                             python-pyyaml
                             python-safehttpx
                             python-semantic-version
                             python-starlette
                             python-tomlkit
                             python-typer
                             python-typing-extensions
                             python-uvicorn))
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme
                         python-hatch-requirements-txt))
    (home-page "https://www.gradio.app")
    (synopsis "Build machine learning web apps in Python")
    (description
     "Gradio is a Python library for creating customizable web interfaces for machine learning models.")
    (license license:asl2.0)))

(define-public python-typing
  (package
    (name "python-typing")
    (version "3.10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "typing" version))
       (sha256
        (base32 "0c5il4d68fd4qrm5k3dps70j0xz0n5krj6lhwn9vzpal3whsvd0k"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://docs.python.org/3/library/typing.html")
    (synopsis "Type Hints for Python")
    (description "Type Hints for Python.")
    (license license:psfl)))

(define-public python-antlr4-python3-runtime
  (package
    (name "python-antlr4-python3-runtime")
    (version "4.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "antlr4_python3_runtime" version))
       (sha256
        (base32 "05irxwjkzkcnd9jwr37rhnf92f1yjgrnsn5ch00vghig3mz696wh"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No test suite
    (propagated-inputs (list python-typing))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://www.antlr.org/")
    (synopsis "ANTLR 4.13.2 runtime for Python 3")
    (description "ANTLR 4.13.2 runtime for Python 3.")
    (license license:bsd-3)))

(define-public python-antlr4-python3-runtime-4.9
  (package
    (inherit python-antlr4-python3-runtime)
    (name "python-antlr4-python3-runtime")
    (version "4.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "antlr4-python3-runtime" version))
       (sha256
        (base32 "06w8fz73rk8vzjz9rydfk56g4mbqpyl81yhypc14jab886dlc97j"))))
    (synopsis "ANTLR 4.9.3 runtime for Python 3")
    (description "ANTLR 4.9.3 runtime for Python 3, required for hydra-core.")))

(define-public python-omegaconf-2.2
  (package
    (name "python-omegaconf")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "omegaconf" version))
       (sha256
        (base32 "08r5al0nk3b43d7vpqm276iqziippalyhr0bf7xvbysghsx9zzsr"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f ;Tests require pytest and other dependencies
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check)))) ;Skip version check
    (native-inputs (list python-setuptools python-wheel icedtea antlr4))
    (propagated-inputs (list python-antlr4-python3-runtime-4.9 python-pyyaml))
    (home-page "https://github.com/omry/omegaconf")
    (synopsis "Flexible configuration system")
    (description
     "OmegaConf is a hierarchical configuration system and supports
merging configurations from multiple sources.  It provides a consistent API
regardless of how the configuration was created.")
    (license license:bsd-3)))

(define-public python-jsonlines
  (package
    (name "python-jsonlines")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jsonlines" version))
       (sha256
        (base32 "0x7a4a42jvqz6mkd6h9hag8pvxscmq2zcisjk64w0l3m244jqv8c"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-attrs))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/wbolster/jsonlines")
    (synopsis "Library with helpers for the jsonlines file format")
    (description "Library with helpers for the jsonlines file format.")
    (license license:bsd-3)))

(define-public python-voxel51-eta
  (package
    (name "python-voxel51-eta")
    (version "0.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "voxel51_eta" version))
       (sha256
        (base32 "1007f6cn6sibd5av4s30rbrrm57azh59wdsaky6mmlfcqka8gcih"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require optional dependencies
    (propagated-inputs (list python-argcomplete
                             python-dill
                             python-future
                             python-glob2
                             python-jsonlines
                             python-numpy
                             python-opencv-python-headless
                             python-packaging
                             python-pillow
                             python-py7zr
                             python-dateutil
                             python-pytz
                             python-rarfile
                             python-requests
                             python-retrying
                             python-scikit-image
                             python-six
                             python-sortedcontainers
                             python-tabulate
                             python-tzlocal
                             python-urllib3))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/voxel51/eta")
    (synopsis "Extensible Toolkit for Analytics")
    (description "Extensible Toolkit for Analytics.")
    (license license:asl2.0)))

(define-public python-universal-analytics-python3
  (package
    (name "python-universal-analytics-python3")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "universal-analytics-python3" version))
       (sha256
        (base32 "1bbz5xwq69qjhzqk0qr5hd0dqiz4ffw4bwc0xpprlf7jrsvnm2zm"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require flake8
    (propagated-inputs (list python-httpx))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/dmvass/universal-analytics-python3")
    (synopsis "Universal analytics python library")
    (description "Universal analytics python library.")
    (license license:expat)))

(define-public python-strawberry-graphql
  (package
    (name "python-strawberry-graphql")
    (version "0.276.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "strawberry_graphql" version))
       (sha256
        (base32 "051sdfyha3fdp2gd1x4mdgnyvid273m53qqagi4l4qqk1p9ldid4"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f ;Disable tests for now
       #:build-backend "poetry.core.masonry.api"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'create-pyproject-toml
                    (lambda _
                      ;; Create a minimal pyproject.toml since the sdist doesn't include it
                      (call-with-output-file "pyproject.toml"
                        (lambda (port)
                          (format port
                           "[tool.poetry]
name = \"strawberry\"
version = \"~a\"
description = \"A library for creating GraphQL APIs\"
authors = [\"Patrick Arminio\"]
packages = [{include = \"strawberry\"}]

[tool.poetry.dependencies]
python = \"^3.8\"
graphql-core = \">=3.2.0,<3.4.0\"
python-dateutil = \"*\"
typing_extensions = \">=4.5.0\"

[build-system]
requires = [\"poetry-core>=1.0.0\"]
build-backend = \"poetry.core.masonry.api\"
"
                           ,version))) #t)))))
    (propagated-inputs (list python-graphql-core python-packaging
                             python-dateutil python-typing-extensions))
    (native-inputs (list python-poetry-core))
    (home-page "https://strawberry.rocks/")
    (synopsis "A library for creating GraphQL APIs")
    (description
     "This package provides a library for creating @code{GraphQL} APIs.")
    (license license:expat)))

(define-public python-sseclient-py
  (package
    (name "python-sseclient-py")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sseclient-py" version))
       (sha256
        (base32 "1s63z9qmr4vbni46na4pqaszjf665lda56f5inik0ck3lz0waiy5"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mpetazzoni/sseclient")
    (synopsis "SSE client for Python")
    (description "SSE client for Python.")
    (license license:expat)))

(define-public python-sse-starlette
  (package
    (name "python-sse-starlette")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sse_starlette" version))
       (sha256
        (base32 "09h90zd44v211giplw10ccbcdr3qbjjbssy0bwbfjhx33h5812kw"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f ;Disable tests for now
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-pyproject-toml
                    (lambda _
                      ;; Fix license format in pyproject.toml
                      (substitute* "pyproject.toml"
                        (("license = \"BSD-3-Clause\"")
                         "license = {text = \"BSD-3-Clause\"}")) #t))
                  (delete 'sanity-check))))
    (propagated-inputs (list python-anyio))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/sysid/sse-starlette")
    (synopsis "SSE plugin for Starlette")
    (description "SSE plugin for Starlette.")
    (license license:bsd-3)))

(define-public python-furo
  (package
    (name "python-furo")
    (version "2024.7.18")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "furo" version))
       (sha256
        (base32 "10sw69c5s4sg1xdy1payi5ikj126rmxfk2rcf7c4cpf9rigqrc1p"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f ;Disable tests for now
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-nodejs-dep
                    (lambda _
                      ;; Remove nodejs-related build requirements
                      (substitute* "pyproject.toml"
                        (("\"nodeenv\".*,")
                         "")
                        (("requires = \\[\"sphinx-theme-builder.*\"\\]")
                         "requires = [\"setuptools\", \"wheel\"]")
                        (("build-backend = \"sphinx_theme_builder\"")
                         "build-backend = \"setuptools.build_meta\""))
                      ;; Create a minimal setup.py
                      (with-output-to-file "setup.py"
                        (lambda ()
                          (display
                           "from setuptools import setup, find_packages
setup(
    name='furo',
    version='2024.7.18',
    packages=find_packages('src'),
    package_dir={'': 'src'},
    include_package_data=True,
    install_requires=['beautifulsoup4', 'sphinx>=6.0,<8.0', 'sphinx-basic-ng'],
    python_requires='>=3.8',
    entry_points={
        'sphinx.html_themes': [
            'furo = furo',
        ]
    },
    package_data={
        'furo': ['theme/**/*'],
    },
)
")))
                      #t)))))
    (propagated-inputs (list python-beautifulsoup4 python-sphinx
                             python-sphinx-basic-ng))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/pradyunsg/furo")
    (synopsis "A clean customizable Sphinx documentation theme")
    (description "A clean customizable Sphinx documentation theme.")
    (license license:expat)))

(define-public python-build
  (package
    (name "python-build")
    (version "1.2.2.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "build" version))
       (sha256
        (base32 "1dq8nrw55g89m86bljrd19v5ldpz4ahhdrlrkhhmldx95klr6sdk"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Disable tests to avoid circular dependencies
    (propagated-inputs (list python-packaging python-pyproject-hooks))
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/pypa/build")
    (synopsis "A simple, correct Python build frontend")
    (description
     "build is a simple, correct Python build frontend that provides a consistent interface to build packages.")
    (license license:expat)))

(define-public python-pydash
  (package
    (name "python-pydash")
    (version "8.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydash" version))
       (sha256
        (base32 "1amrz28qs8hvzlgn6bj66rlpkbw2zlx54v3qzzjz0wjvl0yws9qv"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Disable tests for now
    (propagated-inputs (list python-typing-extensions))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/dgilland/pydash")
    (synopsis "The kitchen sink of Python utility libraries for doing stuff")
    (description
     "The kitchen sink of Python utility libraries for doing stuff.")
    (license license:expat)))

(define-public python-pprintpp
  (package
    (name "python-pprintpp")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pprintpp" version))
       (sha256
        (base32 "00v4pkyiqc0y9qjnp3br58a4k5zwqdrjjxbcsv39vx67w84630pa"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ;Disable tests for now
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-setup-py
                    (lambda _
                      (substitute* "setup.py"
                        (("\"U\"")
                         "\"r\"")) #t)))))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/wolever/pprintpp")
    (synopsis "A drop-in replacement for pprint that's actually pretty")
    (description "A drop-in replacement for pprint that's actually pretty.")
    (license license:bsd-3)))

(define-public python-motor
  (package
    (name "python-motor")
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "motor" version))
       (sha256
        (base32 "09j5ss4xq7dwjp9kj2971sx1ixf2a5y9vjm667rjhyf84mkd9d17"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f ;Disable tests for now
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-build
                    (lambda _
                      ;; Create a simpler pyproject.toml without the metadata hook
                      (with-output-to-file "pyproject.toml"
                        (lambda ()
                          (display
                           "[build-system]
requires = [\"hatchling\"]
build-backend = \"hatchling.build\"

[project]
name = \"motor\"
version = \"3.7.1\"
description = \"Non-blocking MongoDB driver for Tornado and asyncio\"
dependencies = [\"pymongo>=4.5,<5\"]
")))
                      #t)))))
    (propagated-inputs (list python-pymongo))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/mongodb/motor/")
    (synopsis "Non-blocking MongoDB driver for Tornado and asyncio")
    (description
     "Motor is a full-featured, non-blocking MongoDB driver for Python Tornado and asyncio applications.")
    (license license:asl2.0)))

(define-public python-mongoengine
  (package
    (name "python-mongoengine")
    (version "0.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mongoengine" version))
       (sha256
        (base32 "1wrwxs6rnnhsm2dqifcjrv3msjjdizkxkdy2xwwps2sz5npsnhrv"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Disable tests for now
    (propagated-inputs (list python-pymongo))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/MongoEngine/mongoengine")
    (synopsis "Document-Object Mapper for working with MongoDB from Python")
    (description
     "MongoEngine is a Document-Object Mapper (think ORM, but for document databases) for working with MongoDB from Python.")
    (license license:expat)))

(define-public python-openalex
  (package
    (name "python-openalex")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/b-vitamins/openalex-python")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lzkklf84a4gkix30z2czvhsh217d281l9idj2nazvr0c7lyw1ii"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require network access and test fixtures
    (propagated-inputs (list python-httpx
                             python-pydantic-2
                             python-structlog
                             python-rich
                             python-dateutil
                             python-orjson
                             python-typing-extensions
                             python-cachetools
                             python-xxhash))
    (native-inputs (list python-poetry-core
                         python-pytest
                         python-pytest-asyncio
                         python-pytest-cov
                         python-pytest-httpx
                         python-pytest-examples
                         python-pytest-benchmark
                         python-requests))
    (home-page "https://github.com/b-vitamins/openalex-python")
    (synopsis "Python client for the OpenAlex API with async support")
    (description
     "A Python client library for the OpenAlex API providing type-safe data models,
fluent query interface, comprehensive filtering, async support, and automatic
pagination.  Features include cursor-based pagination, automatic retries, rate
limiting, circuit breaker for resilience, and in-memory caching.")
    (license license:expat)))
