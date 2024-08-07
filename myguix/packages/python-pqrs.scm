(define-module (myguix packages python-pqrs)
  #:use-module (myguix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages machine-learning)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (myguix packages video))

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
    (native-inputs (list python-pytest python-pytest-xdist))
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

(define-public python-bristol
  (package
    (name "python-bristol")
    (version "0.2.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bristol" version))
       (sha256
        (base32 "1l8k6ndv9hssxkl0f4ylzidm9s59r1h8d3fiaa8dqwjnx7c8pa01"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-pytorch python-torchvision))
    (home-page "https://github.com/msuzen/bristol")
    (synopsis
     "Parallel random matrix tools and random matrix theory deep learning applications. Generate matrices from Circular Unitary Ensemble (CUE), Circular Ortogonal Ensemble (COE) and Circular Symplectic Ensemble (CSE). Additional spectral analysis utilities are also implemented, such as computation of spectral density and spectral ergodicity for complexity of deep learning architectures.")
    (description
     "Parallel random matrix tools and random matrix theory deep learning
applications.  Generate matrices from Circular Unitary Ensemble (CUE), Circular
Ortogonal Ensemble (COE) and Circular Symplectic Ensemble (CSE).  Additional
spectral analysis utilities are also implemented, such as computation of
spectral density and spectral ergodicity for complexity of deep learning
architectures.")
    (license license:asl2.0)))

(define-public python-pele
  (package
    (name "python-pele")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pele-python/pele")
             (commit "7da72ba")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b8s182w3i6dzjgwzgj9pb927fd3yqypma9hys86r4c7ij4kbmpb"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (invoke "python" "setup.py" "build")))
                  (replace 'install
                    (lambda _
                      (invoke "python" "setup.py" "install"))))))
    (inputs (list python
                  gfortran-toolchain
                  python-numpy
                  python-scipy
                  python-cython
                  python-networkx
                  python-matplotlib
                  python-pyro4
                  python-sqlalchemy))
    (propagated-inputs (list python-decorator))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/pele-python/")
    (synopsis "Energy landscapes.")
    (description "TBA.")
    (license license:bsd-3)))

(define-public python-attrs
  (package
    (name "python-attrs")
    (version "23.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "attrs" version))
       (sha256
        (base32 "0c0zjwcqzbmpl93izm2g37gc3lsbbb9pf275fv7zcqn256sw6pck"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme
                         python-hatch-vcs))
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/python-attrs/attrs/")
    (synopsis "Attributes without boilerplate")
    (description
     "@code{attrs} is a Python package with class decorators that
     ease the chores of implementing the most common attribute-related object
     protocols.")
    (license license:expat)))

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

(define-public python-jedi
  (package
    (name "python-jedi")
    (version "0.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/davidhalter/jedi")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lpvxa16zyhg95s8ji3sm19qz3bawal172xwlzcl5h80mhhfagih"))
       (modules '((guix build utils)))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (setenv "HOME" "/tmp")
                        (invoke "python" "-m" "pytest" "-vv")))))))
    (native-inputs (list python-colorama python-docopt python-pytest))
    (propagated-inputs (list python-parso))
    (home-page "https://github.com/davidhalter/jedi")
    (synopsis "Autocompletion and static analysis library for Python")
    (description
     "Jedi is a static analysis tool for Python that can be used in Integrated
Development Environments (@dfn{IDE}s) and text editors.  It understands Python
on a deeper level than many other static analysis frameworks for Python.

Jedi understands docstrings and you can use Jedi autocompletion in your REPL
as well.")
    (license license:expat)))

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

(define-public python-pymupdfb
  (package
    (name "python-pymupdfb")
    (version "1.24.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyMuPDFb" version))
       (sha256
        (base32 "0b24dxj5fanxwasab8v2y21dq2m7viymd25v3pqf0q6i64qdmibw"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/pymupdf/pymupdf")
    (synopsis "MuPDF shared libraries for PyMuPDF.")
    (description "@code{MuPDF} shared libraries for @code{PyMuPDF}.")
    (license license:gpl3+)))

(define-public python-pymupdf
  (package
    (name "python-pymupdf")
    (version "1.24.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyMuPDF" version))
       (sha256
        (base32 "0lcmljpasq57ay0jfvnijy2xqxql0fvysr4vzk91az7s8pv2shdg"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pymupdfb))
    (home-page "https://github.com/pymupdf/pymupdf")
    (synopsis
     "A high performance Python library for data extraction, analysis, conversion & manipulation of PDF (and other) documents.")
    (description
     "This package provides a high performance Python library for data extraction,
analysis, conversion & manipulation of PDF (and other) documents.")
    (license license:gpl3+)))

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
    (native-inputs (list python-pytest))
    (propagated-inputs (list python-importlib-metadata
                             python-importlib-resources))
    (home-page "")
    (synopsis "Up-to-date simple useragent faker with real world database")
    (description "Up-to-date simple useragent faker with real world database")
    (license license:asl2.0)))

(define-public python-scholarly
  (package
    (name "python-scholarly")
    (version "1.7.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scholarly-python-package/scholarly")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pzgqc1c6sd06bqlrddk425v2vlvx80fzjp1n7q2iha6ggmv1xya"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-arrow
                             python-beautifulsoup4
                             python-bibtexparser
                             python-deprecated
                             python-fake-useragent
                             python-free-proxy
                             python-httpx
                             python-dotenv
                             python-requests
                             python-selenium
                             python-sphinx-rtd-theme
                             python-typing-extensions))
    (home-page "https://github.com/scholarly-python-package/scholarly")
    (synopsis "Simple access to Google Scholar authors and citations")
    (description "Simple access to Google Scholar authors and citations")
    (license #f)))

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
    (home-page "https://github.com/chardet/chardet")
    (synopsis "Universal encoding detector for Python 3")
    (description "Universal encoding detector for Python 3.")
    (license #f)))

(define-public python-idna
  (package
    (name "python-idna")
    (version "2.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "idna" version))
       (sha256
        (base32 "01rlkigdxg17sf9yar1jl8n18ls59367wqh59hnawlyg53vb6my3"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/kjd/idna")
    (synopsis "Internationalized Domain Names in Applications (IDNA)")
    (description "Internationalized Domain Names in Applications (IDNA).")
    (license #f)))

(define-public python-urllib3
  (package
    (name "python-urllib3")
    (version "1.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "urllib3" version))
       (sha256
        (base32 "1nq2k4pss1ihsjh02r41sqpjpm5rfqkjfysyq7g7n2i1p7c66c55"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/urllib3")
    (synopsis
     "HTTP library with thread-safe connection pooling, file post, and more.")
    (description
     "HTTP library with thread-safe connection pooling, file post, and more.")
    (license #f)))

(define-public python-requests
  (package
    (name "python-requests")
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests" version))
       (sha256
        (base32 "1d5ybh11jr5sm7xp6mz8fyc7vrp4syifds91m7sj60xalal0gq0i"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-certifi python-chardet python-idna
                             python-urllib3))
    (home-page "https://requests.readthedocs.io")
    (synopsis "Python HTTP for Humans.")
    (description "Python HTTP for Humans.")
    (license #f)))

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
    (native-inputs (list python-bump2version python-pytest python-pytest-cov
                         python-sphinx python-tox))
    (home-page "https://github.com/tantale/deprecated")
    (synopsis
     "Python @deprecated decorator to deprecate old python classes, functions or methods.")
    (description
     "Python @@deprecated decorator to deprecate old python classes, functions or
methods.")
    (license license:expat)))

(define-public python-tld
  (package
    (name "python-tld")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tld" version))
       (sha256
        (base32 "0aanzdsf10vlbp076nr3mgs2379p0q3l9bkfjx21ipsbq3hybpck"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/barseghyanartur/tld")
    (synopsis "Extract the top-level domain (TLD) from the URL given.")
    (description "Extract the top-level domain (TLD) from the URL given.")
    (license #f)))

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
    (home-page "https://python-future.org")
    (synopsis "Clean single-source support for Python 3 and 2")
    (description "Clean single-source support for Python 3 and 2.")
    (license license:expat)))

(define-public python-openreview-py
  (package
    (name "python-openreview-py")
    (version "1.40.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openreview/openreview-py")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rfk9pvznxmplnabjhzr2380j05bdf1298swk4xgag575iv8dhvw"))
       (snippet #~(begin
                    (use-modules (guix build utils))
                    (substitute* "setup.py"
                      (("setuptools==65.5.1")
                       "setuptools"))))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'compile-bytecode))))
    (inputs (list python-future python-requests))
    (propagated-inputs (list python-pycryptodome
                             python-tld
                             python-deprecated
                             python-tqdm
                             python-pylatexenc
                             python-pyjwt))
    (home-page "https://github.com/theblackcat102/openreview_api_py")
    (synopsis "Openreview web API client")
    (description "Openreview web API client.")
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
    (native-inputs (list python-pytest))
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

(define-public python-openai
  (package
    (name "python-openai")
    (version "1.35.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openai" version))
       (sha256
        (base32 "02gb9cp4gmk9nrl8rnf6pdb5ccj2s4v5krbvv1jfziy90hazm6q0"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme
                         python-hatch-vcs))
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-anyio
                             python-cached-property
                             python-distro
                             python-httpx
                             python-pydantic
                             python-sniffio
                             python-tqdm
                             python-typing-extensions))
    (home-page "https://openai.com")
    (synopsis "The official Python library for the openai API")
    (description "The official Python library for the openai API.")
    (license #f)))

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

(define-public python-peachpy-f45429
  (let ((commit "f45429b087dd7d5bc78bb40dc7cf06425c252d67")
        (version "0.2.0")
        (revision "2"))
    (package
      (inherit python-peachpy)
      (name "python-peachpy")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/PeachPy")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03apb6kwb2zgh8rsqmys51zznpnwxwkfn19rq84xnglrwff5ya3v"))
         (patches (search-patches "python-peachpy-determinism-rev2.patch")))))))
