(define-module (myguix packages go-pqrs)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-xyz)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp))

;;;
;;; Go packages needed for Ollama v0.9.1 packaging
;;;
;;; This file contains stubs and documentation for all Go dependencies
;;; required to build Ollama from source in Guix. Based on analysis of
;;; Ollama v0.9.1's go.mod and go.sum files.
;;;
;;; Total estimated packages needed: 25-40
;;; Packaging priority: High → Medium → Low
;;; Some packages may already exist in Guix - check before implementing.
;;;

;;; ============================================================================
;;; HIGH PRIORITY - Core AI/ML Libraries (Phase 1: 5-8 packages)
;;; ============================================================================
;;; These are specialized AI/ML libraries that are definitely not in Guix
;;; and are essential for Ollama's core functionality.

;; TODO: Package github.com/x448/float16@v0.8.4
;; Float16 (half precision) floating point arithmetic
;; Critical for AI model inference performance
;; License: MIT
;; (define-public go-github-com-x448-float16 ...)

;; TODO: Package github.com/d4l3k/go-bfloat16@v0.0.0-20211005043715-690c3bdd05f1
;; Brain float16 format support for Google's ML frameworks
;; Uses commit hash - version: 690c3bdd05f1
;; License: MIT
;; (define-public go-github-com-d4l3k-go-bfloat16 ...)

;; TODO: Package github.com/nlpodyssey/gopickle@v0.3.0
;; Python pickle format reader/writer for Go
;; Essential for loading PyTorch/Python ML models
;; License: BSD-2-Clause
;; (define-public go-github-com-nlpodyssey-gopickle ...)

;; TODO: Package github.com/pdevine/tensor@v0.0.0-20240510204454-f88f4562727c
;; Tensor operations library for Go
;; Uses commit hash - version: f88f4562727c
;; License: Apache-2.0
;; (define-public go-github-com-pdevine-tensor ...)

;; TODO: Package gonum.org/v1/gonum@v0.15.1
;; Comprehensive numerical library for Go
;; May already exist in Guix - CHECK FIRST
;; License: BSD-3-Clause
;; (define-public go-gonum-org-v1-gonum ...)

;; TODO: Package gorgonia.org/vecf32@v0.9.0
;; 32-bit float vector operations
;; License: Apache-2.0
;; (define-public go-gorgonia-org-vecf32 ...)

;; TODO: Package gorgonia.org/vecf64@v0.9.0
;; 64-bit float vector operations
;; License: Apache-2.0
;; (define-public go-gorgonia-org-vecf64 ...)

;;; ============================================================================
;;; MEDIUM PRIORITY - Data Processing & Performance (Phase 2: 8-12 packages)
;;; ============================================================================

;; TODO: Package github.com/google/flatbuffers@v24.3.25+incompatible
;; FlatBuffers serialization library
;; Note: Incompatible version tag - may need special handling
;; License: Apache-2.0
;; (define-public go-github-com-google-flatbuffers ...)

;; TODO: Package github.com/apache/arrow/go/arrow@v0.0.0-20211112161151-bc219186db40
;; Apache Arrow columnar memory format
;; Uses commit hash - version: bc219186db40
;; License: Apache-2.0
;; (define-public go-github-com-apache-arrow-go-arrow ...)

;; TODO: Package github.com/bytedance/sonic@v1.11.6
;; High-performance JSON serialization library
;; License: Apache-2.0
;; (define-public go-github-com-bytedance-sonic ...)

;; TODO: Package github.com/cloudwego/base64x@v0.1.4
;; High-performance base64 encoding/decoding
;; License: Apache-2.0
;; (define-public go-github-com-cloudwego-base64x ...)

;; TODO: Package github.com/cloudwego/iasm@v0.2.0
;; Assembly code generation library
;; License: Apache-2.0
;; (define-public go-github-com-cloudwego-iasm ...)

;; TODO: Package github.com/klauspost/cpuid/v2@v2.2.7
;; CPU feature detection library
;; May already exist in Guix - CHECK FIRST
;; License: MIT
;; (define-public go-github-com-klauspost-cpuid-v2 ...)

;; TODO: Package github.com/twitchyliquid64/golang-asm@v0.15.1
;; Runtime assembly code generation
;; License: MIT
;; (define-public go-github-com-twitchyliquid64-golang-asm ...)

;;; ============================================================================
;;; MEDIUM PRIORITY - Web Framework & CLI (Phase 2 continued)
;;; ============================================================================

;; TODO: Check if github.com/gin-gonic/gin@v1.10.0 exists in Guix
;; HTTP web framework (very popular - likely already packaged)
;; License: MIT
;; (define-public go-github-com-gin-gonic-gin ...)

;; TODO: Package github.com/gin-contrib/cors@v1.7.2
;; CORS middleware for Gin framework
;; License: MIT
;; (define-public go-github-com-gin-contrib-cors ...)

;; TODO: Package github.com/gabriel-vasile/mimetype@v1.4.3
;; MIME type detection library
;; License: MIT
;; (define-public go-github-com-gabriel-vasile-mimetype ...)

;; TODO: Package github.com/go-playground/validator/v10@v10.20.0
;; Struct and field validation library
;; May already exist in Guix - CHECK FIRST
;; License: MIT
;; (define-public go-github-com-go-playground-validator-v10 ...)

;; TODO: Check if github.com/spf13/cobra@v1.7.0 exists in Guix
;; CLI application framework (very popular - likely already packaged)
;; License: Apache-2.0
;; (define-public go-github-com-spf13-cobra ...)

;; TODO: Package github.com/containerd/console@v1.0.4
;; Console utility functions
;; May already exist as part of containerd packages
;; License: Apache-2.0
;; (define-public go-github-com-containerd-console ...)

;; TODO: Package github.com/agnivade/levenshtein@v1.1.1
;; Levenshtein distance calculation
;; License: MIT
;; (define-public go-github-com-agnivade-levenshtein ...)

;;; ============================================================================
;;; MEDIUM PRIORITY - JSON & Data Processing
;;; ============================================================================

;; TODO: Package github.com/goccy/go-json@v0.10.2
;; High-performance JSON library
;; License: MIT
;; (define-public go-github-com-goccy-go-json ...)

;; TODO: Check if github.com/pelletier/go-toml/v2@v2.2.2 exists in Guix
;; TOML parser (common library - may already exist)
;; License: Apache-2.0
;; (define-public go-github-com-pelletier-go-toml-v2 ...)

;; TODO: Package github.com/ugorji/go/codec@v1.2.12
;; High-performance serialization codecs
;; License: MIT
;; (define-public go-github-com-ugorji-go-codec ...)

;;; ============================================================================
;;; LOW PRIORITY - Standard Extensions (Phase 3: Check Guix first)
;;; ============================================================================
;;; These are likely already available in Guix golang packages

;; TODO: Verify golang.org/x/crypto@v0.29.0 exists and is recent enough
;; Extended cryptography library (standard - should exist)

;; TODO: Verify golang.org/x/image@v0.22.0 exists and is recent enough
;; Extended image processing library (standard - should exist)

;; TODO: Verify golang.org/x/sys@v0.27.0 exists and is recent enough
;; Extended system call library (standard - should exist)

;; TODO: Verify golang.org/x/net@v0.31.0 exists and is recent enough
;; Extended networking library (standard - should exist)

;; TODO: Verify golang.org/x/text@v0.20.0 exists and is recent enough
;; Extended text processing library (standard - should exist)

;; TODO: Verify golang.org/x/exp@v0.0.0-20250218142911-aa4b98e5adaa exists
;; Experimental Go features
;; Uses commit hash - version: aa4b98e5adaa

;;; ============================================================================
;;; LOW PRIORITY - Common Utilities (Phase 3: Likely available)
;;; ============================================================================

;; TODO: Check if google.golang.org/protobuf@v1.34.1 exists in Guix
;; Protocol Buffers library (very common - likely exists)

;; TODO: Check if github.com/google/uuid@v1.6.0 exists in Guix
;; UUID generation (very common - likely exists)

;; TODO: Check if github.com/mattn/go-isatty@v0.0.20 exists in Guix
;; TTY detection utility (common - likely exists)

;; TODO: Check if github.com/stretchr/testify@v1.9.0 exists in Guix
;; Testing framework (very popular - likely exists)

;; TODO: Check if gopkg.in/yaml.v3@v3.0.1 exists in Guix
;; YAML parser (very common - likely exists)

;;; ============================================================================
;;; ADDITIONAL DEPENDENCIES (Lower priority, specialized)
;;; ============================================================================

;; TODO: Package github.com/chewxy/hm@v1.0.0 (if needed)
;; Hindley-Milner type system for Go

;; TODO: Package github.com/chewxy/math32@v1.10.1 (if needed)
;; 32-bit math functions

;; TODO: Package github.com/pkg/errors@v0.9.1 (if needed)
;; Error handling utilities (may already exist)

;; TODO: Package gorgonia.org/tensor@v0.9.24 (if needed)
;; Tensor library for Gorgonia

;;; ============================================================================
;;; PACKAGING NOTES & STRATEGY
;;; ============================================================================

;;; PHASE 1 IMPLEMENTATION ORDER:
;;; 1. go-github-com-x448-float16 (simplest, pure Go)
;;; 2. go-github-com-d4l3k-go-bfloat16 (simple, commit version)
;;; 3. go-gonum-org-v1-gonum (check if exists first, large but standard)
;;; 4. go-github-com-nlpodyssey-gopickle (medium complexity)
;;; 5. go-github-com-pdevine-tensor (complex, may depend on others)

;;; SPECIAL CONSIDERATIONS:
;;; - Many packages use commit hashes instead of semantic versions
;;; - Performance libraries may require specific Go build tags
;;; - ML libraries may have native dependencies (check carefully)
;;; - Some packages marked as "incompatible" may need special handling
;;; - Test with Go 1.24.3+ as Ollama requires this minimum version

;;; TESTING STRATEGY:
;;; - Build each package individually first
;;; - Test with simple import verification
;;; - Build intermediate test programs before full Ollama build
;;; - Use guix environment for development and testing

;;; VERSION HANDLING:
;;; - Use git-reference for commit-based versions
;;; - Document exact commit hashes from go.sum verification
;;; - Handle +incompatible versions according to Go modules spec
;;; - Verify version constraints match Ollama's requirements

;; This file serves as a roadmap for the Go packaging effort.
;; Update package definitions here as they are implemented.
;; Mark completed packages with actual definitions below.

;;; ============================================================================
;;; IMPLEMENTED PACKAGES (Add actual package definitions here)
;;; ============================================================================

(define-public go-github-com-xtgo-set
  (package
    (name "go-github-com-xtgo-set")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xtgo/set")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "148jb5f87lf7090jg8340f24r29818krydajkm75vpzylaw6yd8w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xtgo/set"))
    (home-page "https://github.com/xtgo/set")
    (synopsis "set")
    (description
     "Package set implements type-safe, non-allocating algorithms that operate on
ordered sets.")
    (license license:bsd-2)))

(define-public go-github-com-chewxy-hm
  (package
    (name "go-github-com-chewxy-hm")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chewxy/hm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f4qwg1q2lc9y64wrl9qxyimqnnandlqg78gn3yv4vsmyci025r7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/chewxy/hm"
      #:tests? #f))
    (propagated-inputs (list go-github-com-pkg-errors go-github-com-xtgo-set))
    (home-page "https://github.com/chewxy/hm")
    (synopsis "HM")
    (description
     "Package hm provides a Hindley-Milner type inferencer and typer.")
    (license license:expat)))

(define-public go-github-com-google-flatbuffers
  (package
    (name "go-github-com-google-flatbuffers")
    (version "24.3.25+incompatible")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/flatbuffers")
             (commit (string-append "v"
                                    (string-drop-right version 13)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q066x1h0x9225aj25jv40gxgz46yvwmiqc2g6q06mkkg1144kxq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/flatbuffers/go"
      #:unpack-path "github.com/google/flatbuffers"))
    (home-page "https://github.com/google/flatbuffers")
    (synopsis "FlatBuffers")
    (description
     "@@strong{@code{FlatBuffers}} is a cross platform serialization library
architected for maximum memory efficiency.  It allows you to directly access
serialized data without parsing/unpacking it first, while still having great
forwards/backwards compatibility.")
    (license license:asl2.0)))

(define-public go-github-com-apache-arrow-go-arrow
  (package
    (name "go-github-com-apache-arrow-go-arrow")
    (version "0.0.0-20211112161151-bc219186db40")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/arrow")
             (commit (go-version->git-ref version
                                          #:subdir "go/arrow"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03nh7c0i3y9rkkzw428knalkrlpb8syr459i00mwp072ijn8v4hx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/apache/arrow/go/arrow"
      #:unpack-path "github.com/apache/arrow"
      #:tests? #f))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-gonum-org-v1-gonum
                             go-golang-org-x-xerrors
                             go-golang-org-x-exp
                             go-github-com-stretchr-testify
                             go-github-com-pierrec-lz4-v4
                             go-github-com-klauspost-compress
                             go-github-com-google-flatbuffers
                             go-github-com-golang-protobuf))
    (home-page "https://github.com/apache/arrow")
    (synopsis "Apache Arrow Go implementation")
    (description "Package arrow provides an implementation of Apache Arrow.")
    (license license:asl2.0)))

(define-public go-github-com-x448-float16
  (package
    (name "go-github-com-x448-float16")
    (version "0.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/x448/float16")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qg6ya30fra20hpa2qzqqzs8l95lvw9yzd87fdzq195xqi6crb2l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/x448/float16"))
    (home-page "https://github.com/x448/float16")
    (synopsis "Float16 (Binary16) in Go/Golang")
    (description
     "@@code{float16} package provides
@@url{https://en.wikipedia.org/wiki/Half-precision_floating-point_format,IEEE
754 half-precision floating-point format (binary16)} with IEEE 754 default
rounding for conversions.  IEEE 754-2008 refers to this 16-bit floating-point
format as binary16.")
    (license license:expat)))

(define-public go-github-com-d4l3k-go-bfloat16
  (package
    (name "go-github-com-d4l3k-go-bfloat16")
    (version "0.0.0-20211005043715-690c3bdd05f1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/d4l3k/go-bfloat16")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bshygdr5lcagznrh349r53whqhlg870j484zpsi3f7ilqv08rvy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/d4l3k/go-bfloat16"))
    (home-page "https://github.com/d4l3k/go-bfloat16")
    (synopsis "go-bfloat16")
    (description "BFloat16 conversion utilities for Go/Golang.")
    (license license:expat)))

(define-public go-github-com-nlpodyssey-gopickle
  (package
    (name "go-github-com-nlpodyssey-gopickle")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nlpodyssey/gopickle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fadbyq63i55g3k91knm7m1pl3j0krxdgpajrl78h27sl3mhnhal"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nlpodyssey/gopickle/..."
      #:unpack-path "github.com/nlpodyssey/gopickle"))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://github.com/nlpodyssey/gopickle")
    (synopsis "GoPickle")
    (description
     "@code{GoPickle} is a Go library for loading Python's data serialized with
@code{pickle} and @code{PyTorch} module files.")
    (license license:bsd-2)))

(define-public go-github-com-pdevine-tensor
  (package
    (name "go-github-com-pdevine-tensor")
    (version "0.0.0-20240510204454-f88f4562727c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pdevine/tensor")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ibc3x2c3dybhqdfnq2rrw6zxqng3b2zkl7nldsmllljfvp39c7s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gorgonia.org/tensor"
      #:unpack-path "github.com/pdevine/tensor"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-import-paths
            (lambda _
              ;; Copy the entire source tree to the expected import path
              (copy-recursively "src/github.com/pdevine/tensor"
                                "src/gorgonia.org/tensor"))))))
    (propagated-inputs (list go-gonum-org-v1-gonum
                             go-gorgonia-org-vecf32
                             go-gorgonia-org-vecf64
                             go-go4-org-unsafe-assume-no-moving-gc
                             go-github-com-gogo-protobuf
                             go-github-com-golang-protobuf
                             go-github-com-apache-arrow-go-arrow
                             go-github-com-google-flatbuffers
                             go-github-com-chewxy-hm
                             go-github-com-pkg-errors))
    (home-page "https://github.com/pdevine/tensor")
    (synopsis "Package")
    (description
     "Package tensor is a package that provides efficient, generic n-dimensional
arrays in Go.  Also in this package are functions and methods that are used
commonly in arithmetic, comparison and linear algebra operations.")
    (license license:asl2.0)))

(define-public go-github-com-chewxy-math32
  (package
    (name "go-github-com-chewxy-math32")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chewxy/math32")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i7jssi872mv7h4rc4y0xa88a0hsr03mydqyrd6mrm8n7q8rfml9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/chewxy/math32"))
    (home-page "https://github.com/chewxy/math32")
    (synopsis "math32")
    (description
     "Package math32 provides basic constants and mathematical functions for float32
types.")
    (license license:bsd-2)))

(define-public go-gorgonia-org-vecf32
  (package
    (name "go-gorgonia-org-vecf32")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorgonia/vecf32")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jggbf98fbbip7znx5m4n2lqqsnw5kqycj3gcbs62ypirr1pp0m9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gorgonia.org/vecf32"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-chewxy-math32))
    (home-page "https://gorgonia.org/vecf32")
    (synopsis "vecf32")
    (description
     "Package vecf32 provides common functions and methods for slices of float32.")
    (license license:expat)))

(define-public go-gorgonia-org-vecf64
  (package
    (name "go-gorgonia-org-vecf64")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorgonia/vecf64")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a8v65cy6gyh7ww2g8q4p6dmjhcd6k7lm7z8ly4vmi4k0vq1w187"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gorgonia.org/vecf64"
      #:tests? #f))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://gorgonia.org/vecf64")
    (synopsis "vecf64")
    (description
     "Package vecf64 provides common functions and methods for slices of float64.")
    (license license:expat)))

(define-public go-go4-org-unsafe-assume-no-moving-gc
  (package
    (name "go-go4-org-unsafe-assume-no-moving-gc")
    (version "0.0.0-20231121144256-b99613f794b6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go4org/unsafe-assume-no-moving-gc")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00ny3qha8k9nnx37ryvls2n5r7lw3bnldz6kwdmjxk8s19mxqim7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go4.org/unsafe/assume-no-moving-gc"))
    (home-page "https://go4.org/unsafe/assume-no-moving-gc")
    (synopsis "go4.org/unsafe/assume-no-moving-gc")
    (description
     "Package go4.org/unsafe/assume-no-moving-gc exists so you can depend on it from
unsafe code that wants to declare that it assumes that the Go runtime does not
use a moving garbage collector.  Specifically, it asserts that the caller is
playing stupid games with the addresses of heap-allocated values.  It says
nothing about values that Go's escape analysis keeps on the stack.  Ensuring
things aren't stack-allocated is the caller's responsibility.")
    (license license:bsd-3)))

(define-public go-github-com-arbovm-levenshtein
  (package
    (name "go-github-com-arbovm-levenshtein")
    (version "0.0.0-20160628152529-48b4e1c0c4d0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arbovm/levenshtein")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nmx2iip8xpnbmy6gvqpc9ikizr33dr40xgv746h0b0by8n7rv7y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/arbovm/levenshtein"))
    (home-page "https://github.com/arbovm/levenshtein")
    (synopsis "Levenshtein Distance")
    (description
     "@@url{http://golang.org,Go} package to calculate the
@@url{http://en.wikipedia.org/wiki/Levenshtein_distance,Levenshtein Distance}.")
    (license license:bsd-3)))

(define-public go-github-com-dgryski-trifles
  (package
    (name "go-github-com-dgryski-trifles")
    (version "0.0.0-20240922021506-5ecb8eeff266")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/trifles")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "071pnsyax99ikc58b110hdvqk1v46mqk6zdd0sshrf9lmwixwpnj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dgryski/trifles"))
    (home-page "https://github.com/dgryski/trifles")
    (synopsis "Collection of small Go programs")
    (description "Trifles is a collection of small Go programs.")
    (license license:expat)))

(define-public go-github-com-agnivade-levenshtein
  (package
    (name "go-github-com-agnivade-levenshtein")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/agnivade/levenshtein")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cq6jc032gxssiqnlkq3y3i1b19qsg73ysgxfbn6hwf8qnrzc2xn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/agnivade/levenshtein"))
    (propagated-inputs (list go-github-com-dgryski-trifles
                             go-github-com-arbovm-levenshtein))
    (home-page "https://github.com/agnivade/levenshtein")
    (synopsis "levenshtein")
    (description
     "Package levenshtein is a Go implementation to calculate Levenshtein Distance.")
    (license license:expat)))

(define-public go-nullprogram-com-x-optparse
  (package
    (name "go-nullprogram-com-x-optparse")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeeto/optparse-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yzpzlhmxdm8gd8ikh9c91qmshjp1jg49l0qsslmm432wk19zym9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "nullprogram.com/x/optparse"))
    (home-page "https://nullprogram.com/x/optparse")
    (synopsis "Traditional long option parser for Go")
    (description
     "Package optparse parses command line arguments very similarly to GNU
@code{getopt_long()}.  It supports long options and optional arguments, but does
not permute arguments.  It is intended as a replacement for Go's flag package.")
    (license license:unlicense)))

(define-public go-github-com-bytedance-sonic-loader
  (package
    (name "go-github-com-bytedance-sonic-loader")
    (version "0.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bytedance/sonic")
             (commit (go-version->git-ref version
                                          #:subdir "loader"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09a7jka6a74802i6a6lgxlc3vp0jnb69hy1l5s772260q1zgnkds"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bytedance/sonic/loader"
      #:unpack-path "github.com/bytedance/sonic"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-davecgh-go-spew))
    (home-page "https://github.com/bytedance/sonic")
    (synopsis "Runtime loader for Sonic")
    (description "Runtime loader module for the Sonic JSON library.")
    (license license:asl2.0)))

(define-public go-github-com-twitchyliquid64-golang-asm
  (package
    (name "go-github-com-twitchyliquid64-golang-asm")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twitchyliquid64/golang-asm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1akw41i0snxqw9lqzmnn4gx6hd5js5dr1vmfkm49wxans4k14vw4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/twitchyliquid64/golang-asm"))
    (home-page "https://github.com/twitchyliquid64/golang-asm")
    (synopsis "golang-asm")
    (description
     "This package provides a mirror of the assembler from the Go compiler, with
import paths re-written for the assembler to be functional as a standalone
library.")
    (license license:bsd-3)))

(define-public go-github-com-cloudwego-base64x
  (package
    (name "go-github-com-cloudwego-base64x")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudwego/base64x")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lgs28mj5w350vp6pazz2265hx2kab3kbjw7vnk0w1skslxbj8kx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cloudwego/base64x"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-klauspost-cpuid-v2
                             go-github-com-davecgh-go-spew
                             go-github-com-bytedance-sonic-loader))
    (home-page "https://github.com/cloudwego/base64x")
    (synopsis "base64x")
    (description
     "High performance drop-in replacement of the @@code{encoding/base64} library.")
    (license (list license:asl2.0 license:asl2.0))))

(define-public go-github-com-cloudwego-iasm
  (package
    (name "go-github-com-cloudwego-iasm")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudwego/iasm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j9jvx6ijlr2xz3am4qrz5py68xpl8np7m7yfq9m2ilkli3ksq9x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cloudwego/iasm"))
    (propagated-inputs (list go-nullprogram-com-x-optparse
                             go-github-com-stretchr-testify
                             go-github-com-knz-go-libedit
                             go-github-com-klauspost-cpuid-v2
                             go-github-com-davecgh-go-spew))
    (home-page "https://github.com/cloudwego/iasm")
    (synopsis "Interactive Assembler for Go")
    (description "Dual-purpose assembly engine written in pure Golang.")
    (license license:asl2.0)))

(define-public go-github-com-bytedance-sonic
  (package
    (name "go-github-com-bytedance-sonic")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bytedance/sonic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kalb4m3cqm22b4m9jkww42rh73icrik7i2b2771lwbn1r3bsick"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bytedance/sonic"))
    (propagated-inputs (list go-golang-org-x-arch
                             go-github-com-twitchyliquid64-golang-asm
                             go-github-com-stretchr-testify
                             go-github-com-klauspost-cpuid-v2
                             go-github-com-davecgh-go-spew
                             go-github-com-cloudwego-base64x
                             go-github-com-bytedance-sonic-loader))
    (home-page "https://github.com/bytedance/sonic")
    (synopsis "High-performance JSON library for Go")
    (description
     "Sonic is a blazingly fast JSON serialization and deserialization library.")
    (license license:asl2.0)))
