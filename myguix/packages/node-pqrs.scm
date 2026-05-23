(define-module (myguix packages node-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages node)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system node)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define %node-next-version
  "25.8.0")
(define %node-next-source-hash
  "08f5i5b68kn5f93b449f8l068ipdjhgymgq9a4w48dwd7ndv6z5s")

(define-public libuv-for-node-next
  (package
    (inherit libuv)
    (name "libuv")
    (version "1.51.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://nodejs.org/dist/v" %node-next-version
                           "/node-v" %node-next-version ".tar.gz"))
       (sha256
        (base32 %node-next-source-hash))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DLIBUV_BUILD_SHARED=ON" "-DLIBUV_BUILD_TESTS=OFF"
              "-DLIBUV_BUILD_BENCH=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-libuv-source-directory
            (lambda _
              (chdir "deps/uv"))))))
    (native-inputs (list pkg-config))
    (properties '((hidden? . #t)))))

(define-public node-agent-base-7.1.4
  (package
    (name "node-agent-base")
    (version "7.1.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/agent-base/-/agent-base-7.1.4.tgz")
       (sha256
        (base32 "0zmmkk3xhnkwb6djnvri7zyxllsdz5aa5w83x7afi959d0badm3x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ws" "jest"
                                                  "ts-jest"
                                                  "tsconfig"
                                                  "@types/ws"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@types/debug"
                                                  "async-listen"
                                                  "@types/semver"))))))))
    (home-page "https://github.com/TooTallNate/proxy-agents#readme")
    (synopsis "Turn a function into an `http.Agent` instance")
    (description "Turn a function into an `http.Agent` instance")
    (license license:expat)))

(define-public node-agentclientprotocol-sdk-0.22.1
  (package
    (name "node-agentclientprotocol-sdk")
    (version "0.22.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@agentclientprotocol/sdk/-/sdk-0.22.1.tgz")
       (sha256
        (base32 "1qbb2c8kh3s070gw3hkm6awppbsgfyyxfzb6g0p44vxnwq95v8h3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@eslint/js"
                                                  "@hey-api/openapi-ts"
                                                  "@types/node"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "concurrently"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "globals"
                                                  "http-server"
                                                  "prettier"
                                                  "tsx"
                                                  "typedoc"
                                                  "typedoc-github-theme"
                                                  "typescript"
                                                  "vitest"
                                                  "zod"))))))))
    (home-page "https://github.com/agentclientprotocol/typescript-sdk#readme")
    (synopsis
     "The Agent Client Protocol (ACP) is a protocol that standardizes communication between *code editors* (interactive programs for viewing and editing source code) and *coding agents* (programs that use generative AI to autonomously modify code).")
    (description
     "The Agent Client Protocol (ACP) is a protocol that standardizes communication between *code editors* (interactive programs for viewing and editing source code) and *coding agents* (programs that use generative AI to autonomously modify code).")
    (license license:asl2.0)))

(define-public node-buffer-from-1.1.2
  (package
    (name "node-buffer-from")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/buffer-from/-/buffer-from-1.1.2.tgz")
       (sha256
        (base32 "0hz3cbll0m805g22c5pnwdgpi1xavmrp5q1734x4d3yakvah6aww"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("standard"))))))))
    (home-page "https://github.com/LinusU/buffer-from#readme")
    (synopsis "Ponyfill for Buffer.from")
    (description "Ponyfill for Buffer.from.")
    (license license:expat)))

(define-public node-deep-extend-0.6.0
  (package
    (name "node-deep-extend")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/deep-extend/-/deep-extend-0.6.0.tgz")
       (sha256
        (base32 "11hk1g7qjw9bj03c8y7v7n8p8mdfacpd9l8n57dga8qcj8s5zk0d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "should"))))))))
    (home-page "https://github.com/unclechu/node-deep-extend")
    (synopsis "Recursive object extending")
    (description "Recursive object extending.")
    (license license:expat)))

(define-public node-ieee754-1.2.1
  (package
    (name "node-ieee754")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ieee754/-/ieee754-1.2.1.tgz")
       (sha256
        (base32 "1b4xiyr6fmgl05cjgc8fiyfk2jagf7xq2y5rknw9scvy76dlpwcf"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("airtap" "standard" "tape"))))))))
    (home-page "https://github.com/feross/ieee754#readme")
    (synopsis "Read and write IEEE754 floating point numbers")
    (description "Read and write IEEE754 floating point numbers.")
    (license license:bsd-3)))

(define-public node-inherits-2.0.4
  (package
    (name "node-inherits")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz")
       (sha256
        (base32 "1bxg4igfni2hymabg8bkw86wd3qhhzhsswran47sridk3dnbqkfr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap"))))))))
    (home-page "https://github.com/isaacs/inherits#readme")
    (synopsis "Browser-friendly inheritance")
    (description "Browser-friendly inheritance compatible with Node.js inherits.")
    (license license:isc)))

(define-public node-isexe-2.0.0
  (package
    (name "node-isexe")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/isexe/-/isexe-2.0.0.tgz")
       (sha256
        (base32 "0nc3rcqjgyb9yyqajwlzzhfcqmsb682z7zinnx9qrql8w1rfiks7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "mkdirp" "rimraf"))))))))
    (home-page "https://github.com/isaacs/isexe#readme")
    (synopsis "Check if a file is executable")
    (description "Minimal module to check if a file is executable.")
    (license license:isc)))

(define-public node-ms-2.1.3
  (package
    (name "node-ms")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ms/-/ms-2.1.3.tgz")
       (sha256
        (base32 "1ii24v83yrryzmj9p369qxmpr53337kkqbdaklpmbv9hwlanwqgn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("husky" "mocha" "eslint"
                                                  "prettier" "expect.js"
                                                  "lint-staged"))))))))
    (home-page "https://github.com/vercel/ms#readme")
    (synopsis "Tiny millisecond conversion utility")
    (description "Tiny millisecond conversion utility.")
    (license license:expat)))

(define-public node-once-1.4.0
  (package
    (name "node-once")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/once/-/once-1.4.0.tgz")
       (sha256
        (base32 "1kygzk36kdcfiqz01dhql2dk75rl256m2vlpigv9iikhlc5lclfg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap"))))))))
    (inputs (list node-wrappy-1.0.2))
    (home-page "https://github.com/isaacs/once#readme")
    (synopsis "Run a function exactly one time")
    (description "Run a function exactly one time.")
    (license license:isc)))

(define-public node-safe-buffer-5.2.1
  (package
    (name "node-safe-buffer")
    (version "5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.2.1.tgz")
       (sha256
        (base32 "1s5kvjpwqsc682zcy71h9c6pxla21sysfwj270x6jjkca421h62x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("standard" "tape"))))))))
    (home-page "https://github.com/feross/safe-buffer")
    (synopsis "Safer Node.js Buffer API")
    (description "Safer Node.js Buffer API.")
    (license license:expat)))

(define-public node-source-map-0.6.1
  (package
    (name "node-source-map")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/source-map/-/source-map-0.6.1.tgz")
       (sha256
        (base32 "11ib173i7xf5sd85da9jfrcbzygr48pppz5csl15hnpz2w6s3g5x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("doctoc" "webpack"))))))))
    (home-page "https://github.com/mozilla/source-map")
    (synopsis "Generate and consume source maps")
    (description "Generate and consume source maps.")
    (license license:bsd-3)))

(define-public node-source-map-support-0.5.21
  (package
    (name "node-source-map-support")
    (version "0.5.21")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/source-map-support/-/source-map-support-0.5.21.tgz")
       (sha256
        (base32 "0dpnahsipxckan03b5w5qrmlibkz8amh7k6gj77zs9387vph96sx"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("browserify" "coffeescript"
                                                  "http-server" "mocha"
                                                  "webpack"))))))))
    (inputs (list node-source-map-0.6.1 node-buffer-from-1.1.2))
    (home-page "https://github.com/evanw/node-source-map-support#readme")
    (synopsis "Fix stack traces for files with source maps")
    (description "Fix stack traces for files with source maps.")
    (license license:expat)))

(define-public node-string-decoder-1.3.0
  (package
    (name "node-string-decoder")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/string_decoder/-/string_decoder-1.3.0.tgz")
       (sha256
        (base32 "1bdkjw2kn1h4lrmqqfdwajsg9yivn92swwc3aciy8i83jh06j0vx"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("babel-polyfill"
                                                  "core-util-is" "inherits"
                                                  "tap"))))))))
    (inputs (list node-safe-buffer-5.2.1))
    (home-page "https://github.com/nodejs/string_decoder")
    (synopsis "String decoder module from Node core")
    (description "String decoder module from Node core.")
    (license license:expat)))

(define-public node-util-deprecate-1.0.2
  (package
    (name "node-util-deprecate")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/util-deprecate/-/util-deprecate-1.0.2.tgz")
       (sha256
        (base32 "1rd3qbgdrwkmcrf7vqx61sh7icma7jvxcmklqj032f8v7jcdx8br"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/TooTallNate/util-deprecate")
    (synopsis "Browser support for util.deprecate")
    (description "Node.js util.deprecate function with browser support.")
    (license license:expat)))

(define-public node-wrappy-1.0.2
  (package
    (name "node-wrappy")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz")
       (sha256
        (base32 "1yzx63jf27yz0bk0m78vy4y1cqzm113d2mi9h91y3cdpj46p7wxg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap"))))))))
    (home-page "https://github.com/npm/wrappy")
    (synopsis "Callback wrapping utility")
    (description "Callback wrapping utility.")
    (license license:isc)))

(define-public node-ajv-8.20.0
  (package
    (name "node-ajv")
    (version "8.20.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ajv/-/ajv-8.20.0.tgz")
       (sha256
        (base32 "1cz7yr42yf4kb0znhwyxslqrxqcr2j5z0538zdgcrf5vjflb7w5j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ajv-validator/config"
                                                  "@rollup/plugin-commonjs"
                                                  "@rollup/plugin-json"
                                                  "@rollup/plugin-node-resolve"
                                                  "@rollup/plugin-typescript"
                                                  "@types/chai"
                                                  "@types/mocha"
                                                  "@types/node"
                                                  "@types/require-from-string"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "ajv-formats"
                                                  "browserify"
                                                  "chai"
                                                  "cross-env"
                                                  "dayjs"
                                                  "dayjs-plugin-utc"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "glob"
                                                  "husky"
                                                  "jimp"
                                                  "js-beautify"
                                                  "json-schema-test"
                                                  "karma"
                                                  "karma-chrome-launcher"
                                                  "karma-mocha"
                                                  "lint-staged"
                                                  "mocha"
                                                  "module-from-string"
                                                  "node-fetch"
                                                  "nyc"
                                                  "prettier"
                                                  "re2"
                                                  "rollup"
                                                  "rollup-plugin-terser"
                                                  "ts-node"
                                                  "tsify"
                                                  "typescript"
                                                  "uri-js"))))))))
    (inputs (list node-require-from-string-2.0.2
                  node-json-schema-traverse-1.0.0 node-fast-uri-3.1.2
                  node-fast-deep-equal))
    (home-page "https://ajv.js.org")
    (synopsis "Another JSON Schema Validator")
    (description "Another JSON Schema Validator")
    (license license:expat)))

(define-public node-ajv-formats-3.0.1
  (package
    (name "node-ajv-formats")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ajv-formats/-/ajv-formats-3.0.1.tgz")
       (sha256
        (base32 "1idca2hn65drqp1bc4v696bqvnv3x08nj1lrj791yf37sc7rimpl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ajv-validator/config"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "ajv"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "husky"
                                                  "jest"
                                                  "json-schema-test"
                                                  "lint-staged"
                                                  "prettier"
                                                  "ts-jest"
                                                  "typescript"))))))))
    (inputs (list node-ajv-8.20.0 node-ajv-8.20.0))
    (home-page "https://github.com/ajv-validator/ajv-formats#readme")
    (synopsis "Format validation for Ajv v7+")
    (description "Format validation for Ajv v7+")
    (license license:expat)))

(define-public node-anthropic-ai-sdk-0.91.1
  (package
    (name "node-anthropic-ai-sdk")
    (version "0.91.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@anthropic-ai/sdk/-/sdk-0.91.1.tgz")
       (sha256
        (base32 "1hpv308b2yvcn35abq8nsb5zc30bcmxdxshgr3xhv60py86dj75w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("zod"))))))))
    (inputs (list node-json-schema-to-ts-3.1.1 node-zod-4.4.3))
    (home-page "https://www.npmjs.com/package/node-anthropic-ai-sdk")
    (synopsis "The official TypeScript library for the Anthropic API")
    (description "The official TypeScript library for the Anthropic API")
    (license license:expat)))

(define-public node-asn1-js-5.4.1
  (package
    (name "node-asn1-js")
    (version "5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/asn1.js/-/asn1.js-5.4.1.tgz")
       (sha256
        (base32 "18f2z3cg0gljsv2ycv8gl6r8cbdhdx6mizn9xkl4w9aza45rxicw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint" "mocha"))))))))
    (inputs (list node-safer-buffer node-minimalistic-assert-1.0.1
                  node-inherits-2.0.4 node-bn-js-4.12.3))
    (home-page "https://github.com/indutny/asn1.js")
    (synopsis "ASN.1 encoder and decoder")
    (description "ASN.1 encoder and decoder")
    (license license:expat)))

(define-public node-aws-crypto-crc32-5.2.0
  (package
    (name "node-aws-crypto-crc32")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@aws-crypto/crc32/-/crc32-5.2.0.tgz")
       (sha256
        (base32 "0zal3pj8lrh9f1g5wh3f8hgsl94i8qs1lrdfdia95lv1mzqc6s7d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-aws-crypto-util-5.2.0 node-aws-sdk-types-3.973.9
                  node-tslib-2.8.1))
    (home-page
     "https://github.com/aws/aws-sdk-js-crypto-helpers/tree/master/packages/crc32")
    (synopsis
     "Pure JS implementation of CRC32 https://en.wikipedia.org/wiki/Cyclic_redundancy_check")
    (description
     "Pure JS implementation of CRC32 https://en.wikipedia.org/wiki/Cyclic_redundancy_check")
    (license license:asl2.0)))

(define-public node-aws-crypto-sha256-browser-5.2.0
  (package
    (name "node-aws-crypto-sha256-browser")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-crypto/sha256-browser/-/sha256-browser-5.2.0.tgz")
       (sha256
        (base32 "13f23v4d91h48a6j6j9517vzywa6rp9y3hdsb7ns9z04g2y38900"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-aws-crypto-supports-web-crypto-5.2.0
                  node-aws-sdk-util-locate-window-3.965.5
                  node-aws-crypto-sha256-js-5.2.0
                  node-smithy-util-utf8-2.3.0
                  node-aws-crypto-util-5.2.0
                  node-aws-sdk-types-3.973.9
                  node-tslib-2.8.1))
    (home-page
     "https://github.com/aws/aws-sdk-js-crypto-helpers/tree/master/packages/sha256-browser")
    (synopsis
     "SHA256 wrapper for browsers that prefers `window.crypto.subtle` but will fall back to a pure JS implementation in @aws-crypto/sha256-js to provide a consistent interface for SHA256.")
    (description
     "SHA256 wrapper for browsers that prefers `window.crypto.subtle` but will fall back to a pure JS implementation in @aws-crypto/sha256-js to provide a consistent interface for SHA256.")
    (license license:asl2.0)))

(define-public node-aws-crypto-sha256-js-5.2.0
  (package
    (name "node-aws-crypto-sha256-js")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-crypto/sha256-js/-/sha256-js-5.2.0.tgz")
       (sha256
        (base32 "0sm9wi14sj7qsscgdwpfkss1slc5s8r4by7glvi5kqdi9y5n6l2p"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-aws-crypto-util-5.2.0 node-aws-sdk-types-3.973.9
                  node-tslib-2.8.1))
    (home-page
     "https://github.com/aws/aws-sdk-js-crypto-helpers/tree/master/packages/sha256-js")
    (synopsis "A pure JS implementation SHA256.")
    (description "A pure JS implementation SHA256.")
    (license license:asl2.0)))

(define-public node-aws-crypto-supports-web-crypto-5.2.0
  (package
    (name "node-aws-crypto-supports-web-crypto")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-crypto/supports-web-crypto/-/supports-web-crypto-5.2.0.tgz")
       (sha256
        (base32 "1f3x1j89zi8hdnf8sdyidslklcf8kvgvl3clixj50rb10z6r216l"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-tslib-2.8.1))
    (home-page
     "https://github.com/aws/aws-sdk-js-crypto-helpers/tree/master/packages/supports-web-crypto")
    (synopsis
     "Provides functions for detecting if the host environment supports the WebCrypto API")
    (description
     "Provides functions for detecting if the host environment supports the WebCrypto API")
    (license license:asl2.0)))

(define-public node-aws-crypto-util-5.2.0
  (package
    (name "node-aws-crypto-util")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@aws-crypto/util/-/util-5.2.0.tgz")
       (sha256
        (base32 "1lvjyz8d2g5lpy6wxfhqgyji61nz8qzl62g04awjhnfvgayrmaj2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-smithy-util-utf8-2.3.0 node-aws-sdk-types-3.973.9
                  node-tslib-2.8.1))
    (home-page
     "https://github.com/aws/aws-sdk-js-crypto-helpers/tree/master/packages/util")
    (synopsis "Helper functions")
    (description "Helper functions")
    (license license:asl2.0)))

(define-public node-aws-lambda-invoke-store-0.2.4
  (package
    (name "node-aws-lambda-invoke-store")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws/lambda-invoke-store/-/lambda-invoke-store-0.2.4.tgz")
       (sha256
        (base32 "0n9ivba53975rf6fl5j7jafnv5nzsmf7srpsd2n751vl7bvsxg7n"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@changesets/cli"
                                                  "@rollup/plugin-node-resolve"
                                                  "@rollup/plugin-typescript"
                                                  "@tsconfig/node18"
                                                  "@types/node"
                                                  "rollup"
                                                  "tslib"
                                                  "typescript"
                                                  "vitest"))))))))
    (home-page "https://github.com/awslabs/aws-lambda-invoke-store")
    (synopsis
     "Invoke scoped data storage for AWS Lambda Node.js Runtime Environment")
    (description
     "Invoke scoped data storage for AWS Lambda Node.js Runtime Environment")
    (license license:asl2.0)))

(define-public node-aws-sdk-client-bedrock-runtime-3.1048.0
  (package
    (name "node-aws-sdk-client-bedrock-runtime")
    (version "3.1048.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/client-bedrock-runtime/-/client-bedrock-runtime-3.1048.0.tgz")
       (sha256
        (base32 "0ayqqflicslxm612s8bcc5gg23a5nxlkfz7n8h6g161k9lbvhkkj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("vitest" "premove"
                                                  "typescript"
                                                  "@types/node"
                                                  "concurrently"
                                                  "downlevel-dts"
                                                  "@tsconfig/node20"
                                                  "@smithy/snapshot-testing"))))))))
    (inputs (list node-aws-sdk-eventstream-handler-node-3.972.17
                  node-aws-sdk-credential-provider-node-3.972.44
                  node-aws-sdk-middleware-eventstream-3.972.13
                  node-aws-sdk-middleware-websocket-3.972.21
                  node-smithy-fetch-http-handler-5.4.4
                  node-aws-crypto-sha256-browser-5.2.0
                  node-smithy-node-http-handler-4.7.4
                  node-aws-sdk-token-providers-3.1048.0
                  node-aws-crypto-sha256-js-5.2.0
                  node-aws-sdk-types-3.973.9
                  node-smithy-types-4.14.2
                  node-aws-sdk-core-3.974.13
                  node-smithy-core-3.24.4
                  node-tslib-2.8.1))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/clients/client-bedrock-runtime")
    (synopsis
     "AWS SDK for JavaScript Bedrock Runtime Client for Node.js, Browser and React Native")
    (description
     "AWS SDK for JavaScript Bedrock Runtime Client for Node.js, Browser and React Native")
    (license license:asl2.0)))

(define-public node-aws-sdk-core-3.974.13
  (package
    (name "node-aws-sdk-core")
    (version "3.974.13")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@aws-sdk/core/-/core-3.974.13.tgz")
       (sha256
        (base32 "1rr0kd2j762i5hsvd9rq745mhhqzzra8qippgmkwz4zpaiwxj93p"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1
                  node-bowser-2.14.1
                  node-smithy-types-4.14.2
                  node-smithy-signature-v4-5.4.4
                  node-smithy-core-3.24.4
                  node-aws-lambda-invoke-store-0.2.4
                  node-aws-sdk-xml-builder-3.972.25
                  node-aws-sdk-types-3.973.9))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/core")
    (synopsis "Core functions & classes shared by multiple AWS SDK clients.")
    (description
     "Core functions & classes shared by multiple AWS SDK clients.")
    (license license:asl2.0)))

(define-public node-aws-sdk-credential-provider-env-3.972.39
  (package
    (name "node-aws-sdk-credential-provider-env")
    (version "3.972.39")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/credential-provider-env/-/credential-provider-env-3.972.39.tgz")
       (sha256
        (base32 "141cslj7hyv9m8wvnqxh2g594fk49n4zmlylx6cqxj2yams2z5i1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-smithy-core-3.24.4 node-aws-sdk-types-3.973.9
                  node-aws-sdk-core-3.974.13))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/credential-provider-env")
    (synopsis
     "AWS credential provider that sources credentials from known environment variables")
    (description
     "AWS credential provider that sources credentials from known environment variables")
    (license license:asl2.0)))

(define-public node-aws-sdk-credential-provider-http-3.972.41
  (package
    (name "node-aws-sdk-credential-provider-http")
    (version "3.972.41")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/credential-provider-http/-/credential-provider-http-3.972.41.tgz")
       (sha256
        (base32 "0hh1bggjw7m05wnvx5z504p1f5rgpji6gaq435iph14z1w8bhqdw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1
                  node-smithy-types-4.14.2
                  node-smithy-node-http-handler-4.7.4
                  node-smithy-fetch-http-handler-5.4.4
                  node-smithy-core-3.24.4
                  node-aws-sdk-types-3.973.9
                  node-aws-sdk-core-3.974.13))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/credential-provider-http")
    (synopsis "AWS credential provider for containers and HTTP sources")
    (description "AWS credential provider for containers and HTTP sources")
    (license license:asl2.0)))

(define-public node-aws-sdk-credential-provider-ini-3.972.43
  (package
    (name "node-aws-sdk-credential-provider-ini")
    (version "3.972.43")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/credential-provider-ini/-/credential-provider-ini-3.972.43.tgz")
       (sha256
        (base32 "0lyz7anp0p2fla0q3f136m49wq8kfqgpgbpx0r92inrzqggspbnj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1
                  node-smithy-types-4.14.2
                  node-smithy-credential-provider-imds-4.3.4
                  node-smithy-core-3.24.4
                  node-aws-sdk-types-3.973.9
                  node-aws-sdk-nested-clients-3.997.11
                  node-aws-sdk-credential-provider-web-identity-3.972.43
                  node-aws-sdk-credential-provider-sso-3.972.43
                  node-aws-sdk-credential-provider-process-3.972.39
                  node-aws-sdk-credential-provider-login-3.972.43
                  node-aws-sdk-credential-provider-http-3.972.41
                  node-aws-sdk-credential-provider-env-3.972.39
                  node-aws-sdk-core-3.974.13))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/credential-provider-ini")
    (synopsis
     "AWS credential provider that sources credentials from ~/.aws/credentials and ~/.aws/config")
    (description
     "AWS credential provider that sources credentials from ~/.aws/credentials and ~/.aws/config")
    (license license:asl2.0)))

(define-public node-aws-sdk-credential-provider-login-3.972.43
  (package
    (name "node-aws-sdk-credential-provider-login")
    (version "3.972.43")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/credential-provider-login/-/credential-provider-login-3.972.43.tgz")
       (sha256
        (base32 "1kk7njgr4bbvr7k88f023j85vnal4q62qx7dsqvxhf1r9igizhba"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1
                  node-smithy-types-4.14.2
                  node-smithy-core-3.24.4
                  node-aws-sdk-types-3.973.9
                  node-aws-sdk-nested-clients-3.997.11
                  node-aws-sdk-core-3.974.13))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/credential-provider-login")
    (synopsis
     "AWS credential provider that sources credentials from aws login cached tokens")
    (description
     "AWS credential provider that sources credentials from aws login cached tokens")
    (license license:asl2.0)))

(define-public node-aws-sdk-credential-provider-node-3.972.44
  (package
    (name "node-aws-sdk-credential-provider-node")
    (version "3.972.44")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/credential-provider-node/-/credential-provider-node-3.972.44.tgz")
       (sha256
        (base32 "11biws67hx02ii4rrqpla2z0lns20w3qphvjzh9zxhjjka8q58gd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1
                  node-smithy-types-4.14.2
                  node-smithy-credential-provider-imds-4.3.4
                  node-smithy-core-3.24.4
                  node-aws-sdk-types-3.973.9
                  node-aws-sdk-credential-provider-web-identity-3.972.43
                  node-aws-sdk-credential-provider-sso-3.972.43
                  node-aws-sdk-credential-provider-process-3.972.39
                  node-aws-sdk-credential-provider-ini-3.972.43
                  node-aws-sdk-credential-provider-http-3.972.41
                  node-aws-sdk-credential-provider-env-3.972.39))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/credential-provider-node")
    (synopsis
     "AWS credential provider that sources credentials from a Node.JS environment. ")
    (description
     "AWS credential provider that sources credentials from a Node.JS environment. ")
    (license license:asl2.0)))

(define-public node-aws-sdk-credential-provider-process-3.972.39
  (package
    (name "node-aws-sdk-credential-provider-process")
    (version "3.972.39")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/credential-provider-process/-/credential-provider-process-3.972.39.tgz")
       (sha256
        (base32 "1i2qrmx1vkgmgmajwcbvvi9vhyd3z0xw45np3mv5qa78h5q3r7i4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-smithy-core-3.24.4 node-aws-sdk-types-3.973.9
                  node-aws-sdk-core-3.974.13))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/credential-provider-process")
    (synopsis
     "AWS credential provider that sources credential_process from ~/.aws/credentials and ~/.aws/config")
    (description
     "AWS credential provider that sources credential_process from ~/.aws/credentials and ~/.aws/config")
    (license license:asl2.0)))

(define-public node-aws-sdk-credential-provider-sso-3.972.43
  (package
    (name "node-aws-sdk-credential-provider-sso")
    (version "3.972.43")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/credential-provider-sso/-/credential-provider-sso-3.972.43.tgz")
       (sha256
        (base32 "0wy6mwp2skz73c6c0kpgwgnijdbcvf7sdpk1pz7jg6q2z6q1m1g9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1
                  node-smithy-types-4.14.2
                  node-smithy-core-3.24.4
                  node-aws-sdk-types-3.973.9
                  node-aws-sdk-token-providers-3.1052.0
                  node-aws-sdk-nested-clients-3.997.11
                  node-aws-sdk-core-3.974.13))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/credential-provider-sso")
    (synopsis
     "AWS credential provider that exchanges a resolved SSO login token file for temporary AWS credentials")
    (description
     "AWS credential provider that exchanges a resolved SSO login token file for temporary AWS credentials")
    (license license:asl2.0)))

(define-public node-aws-sdk-credential-provider-web-identity-3.972.43
  (package
    (name "node-aws-sdk-credential-provider-web-identity")
    (version "3.972.43")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/credential-provider-web-identity/-/credential-provider-web-identity-3.972.43.tgz")
       (sha256
        (base32 "09yjwzf1ggpkrnilhzc6gvsgm7b0p2wj3i7nki02a5k2xia58zi5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1
                  node-smithy-types-4.14.2
                  node-smithy-core-3.24.4
                  node-aws-sdk-types-3.973.9
                  node-aws-sdk-nested-clients-3.997.11
                  node-aws-sdk-core-3.974.13))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/credential-provider-web-identity")
    (synopsis
     "AWS credential provider that calls STS assumeRole for temporary AWS credentials")
    (description
     "AWS credential provider that calls STS assumeRole for temporary AWS credentials")
    (license license:asl2.0)))

(define-public node-aws-sdk-eventstream-handler-node-3.972.17
  (package
    (name "node-aws-sdk-eventstream-handler-node")
    (version "3.972.17")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/eventstream-handler-node/-/eventstream-handler-node-3.972.17.tgz")
       (sha256
        (base32 "1jjzxpjcm5ydryg93drafz2r0km8hc3cv0afai2gi62fb1i58xva"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-smithy-core-3.24.4 node-aws-sdk-types-3.973.9))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/eventstream-handler-node")
    (synopsis
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/eventstream-handler-node/latest.svg)](https://www.npmjs.com/package/@aws-sdk/eventstream-handler-node) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/eventstream-handler-node.svg)](https://ww")
    (description
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/eventstream-handler-node/latest.svg)](https://www.npmjs.com/package/@aws-sdk/eventstream-handler-node) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/eventstream-handler-node.svg)](https://ww")
    (license license:asl2.0)))

(define-public node-aws-sdk-middleware-eventstream-3.972.13
  (package
    (name "node-aws-sdk-middleware-eventstream")
    (version "3.972.13")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/middleware-eventstream/-/middleware-eventstream-3.972.13.tgz")
       (sha256
        (base32 "19g1fzg910gh1a727vnzbj5igr3lw66a85dsxcnc802flnnr67s3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-smithy-core-3.24.4 node-aws-sdk-types-3.973.9))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/middleware-eventstream")
    (synopsis
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/middleware-eventstream/latest.svg)](https://www.npmjs.com/package/@aws-sdk/middleware-eventstream) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/middleware-eventstream.svg)](https://www.npmj")
    (description
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/middleware-eventstream/latest.svg)](https://www.npmjs.com/package/@aws-sdk/middleware-eventstream) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/middleware-eventstream.svg)](https://www.npmj")
    (license license:asl2.0)))

(define-public node-aws-sdk-middleware-websocket-3.972.21
  (package
    (name "node-aws-sdk-middleware-websocket")
    (version "3.972.21")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/middleware-websocket/-/middleware-websocket-3.972.21.tgz")
       (sha256
        (base32 "1mp0qx8vkp6rywr4vjkqmjp0z49q7r4y5v2pbszw1sfdqgjk5r9x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "concurrently"
                                                  "downlevel-dts"
                                                  "mock-socket"
                                                  "premove"
                                                  "typescript"
                                                  "vitest-websocket-mock"))))))))
    (inputs (list node-tslib-2.8.1
                  node-smithy-types-4.14.2
                  node-smithy-signature-v4-5.4.4
                  node-smithy-fetch-http-handler-5.4.4
                  node-smithy-core-3.24.4
                  node-aws-sdk-types-3.973.9
                  node-aws-sdk-core-3.974.13))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/middleware-websocket")
    (synopsis
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/middleware-websocket/latest.svg)](https://www.npmjs.com/package/@aws-sdk/middleware-websocket) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/middleware-websocket.svg)](https://www.npmjs.com/")
    (description
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/middleware-websocket/latest.svg)](https://www.npmjs.com/package/@aws-sdk/middleware-websocket) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/middleware-websocket.svg)](https://www.npmjs.com/")
    (license license:asl2.0)))

(define-public node-aws-sdk-nested-clients-3.997.11
  (package
    (name "node-aws-sdk-nested-clients")
    (version "3.997.11")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/nested-clients/-/nested-clients-3.997.11.tgz")
       (sha256
        (base32 "07l20gyd232n2kwl7l1n4jj49spk7yx0ac9q4b3p8ijk24j15gx0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1
                  node-smithy-types-4.14.2
                  node-smithy-node-http-handler-4.7.4
                  node-smithy-fetch-http-handler-5.4.4
                  node-smithy-core-3.24.4
                  node-aws-sdk-types-3.973.9
                  node-aws-sdk-signature-v4-multi-region-3.996.28
                  node-aws-sdk-core-3.974.13
                  node-aws-crypto-sha256-js-5.2.0
                  node-aws-crypto-sha256-browser-5.2.0))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages/nested-clients")
    (synopsis "Nested clients for AWS SDK packages.")
    (description "Nested clients for AWS SDK packages.")
    (license license:asl2.0)))

(define-public node-aws-sdk-signature-v4-multi-region-3.996.28
  (package
    (name "node-aws-sdk-signature-v4-multi-region")
    (version "3.996.28")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/signature-v4-multi-region/-/signature-v4-multi-region-3.996.28.tgz")
       (sha256
        (base32 "0xmbxqh75shb7iz7knwadp54a08apxn42mr5nxr422l5cz99lddn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-smithy-signature-v4-5.4.4 node-smithy-core-3.24.4
                  node-aws-sdk-types-3.973.9))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages/signature-v4-multi-region")
    (synopsis
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/signature-v4-multi-region/latest.svg)](https://www.npmjs.com/package/@aws-sdk/signature-v4-multi-region) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/signature-v4-multi-region.svg)](https:/")
    (description
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/signature-v4-multi-region/latest.svg)](https://www.npmjs.com/package/@aws-sdk/signature-v4-multi-region) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/signature-v4-multi-region.svg)](https:/")
    (license license:asl2.0)))

(define-public node-aws-sdk-token-providers-3.1048.0
  (package
    (name "node-aws-sdk-token-providers")
    (version "3.1048.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/token-providers/-/token-providers-3.1048.0.tgz")
       (sha256
        (base32 "1bsk3lx3iry076imksh7n2s1cfminlvx1vfqw5qgw0z6fz1rbvlw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("premove" "typescript"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts"
                                                  "@tsconfig/recommended"))))))))
    (inputs (list node-aws-sdk-nested-clients-3.997.11
                  node-aws-sdk-types-3.973.9
                  node-smithy-types-4.14.2
                  node-aws-sdk-core-3.974.13
                  node-smithy-core-3.24.4
                  node-tslib-2.8.1))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages/token-providers")
    (synopsis "A collection of token providers")
    (description "A collection of token providers")
    (license license:asl2.0)))

(define-public node-aws-sdk-token-providers-3.1052.0
  (package
    (name "node-aws-sdk-token-providers")
    (version "3.1052.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/token-providers/-/token-providers-3.1052.0.tgz")
       (sha256
        (base32 "1158y51yiscbp2x80y0h6vygdsk1ak4iy6wg4xgryzgr63kd3w6r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("premove" "typescript"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts"
                                                  "@tsconfig/recommended"))))))))
    (inputs (list node-aws-sdk-nested-clients-3.997.11
                  node-aws-sdk-types-3.973.9
                  node-smithy-types-4.14.2
                  node-aws-sdk-core-3.974.13
                  node-smithy-core-3.24.4
                  node-tslib-2.8.1))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages/token-providers")
    (synopsis "A collection of token providers")
    (description "A collection of token providers")
    (license license:asl2.0)))

(define-public node-aws-sdk-types-3.973.9
  (package
    (name "node-aws-sdk-types")
    (version "3.973.9")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@aws-sdk/types/-/types-3.973.9.tgz")
       (sha256
        (base32 "1yc306yxx9jm18cq09b6rpivw9k83n00sfjwgd1kq98ld5gdjrn4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/types")
    (synopsis "Types for the AWS SDK")
    (description "Types for the AWS SDK")
    (license license:asl2.0)))

(define-public node-aws-sdk-util-locate-window-3.965.5
  (package
    (name "node-aws-sdk-util-locate-window")
    (version "3.965.5")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/util-locate-window/-/util-locate-window-3.965.5.tgz")
       (sha256
        (base32 "07kk7mvc8mjqzvpp5w4fm1dvcarkqigx2k0wmqaizkj7qyy1v1nd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/util-locate-window")
    (synopsis
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/util-locate-window/latest.svg)](https://www.npmjs.com/package/@aws-sdk/util-locate-window) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/util-locate-window.svg)](https://www.npmjs.com/packag")
    (description
     "[![NPM version](https://img.shields.io/npm/v/@aws-sdk/util-locate-window/latest.svg)](https://www.npmjs.com/package/@aws-sdk/util-locate-window) [![NPM downloads](https://img.shields.io/npm/dm/@aws-sdk/util-locate-window.svg)](https://www.npmjs.com/packag")
    (license license:asl2.0)))

(define-public node-aws-sdk-xml-builder-3.972.25
  (package
    (name "node-aws-sdk-xml-builder")
    (version "3.972.25")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@aws-sdk/xml-builder/-/xml-builder-3.972.25.tgz")
       (sha256
        (base32 "0b6ngh9vz8pn3hshsrg7kwn7nqdnms6yhxbm3gi9rnwg4yps1ny2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tsconfig/recommended"
                                                  "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typescript"))))))))
    (inputs (list node-tslib-2.8.1 node-fast-xml-parser-5.7.3
                  node-smithy-types-4.14.2 node-nodable-entities-2.1.0))
    (home-page
     "https://github.com/aws/aws-sdk-js-v3/tree/main/packages-internal/xml-builder")
    (synopsis "XML utilities for the AWS SDK")
    (description "XML utilities for the AWS SDK")
    (license license:asl2.0)))

(define-public node-babel-runtime-7.29.2
  (package
    (name "node-babel-runtime")
    (version "7.29.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@babel/runtime/-/runtime-7.29.2.tgz")
       (sha256
        (base32 "1rc71zqm9k7cwhhqrn7v0njgk04vw5ppznlbbshcq545kskdvmli"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://babel.dev/docs/en/next/babel-runtime")
    (synopsis "babel's modular runtime helpers")
    (description "babel's modular runtime helpers")
    (license license:expat)))

(define-public node-balanced-match-4.0.4
  (package
    (name "node-balanced-match")
    (version "4.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/balanced-match/-/balanced-match-4.0.4.tgz")
       (sha256
        (base32 "05pva90symmxg31miw4msmd9g0q1aspf7764pcqybvi5j66m09ch"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/brace-expansion"
                                                  "@types/node"
                                                  "mkdirp"
                                                  "prettier"
                                                  "tap"
                                                  "tshy"
                                                  "typedoc"))))))))
    (home-page "https://github.com/juliangruber/balanced-match#readme")
    (synopsis "Match balanced character pairs, like \"{\" and \"}\"")
    (description "Match balanced character pairs, like \"{\" and \"}\"")
    (license license:expat)))

(define-public node-base64-js-1.5.1
  (package
    (name "node-base64-js")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/base64-js/-/base64-js-1.5.1.tgz")
       (sha256
        (base32 "118a46skxnrgx5bdd68ny9xxjcvyb7b1clj2hf82d196nm2skdxi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("babel-minify" "benchmark"
                                                  "browserify" "standard"
                                                  "tape"))))))))
    (home-page "https://github.com/beatgammit/base64-js")
    (synopsis "Base64 encoding/decoding in pure JS")
    (description "Base64 encoding/decoding in pure JS")
    (license license:expat)))

(define-public node-bignumber-js-9.3.1
  (package
    (name "node-bignumber-js")
    (version "9.3.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bignumber.js/-/bignumber.js-9.3.1.tgz")
       (sha256
        (base32 "10ifa4ic5in9v44xgafclh1fxi5py4pyvamdp56gcrhf57m47agm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/MikeMcl/bignumber.js#readme")
    (synopsis
     "A library for arbitrary-precision decimal and non-decimal arithmetic")
    (description
     "A library for arbitrary-precision decimal and non-decimal arithmetic")
    (license license:expat)))

(define-public node-bl-4.1.0
  (package
    (name "node-bl")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bl/-/bl-4.1.0.tgz")
       (sha256
        (base32 "1jx7lm4mr80nzdw0k873llpl1x6i1n0m422v1scwla8qml4vkpl3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape" "faucet" "standard"))))))))
    (inputs (list node-readable-stream-3.6.2 node-inherits-2.0.4
                  node-buffer-5.7.1))
    (home-page "https://github.com/rvagg/bl")
    (synopsis
     "Buffer List: collect buffers and access with a standard readable Buffer interface, streamable too!")
    (description
     "Buffer List: collect buffers and access with a standard readable Buffer interface, streamable too!")
    (license license:expat)))

(define-public node-bn-js-4.12.3
  (package
    (name "node-bn-js")
    (version "4.12.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bn.js/-/bn.js-4.12.3.tgz")
       (sha256
        (base32 "1iny8ivyxzg733zs8wirz4j8jb0fk2x0y63ydprnp3h37dd5328q"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "istanbul"
                                                  "semistandard"))))))))
    (home-page "https://github.com/indutny/bn.js")
    (synopsis "Big number implementation in pure javascript")
    (description "Big number implementation in pure javascript")
    (license license:expat)))

(define-public node-body-parser-2.2.2
  (package
    (name "node-body-parser")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/body-parser/-/body-parser-2.2.2.tgz")
       (sha256
        (base32 "02x3fgd42cf09bdadwb84k7jb31b0nq0m8kkb70aqpc0gqs9g3f2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "supertest"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (inputs (list node-content-type
                  node-on-finished
                  node-http-errors-2.0.1
                  node-iconv-lite-0.7.2
                  node-raw-body-3.0.2
                  node-type-is-2.1.0
                  node-debug-4.4.3
                  node-bytes
                  node-qs-6.14.1))
    (home-page "https://github.com/expressjs/body-parser#readme")
    (synopsis "Node.js body parsing middleware")
    (description "Node.js body parsing middleware")
    (license license:expat)))

(define-public node-boolbase-1.0.0
  (package
    (name "node-boolbase")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/boolbase/-/boolbase-1.0.0.tgz")
       (sha256
        (base32 "1a6aqq33c0srw34mb1qkmrdn286mk3d2j3sax90gjprc17w8yqk9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/fb55/boolbase")
    (synopsis "two functions: One that returns true, one that returns false")
    (description
     "two functions: One that returns true, one that returns false")
    (license license:isc)))

(define-public node-borewit-text-codec-0.2.2
  (package
    (name "node-borewit-text-codec")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@borewit/text-codec/-/text-codec-0.2.2.tgz")
       (sha256
        (base32 "186bb9sxzldy569cp3cdfddils3n2hh7prmksl95m06x79v8f7cr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@biomejs/biome"
                                                  "@types/chai"
                                                  "@types/mocha"
                                                  "chai"
                                                  "mocha"
                                                  "ts-node"
                                                  "typescript"))))))))
    (home-page "https://github.com/Borewit/text-codec#readme")
    (synopsis "Text Decoder")
    (description "Text Decoder")
    (license license:expat)))

(define-public node-bottleneck-2.19.5
  (package
    (name "node-bottleneck")
    (version "2.19.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bottleneck/-/bottleneck-2.19.5.tgz")
       (sha256
        (base32 "0q3hv50a3vprnmibiqr68sik41d2073aqdkx0vg8y5302a5cqbd6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/preset-env"
                                                  "@types/es6-promise"
                                                  "assert"
                                                  "coffeescript"
                                                  "ejs-cli"
                                                  "ioredis"
                                                  "leakage"
                                                  "mocha"
                                                  "redis"
                                                  "regenerator-runtime"
                                                  "rollup"
                                                  "rollup-plugin-babel"
                                                  "rollup-plugin-commonjs"
                                                  "rollup-plugin-json"
                                                  "rollup-plugin-node-resolve"
                                                  "typescript"))))))))
    (home-page "https://github.com/SGrondin/bottleneck#readme")
    (synopsis "Distributed task scheduler and rate limiter")
    (description "Distributed task scheduler and rate limiter")
    (license license:expat)))

(define-public node-bowser-2.14.1
  (package
    (name "node-bowser")
    (version "2.14.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bowser/-/bowser-2.14.1.tgz")
       (sha256
        (base32 "186a857fp2d7sh47byjd1qffcmfrsiwqk920jyphqnz2l5bs40cp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/cli" "@babel/core"
                                                  "@babel/polyfill"
                                                  "@babel/preset-env"
                                                  "@babel/register"
                                                  "ava"
                                                  "babel-eslint"
                                                  "babel-loader"
                                                  "babel-plugin-add-module-exports"
                                                  "babel-plugin-istanbul"
                                                  "compression-webpack-plugin"
                                                  "coveralls"
                                                  "docdash"
                                                  "eslint"
                                                  "eslint-config-airbnb-base"
                                                  "eslint-plugin-ava"
                                                  "eslint-plugin-import"
                                                  "gh-pages"
                                                  "jsdoc"
                                                  "nyc"
                                                  "sinon"
                                                  "testem"
                                                  "webpack"
                                                  "webpack-bundle-analyzer"
                                                  "webpack-cli"
                                                  "yamljs"))))))))
    (home-page "https://github.com/bowser-js/bowser")
    (synopsis "Lightweight browser detector")
    (description "Lightweight browser detector")
    (license license:expat)))

(define-public node-brace-expansion-5.0.6
  (package
    (name "node-brace-expansion")
    (version "5.0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/brace-expansion/-/brace-expansion-5.0.6.tgz")
       (sha256
        (base32 "000692q8z98k7l1xj8na27bm1pv9gvvsjpwi5crss9kcbgswg9gw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/brace-expansion"
                                                  "@types/node"
                                                  "mkdirp"
                                                  "prettier"
                                                  "tap"
                                                  "tshy"
                                                  "typedoc"))))))))
    (inputs (list node-balanced-match-4.0.4))
    (home-page "https://github.com/juliangruber/brace-expansion#readme")
    (synopsis "Brace expansion as known from sh/bash")
    (description "Brace expansion as known from sh/bash")
    (license license:expat)))

(define-public node-buffer-5.7.1
  (package
    (name "node-buffer")
    (version "5.7.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/buffer/-/buffer-5.7.1.tgz")
       (sha256
        (base32 "1g60az00dzb1grcszyg12gyrl9jr9bwvrk2y9xjdwym3nxasrgwq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("airtap" "benchmark"
                                                  "browserify"
                                                  "concat-stream"
                                                  "hyperquest"
                                                  "is-buffer"
                                                  "is-nan"
                                                  "split"
                                                  "standard"
                                                  "tape"
                                                  "through2"
                                                  "uglify-js"))))))))
    (inputs (list node-ieee754-1.2.1 node-base64-js-1.5.1))
    (home-page "https://github.com/feross/buffer")
    (synopsis "Node.js Buffer API, for the browser")
    (description "Node.js Buffer API, for the browser")
    (license license:expat)))

(define-public node-buffer-equal-constant-time-1.0.1
  (package
    (name "node-buffer-equal-constant-time")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/buffer-equal-constant-time/-/buffer-equal-constant-time-1.0.1.tgz")
       (sha256
        (base32 "0np7kzq65a7yvs7ch5vrhm6i9ayv7v3lqspdaiw3w422wdcm2icg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha"))))))))
    (home-page "https://www.npmjs.com/package/node-buffer-equal-constant-time")
    (synopsis "Constant-time comparison of Buffers")
    (description "Constant-time comparison of Buffers")
    (license license:bsd-3)))

(define-public node-bufferutil-4.1.0
  (package
    (name "node-bufferutil")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bufferutil/-/bufferutil-4.1.0.tgz")
       (sha256
        (base32 "11va0j8hb4ipgiqfs34360nrmdpyl6s5mab1z2xbhfc711w5kbqr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "node-gyp"
                                                  "prebuildify"))))))))
    (inputs (list node-node-gyp-build))
    (home-page "https://github.com/websockets/bufferutil")
    (synopsis "WebSocket buffer utils")
    (description "WebSocket buffer utils")
    (license license:expat)))

(define-public node-camelcase-5.3.1
  (package
    (name "node-camelcase")
    (version "5.3.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/camelcase/-/camelcase-5.3.1.tgz")
       (sha256
        (base32 "15l68n2iq0ys0cf49h9adyvwk030kcqwrpalfcpmylc9p15342f6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"))))))))
    (home-page "https://github.com/sindresorhus/camelcase#readme")
    (synopsis
     "Convert a dash/dot/underscore/space separated string to camelCase or PascalCase: `foo-bar` â `fooBar`")
    (description
     "Convert a dash/dot/underscore/space separated string to camelCase or PascalCase: `foo-bar` â `fooBar`")
    (license license:expat)))

(define-public node-canvas-3.2.3
  (package
    (name "node-canvas")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/canvas/-/canvas-3.2.3.tgz")
       (sha256
        (base32 "05v2z6ibqf9ybfdqgdm42dgl5cnh6n0aczbrivd6c5aiyf6ra3lb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'build-from-source
            (lambda _
              (substitute* "package.json"
                (("prebuild-install -r napi \\|\\| node-gyp rebuild")
                 "node-gyp rebuild"))))
          (add-after 'set-home 'set-compiler
            (lambda _
              (setenv "CC" "gcc")
              (setenv "CXX" "g++")))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node"
                                                  "assert-rejects"
                                                  "express"
                                                  "js-yaml"
                                                  "mocha"
                                                  "pixelmatch"
                                                  "standard"
                                                  "tsd"
                                                  "typescript"))))))))
    (native-inputs (list pkg-config python))
    (inputs (list cairo
                  fontconfig
                  freetype
                  giflib
                  libjpeg-turbo
                  libpng
                  node-prebuild-install-7.1.3
                  node-node-addon-api-7.1.1
                  pango))
    (home-page "https://github.com/Automattic/node-canvas")
    (synopsis "Canvas graphics API backed by Cairo")
    (description "Canvas graphics API backed by Cairo")
    (license license:expat)))

(define-public node-cfworker-json-schema-4.1.1
  (package
    (name "node-cfworker-json-schema")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@cfworker/json-schema/-/json-schema-4.1.1.tgz")
       (sha256
        (base32 "00q2c41ji1v1jar17yvd5hz8h9kpbvxnx2m73xkz01858mmhga7k"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/chai" "@types/mocha"
                                                  "chai"
                                                  "json-schema-test-suite"
                                                  "esbuild"
                                                  "mocha"
                                                  "typescript"
                                                  "wrangler"))))))))
    (home-page
     "https://github.com/cfworker/cfworker/tree/master/packages/json-schema/README.md")
    (synopsis
     "A JSON schema validator that will run on Cloudflare workers. Supports drafts 4, 7, 2019-09, and 2020-12.")
    (description
     "A JSON schema validator that will run on Cloudflare workers. Supports drafts 4, 7, 2019-09, and 2020-12.")
    (license license:expat)))

(define-public node-chalk-5.6.2
  (package
    (name "node-chalk")
    (version "5.6.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/chalk/-/chalk-5.6.2.tgz")
       (sha256
        (base32 "1zagawvlzqw1xwp9hzs0bh1dh9w297aj53qcnsfkpal3lhapg5cl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "xo"
                                                  "ava"
                                                  "tsd"
                                                  "execa"
                                                  "matcha"
                                                  "log-update"
                                                  "yoctodelay"
                                                  "@types/node"
                                                  "color-convert"))))))))
    (home-page "https://github.com/chalk/chalk#readme")
    (synopsis "Terminal string styling done right")
    (description "Terminal string styling done right")
    (license license:expat)))

(define-public node-chokidar-5.0.0
  (package
    (name "node-chokidar")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/chokidar/-/chokidar-5.0.0.tgz")
       (sha256
        (base32 "1qzmyw8jg7gr3zj0f593phii9p4yqiy58g5b6g3q5r3ysnkpxl25"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("upath" "tinyspy" "prettier"
                                                  "typescript" "@types/node"
                                                  "@paulmillr/jsbt"))))))))
    (inputs (list node-readdirp-5.0.0))
    (home-page "https://github.com/paulmillr/chokidar")
    (synopsis "Minimal and efficient cross-platform file watching library")
    (description "Minimal and efficient cross-platform file watching library")
    (license license:expat)))

(define-public node-chownr-1.1.4
  (package
    (name "node-chownr")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/chownr/-/chownr-1.1.4.tgz")
       (sha256
        (base32 "0gl3b5fqhvgq3glfdq341jmkyi7hmw4kdg4wd3pis0sdsc2kx1kj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mkdirp" "rimraf" "tap"))))))))
    (home-page "https://github.com/isaacs/chownr#readme")
    (synopsis "like `chown -R`")
    (description "like `chown -R`")
    (license license:isc)))

(define-public node-chownr-3.0.0
  (package
    (name "node-chownr")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/chownr/-/chownr-3.0.0.tgz")
       (sha256
        (base32 "0q0k3cr7h7bmjxc85a2hji3ym47hgmgnbpl9v992451hlvdga92c"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "mkdirp"
                                                  "prettier"
                                                  "rimraf"
                                                  "tap"
                                                  "tshy"
                                                  "typedoc"))))))))
    (home-page "https://github.com/isaacs/chownr#readme")
    (synopsis "like `chown -R`")
    (description "like `chown -R`")
    (license license:blue-oak1.0.0)))

(define-public node-clack-core-1.3.1
  (package
    (name "node-clack-core")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@clack/core/-/core-1.3.1.tgz")
       (sha256
        (base32 "0zxvab1abkpdwkmhdiygq7bnv1vl4a4gziwlra4zbcb58ada8ci4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("vitest"))))))))
    (inputs (list node-sisteransi-1.0.5 node-fast-wrap-ansi-0.2.2))
    (home-page
     "https://github.com/bombshell-dev/clack/tree/main/packages/core#readme")
    (synopsis
     "Clack contains low-level primitives for implementing your own command-line applications.")
    (description
     "Clack contains low-level primitives for implementing your own command-line applications.")
    (license license:expat)))

(define-public node-clack-prompts-1.4.0
  (package
    (name "node-clack-prompts")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@clack/prompts/-/prompts-1.4.0.tgz")
       (sha256
        (base32 "1q7lk6q4vaf00ww8y0zyg22jzb3sqb72mm4ynbsjv2hpidlnyrd0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("is-unicode-supported"
                                                  "memfs" "vitest"
                                                  "vitest-ansi-serializer"))))))))
    (inputs (list node-clack-core-1.3.1 node-sisteransi-1.0.5
                  node-fast-wrap-ansi-0.2.2 node-fast-string-width-3.0.2))
    (home-page
     "https://github.com/bombshell-dev/clack/tree/main/packages/prompts#readme")
    (synopsis
     "Effortlessly build beautiful command-line apps ðª [Try the demo](https://stackblitz.com/edit/clack-prompts?file=index.js)")
    (description
     "Effortlessly build beautiful command-line apps ðª [Try the demo](https://stackblitz.com/edit/clack-prompts?file=index.js)")
    (license license:expat)))

(define-public node-cliui-6.0.0
  (package
    (name "node-cliui")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cliui/-/cliui-6.0.0.tgz")
       (sha256
        (base32 "1n6b3bqgzapskhabby3c8hcdmfiv3da7wvn42sw8x3rkafbzr2sk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "chai" "chalk" "mocha"
                                                  "standard" "coveralls"))))))))
    (inputs (list node-string-width node-strip-ansi
                  node-wrap-ansi-6.2.0))
    (home-page "https://github.com/yargs/cliui#readme")
    (synopsis "easily create complex multi-column command-line-interfaces")
    (description "easily create complex multi-column command-line-interfaces")
    (license license:isc)))

(define-public node-cliui-8.0.1
  (package
    (name "node-cliui")
    (version "8.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cliui/-/cliui-8.0.1.tgz")
       (sha256
        (base32 "13cc8rvmzcvvlvf2prwxj1zjyxhybsznl1nirnlrwd4abvid3yny"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "gts"
                                                  "chai"
                                                  "chalk"
                                                  "mocha"
                                                  "eslint"
                                                  "rimraf"
                                                  "rollup"
                                                  "cross-env"
                                                  "standardx"
                                                  "typescript"
                                                  "@types/node"
                                                  "rollup-plugin-ts"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-string-width node-strip-ansi
                  node-wrap-ansi))
    (home-page "https://github.com/yargs/cliui#readme")
    (synopsis "easily create complex multi-column command-line-interfaces")
    (description "easily create complex multi-column command-line-interfaces")
    (license license:isc)))

(define-public node-commander-14.0.3
  (package
    (name "node-commander")
    (version "14.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/commander/-/commander-14.0.3.tgz")
       (sha256
        (base32 "0v1gv6m55pdh5rqyjyxv6zy4iv7rl6yxhh0kn17343qfa0y70jaq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tsd" "jest"
                                                  "eslint"
                                                  "globals"
                                                  "ts-jest"
                                                  "prettier"
                                                  "@eslint/js"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "typescript-eslint"
                                                  "eslint-plugin-jest"
                                                  "eslint-config-prettier"))))))))
    (home-page "https://github.com/tj/commander.js#readme")
    (synopsis "the complete solution for node.js command-line programs")
    (description "the complete solution for node.js command-line programs")
    (license license:expat)))

(define-public node-content-disposition-1.1.0
  (package
    (name "node-content-disposition")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/content-disposition/-/content-disposition-1.1.0.tgz")
       (sha256
        (base32 "0g09yjp20gl96bzgyq18bzc1vz5a23y244y10rkah8xim0jik2rg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "eslint"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/content-disposition#readme")
    (synopsis "Create and parse Content-Disposition header")
    (description "Create and parse Content-Disposition header")
    (license license:expat)))

(define-public node-content-type-2.0.0
  (package
    (name "node-content-type")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/content-type/-/content-type-2.0.0.tgz")
       (sha256
        (base32 "01mwl6jp5rqkwinh99cl76qlnbcz4w0qxx9ra0n2rppl8jfnim52"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@borderless/ts-scripts"
                                                  "@vitest/coverage-v8"
                                                  "typescript" "vitest"))))))))
    (home-page "https://github.com/jshttp/content-type#readme")
    (synopsis "Create and parse HTTP Content-Type header")
    (description "Create and parse HTTP Content-Type header")
    (license license:expat)))

(define-public node-core-util-is-1.0.3
  (package
    (name "node-core-util-is")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/core-util-is/-/core-util-is-1.0.3.tgz")
       (sha256
        (base32 "032dwykfbxff1q7s0kgqdkwwzmm25mlrlfqijzibbwrc3z3zsc24"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap"))))))))
    (home-page "https://github.com/isaacs/core-util-is#readme")
    (synopsis "The `util.is*` functions introduced in Node v0.12.")
    (description "The `util.is*` functions introduced in Node v0.12.")
    (license license:expat)))

(define-public node-cors-2.8.6
  (package
    (name "node-cors")
    (version "2.8.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cors/-/cors-2.8.6.tgz")
       (sha256
        (base32 "1zy1q5b9ny116yfmsr8vr1c9srklrns558iq28780h3i74j2291j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("after" "eslint" "express"
                                                  "mocha" "nyc" "supertest"))))))))
    (inputs (list node-vary node-object-assign))
    (home-page "https://github.com/expressjs/cors#readme")
    (synopsis "Node.js CORS middleware")
    (description "Node.js CORS middleware")
    (license license:expat)))

(define-public node-croner-10.0.1
  (package
    (name "node-croner")
    (version "10.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/croner/-/croner-10.0.1.tgz")
       (sha256
        (base32 "1vhcn3y7r8cc4hlgwqxvaqnyfx5a3pzpd9c6p5kxbzi5c4l3rnfn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://croner.56k.guru")
    (synopsis
     "Trigger functions and/or evaluate cron expressions in JavaScript. No dependencies. Most features. All environments.")
    (description
     "Trigger functions and/or evaluate cron expressions in JavaScript. No dependencies. Most features. All environments.")
    (license license:expat)))

(define-public node-cross-spawn-7.0.6
  (package
    (name "node-cross-spawn")
    (version "7.0.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cross-spawn/-/cross-spawn-7.0.6.tgz")
       (sha256
        (base32 "1siqxlydjwpihy7klgd15cah56vsmxrdm3q90gndyfj1vh63530q"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "husky"
                                                  "eslint"
                                                  "mkdirp"
                                                  "rimraf"
                                                  "babel-core"
                                                  "babel-jest"
                                                  "lint-staged"
                                                  "@commitlint/cli"
                                                  "standard-version"
                                                  "babel-preset-moxy"
                                                  "eslint-config-moxy"
                                                  "@commitlint/config-conventional"))))))))
    (inputs (list node-shebang-command-2.0.0 node-path-key-3.1.1
                  node-which-2.0.2))
    (home-page "https://github.com/moxystudio/node-cross-spawn")
    (synopsis "Cross platform child_process#spawn and child_process#spawnSync")
    (description
     "Cross platform child_process#spawn and child_process#spawnSync")
    (license license:expat)))

(define-public node-css-select-5.2.2
  (package
    (name "node-css-select")
    (version "5.2.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/css-select/-/css-select-5.2.2.tgz")
       (sha256
        (base32 "0xdfnahx0iq1jjqg88wnkvnq0z58dms6d7skpgj59hxb8yf2lg5b"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "eslint"
                                                  "ts-jest"
                                                  "prettier"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "htmlparser2"
                                                  "@types/boolbase"
                                                  "cheerio-soupselect"
                                                  "eslint-config-prettier"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-domhandler-5.0.3 node-nth-check-2.1.1
                  node-domutils-3.2.2 node-css-what-6.2.2 node-boolbase-1.0.0))
    (home-page "https://github.com/fb55/css-select#readme")
    (synopsis "a CSS selector compiler/engine")
    (description "a CSS selector compiler/engine")
    (license license:bsd-2)))

(define-public node-css-what-6.2.2
  (package
    (name "node-css-what")
    (version "6.2.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/css-what/-/css-what-6.2.2.tgz")
       (sha256
        (base32 "1c3s4apsrvi2y1awmx1fv1f55b650cszlwqx8kigdd84dhfzapi7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "eslint"
                                                  "ts-jest"
                                                  "prettier"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "eslint-plugin-node"
                                                  "eslint-config-prettier"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://github.com/fb55/css-what#readme")
    (synopsis "a CSS selector parser")
    (description "a CSS selector parser")
    (license license:bsd-2)))

(define-public node-cssom-0.5.0
  (package
    (name "node-cssom")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cssom/-/cssom-0.5.0.tgz")
       (sha256
        (base32 "16g5id1q40rlsynfxixvxin5m5rpg5sxnsk0b8c2zlp6k458fp9w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/NV/CSSOM#readme")
    (synopsis "CSS Object Model implementation and CSS parser")
    (description "CSS Object Model implementation and CSS parser")
    (license license:expat)))

(define-public node-data-uri-to-buffer-4.0.1
  (package
    (name "node-data-uri-to-buffer")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/data-uri-to-buffer/-/data-uri-to-buffer-4.0.1.tgz")
       (sha256
        (base32 "18a22rwk14m78xxhh8kkqkhp9651ghpy3xrgavxjj2sxfwdjnx55"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "ts-jest"
                                                  "typescript" "@types/jest"
                                                  "@types/node"))))))))
    (home-page "https://github.com/TooTallNate/node-data-uri-to-buffer")
    (synopsis "Generate a Buffer instance from a Data URI string")
    (description "Generate a Buffer instance from a Data URI string")
    (license license:expat)))

(define-public node-debug-4.4.3
  (package
    (name "node-debug")
    (version "4.4.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/debug/-/debug-4.4.3.tgz")
       (sha256
        (base32 "19z48fpic8jbb2833gh3bviylzp9512i2dsqhxd91s3fjjfarhc9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("brfs" "browserify"
                                                  "coveralls"
                                                  "karma"
                                                  "karma-browserify"
                                                  "karma-chrome-launcher"
                                                  "karma-mocha"
                                                  "mocha"
                                                  "mocha-lcov-reporter"
                                                  "sinon"
                                                  "xo"))))))))
    (inputs (list node-ms-2.1.3))
    (home-page "https://github.com/debug-js/debug#readme")
    (synopsis "Lightweight debugging utility for Node.js and the browser")
    (description "Lightweight debugging utility for Node.js and the browser")
    (license license:expat)))

(define-public node-decamelize-1.2.0
  (package
    (name "node-decamelize")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/decamelize/-/decamelize-1.2.0.tgz")
       (sha256
        (base32 "0r187qd80plv8mm8riqk3xcmpip3zcpsgjrvf013m37323syzbdl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"))))))))
    (home-page "https://github.com/sindresorhus/decamelize#readme")
    (synopsis
     "Convert a camelized string into a lowercased one with a custom separator: unicornRainbow â unicorn_rainbow")
    (description
     "Convert a camelized string into a lowercased one with a custom separator: unicornRainbow â unicorn_rainbow")
    (license license:expat)))

(define-public node-decompress-response-6.0.0
  (package
    (name "node-decompress-response")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/decompress-response/-/decompress-response-6.0.0.tgz")
       (sha256
        (base32 "0krv58fihkajrskzakm1i69kklv0ycabrjmlgk2a9kkv0idlqznm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd" "pify"
                                                  "get-stream" "@types/node"))))))))
    (inputs (list node-mimic-response-3.1.0))
    (home-page "https://github.com/sindresorhus/decompress-response#readme")
    (synopsis "Decompress a HTTP response if needed")
    (description "Decompress a HTTP response if needed")
    (license license:expat)))

(define-public node-detect-libc-2.1.2
  (package
    (name "node-detect-libc")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/detect-libc/-/detect-libc-2.1.2.tgz")
       (sha256
        (base32 "09wlldyqvhf2w7q4xcch4xh6pvldy7c2vbx83m48dzvcq07yq397"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "benchmark"
                                                  "conventional-changelog-cli"
                                                  "eslint-config-standard"
                                                  "nyc"
                                                  "proxyquire"
                                                  "semistandard"))))))))
    (home-page "https://github.com/lovell/detect-libc#readme")
    (synopsis
     "Node.js module to detect the C standard library (libc) implementation family and version")
    (description
     "Node.js module to detect the C standard library (libc) implementation family and version")
    (license license:asl2.0)))

(define-public node-diff-8.0.4
  (package
    (name "node-diff")
    (version "8.0.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/diff/-/diff-8.0.4.tgz")
       (sha256
        (base32 "1mlkjmimccf2yw8wrbqgpn0940m5p9g9y8966d9wyhw2smx34lgg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "tsd"
                                                  "chai"
                                                  "karma"
                                                  "mocha"
                                                  "eslint"
                                                  "rollup"
                                                  "globals"
                                                  "webpack"
                                                  "cross-env"
                                                  "uglify-js"
                                                  "@eslint/js"
                                                  "typescript"
                                                  "@babel/core"
                                                  "karma-mocha"
                                                  "babel-loader"
                                                  "karma-webpack"
                                                  "@colors/colors"
                                                  "@babel/register"
                                                  "@babel/preset-env"
                                                  "typescript-eslint"
                                                  "webpack-dev-server"
                                                  "karma-mocha-reporter"
                                                  "@arethetypeswrong/cli"
                                                  "babel-plugin-istanbul"
                                                  "karma-sourcemap-loader"))))))))
    (home-page "https://www.npmjs.com/package/node-diff")
    (synopsis "A JavaScript text diff implementation.")
    (description "A JavaScript text diff implementation.")
    (license license:bsd-3)))

(define-public node-dijkstrajs-1.0.3
  (package
    (name "node-dijkstrajs")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/dijkstrajs/-/dijkstrajs-1.0.3.tgz")
       (sha256
        (base32 "002czy5ryz3jnnj9c295gfhvh90bfw91krldgci9qacqmf39h507"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("expect.js" "jshint" "mocha"))))))))
    (home-page "https://github.com/tcort/dijkstrajs")
    (synopsis
     "A simple JavaScript implementation of Dijkstra's single-source shortest-paths algorithm.")
    (description
     "A simple JavaScript implementation of Dijkstra's single-source shortest-paths algorithm.")
    (license license:expat)))

(define-public node-dom-serializer-2.0.0
  (package
    (name "node-dom-serializer")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/dom-serializer/-/dom-serializer-2.0.0.tgz")
       (sha256
        (base32 "16gkx3lnppbrgr01ghx70xgrdp97xnqhzmkbag738szw8kqi2ig3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "eslint"
                                                  "cheerio"
                                                  "ts-jest"
                                                  "prettier"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "htmlparser2"
                                                  "eslint-config-prettier"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-domelementtype-2.3.0 node-domhandler-5.0.3
                  node-entities-4.5.0))
    (home-page "https://github.com/cheeriojs/dom-serializer#readme")
    (synopsis "render domhandler DOM nodes to a string")
    (description "render domhandler DOM nodes to a string")
    (license license:expat)))

(define-public node-domelementtype-2.3.0
  (package
    (name "node-domelementtype")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/domelementtype/-/domelementtype-2.3.0.tgz")
       (sha256
        (base32 "0igbbzi58harf2sbp1j4zgk7lh0maig2x34nppnfmsllm6s68wf6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint" "prettier"
                                                  "typescript"
                                                  "eslint-config-prettier"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://github.com/fb55/domelementtype#readme")
    (synopsis "all the types of nodes in htmlparser2's dom")
    (description "all the types of nodes in htmlparser2's dom")
    (license license:bsd-2)))

(define-public node-domhandler-5.0.3
  (package
    (name "node-domhandler")
    (version "5.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/domhandler/-/domhandler-5.0.3.tgz")
       (sha256
        (base32 "1127rbb67ldxj9b1lyni908sdazxn77nhbc28blxhd96gsxjm5gk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "eslint"
                                                  "ts-jest"
                                                  "prettier"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "htmlparser2"
                                                  "eslint-config-prettier"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-domelementtype-2.3.0))
    (home-page "https://github.com/fb55/domhandler#readme")
    (synopsis "Handler for htmlparser2 that turns pages into a dom")
    (description "Handler for htmlparser2 that turns pages into a dom")
    (license license:bsd-2)))

(define-public node-domutils-3.2.2
  (package
    (name "node-domutils")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/domutils/-/domutils-3.2.2.tgz")
       (sha256
        (base32 "0h6129g00ixz6iplfvzh8cjgp670ci0rarink3f3s2by7shiha97"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "eslint"
                                                  "ts-jest"
                                                  "typedoc"
                                                  "prettier"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "htmlparser2"
                                                  "eslint-plugin-jsdoc"
                                                  "eslint-config-prettier"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-domelementtype-2.3.0 node-dom-serializer-2.0.0
                  node-domhandler-5.0.3))
    (home-page "https://github.com/fb55/domutils#readme")
    (synopsis "Utilities for working with htmlparser2's dom")
    (description "Utilities for working with htmlparser2's dom")
    (license license:bsd-2)))

(define-public node-dotenv-17.4.2
  (package
    (name "node-dotenv")
    (version "17.4.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/dotenv/-/dotenv-17.4.2.tgz")
       (sha256
        (base32 "160kmw4fg071z4gdv3nckyp7zkhjxp1xqxfa9nri1490x0mqaj46"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "sinon"
                                                  "decache"
                                                  "standard"
                                                  "typescript"
                                                  "@types/node"
                                                  "standard-version"))))))))
    (home-page "https://github.com/motdotla/dotenv#readme")
    (synopsis "Loads environment variables from .env file")
    (description "Loads environment variables from .env file")
    (license license:bsd-2)))

(define-public node-earendil-works-pi-agent-core-0.75.4
  (package
    (name "node-earendil-works-pi-agent-core")
    (version "0.75.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.75.4.tgz")
       (sha256
        (base32 "0kb8ikgjks6w5mkv324923yqfnz0wms0n4yli22qcnica7bfcqgv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("vitest" "typescript"
                                                  "@types/node"
                                                  "@vitest/coverage-v8"))))))))
    (inputs (list node-earendil-works-pi-ai-0.75.4 node-typebox-1.1.38
                  node-ignore-7.0.5 node-yaml-2.9.0))
    (home-page "https://github.com/earendil-works/pi-mono#readme")
    (synopsis
     "General-purpose agent with transport abstraction, state management, and attachment support")
    (description
     "General-purpose agent with transport abstraction, state management, and attachment support")
    (license license:expat)))

(define-public node-earendil-works-pi-ai-0.75.4
  (package
    (name "node-earendil-works-pi-ai")
    (version "0.75.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.75.4.tgz")
       (sha256
        (base32 "11zczs3jcaxw1nj1bb5zjs1dznl388d7gdqmfqa8i4k19hhvyx5s"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("canvas" "vitest"
                                                  "@types/node"))))))))
    (inputs (list node-aws-sdk-client-bedrock-runtime-3.1048.0
                  node-mistralai-mistralai-2.2.1
                  node-https-proxy-agent
                  node-anthropic-ai-sdk-0.91.1
                  node-http-proxy-agent-7.0.2
                  node-google-genai-1.52.0
                  node-partial-json-0.1.7
                  node-typebox-1.1.38
                  node-openai-6.26.0))
    (home-page "https://github.com/earendil-works/pi-mono#readme")
    (synopsis
     "Unified LLM API with automatic model discovery and provider configuration")
    (description
     "Unified LLM API with automatic model discovery and provider configuration")
    (license license:expat)))

(define-public node-graceful-fs-4.2.11
  (package
    (name "node-graceful-fs")
    (version "4.2.11")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.2.11.tgz")
       (sha256
        (base32 "1709vla02prpbf34xqsvkqngvsmp5ypnljvg1pcgxrk1l553fq9r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("import-fresh" "mkdirp"
                                                  "rimraf" "tap"))))))))
    (home-page "https://github.com/isaacs/node-graceful-fs#readme")
    (synopsis "Drop-in replacement for fs")
    (description "Drop-in replacement for fs.")
    (license license:isc)))

(define-public node-retry-0.12.0
  (package
    (name "node-retry")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/retry/-/retry-0.12.0.tgz")
       (sha256
        (base32 "0a5l61f1aqn124j25m2q6m0j60mv7d9h74a8gfqnmp5ajz8wcqfz"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("fake" "istanbul" "tape"))))))))
    (home-page "https://github.com/tim-kos/node-retry")
    (synopsis "Abstraction for retry strategies")
    (description
     "Abstraction for exponential and custom retry strategies for failed operations.")
    (license license:expat)))

(define-public node-signal-exit-3.0.7
  (package
    (name "node-signal-exit")
    (version "3.0.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/signal-exit/-/signal-exit-3.0.7.tgz")
       (sha256
        (base32 "1a10ixkiak24yy6s7p9m7c6v9jkz2fm7wxgc2l3614dbdbx275j3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "coveralls" "nyc"
                                                  "standard-version" "tap"))))))))
    (home-page "https://github.com/tapjs/signal-exit")
    (synopsis "Fire an event on process exit")
    (description "Fire an event no matter how a process exits.")
    (license license:isc)))

(define-public node-proper-lockfile-4.1.2
  (package
    (name "node-proper-lockfile")
    (version "4.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/proper-lockfile/-/proper-lockfile-4.1.2.tgz")
       (sha256
        (base32 "0s49g8x645nacdxwmyy0w4rgl08ba5lv73wjpgnwpbkwlm5r2y7x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@commitlint/cli"
                                                  "@commitlint/config-conventional"
                                                  "@segment/clear-timeouts"
                                                  "delay"
                                                  "eslint"
                                                  "eslint-config-moxy"
                                                  "execa"
                                                  "husky"
                                                  "jest"
                                                  "lint-staged"
                                                  "mkdirp"
                                                  "p-defer"
                                                  "rimraf"
                                                  "stable"
                                                  "standard-version"
                                                  "thread-sleep"))))))))
    (inputs (list node-signal-exit-3.0.7 node-retry-0.12.0
                  node-graceful-fs-4.2.11))
    (home-page "https://github.com/moxystudio/node-proper-lockfile")
    (synopsis "Inter-process and inter-machine lockfile utility")
    (description
     "Inter-process and inter-machine lockfile utility that works on local or network file systems.")
    (license license:expat)))

(define-public node-earendil-works-pi-coding-agent-0.75.4
  (package
    (name "node-earendil-works-pi-coding-agent")
    (version "0.75.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-coding-agent/-/pi-coding-agent-0.75.4.tgz")
       (sha256
        (base32 "15yn32wbkbhl5gi2nl43gv1lvn53sqz8bfp7fnv2qjw2l9sbaryp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("shx" "vitest"
                                                  "@types/ms"
                                                  "typescript"
                                                  "@types/diff"
                                                  "@types/node"
                                                  "@types/cross-spawn"
                                                  "@types/hosted-git-info"
                                                  "@types/proper-lockfile"))))))))
    (inputs (list node-earendil-works-pi-agent-core-0.75.4
                  node-silvia-odwyer-photon-node-0.3.4
                  node-earendil-works-pi-tui-0.75.4
                  node-earendil-works-pi-ai-0.75.4
                  node-proper-lockfile-4.1.2
                  node-hosted-git-info-9.0.3
                  node-highlight-js
                  node-cross-spawn-7.0.6
                  node-minimatch-10.2.5
                  node-typebox-1.1.38
                  node-undici-8.3.0
                  node-ignore-7.0.5
                  node-chalk-5.6.2
                  node-yaml-2.9.0
                  node-jiti-2.7.0
                  node-glob-13.0.6
                  node-diff-8.0.4))
    (home-page "https://github.com/earendil-works/pi-mono#readme")
    (synopsis
     "Coding agent CLI with read, bash, edit, write tools and session management")
    (description
     "Coding agent CLI with read, bash, edit, write tools and session management")
    (license license:expat)))

(define-public node-earendil-works-pi-tui-0.75.4
  (package
    (name "node-earendil-works-pi-tui")
    (version "0.75.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.75.4.tgz")
       (sha256
        (base32 "0idd9gs7js5nbr2q7rwqln930xbpzhvlhvzixpr2jk77fs7cwcpm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chalk" "@xterm/xterm"
                                                  "@xterm/headless"))))))))
    (inputs (list node-get-east-asian-width-1.6.0 node-marked))
    (home-page "https://github.com/earendil-works/pi-mono#readme")
    (synopsis
     "Terminal User Interface library with differential rendering for efficient text-based applications")
    (description
     "Terminal User Interface library with differential rendering for efficient text-based applications")
    (license license:expat)))

(define-public node-ecdsa-sig-formatter-1.0.11
  (package
    (name "node-ecdsa-sig-formatter")
    (version "1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/ecdsa-sig-formatter/-/ecdsa-sig-formatter-1.0.11.tgz")
       (sha256
        (base32 "1zj8r1gp6vg3as5d0qs2qsycn0qwwkjaaj5n7cn7f514zx6vjz28"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("bench" "chai"
                                                  "coveralls"
                                                  "eslint"
                                                  "eslint-config-brightspace"
                                                  "istanbul"
                                                  "jwk-to-pem"
                                                  "mocha"
                                                  "native-crypto"))))))))
    (inputs (list node-safe-buffer-5.2.1))
    (home-page
     "https://github.com/Brightspace/node-ecdsa-sig-formatter#readme")
    (synopsis
     "Translate ECDSA signatures between ASN.1/DER and JOSE-style concatenation")
    (description
     "Translate ECDSA signatures between ASN.1/DER and JOSE-style concatenation")
    (license license:asl2.0)))

(define-public node-end-of-stream-1.4.5
  (package
    (name "node-end-of-stream")
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/end-of-stream/-/end-of-stream-1.4.5.tgz")
       (sha256
        (base32 "02d3hkin7l3ac84mn3icdm1dp30khbx9v5x4ndvjnr6w61h2m73d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape"))))))))
    (inputs (list node-once-1.4.0))
    (home-page "https://github.com/mafintosh/end-of-stream")
    (synopsis
     "Call a callback when a readable/writable/duplex stream has completed or failed.")
    (description
     "Call a callback when a readable/writable/duplex stream has completed or failed.")
    (license license:expat)))

(define-public node-entities-4.5.0
  (package
    (name "node-entities")
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/entities/-/entities-4.5.0.tgz")
       (sha256
        (base32 "0cm6cgsfcsgk3djx00wiv3vfrrq9kwlvkny83yxna2qs6pvn033b"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "eslint"
                                                  "ts-jest"
                                                  "typedoc"
                                                  "prettier"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "eslint-plugin-node"
                                                  "eslint-config-prettier"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://github.com/fb55/entities#readme")
    (synopsis "Encode & decode XML and HTML entities with ease & speed")
    (description "Encode & decode XML and HTML entities with ease & speed")
    (license license:bsd-2)))

(define-public node-entities-7.0.1
  (package
    (name "node-entities")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/entities/-/entities-7.0.1.tgz")
       (sha256
        (base32 "1843sjzia0074bh93aighhlnhz6p8bx12h3i742dm7psg4m7wjsm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("he" "tsx"
                                                  "tshy"
                                                  "eslint"
                                                  "vitest"
                                                  "typedoc"
                                                  "@types/he"
                                                  "tinybench"
                                                  "typescript"
                                                  "@types/node"
                                                  "html-entities"
                                                  "@biomejs/biome"
                                                  "parse-entities"
                                                  "eslint-plugin-n"
                                                  "@vitest/coverage-v8"
                                                  "eslint-config-biome"
                                                  "eslint-plugin-unicorn"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://github.com/fb55/entities#readme")
    (synopsis "Encode & decode XML and HTML entities with ease & speed")
    (description "Encode & decode XML and HTML entities with ease & speed")
    (license license:bsd-2)))

(define-public node-eventsource-3.0.7
  (package
    (name "node-eventsource")
    (version "3.0.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/eventsource/-/eventsource-3.0.7.tgz")
       (sha256
        (base32 "14hw12k1s7h7bdh5x7sdlx4ic9p4dw6mb7ppbafb6nbf36xx8qkw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tsx" "sinon"
                                                  "eslint"
                                                  "rimraf"
                                                  "undici"
                                                  "esbuild"
                                                  "prettier"
                                                  "playwright"
                                                  "typescript"
                                                  "@types/sinon"
                                                  "semantic-release"
                                                  "@sanity/pkg-utils"
                                                  "@tsconfig/strictest"
                                                  "eventsource-encoder"
                                                  "eslint-config-sanity"
                                                  "eslint-config-prettier"
                                                  "rollup-plugin-visualizer"
                                                  "@typescript-eslint/parser"
                                                  "@sanity/semantic-release-preset"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-eventsource-parser-3.0.8))
    (home-page "https://github.com/EventSource/eventsource#readme")
    (synopsis
     "WhatWG/W3C compliant EventSource client for Node.js and browsers")
    (description
     "WhatWG/W3C compliant EventSource client for Node.js and browsers")
    (license license:expat)))

(define-public node-eventsource-parser-3.0.8
  (package
    (name "node-eventsource-parser")
    (version "3.0.8")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/eventsource-parser/-/eventsource-parser-3.0.8.tgz")
       (sha256
        (base32 "0l2ylzx59aayg43a8qx0kcapcwc9pnl7ag85xg9789cv61ylfmnd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@sanity/pkg-utils"
                                                  "@sanity/semantic-release-preset"
                                                  "@sanity/tsconfig"
                                                  "@types/node"
                                                  "eventsource-encoder"
                                                  "knip"
                                                  "mitata"
                                                  "oxfmt"
                                                  "oxlint"
                                                  "rimraf"
                                                  "rollup-plugin-visualizer"
                                                  "semantic-release"
                                                  "terser"
                                                  "typescript"
                                                  "vitest"))))))))
    (home-page "https://github.com/rexxars/eventsource-parser#readme")
    (synopsis
     "Streaming, source-agnostic EventSource/Server-Sent Events parser")
    (description
     "Streaming, source-agnostic EventSource/Server-Sent Events parser")
    (license license:expat)))

(define-public node-expand-template-2.0.3
  (package
    (name "node-expand-template")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/expand-template/-/expand-template-2.0.3.tgz")
       (sha256
        (base32 "1pz0q0dhvi52icyjd1szsd24y3waadcdkhfn2hn6yys4kk0r4m8k"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("standard" "tape"))))))))
    (home-page "https://github.com/ralphtheninja/expand-template")
    (synopsis "Expand placeholders in a template string")
    (description "Expand placeholders in a template string")
    (license #f)))

(define-public node-express-5.2.1
  (package
    (name "node-express")
    (version "5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/express/-/express-5.2.1.tgz")
       (sha256
        (base32 "01fhbm4bndc5d6sign60v8mh5xv828hlv74v8x9jchml09na2wqp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ejs" "hbs"
                                                  "nyc"
                                                  "after"
                                                  "mocha"
                                                  "vhost"
                                                  "eslint"
                                                  "marked"
                                                  "morgan"
                                                  "supertest"
                                                  "connect-redis"
                                                  "cookie-parser"
                                                  "cookie-session"
                                                  "express-session"
                                                  "method-override"
                                                  "pbkdf2-password"))))))))
    (inputs (list node-content-disposition-1.1.0
                  node-merge-descriptors
                  node-cookie-signature
                  node-serve-static-2.2.1
                  node-range-parser
                  node-finalhandler-2.1.1
                  node-content-type
                  node-on-finished
                  node-http-errors-2.0.1
                  node-escape-html
                  node-body-parser-2.2.2
                  node-proxy-addr
                  node-mime-types-3.0.2
                  node-encodeurl
                  node-statuses-2.0.2
                  node-parseurl
                  node-type-is-2.1.0
                  node-accepts
                  node-router
                  node-cookie
                  node-fresh
                  node-debug-4.4.3
                  node-vary
                  node-send-1.2.1
                  node-once-1.4.0
                  node-etag
                  node-depd
                  node-qs-6.14.1))
    (home-page "https://expressjs.com/")
    (synopsis "Fast, unopinionated, minimalist web framework")
    (description "Fast, unopinionated, minimalist web framework")
    (license license:expat)))

(define-public node-express-rate-limit-8.5.2
  (package
    (name "node-express-rate-limit")
    (version "8.5.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/express-rate-limit/-/express-rate-limit-8.5.2.tgz")
       (sha256
        (base32 "14rs37ck9n7j3kzafp63jz512k3agcvpz3rarg08y4wcg588qdxm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@biomejs/biome"
                                                  "@express-rate-limit/prettier"
                                                  "@express-rate-limit/tsconfig"
                                                  "@jest/globals"
                                                  "@types/express"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@types/supertest"
                                                  "del-cli"
                                                  "dts-bundle-generator"
                                                  "esbuild"
                                                  "express"
                                                  "husky"
                                                  "jest"
                                                  "lint-staged"
                                                  "mintlify"
                                                  "npm-run-all"
                                                  "prettier"
                                                  "ratelimit-header-parser"
                                                  "supertest"
                                                  "ts-jest"
                                                  "ts-node"
                                                  "typescript"))))))))
    (inputs (list node-ip-address-10.2.0 node-express-5.2.1))
    (home-page "https://github.com/express-rate-limit/express-rate-limit")
    (synopsis
     "Basic IP rate-limiting middleware for Express. Use to limit repeated requests to public APIs and/or endpoints such as password reset.")
    (description
     "Basic IP rate-limiting middleware for Express. Use to limit repeated requests to public APIs and/or endpoints such as password reset.")
    (license license:expat)))

(define-public node-extend-3.0.2
  (package
    (name "node-extend")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/extend/-/extend-3.0.2.tgz")
       (sha256
        (base32 "1ckjrzapv4awrafybcvq3n5rcqm6ljswfdx97wibl355zaqd148x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ljharb/eslint-config"
                                                  "covert" "eslint" "jscs"
                                                  "tape"))))))))
    (home-page "https://github.com/justmoon/node-extend#readme")
    (synopsis "Port of jQuery.extend for node.js and the browser")
    (description "Port of jQuery.extend for node.js and the browser")
    (license license:expat)))

(define-public node-fast-string-truncated-width-3.0.3
  (package
    (name "node-fast-string-truncated-width")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/fast-string-truncated-width/-/fast-string-truncated-width-3.0.3.tgz")
       (sha256
        (base32 "1cay7d986m3yy4rkh8ybcn54n4jjj10j1fjxxm9v8w1l88swxnmm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("benchloop" "fava" "tsex"
                                                  "typescript"))))))))
    (home-page
     "https://github.com/fabiospampinato/fast-string-truncated-width#readme")
    (synopsis
     "A fast function for calculating where a string should be truncated, given an optional width limit and an ellipsis string.")
    (description
     "A fast function for calculating where a string should be truncated, given an optional width limit and an ellipsis string.")
    (license license:expat)))

(define-public node-fast-string-width-3.0.2
  (package
    (name "node-fast-string-width")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/fast-string-width/-/fast-string-width-3.0.2.tgz")
       (sha256
        (base32 "0akmnc694qnsl8vdmiargfh9va9wd1i5zzr7l9ahx6r0vw9z3nkj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("fava" "tsex" "typescript"))))))))
    (inputs (list node-fast-string-truncated-width-3.0.3))
    (home-page "https://github.com/fabiospampinato/fast-string-width#readme")
    (synopsis
     "A fast function for calculating the visual width of a string once printed to the terminal.")
    (description
     "A fast function for calculating the visual width of a string once printed to the terminal.")
    (license license:expat)))

(define-public node-fast-uri-3.1.2
  (package
    (name "node-fast-uri")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/fast-uri/-/fast-uri-3.1.2.tgz")
       (sha256
        (base32 "0hy16g7i9f5yw6spsl90fh2gygl7h17kr9hvg28sp3gz07ly7zlk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ajv" "eslint" "neostandard"
                                                  "playwright-test" "tape"
                                                  "tsd"))))))))
    (home-page "https://github.com/fastify/fast-uri")
    (synopsis "Dependency-free RFC 3986 URI toolbox")
    (description "Dependency-free RFC 3986 URI toolbox")
    (license license:bsd-3)))

(define-public node-fast-wrap-ansi-0.2.2
  (package
    (name "node-fast-wrap-ansi")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/fast-wrap-ansi/-/fast-wrap-ansi-0.2.2.tgz")
       (sha256
        (base32 "0lg0zb9z4bx13v8rwh3zjwas7axi556bwd0p3bq0ai3b6h7s3d7n"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@eslint/js" "@types/node"
                                                  "eslint"
                                                  "fast-wrap-ansi-prod"
                                                  "picocolors"
                                                  "prettier"
                                                  "tinybench"
                                                  "typescript"
                                                  "typescript-eslint"
                                                  "vitest"
                                                  "wrap-ansi"))))))))
    (inputs (list node-fast-string-width-3.0.2))
    (home-page "https://github.com/43081j/fast-wrap-ansi#readme")
    (synopsis
     "A tiny and fast text wrap library which takes ANSI escapes into account.")
    (description
     "A tiny and fast text wrap library which takes ANSI escapes into account.")
    (license license:expat)))

(define-public node-fast-xml-builder-1.2.0
  (package
    (name "node-fast-xml-builder")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/fast-xml-builder/-/fast-xml-builder-1.2.0.tgz")
       (sha256
        (base32 "11zliaf1pf2ngssk4zk0p5nx71453jcq6j31sk9v42mqf6k4wd5r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/plugin-transform-runtime"
                                                  "@babel/preset-env"
                                                  "@babel/register"
                                                  "@types/node"
                                                  "babel-loader"
                                                  "c8"
                                                  "eslint"
                                                  "fast-xml-parser"
                                                  "he"
                                                  "jasmine"
                                                  "prettier"
                                                  "publish-please"
                                                  "typescript"
                                                  "webpack"
                                                  "webpack-cli"))))))))
    (inputs (list node-xml-naming-0.1.0 node-path-expression-matcher-1.5.0))
    (home-page
     "https://github.com/NaturalIntelligence/fast-xml-builder#readme")
    (synopsis "Build XML from JSON without C/C++ based libraries")
    (description "Build XML from JSON without C/C++ based libraries")
    (license license:expat)))

(define-public node-fast-xml-parser-5.7.3
  (package
    (name "node-fast-xml-parser")
    (version "5.7.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/fast-xml-parser/-/fast-xml-parser-5.7.3.tgz")
       (sha256
        (base32 "1d34x6rlfra3m91c6h7ln0m7ddijdz5wcwcnca377h8glszpzg8c"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "eslint"
                                                  "jasmine"
                                                  "webpack"
                                                  "prettier"
                                                  "typescript"
                                                  "@babel/core"
                                                  "@types/node"
                                                  "webpack-cli"
                                                  "babel-loader"
                                                  "publish-please"
                                                  "@babel/register"
                                                  "@babel/preset-env"
                                                  "@babel/plugin-transform-runtime"))))))))
    (inputs (list node-path-expression-matcher-1.5.0
                  node-nodable-entities-2.1.0 node-fast-xml-builder-1.2.0
                  node-strnum-2.3.0))
    (home-page "https://github.com/NaturalIntelligence/fast-xml-parser#readme")
    (synopsis
     "Validate XML, Parse XML, Build XML without C/C++ based libraries")
    (description
     "Validate XML, Parse XML, Build XML without C/C++ based libraries")
    (license license:expat)))

(define-public node-fetch-blob-3.2.0
  (package
    (name "node-fetch-blob")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/fetch-blob/-/fetch-blob-3.2.0.tgz")
       (sha256
        (base32 "0lhcwk678vgadhilyfjmx5im77y55c52i37080icwzwplic0vgsa"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "c8"
                                                  "typescript"))))))))
    (inputs (list node-web-streams-polyfill-3.3.3 node-node-domexception-1.0.0))
    (home-page "https://github.com/node-fetch/fetch-blob#readme")
    (synopsis
     "Blob & File implementation in Node.js, originally from node-fetch.")
    (description
     "Blob & File implementation in Node.js, originally from node-fetch.")
    (license license:expat)))

(define-public node-file-type-22.0.1
  (package
    (name "node-file-type")
    (version "22.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/file-type/-/file-type-22.0.1.tgz")
       (sha256
        (base32 "10174fny3gqzz0rjgl2511v6r5sy2rsfggfz7qvlgv3igrawdzc7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@tokenizer/token"
                                                  "@types/node"
                                                  "ava"
                                                  "commonmark"
                                                  "esbuild"
                                                  "get-stream"
                                                  "tsd"
                                                  "xo"))))))))
    (inputs (list node-uint8array-extras-1.5.0 node-token-types-6.1.2
                  node-strtok3-10.3.5 node-tokenizer-inflate-0.4.1))
    (home-page "https://github.com/sindresorhus/file-type#readme")
    (synopsis "Detect the file type of a file, stream, or data")
    (description "Detect the file type of a file, stream, or data")
    (license license:expat)))

(define-public node-finalhandler-2.1.1
  (package
    (name "node-finalhandler")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/finalhandler/-/finalhandler-2.1.1.tgz")
       (sha256
        (base32 "0wmqm14f4xd6gf6s6117sn0xgvx935bdcrxyidcv6856a7y9p512"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"
                                                  "supertest"))))))))
    (inputs (list node-statuses-2.0.2
                  node-parseurl
                  node-on-finished
                  node-escape-html
                  node-encodeurl
                  node-debug-4.4.3))
    (home-page "https://github.com/pillarjs/finalhandler#readme")
    (synopsis "Node.js final http responder")
    (description "Node.js final http responder")
    (license license:expat)))

(define-public node-find-up-4.1.0
  (package
    (name "node-find-up")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/find-up/-/find-up-4.1.0.tgz")
       (sha256
        (base32 "1sr6b86slwxig85zcvjpgmvaqljb6il8n21719gf1lh6ad9v1a9k"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd" "tempy"
                                                  "is-path-inside"))))))))
    (inputs (list node-path-exists-4.0.0 node-locate-path-5.0.0))
    (home-page "https://github.com/sindresorhus/find-up#readme")
    (synopsis "Find a file or directory by walking up parent directories")
    (description "Find a file or directory by walking up parent directories")
    (license license:expat)))

(define-public node-formdata-polyfill-4.0.10
  (package
    (name "node-formdata-polyfill")
    (version "4.0.10")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/formdata-polyfill/-/formdata-polyfill-4.0.10.tgz")
       (sha256
        (base32 "1sc7hip8lwxbz2jg2k0snyqqwb4s8087kdj11zyz0cza710kpxqz"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/google-closure-compiler"
                                                  "@types/node"
                                                  "google-closure-compiler"))))))))
    (inputs (list node-fetch-blob-3.2.0))
    (home-page "https://github.com/jimmywarting/FormData#readme")
    (synopsis "HTML5 `FormData` for Browsers and Node.")
    (description "HTML5 `FormData` for Browsers and Node.")
    (license license:expat)))

(define-public node-fs-constants-1.0.0
  (package
    (name "node-fs-constants")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/fs-constants/-/fs-constants-1.0.0.tgz")
       (sha256
        (base32 "1yn5qyvxf9i3zrfly77wgmi3j9fl61gh1i0jjgamnir43dz6v4z7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/mafintosh/fs-constants")
    (synopsis "Require constants across node and the browser")
    (description "Require constants across node and the browser")
    (license license:expat)))

(define-public node-gaxios-7.1.4
  (package
    (name "node-gaxios")
    (version "7.1.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/gaxios/-/gaxios-7.1.4.tgz")
       (sha256
        (base32 "1ckyy5x8c0kkq2smsmw6sqh91as7bskf2amhmj3vg30hclzva7r3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/plugin-proposal-private-methods"
                                                  "@types/cors"
                                                  "@types/express"
                                                  "@types/extend"
                                                  "@types/mocha"
                                                  "@types/multiparty"
                                                  "@types/mv"
                                                  "@types/ncp"
                                                  "@types/node"
                                                  "@types/sinon"
                                                  "@types/tmp"
                                                  "assert"
                                                  "browserify"
                                                  "c8"
                                                  "cors"
                                                  "express"
                                                  "gts"
                                                  "is-docker"
                                                  "jsdoc"
                                                  "jsdoc-fresh"
                                                  "jsdoc-region-tag"
                                                  "karma"
                                                  "karma-chrome-launcher"
                                                  "karma-coverage"
                                                  "karma-firefox-launcher"
                                                  "karma-mocha"
                                                  "karma-remap-coverage"
                                                  "karma-sourcemap-loader"
                                                  "karma-webpack"
                                                  "linkinator"
                                                  "mocha"
                                                  "multiparty"
                                                  "mv"
                                                  "ncp"
                                                  "nock"
                                                  "null-loader"
                                                  "pack-n-play"
                                                  "puppeteer"
                                                  "sinon"
                                                  "stream-browserify"
                                                  "tmp"
                                                  "ts-loader"
                                                  "typescript"
                                                  "webpack"
                                                  "webpack-cli"))))))))
    (inputs (list node-node-fetch-3.3.2 node-https-proxy-agent
                  node-extend-3.0.2))
    (home-page
     "https://github.com/googleapis/google-cloud-node-core/tree/main/packages/gaxios")
    (synopsis
     "A simple common HTTP client specifically for Google APIs and services.")
    (description
     "A simple common HTTP client specifically for Google APIs and services.")
    (license license:asl2.0)))

(define-public node-gcp-metadata-8.1.2
  (package
    (name "node-gcp-metadata")
    (version "8.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/gcp-metadata/-/gcp-metadata-8.1.2.tgz")
       (sha256
        (base32 "156v633mndhk6r6c7102idkkdian7irr3lpca90hfp3md34g5hzr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@google-cloud/functions"
                                                  "@types/json-bigint"
                                                  "@types/mocha"
                                                  "@types/ncp"
                                                  "@types/node"
                                                  "@types/sinon"
                                                  "@types/tmp"
                                                  "c8"
                                                  "cross-env"
                                                  "gcbuild"
                                                  "gcx"
                                                  "gts"
                                                  "jsdoc"
                                                  "jsdoc-fresh"
                                                  "jsdoc-region-tag"
                                                  "linkinator"
                                                  "mocha"
                                                  "ncp"
                                                  "nock"
                                                  "sinon"
                                                  "tmp"
                                                  "typescript"))))))))
    (inputs (list node-json-bigint-1.0.0 node-google-logging-utils-1.1.3
                  node-gaxios-7.1.4))
    (home-page
     "https://github.com/googleapis/google-cloud-node-core/tree/main/packages/gcp-metadata")
    (synopsis "Get the metadata from a Google Cloud Platform environment")
    (description "Get the metadata from a Google Cloud Platform environment")
    (license license:asl2.0)))

(define-public node-get-east-asian-width-1.6.0
  (package
    (name "node-get-east-asian-width")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/get-east-asian-width/-/get-east-asian-width-1.6.0.tgz")
       (sha256
        (base32 "1mz6xqcd840s6aj191nw2f23lzg99fv2bfmlx1xg4kjs5y7gala4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "outdent"
                                                  "simplify-ranges"
                                                  "typescript" "xo"))))))))
    (home-page "https://github.com/sindresorhus/get-east-asian-width#readme")
    (synopsis "Determine the East Asian Width of a Unicode character")
    (description "Determine the East Asian Width of a Unicode character")
    (license license:expat)))

(define-public node-github-from-package-0.0.0
  (package
    (name "node-github-from-package")
    (version "0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/github-from-package/-/github-from-package-0.0.0.tgz")
       (sha256
        (base32 "1yj38h8r3z0gdfklghk3xz5gmfc2f6ya50rk7awfjakpw22z2iiy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "tape"))))))))
    (home-page "https://github.com/substack/github-from-package")
    (synopsis "return the github url from a package.json file")
    (description "return the github url from a package.json file")
    (license license:expat)))

(define-public node-glob-13.0.6
  (package
    (name "node-glob")
    (version "13.0.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/glob/-/glob-13.0.6.tgz")
       (sha256
        (base32 "0w49ggh984wkrj0myy3gbzh5hnmjljxisg9wydf91qn4m57ih3q2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "esbuild"
                                                  "memfs"
                                                  "mkdirp"
                                                  "prettier"
                                                  "rimraf"
                                                  "tap"
                                                  "tshy"
                                                  "typedoc"))))))))
    (inputs (list node-path-scurry-2.0.2 node-minipass-7.1.3
                  node-minimatch-10.2.5))
    (home-page "https://github.com/isaacs/node-glob#readme")
    (synopsis
     "the most correct and second fastest glob implementation in JavaScript")
    (description
     "the most correct and second fastest glob implementation in JavaScript")
    (license license:blue-oak1.0.0)))

(define-public node-google-auth-library-10.6.2
  (package
    (name "node-google-auth-library")
    (version "10.6.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/google-auth-library/-/google-auth-library-10.6.2.tgz")
       (sha256
        (base32 "198vxvsblk58fqx68lgnd4lr9as5hzgbhn1p1apaa5z6vbijcykh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/base64-js"
                                                  "@types/jws"
                                                  "@types/mocha"
                                                  "@types/mv"
                                                  "@types/ncp"
                                                  "@types/node"
                                                  "@types/sinon"
                                                  "assert-rejects"
                                                  "c8"
                                                  "codecov"
                                                  "gts"
                                                  "is-docker"
                                                  "jsdoc"
                                                  "jsdoc-fresh"
                                                  "jsdoc-region-tag"
                                                  "karma"
                                                  "karma-chrome-launcher"
                                                  "karma-coverage"
                                                  "karma-firefox-launcher"
                                                  "karma-mocha"
                                                  "karma-sourcemap-loader"
                                                  "karma-webpack"
                                                  "keypair"
                                                  "mocha"
                                                  "mv"
                                                  "ncp"
                                                  "nock"
                                                  "null-loader"
                                                  "puppeteer"
                                                  "sinon"
                                                  "ts-loader"
                                                  "typescript"
                                                  "webpack"
                                                  "webpack-cli"))))))))
    (inputs (list node-jws-4.0.1
                  node-google-logging-utils-1.1.3
                  node-gcp-metadata-8.1.2
                  node-gaxios-7.1.4
                  node-ecdsa-sig-formatter-1.0.11
                  node-base64-js-1.5.1))
    (home-page
     "https://github.com/googleapis/google-cloud-node-core/tree/main/packages/google-auth-library-nodejs")
    (synopsis "Google APIs Authentication Client Library for Node.js")
    (description "Google APIs Authentication Client Library for Node.js")
    (license license:asl2.0)))

(define-public node-google-genai-1.52.0
  (package
    (name "node-google-genai")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@google/genai/-/genai-1.52.0.tgz")
       (sha256
        (base32 "108rbj4inq67nxc61vlrwdsb3z033k16fsvp853qw46ynb1g8285"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "gts"
                                                  "nyc"
                                                  "tsx"
                                                  "zod"
                                                  "tslib"
                                                  "eslint"
                                                  "undici"
                                                  "jasmine"
                                                  "ts-node"
                                                  "typedoc"
                                                  "prettier"
                                                  "@types/ws"
                                                  "@eslint/js"
                                                  "node-fetch"
                                                  "typescript"
                                                  "@types/node"
                                                  "npm-run-all"
                                                  "@types/unist"
                                                  "undici-types"
                                                  "patch-package"
                                                  "@types/jasmine"
                                                  "protobufjs-cli"
                                                  "test-server-sdk"
                                                  "@types/node-fetch"
                                                  "jasmine-reporters"
                                                  "typescript-eslint"
                                                  "zod-to-json-schema"
                                                  "@rollup/plugin-json"
                                                  "@cfworker/json-schema"
                                                  "@microsoft/api-extractor"
                                                  "@modelcontextprotocol/sdk"
                                                  "rollup-plugin-typescript2"
                                                  "prettier-plugin-organize-imports"))))))))
    (inputs (list node-google-auth-library-10.6.2 node-protobufjs-7.6.1
                  node-p-retry-4.6.2 node-ws-8.21.0
                  node-modelcontextprotocol-sdk-1.29.0))
    (home-page "https://github.com/googleapis/js-genai#readme")
    (synopsis
     "[![NPM Downloads](https://img.shields.io/npm/dw/%40google%2Fgenai)](https://www.npmjs.com/package/@google/genai) [![Node Current](https://img.shields.io/node/v/%40google%2Fgenai)](https://www.npmjs.com/package/@google/genai)")
    (description
     "[![NPM Downloads](https://img.shields.io/npm/dw/%40google%2Fgenai)](https://www.npmjs.com/package/@google/genai) [![Node Current](https://img.shields.io/node/v/%40google%2Fgenai)](https://www.npmjs.com/package/@google/genai)")
    (license license:asl2.0)))

(define-public node-google-genai-2.5.0
  (package
    (name "node-google-genai")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@google/genai/-/genai-2.5.0.tgz")
       (sha256
        (base32 "0ii8wycb9rc272bl5xs09v41vwss4rf98i47sx4ccwsaz3p2h95q"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "gts"
                                                  "nyc"
                                                  "tsx"
                                                  "zod"
                                                  "tslib"
                                                  "eslint"
                                                  "undici"
                                                  "jasmine"
                                                  "ts-node"
                                                  "typedoc"
                                                  "prettier"
                                                  "@types/ws"
                                                  "@eslint/js"
                                                  "node-fetch"
                                                  "typescript"
                                                  "@types/node"
                                                  "npm-run-all"
                                                  "@types/unist"
                                                  "undici-types"
                                                  "patch-package"
                                                  "@types/jasmine"
                                                  "protobufjs-cli"
                                                  "test-server-sdk"
                                                  "@types/node-fetch"
                                                  "jasmine-reporters"
                                                  "typescript-eslint"
                                                  "zod-to-json-schema"
                                                  "@rollup/plugin-json"
                                                  "@cfworker/json-schema"
                                                  "@microsoft/api-extractor"
                                                  "@modelcontextprotocol/sdk"
                                                  "rollup-plugin-typescript2"
                                                  "prettier-plugin-organize-imports"))))))))
    (inputs (list node-google-auth-library-10.6.2 node-protobufjs-7.6.1
                  node-p-retry-4.6.2 node-ws-8.21.0
                  node-modelcontextprotocol-sdk-1.29.0))
    (home-page "https://github.com/googleapis/js-genai#readme")
    (synopsis
     "[![NPM Downloads](https://img.shields.io/npm/dw/%40google%2Fgenai)](https://www.npmjs.com/package/@google/genai) [![Node Current](https://img.shields.io/node/v/%40google%2Fgenai)](https://www.npmjs.com/package/@google/genai)")
    (description
     "[![NPM Downloads](https://img.shields.io/npm/dw/%40google%2Fgenai)](https://www.npmjs.com/package/@google/genai) [![Node Current](https://img.shields.io/node/v/%40google%2Fgenai)](https://www.npmjs.com/package/@google/genai)")
    (license license:asl2.0)))

(define-public node-google-logging-utils-1.1.3
  (package
    (name "node-google-logging-utils")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/google-logging-utils/-/google-logging-utils-1.1.3.tgz")
       (sha256
        (base32 "1g8bjykjsax507xazrgsz23qspq2237c1hkgfaz98grk9q6ir4is"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/mocha" "@types/node"
                                                  "@types/sinon"
                                                  "c8"
                                                  "gts"
                                                  "mocha"
                                                  "sinon"
                                                  "typescript"))))))))
    (home-page
     "https://github.com/googleapis/google-cloud-node-core/tree/main/dev-packages/logging-utils")
    (synopsis "A debug logger package for other Google libraries")
    (description "A debug logger package for other Google libraries")
    (license license:asl2.0)))

(define-public node-grammy-1.43.0
  (package
    (name "node-grammy")
    (version "1.43.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/grammy/-/grammy-1.43.0.tgz")
       (sha256
        (base32 "0scw1ccwgcycwy5c1jl2kiq4krm30jb6zp0vl3r7l8f8h6lzdk27"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/debug" "@types/node"
                                                  "@types/node-fetch"
                                                  "deno2node"))))))))
    (inputs (list node-node-fetch node-debug-4.4.3
                  node-abort-controller node-grammyjs-types-3.27.3))
    (home-page "https://grammy.dev/")
    (synopsis "The Telegram Bot Framework.")
    (description "The Telegram Bot Framework.")
    (license license:expat)))

(define-public node-grammyjs-runner-2.0.3
  (package
    (name "node-grammyjs-runner")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@grammyjs/runner/-/runner-2.0.3.tgz")
       (sha256
        (base32 "0ng734dcm7xgl6l5bvmpv4wv8652f4va01zyvq5d1j2qg2fh9cb5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("deno2node" "@types/node"
                                                  "grammy"))))))))
    (inputs (list node-abort-controller node-grammy-1.43.0))
    (home-page "https://grammy.dev/plugins/runner.html")
    (synopsis "Scale grammY bots that use long polling")
    (description "Scale grammY bots that use long polling")
    (license license:expat)))

(define-public node-grammyjs-transformer-throttler-1.2.1
  (package
    (name "node-grammyjs-transformer-throttler")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@grammyjs/transformer-throttler/-/transformer-throttler-1.2.1.tgz")
       (sha256
        (base32 "1v7axbg10zv465wlsxx41646xb6ibkrdf7px2fzvxa2s790a3q23"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("grammy" "deno2node"
                                                  "@types/debug"
                                                  "@tsconfig/node12"
                                                  "@types/node-fetch"))))))))
    (inputs (list node-bottleneck-2.19.5 node-grammy-1.43.0))
    (home-page "https://github.com/grammyjs/transformer-throttler")
    (synopsis "Throttling transformer for Grammy")
    (description "Throttling transformer for Grammy")
    (license license:expat)))

(define-public node-grammyjs-types-3.27.3
  (package
    (name "node-grammyjs-types")
    (version "3.27.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@grammyjs/types/-/types-3.27.3.tgz")
       (sha256
        (base32 "1dz0gkxxf6ifwq8x9blkpfq2abafxaylmkp39xsk4q8z595kn7g7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("deno-bin"))))))))
    (home-page "https://grammy.dev/")
    (synopsis "Telegram Bot API type declarations for grammY")
    (description "Telegram Bot API type declarations for grammY")
    (license license:expat)))

(define-public node-homebridge-ciao-1.3.8
  (package
    (name "node-homebridge-ciao")
    (version "1.3.8")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@homebridge/ciao/-/ciao-1.3.8.tgz")
       (sha256
        (base32 "0kzlal4g7fyikffhy6r56nhxdlj154lzd8w9sd4cl3j9m06d805j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "eslint"
                                                  "rimraf"
                                                  "ts-jest"
                                                  "ts-node"
                                                  "typedoc"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@types/debug"
                                                  "@types/source-map-support"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-source-map-support-0.5.21 node-fast-deep-equal
                  node-tslib-2.8.1 node-debug-4.4.3))
    (home-page "https://github.com/homebridge/ciao")
    (synopsis
     "ciao is a RFC 6763 compliant dns-sd library, advertising on multicast dns (RFC 6762) implemented in plain Typescript/JavaScript")
    (description
     "ciao is a RFC 6763 compliant dns-sd library, advertising on multicast dns (RFC 6762) implemented in plain Typescript/JavaScript")
    (license license:expat)))

(define-public node-hono-4.12.22
  (package
    (name "node-hono")
    (version "4.12.22")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/hono/-/hono-4.12.22.tgz")
       (sha256
        (base32 "1c4ndh39ipjc1wair4jnfw8n0yxm8x8hpj8kq1y5qbklh0v70rix"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@hono/eslint-config"
                                                  "@hono/node-server"
                                                  "@types/glob"
                                                  "@types/jsdom"
                                                  "@types/node"
                                                  "@types/ws"
                                                  "@typescript/native-preview"
                                                  "@vitest/coverage-v8"
                                                  "arg"
                                                  "bun-types"
                                                  "editorconfig-checker"
                                                  "esbuild"
                                                  "eslint"
                                                  "glob"
                                                  "jsdom"
                                                  "msw"
                                                  "np"
                                                  "oxc-parser"
                                                  "pkg-pr-new"
                                                  "prettier"
                                                  "publint"
                                                  "typescript"
                                                  "undici"
                                                  "vite-plugin-fastly-js-compute"
                                                  "vitest"
                                                  "wrangler"
                                                  "ws"
                                                  "zod"))))))))
    (home-page "https://hono.dev")
    (synopsis "Web framework built on Web Standards")
    (description "Web framework built on Web Standards")
    (license license:expat)))

(define-public node-hono-node-server-1.19.14
  (package
    (name "node-hono-node-server")
    (version "1.19.14")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@hono/node-server/-/node-server-1.19.14.tgz")
       (sha256
        (base32 "1bdrlpbjymv8n1zbjgrryivkhbbs4rp5hkpf0ffvin4ndws6z58b"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("np" "hono"
                                                  "jest"
                                                  "tsup"
                                                  "eslint"
                                                  "publint"
                                                  "ts-jest"
                                                  "prettier"
                                                  "supertest"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@types/supertest"
                                                  "@whatwg-node/fetch"
                                                  "@hono/eslint-config"))))))))
    (home-page "https://github.com/honojs/node-server")
    (synopsis "Node.js Adapter for Hono")
    (description "Node.js Adapter for Hono")
    (license license:expat)))

(define-public node-hosted-git-info-9.0.3
  (package
    (name "node-hosted-git-info")
    (version "9.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/hosted-git-info/-/hosted-git-info-9.0.3.tgz")
       (sha256
        (base32 "0wxnrvdfn0sm0kfg3ama55xii7rsf3ak9jy2hb264qizb7x1rfmm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@npmcli/template-oss"
                                                  "@npmcli/eslint-config"))))))))
    (inputs (list node-lru-cache-11.5.0))
    (home-page "https://github.com/npm/hosted-git-info")
    (synopsis
     "Provides metadata and conversions from repository urls for GitHub, Bitbucket and GitLab")
    (description
     "Provides metadata and conversions from repository urls for GitHub, Bitbucket and GitLab")
    (license license:isc)))

(define-public node-html-escaper-3.0.3
  (package
    (name "node-html-escaper")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/html-escaper/-/html-escaper-3.0.3.tgz")
       (sha256
        (base32 "02fidnwy7dccc1xpdqv1f4jspxg85q9q0b5zvnh5cb83m1y65i8r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ascjs" "c8" "coveralls"
                                                  "rollup" "uglify-es"))))))))
    (home-page "https://github.com/WebReflection/html-escaper")
    (synopsis "fast and safe way to escape and unescape &<>'\" chars")
    (description "fast and safe way to escape and unescape &<>'\" chars")
    (license license:expat)))

(define-public node-htmlparser2-10.1.0
  (package
    (name "node-htmlparser2")
    (version "10.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/htmlparser2/-/htmlparser2-10.1.0.tgz")
       (sha256
        (base32 "0zc7mm7h13g7h5llihpa8hx8c57w76nsck7vfy60qlxzdmvynl86"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tshy" "eslint"
                                                  "vitest"
                                                  "globals"
                                                  "prettier"
                                                  "@eslint/js"
                                                  "typescript"
                                                  "@types/node"
                                                  "eslint-plugin-n"
                                                  "typescript-eslint"
                                                  "@vitest/coverage-v8"
                                                  "eslint-plugin-unicorn"
                                                  "eslint-config-prettier"))))))))
    (inputs (list node-domelementtype-2.3.0 node-domhandler-5.0.3
                  node-entities-7.0.1 node-domutils-3.2.2))
    (home-page "https://github.com/fb55/htmlparser2#readme")
    (synopsis "Fast & forgiving HTML/XML parser")
    (description "Fast & forgiving HTML/XML parser")
    (license license:expat)))

(define-public node-http-ece-1.2.0
  (package
    (name "node-http-ece")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/http_ece/-/http_ece-1.2.0.tgz")
       (sha256
        (base32 "0vkhny74mfbhkk5njmb6gi2q71mk6i3f7jzc8i2v62cgvainpm94"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/martinthomson/encrypted-content-encoding")
    (synopsis "Encrypted Content-Encoding for HTTP")
    (description "Encrypted Content-Encoding for HTTP")
    (license license:expat)))

(define-public node-http-errors-2.0.1
  (package
    (name "node-http-errors")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/http-errors/-/http-errors-2.0.1.tgz")
       (sha256
        (base32 "0qbc38g805qi9bcywg082sz8kdgxkq4fssgjlfcrc1xg3fqvnqmd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"))))))))
    (inputs (list node-toidentifier node-statuses-2.0.2
                  node-setprototypeof node-inherits-2.0.4
                  node-depd))
    (home-page "https://github.com/jshttp/http-errors#readme")
    (synopsis "Create HTTP error objects")
    (description "Create HTTP error objects")
    (license license:expat)))

(define-public node-http-proxy-agent-7.0.2
  (package
    (name "node-http-proxy-agent")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/http-proxy-agent/-/http-proxy-agent-7.0.2.tgz")
       (sha256
        (base32 "00kgi96l0vs04g2vl2xw3g53saxb4n9za2x23pbaiyrbm7x76pvq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "proxy"
                                                  "ts-jest"
                                                  "tsconfig"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@types/debug"
                                                  "async-listen"))))))))
    (inputs (list node-agent-base-7.1.4 node-debug-4.4.3))
    (home-page "https://github.com/TooTallNate/proxy-agents#readme")
    (synopsis "An HTTP(s) proxy `http.Agent` implementation for HTTP")
    (description "An HTTP(s) proxy `http.Agent` implementation for HTTP")
    (license license:expat)))

(define-public node-iconv-lite-0.7.2
  (package
    (name "node-iconv-lite")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.7.2.tgz")
       (sha256
        (base32 "0gm0im7640vi4j29ssh9zf1ynfrffkjm4mig9np3b4hzwxlngm31"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "async"
                                                  "errto"
                                                  "iconv"
                                                  "mocha"
                                                  "unorm"
                                                  "eslint"
                                                  "semver"
                                                  "request"
                                                  "bench-node"
                                                  "typescript"
                                                  "@types/node"
                                                  "expect-type"
                                                  "neostandard"
                                                  "@arethetypeswrong/cli"
                                                  "@stylistic/eslint-plugin"
                                                  "@stylistic/eslint-plugin-js"))))))))
    (inputs (list node-safer-buffer))
    (home-page "https://github.com/pillarjs/iconv-lite")
    (synopsis "Convert character encodings in pure javascript.")
    (description "Convert character encodings in pure javascript.")
    (license license:expat)))

(define-public node-ignore-7.0.5
  (package
    (name "node-ignore")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ignore/-/ignore-7.0.5.tgz")
       (sha256
        (base32 "1062hjm3bgg9013nvl33mmz3cchvcjhndq6gj61sgzazvqwnqbg8"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/cli" "@babel/core"
                                                  "@babel/preset-env"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "debug"
                                                  "eslint"
                                                  "eslint-config-ostai"
                                                  "eslint-plugin-import"
                                                  "mkdirp"
                                                  "pre-suf"
                                                  "rimraf"
                                                  "spawn-sync"
                                                  "tap"
                                                  "tmp"
                                                  "ts-node"
                                                  "typescript"))))))))
    (home-page "https://github.com/kaelzhang/node-ignore#readme")
    (synopsis
     "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
    (description
     "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
    (license license:expat)))

(define-public node-immediate-3.0.6
  (package
    (name "node-immediate")
    (version "3.0.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/immediate/-/immediate-3.0.6.tgz")
       (sha256
        (base32 "04cxfcl4zm2qfsrrd19n5w4w8k8309wl1k2xq0c0ic2hjvrr5iwq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("browserify"
                                                  "browserify-transform-cli"
                                                  "derequire"
                                                  "inline-process-browser"
                                                  "jshint"
                                                  "tape"
                                                  "uglify-js"
                                                  "unreachable-branch-transform"))))))))
    (home-page "https://www.npmjs.com/package/node-immediate")
    (synopsis "A cross browser microtask library")
    (description "A cross browser microtask library")
    (license license:expat)))

(define-public node-ini-1.3.8
  (package
    (name "node-ini")
    (version "1.3.8")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ini/-/ini-1.3.8.tgz")
       (sha256
        (base32 "0nk92bp5is23lsi1ip4qz5bjkzmjxkz9c1g79sx991ad212i37px"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "eslint"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/isaacs/ini#readme")
    (synopsis "An ini encoder/decoder for node")
    (description "An ini encoder/decoder for node")
    (license license:isc)))

(define-public node-ip-address-10.2.0
  (package
    (name "node-ip-address")
    (version "10.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ip-address/-/ip-address-10.2.0.tgz")
       (sha256
        (base32 "1fn0lzwpagd0dslpzw272g7wvnqwqhib01a43g0pc5ad7z0dvq1p"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/chai" "@types/mocha"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "c8"
                                                  "chai"
                                                  "eslint"
                                                  "eslint_d"
                                                  "eslint-config-airbnb"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-filenames"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-jsx-a11y"
                                                  "eslint-plugin-prettier"
                                                  "eslint-plugin-sort-imports-es6-autofix"
                                                  "mocha"
                                                  "monocart-coverage-reports"
                                                  "prettier"
                                                  "source-map-support"
                                                  "tsx"
                                                  "typedoc"
                                                  "typescript"))))))))
    (home-page "https://github.com/beaugunderson/ip-address#readme")
    (synopsis
     "A library for parsing IPv4 and IPv6 IP addresses in node and the browser.")
    (description
     "A library for parsing IPv4 and IPv6 IP addresses in node and the browser.")
    (license license:expat)))

(define-public node-ipaddr-js-2.4.0
  (package
    (name "node-ipaddr-js")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ipaddr.js/-/ipaddr.js-2.4.0.tgz")
       (sha256
        (base32 "0n1r0wr4lldlfamf3i8gnr8hx5fgs1vhqgi3cyl4fdxy0pfkqkn4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint" "uglify-es"))))))))
    (home-page "https://github.com/whitequark/ipaddr.js#readme")
    (synopsis
     "A library for manipulating IPv4 and IPv6 addresses in JavaScript.")
    (description
     "A library for manipulating IPv4 and IPv6 addresses in JavaScript.")
    (license license:expat)))

(define-public node-isaacs-fs-minipass-4.0.1
  (package
    (name "node-isaacs-fs-minipass")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@isaacs/fs-minipass/-/fs-minipass-4.0.1.tgz")
       (sha256
        (base32 "0dl2v4741w4lj36m2yg1h8qpvfbbxyfh7qkr1scah7ncrkiqdhks"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "mutate-fs"
                                                  "prettier" "tap" "tshy"
                                                  "typedoc"))))))))
    (inputs (list node-minipass-7.1.3))
    (home-page "https://github.com/npm/fs-minipass#readme")
    (synopsis "fs read and write streams based on minipass")
    (description "fs read and write streams based on minipass")
    (license license:isc)))

(define-public node-isarray-1.0.0
  (package
    (name "node-isarray")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/isarray/-/isarray-1.0.0.tgz")
       (sha256
        (base32 "11qcjpdzigcwcprhv7nyarlzjcwf3sv5i66q75zf08jj9zqpcg72"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape"))))))))
    (home-page "https://github.com/juliangruber/isarray")
    (synopsis "Array#isArray for older browsers")
    (description "Array#isArray for older browsers")
    (license license:expat)))

(define-public node-jiti-2.7.0
  (package
    (name "node-jiti")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/jiti/-/jiti-2.7.0.tgz")
       (sha256
        (base32 "1ji8rzdyqd7w8r1hk4j4q9phba9dzgv0v86n8b6fgh29zdjx7l4d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/helper-module-imports"
                                                  "@babel/helper-module-transforms"
                                                  "@babel/helper-plugin-utils"
                                                  "@babel/helper-simple-access"
                                                  "@babel/plugin-proposal-decorators"
                                                  "@babel/plugin-syntax-class-properties"
                                                  "@babel/plugin-syntax-import-assertions"
                                                  "@babel/plugin-syntax-jsx"
                                                  "@babel/plugin-transform-explicit-resource-management"
                                                  "@babel/plugin-transform-export-namespace-from"
                                                  "@babel/plugin-transform-react-jsx"
                                                  "@babel/plugin-transform-typescript"
                                                  "@babel/preset-typescript"
                                                  "@babel/template"
                                                  "@babel/traverse"
                                                  "@babel/types"
                                                  "@rspack/cli"
                                                  "@rspack/core"
                                                  "@types/babel__core"
                                                  "@types/babel__helper-module-imports"
                                                  "@types/babel__helper-plugin-utils"
                                                  "@types/babel__template"
                                                  "@types/babel__traverse"
                                                  "@types/node"
                                                  "@typescript/native-preview"
                                                  "@vitest/coverage-v8"
                                                  "acorn"
                                                  "babel-plugin-parameter-decorator"
                                                  "changelogen"
                                                  "config"
                                                  "consola"
                                                  "defu"
                                                  "destr"
                                                  "escape-string-regexp"
                                                  "eslint"
                                                  "eslint-config-unjs"
                                                  "estree-walker"
                                                  "etag"
                                                  "fast-glob"
                                                  "get-tsconfig"
                                                  "is-installed-globally"
                                                  "mime"
                                                  "mitata"
                                                  "mlly"
                                                  "moment-timezone"
                                                  "nano-jsx"
                                                  "pathe"
                                                  "pkg-types"
                                                  "preact"
                                                  "preact-render-to-string"
                                                  "prettier"
                                                  "react"
                                                  "react-dom"
                                                  "reflect-metadata"
                                                  "rolldown"
                                                  "solid-js"
                                                  "std-env"
                                                  "terser-webpack-plugin"
                                                  "tinyexec"
                                                  "ts-loader"
                                                  "typescript"
                                                  "vitest"
                                                  "vue"
                                                  "yoctocolors"
                                                  "zod"))))))))
    (home-page "https://github.com/unjs/jiti#readme")
    (synopsis "Runtime typescript and ESM support for Node.js")
    (description "Runtime typescript and ESM support for Node.js")
    (license license:expat)))

(define-public node-jose-6.2.3
  (package
    (name "node-jose")
    (version "6.2.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/jose/-/jose-6.2.3.tgz")
       (sha256
        (base32 "19amggxlhcyffqqakbkq1f1yc2617smjjshv75jhciqqkzh8ni64"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/panva/jose")
    (synopsis
     "JWA, JWS, JWE, JWT, JWK, JWKS for Node.js, Browser, Cloudflare Workers, Deno, Bun, and other Web-interoperable runtimes")
    (description
     "JWA, JWS, JWE, JWT, JWK, JWKS for Node.js, Browser, Cloudflare Workers, Deno, Bun, and other Web-interoperable runtimes")
    (license license:expat)))

(define-public node-json-bigint-1.0.0
  (package
    (name "node-json-bigint")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/json-bigint/-/json-bigint-1.0.0.tgz")
       (sha256
        (base32 "1dh3z67vh5084b07y8sklacnidzrddkqjjbc3kz6rcygaclm0ksc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "mocha"))))))))
    (inputs (list node-bignumber-js-9.3.1))
    (home-page "https://github.com/sidorares/json-bigint#readme")
    (synopsis "JSON.parse with bigints support")
    (description "JSON.parse with bigints support")
    (license license:expat)))

(define-public node-json-schema-to-ts-3.1.1
  (package
    (name "node-json-schema-to-ts")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/json-schema-to-ts/-/json-schema-to-ts-3.1.1.tgz")
       (sha256
        (base32 "0r639hff6d5z17lzkzqb0c8p1blpl97ssi7nx59qgjis8m74xwzn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/cli" "@babel/core"
                                                  "@babel/plugin-transform-runtime"
                                                  "@babel/preset-env"
                                                  "@babel/preset-typescript"
                                                  "@rollup/plugin-typescript"
                                                  "@trivago/prettier-plugin-sort-imports"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "@zerollup/ts-transform-paths"
                                                  "ajv"
                                                  "babel-plugin-module-resolver"
                                                  "dependency-cruiser"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-import-resolver-typescript"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-jest"
                                                  "eslint-plugin-jsdoc"
                                                  "eslint-plugin-prefer-arrow"
                                                  "eslint-plugin-prettier"
                                                  "eslint-plugin-unused-imports"
                                                  "jest"
                                                  "prettier"
                                                  "rollup"
                                                  "rollup-plugin-dts"
                                                  "rollup-plugin-import-map"
                                                  "rollup-plugin-typescript-paths"
                                                  "ts-jest"
                                                  "ts-node"
                                                  "ts-toolbelt"
                                                  "ts-unused-exports"
                                                  "tsc-alias"
                                                  "typescript"))))))))
    (inputs (list node-ts-algebra-2.0.0 node-babel-runtime-7.29.2))
    (home-page "https://github.com/ThomasAribart/json-schema-to-ts#readme")
    (synopsis "Infer typescript types from your JSON schemas!")
    (description "Infer typescript types from your JSON schemas!")
    (license license:expat)))

(define-public node-json-schema-traverse-1.0.0
  (package
    (name "node-json-schema-traverse")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/json-schema-traverse/-/json-schema-traverse-1.0.0.tgz")
       (sha256
        (base32 "08cvg5wysj4r0ax2lvhx7j74l7da8w75klz5pmsc57zj5mi24ch2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint" "mocha" "nyc"
                                                  "pre-commit"))))))))
    (home-page "https://github.com/epoberezkin/json-schema-traverse#readme")
    (synopsis "Traverse JSON Schema passing each schema object to callback")
    (description "Traverse JSON Schema passing each schema object to callback")
    (license license:expat)))

(define-public node-json-schema-typed-8.0.2
  (package
    (name "node-json-schema-typed")
    (version "8.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/json-schema-typed/-/json-schema-typed-8.0.2.tgz")
       (sha256
        (base32 "1jbbs2nxga2rfg4hxcipg0zn7j5d3r9rjziz7c3w7x7wwhk43p77"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page
     "https://github.com/RemyRylan/json-schema-typed/tree/main/dist/node")
    (synopsis
     "JSON Schema TypeScript definitions with complete inline documentation.")
    (description
     "JSON Schema TypeScript definitions with complete inline documentation.")
    (license license:bsd-2)))

(define-public node-json5-2.2.3
  (package
    (name "node-json5")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/json5/-/json5-2.2.3.tgz")
       (sha256
        (base32 "0yrpsb1frqahc48n6w2jzvhd7m8r9w2w9ylqkg41zl80nqyv7bq8"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("core-js" "eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "npm-run-all"
                                                  "regenerate"
                                                  "rollup"
                                                  "rollup-plugin-buble"
                                                  "rollup-plugin-commonjs"
                                                  "rollup-plugin-node-resolve"
                                                  "rollup-plugin-terser"
                                                  "sinon"
                                                  "tap"
                                                  "unicode-10.0.0"))))))))
    (home-page "http://json5.org/")
    (synopsis "JSON for Humans")
    (description "JSON for Humans")
    (license license:expat)))

(define-public node-jszip-3.10.1
  (package
    (name "node-jszip")
    (version "3.10.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/jszip/-/jszip-3.10.1.tgz")
       (sha256
        (base32 "08qvnasvaajkjkjr2lp7jz63b0fmb9brr0ivpw3v7bj5lsig85si"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tmp" "grunt"
                                                  "qunit"
                                                  "eslint"
                                                  "benchmark"
                                                  "grunt-cli"
                                                  "browserify"
                                                  "playwright"
                                                  "typescript"
                                                  "http-server"
                                                  "jszip-utils"
                                                  "grunt-browserify"
                                                  "grunt-contrib-uglify"
                                                  "package-json-versionify"))))))))
    (inputs (list node-readable-stream-2.3.8 node-setimmediate-1.0.5
                  node-pako-1.0.11 node-lie-3.3.0))
    (home-page "https://github.com/Stuk/jszip#readme")
    (synopsis
     "Create, read and edit .zip files with JavaScript http://stuartk.com/jszip")
    (description
     "Create, read and edit .zip files with JavaScript http://stuartk.com/jszip")
    (license #f)))

(define-public node-jwa-2.0.1
  (package
    (name "node-jwa")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/jwa/-/jwa-2.0.1.tgz")
       (sha256
        (base32 "079lm1m5malvssgz4lxiqvp300gpq6wpmygknvqx5jdxzgwhrg68"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "semver" "base64url"
                                                  "jwk-to-pem"))))))))
    (inputs (list node-buffer-equal-constant-time-1.0.1
                  node-ecdsa-sig-formatter-1.0.11 node-safe-buffer-5.2.1))
    (home-page "https://github.com/brianloveswords/node-jwa#readme")
    (synopsis "JWA implementation (supports all JWS algorithms)")
    (description "JWA implementation (supports all JWS algorithms)")
    (license license:expat)))

(define-public node-jws-4.0.1
  (package
    (name "node-jws")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/jws/-/jws-4.0.1.tgz")
       (sha256
        (base32 "04ifx47v412kfslgqgl7shj9im3wivl3f2p3r0311kpq4yq5yfpd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("semver" "tape"))))))))
    (inputs (list node-safe-buffer-5.2.1 node-jwa-2.0.1))
    (home-page "https://github.com/brianloveswords/node-jws#readme")
    (synopsis "Implementation of JSON Web Signatures")
    (description "Implementation of JSON Web Signatures")
    (license license:expat)))

(define-public node-kysely-0.29.2
  (package
    (name "node-kysely")
    (version "0.29.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/kysely/-/kysely-0.29.2.tgz")
       (sha256
        (base32 "017sksnnbdxjh5wv61yszkcn2mjzwczb1f3a45cswwk5qab6djwn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ark/attest"
                                                  "@electric-sql/pglite"
                                                  "@types/better-sqlite3"
                                                  "@types/chai"
                                                  "@types/chai-as-promised"
                                                  "@types/mocha"
                                                  "@types/node"
                                                  "@types/pg"
                                                  "@types/pg-cursor"
                                                  "@types/prototype-pollution-vulnerable-lodash.merge-dont-upgrade"
                                                  "@types/semver"
                                                  "@types/sinon"
                                                  "better-sqlite3"
                                                  "chai"
                                                  "chai-as-promised"
                                                  "esbuild"
                                                  "jsr"
                                                  "mocha"
                                                  "mysql2"
                                                  "pathe"
                                                  "pg"
                                                  "pg-cursor"
                                                  "playwright"
                                                  "prettier"
                                                  "prototype-pollution-vulnerable-lodash.merge-dont-upgrade"
                                                  "remeda"
                                                  "semver"
                                                  "sinon"
                                                  "std-env"
                                                  "tarn"
                                                  "tedious"
                                                  "tsd"
                                                  "tsx"
                                                  "typescript"))))))))
    (home-page "https://kysely.dev")
    (synopsis "Type safe SQL query builder")
    (description "Type safe SQL query builder")
    (license license:expat)))

(define-public node-lie-3.3.0
  (package
    (name "node-lie")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/lie/-/lie-3.3.0.tgz")
       (sha256
        (base32 "0vm3wrzfrjmv2qjcf6fch7znad6k6kd5b34cb31jy3m1shf4df1d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("browserify"
                                                  "browserify-transform-cli"
                                                  "bundle-collapser"
                                                  "copyfiles"
                                                  "derequire"
                                                  "es3ify"
                                                  "inline-process-browser"
                                                  "istanbul"
                                                  "jshint"
                                                  "mkdirp"
                                                  "mocha"
                                                  "mocha-phantomjs"
                                                  "phantomjs"
                                                  "promises-aplus-tests"
                                                  "rimraf"
                                                  "typescript"
                                                  "uglify-js"
                                                  "unreachable-branch-transform"))))))))
    (inputs (list node-immediate-3.0.6))
    (home-page "https://github.com/calvinmetcalf/lie#readme")
    (synopsis "A basic but performant promise implementation")
    (description "A basic but performant promise implementation")
    (license license:expat)))

(define-public node-linkedom-0.18.12
  (package
    (name "node-linkedom")
    (version "0.18.12")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/linkedom/-/linkedom-0.18.12.tgz")
       (sha256
        (base32 "03bmgx3lvs8dviqwbh1fgwkninkszn31mc0rarw77k9l88rccp5i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@rollup/plugin-commonjs"
                                                  "@rollup/plugin-json"
                                                  "@rollup/plugin-node-resolve"
                                                  "ascjs"
                                                  "c8"
                                                  "eslint"
                                                  "rollup"
                                                  "typescript"
                                                  "canvas"))))))))
    (inputs (list node-uhyphen-0.2.0
                  node-htmlparser2-10.1.0
                  node-html-escaper-3.0.3
                  node-cssom-0.5.0
                  node-css-select-5.2.2
                  node-canvas-3.2.3))
    (home-page "https://github.com/WebReflection/linkedom#readme")
    (synopsis "A triple-linked lists based DOM implementation")
    (description "A triple-linked lists based DOM implementation")
    (license license:isc)))

(define-public node-linkify-it-5.0.0
  (package
    (name "node-linkify-it")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/linkify-it/-/linkify-it-5.0.0.tgz")
       (sha256
        (base32 "1absn8zlv4xg3f5np1ff2i2wjhaarw3xf863gf8vphi65njwai31"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@rollup/plugin-node-resolve"
                                                  "ansi"
                                                  "benchmark"
                                                  "c8"
                                                  "eslint"
                                                  "eslint-config-standard"
                                                  "gh-pages"
                                                  "mdurl"
                                                  "mocha"
                                                  "ndoc"
                                                  "rollup"
                                                  "shelljs"
                                                  "shx"
                                                  "tlds"))))))))
    (inputs (list node-uc-micro-2.1.0))
    (home-page "https://github.com/markdown-it/linkify-it#readme")
    (synopsis "Links recognition library with FULL unicode support")
    (description "Links recognition library with FULL unicode support")
    (license license:expat)))

(define-public node-locate-path-5.0.0
  (package
    (name "node-locate-path")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/locate-path/-/locate-path-5.0.0.tgz")
       (sha256
        (base32 "1dsk824x6gzp2n7s0f9z7iwxsc4nyllxmix8h4588dd4c29ingdf"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"))))))))
    (inputs (list node-p-locate-4.1.0))
    (home-page "https://github.com/sindresorhus/locate-path#readme")
    (synopsis "Get the first path that exists on disk of multiple paths")
    (description "Get the first path that exists on disk of multiple paths")
    (license license:expat)))

(define-public node-long-5.3.2
  (package
    (name "node-long")
    (version "5.3.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/long/-/long-5.3.2.tgz")
       (sha256
        (base32 "09kbcinla92p75h69i90v5n730wxfaxl34z5r597vpvkcx33w0v8"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("esm2umd" "prettier"
                                                  "typescript"))))))))
    (home-page "https://github.com/dcodeIO/long.js#readme")
    (synopsis
     "A Long class for representing a 64-bit two's-complement integer value.")
    (description
     "A Long class for representing a 64-bit two's-complement integer value.")
    (license license:asl2.0)))

(define-public node-lru-cache-11.5.0
  (package
    (name "node-lru-cache")
    (version "11.5.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/lru-cache/-/lru-cache-11.5.0.tgz")
       (sha256
        (base32 "17p3xnkzxgv7sna4n807i4qps976ix84iz7nzq17fby3p3wjb2xx"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("benchmark" "esbuild"
                                                  "marked"
                                                  "mkdirp"
                                                  "oxlint"
                                                  "oxlint-tsgolint"
                                                  "prettier"
                                                  "tap"
                                                  "tshy"
                                                  "typedoc"))))))))
    (home-page "https://github.com/isaacs/node-lru-cache#readme")
    (synopsis "A cache object that deletes the least-recently-used items.")
    (description "A cache object that deletes the least-recently-used items.")
    (license license:blue-oak1.0.0)))

(define-public node-lydell-node-pty-1.2.0-beta.12
  (package
    (name "node-lydell-node-pty")
    (version "1.2.0-beta.12")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@lydell/node-pty/-/node-pty-1.2.0-beta.12.tgz")
       (sha256
        (base32 "08zlc75pzyhl2yirzf2qzg2rfbmqb3lydwbxhycdzq5yjdlwwsfc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/lydell/node-pty#readme")
    (synopsis "Smaller distribution of node-pty.")
    (description "Smaller distribution of node-pty.")
    (license license:expat)))

(define-public node-markdown-it-14.1.1
  (package
    (name "node-markdown-it")
    (version "14.1.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/markdown-it/-/markdown-it-14.1.1.tgz")
       (sha256
        (base32 "05bz8fjdf4x34whm6zpzf0s3c3pd3wl0wrinkbgcyv6ccbb99bi6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@rollup/plugin-babel"
                                                  "@rollup/plugin-commonjs"
                                                  "@rollup/plugin-node-resolve"
                                                  "@rollup/plugin-terser"
                                                  "ansi"
                                                  "benchmark"
                                                  "c8"
                                                  "chai"
                                                  "eslint"
                                                  "eslint-config-standard"
                                                  "express"
                                                  "gh-pages"
                                                  "highlight.js"
                                                  "jest-worker"
                                                  "markdown-it-abbr"
                                                  "markdown-it-container"
                                                  "markdown-it-deflist"
                                                  "markdown-it-emoji"
                                                  "markdown-it-footnote"
                                                  "markdown-it-for-inline"
                                                  "markdown-it-ins"
                                                  "markdown-it-mark"
                                                  "markdown-it-sub"
                                                  "markdown-it-sup"
                                                  "markdown-it-testgen"
                                                  "mocha"
                                                  "ndoc"
                                                  "needle"
                                                  "rollup"
                                                  "shelljs"
                                                  "supertest"))))))))
    (inputs (list node-uc-micro-2.1.0
                  node-punycode-js-2.3.1
                  node-mdurl-2.0.0
                  node-linkify-it-5.0.0
                  node-entities-4.5.0
                  node-argparse))
    (home-page "https://github.com/markdown-it/markdown-it#readme")
    (synopsis "Markdown-it - modern pluggable markdown parser.")
    (description "Markdown-it - modern pluggable markdown parser.")
    (license license:expat)))

(define-public node-mdurl-2.0.0
  (package
    (name "node-mdurl")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/mdurl/-/mdurl-2.0.0.tgz")
       (sha256
        (base32 "1d912fi1xzbhf3lhs01d8ik261sgyfbwr972h5wx3nih0wrcn34f"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "eslint"
                                                  "eslint-config-standard"
                                                  "mocha" "rollup"))))))))
    (home-page "https://github.com/markdown-it/mdurl#readme")
    (synopsis "URL utilities for markdown-it")
    (description "URL utilities for markdown-it")
    (license license:expat)))

(define-public node-mime-types-3.0.2
  (package
    (name "node-mime-types")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/mime-types/-/mime-types-3.0.2.tgz")
       (sha256
        (base32 "1b6j7px7npv0gli3v249m5a1rc2m8x3qxxpva23zy0y3af1x579g"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"))))))))
    (inputs (list node-mime-db))
    (home-page "https://github.com/jshttp/mime-types#readme")
    (synopsis "The ultimate javascript content-type utility.")
    (description "The ultimate javascript content-type utility.")
    (license license:expat)))

(define-public node-mimic-response-3.1.0
  (package
    (name "node-mimic-response")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/mimic-response/-/mimic-response-3.1.0.tgz")
       (sha256
        (base32 "0m6xcg030cw88cja84avw63q3jkvqgbdp74xvs1p3m2w2my9n7ij"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "ava"
                                                  "create-test-server"
                                                  "p-event"
                                                  "pify"
                                                  "tsd"
                                                  "xo"))))))))
    (home-page "https://github.com/sindresorhus/mimic-response#readme")
    (synopsis "Mimic a Node.js HTTP response stream")
    (description "Mimic a Node.js HTTP response stream")
    (license license:expat)))

(define-public node-minimalistic-assert-1.0.1
  (package
    (name "node-minimalistic-assert")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/minimalistic-assert/-/minimalistic-assert-1.0.1.tgz")
       (sha256
        (base32 "187k0gdixs2zqkfvv6lm72w90c15rin2kx2zkyly7nyn8z4j4rgi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/calvinmetcalf/minimalistic-assert")
    (synopsis "minimalistic-assert ===")
    (description "minimalistic-assert ===")
    (license license:isc)))

(define-public node-minimatch-10.2.5
  (package
    (name "node-minimatch")
    (version "10.2.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/minimatch/-/minimatch-10.2.5.tgz")
       (sha256
        (base32 "1rd99j1d6x4lfb5ajnda6d16m7agx81x16qgwvxc7imz2mls9km6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "mkdirp"
                                                  "oxlint"
                                                  "oxlint-tsgolint"
                                                  "prettier"
                                                  "tap"
                                                  "tshy"
                                                  "typedoc"))))))))
    (inputs (list node-brace-expansion-5.0.6))
    (home-page "https://github.com/isaacs/minimatch#readme")
    (synopsis "a glob matcher in javascript")
    (description "a glob matcher in javascript")
    (license license:blue-oak1.0.0)))

(define-public node-minimist
  (package
    (name "node-minimist")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/minimist/-/minimist-1.2.8.tgz")
       (sha256
        (base32 "10yfwkrl00d8gy9z622yrklg1jax3qk38j354jfw34xk2p0pc2im"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("aud" "nyc" "tape"
                                                  "eslint"
                                                  "npmignore"
                                                  "in-publish"
                                                  "auto-changelog"
                                                  "safe-publish-latest"
                                                  "@ljharb/eslint-config"))))))))
    (home-page "https://github.com/minimistjs/minimist")
    (synopsis "Parse argument options")
    (description "Parse argument options")
    (license license:expat)))

(define-public node-minipass-7.1.3
  (package
    (name "node-minipass")
    (version "7.1.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/minipass/-/minipass-7.1.3.tgz")
       (sha256
        (base32 "04kxs8if6f6vj9vkhhrnzp3y58fls2300mlfr7yy6m9pfjz63b2j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/end-of-stream"
                                                  "@types/node"
                                                  "end-of-stream"
                                                  "node-abort-controller"
                                                  "prettier"
                                                  "tap"
                                                  "through2"
                                                  "tshy"
                                                  "typedoc"))))))))
    (home-page "https://github.com/isaacs/minipass#readme")
    (synopsis "minimal implementation of a PassThrough stream")
    (description "minimal implementation of a PassThrough stream")
    (license license:blue-oak1.0.0)))

(define-public node-minizlib-3.1.0
  (package
    (name "node-minizlib")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/minizlib/-/minizlib-3.1.0.tgz")
       (sha256
        (base32 "1qz2xkdz5zpr70z4g79pwb3ipxv81i4kfiv5y1qxswl1c4ljxgwr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "tap" "tshy"
                                                  "typedoc"))))))))
    (inputs (list node-minipass-7.1.3))
    (home-page "https://github.com/isaacs/minizlib#readme")
    (synopsis
     "A small fast zlib stream built on [minipass](http://npm.im/minipass) and Node.js's zlib binding.")
    (description
     "A small fast zlib stream built on [minipass](http://npm.im/minipass) and Node.js's zlib binding.")
    (license license:expat)))

(define-public node-mistralai-mistralai-2.2.1
  (package
    (name "node-mistralai-mistralai")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@mistralai/mistralai/-/mistralai-2.2.1.tgz")
       (sha256
        (base32 "1fr29qbbyzcvq08sw6r0599bxxralms30gz2kc7qqdlfngy1c79g"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("oxlint" "@types/ws"
                                                  "typescript" "@types/node"
                                                  "@typescript/native-preview"))))))))
    (inputs (list node-zod-to-json-schema-3.25.2 node-zod-4.4.3 node-ws-8.21.0))
    (home-page "https://github.com/mistralai/client-ts#readme")
    (synopsis "TypeScript client library for the Mistral AI API")
    (description "TypeScript client library for the Mistral AI API")
    (license license:asl2.0)))

(define-public node-mkdirp-classic-0.5.3
  (package
    (name "node-mkdirp-classic")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/mkdirp-classic/-/mkdirp-classic-0.5.3.tgz")
       (sha256
        (base32 "0ijj0y0ajccyfxfck4iq5yfkhlwcqipg2i1z9idlmk5mz56v9g19"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/mafintosh/mkdirp-classic")
    (synopsis "Mirror of mkdirp 0.5.2")
    (description "Mirror of mkdirp 0.5.2")
    (license license:expat)))

(define-public node-modelcontextprotocol-sdk-1.29.0
  (package
    (name "node-modelcontextprotocol-sdk")
    (version "1.29.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@modelcontextprotocol/sdk/-/sdk-1.29.0.tgz")
       (sha256
        (base32 "0v09ddnk77y2gxcazjxjks3bbsnk2zi4ifyqlm2fg2i8r874fl8w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'patch-overrides
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((qs (string-append (assoc-ref inputs "node-qs")
                                       "/lib/node_modules/qs")))
                (modify-json
                 (lambda (pkg-meta)
                   (assoc-set! pkg-meta "overrides"
                               (assoc-set! (assoc-ref pkg-meta "overrides")
                                           "qs" qs)))))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@cfworker/json-schema"
                                                  "@eslint/js"
                                                  "@modelcontextprotocol/conformance"
                                                  "@types/content-type"
                                                  "@types/cors"
                                                  "@types/cross-spawn"
                                                  "@types/eventsource"
                                                  "@types/express"
                                                  "@types/express-serve-static-core"
                                                  "@types/node"
                                                  "@types/supertest"
                                                  "@types/ws"
                                                  "@typescript/native-preview"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-n"
                                                  "prettier"
                                                  "supertest"
                                                  "tsx"
                                                  "typescript"
                                                  "typescript-eslint"
                                                  "vitest"
                                                  "ws"
                                                  "zod"))))))))
    (inputs (list node-zod-to-json-schema-3.25.2
                  node-zod-4.4.3
                  node-raw-body-3.0.2
                  node-qs-6.14.1
                  node-pkce-challenge-5.0.1
                  node-json-schema-typed-8.0.2
                  node-jose-6.2.3
                  node-hono-4.12.22
                  node-express-rate-limit-8.5.2
                  node-express-5.2.1
                  node-eventsource-parser-3.0.8
                  node-eventsource-3.0.7
                  node-cross-spawn-7.0.6
                  node-cors-2.8.6
                  node-content-type
                  node-ajv-formats-3.0.1
                  node-ajv-8.20.0
                  node-hono-node-server-1.19.14
                  node-zod-4.4.3
                  node-cfworker-json-schema-4.1.1))
    (home-page "https://modelcontextprotocol.io")
    (synopsis "Model Context Protocol implementation for TypeScript")
    (description "Model Context Protocol implementation for TypeScript")
    (license license:expat)))

(define-public node-mozilla-readability-0.6.0
  (package
    (name "node-mozilla-readability")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@mozilla/readability/-/readability-0.6.0.tgz")
       (sha256
        (base32 "06frg9i7ajd4w0m5yn0x5lkd535msmkdrsjxj91valdd1p2kqg3d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@release-it/keep-a-changelog"
                                                  "chai"
                                                  "eslint"
                                                  "eslint-plugin-mozilla"
                                                  "eslint-plugin-no-unsanitized"
                                                  "htmltidy2"
                                                  "js-beautify"
                                                  "jsdom"
                                                  "mocha"
                                                  "prettier"
                                                  "release-it"
                                                  "sinon"
                                                  "xml-name-validator"))))))))
    (home-page "https://github.com/mozilla/readability")
    (synopsis
     "A standalone version of the readability library used for Firefox Reader View.")
    (description
     "A standalone version of the readability library used for Firefox Reader View.")
    (license license:asl2.0)))

(define-public node-napi-build-utils-2.0.0
  (package
    (name "node-napi-build-utils")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/napi-build-utils/-/napi-build-utils-2.0.0.tgz")
       (sha256
        (base32 "17ka0j4yhfm15asnzi7rjz5qkaikzarlxkfdvx6nqh6hcqxn75x5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "jsdoc-to-markdown"
                                                  "mocha" "standard"))))))))
    (home-page "https://github.com/inspiredware/napi-build-utils#readme")
    (synopsis
     "A set of utilities to assist developers of tools that build N-API native add-ons")
    (description
     "A set of utilities to assist developers of tools that build N-API native add-ons")
    (license license:expat)))

(define-public node-next
  (package
    (inherit node)
    (name "node-next")
    (version %node-next-version)
    (source
     (origin
       (inherit (package-source node))
       (uri (string-append "https://nodejs.org/dist/v" version "/node-v"
                           version ".tar.gz"))
       (sha256
        (base32 %node-next-source-hash))))
    (arguments
     (substitute-keyword-arguments (package-arguments node)
       ((#:tests? _ #f)
        #f)
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'replace-llhttp-sources)
            (add-after 'unpack 'delete-stale-generated-files
              (lambda _
                (when (file-exists? "out")
                  (delete-file-recursively "out"))
                (when (file-exists? "config.gypi")
                  (delete-file "config.gypi"))
                (when (file-exists? "config.mk")
                  (delete-file "config.mk"))
                (when (file-exists? "config.status")
                  (delete-file "config.status"))))
            (add-after 'unpack 'disable-single-executable-application-and-lief
              (lambda _
                (substitute* "configure.py"
                  (("o\\['variables'\\]\\['single_executable_application'\\] = b\\(not options\\.disable_single_executable_application\\)")
                   "o['variables']['single_executable_application'] = 'false'")
                  (("if options\\.disable_single_executable_application:")
                   "if True:"))))
            (replace 'ignore-number-of-hardlinks
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (files (list (string-append out
                                     "/lib/node_modules/npm/node_modules/tar/dist/commonjs/write-entry.js")
                                    (string-append out
                                     "/lib/node_modules/npm/node_modules/tar/dist/esm/write-entry.js"))))
                  (for-each (lambda (file)
                              (when (file-exists? file)
                                (substitute* file
                                  (("this\\.stat\\.nlink > 1")
                                   "false")))) files))))
            (replace 'configure
              (lambda* (#:key outputs native-inputs inputs #:allow-other-keys)
                (let* ((prefix (assoc-ref outputs "out"))
                       (flags (list (string-append "--prefix=" prefix)
                                    "--shared-cares"
                                    "--shared-libuv"
                                    "--shared-nghttp2"
                                    "--shared-openssl"
                                    "--shared-zlib"
                                    "--shared-brotli"
                                    "--with-intl=system-icu"
                                    "--shared-ngtcp2"
                                    "--shared-nghttp3"
                                    "--v8-enable-snapshot-compression"
                                    "--disable-single-executable-application"
                                    "--without-lief")))
                  (format #t "build directory: ~s~%"
                          (getcwd))
                  (format #t "configure flags: ~s~%" flags)
                  (setenv "CC_host" "gcc")
                  (setenv "CXX_host" "g++")
                  (setenv "CC" "gcc")
                  (setenv "CXX" "g++")
                  (setenv "PKG_CONFIG" "pkg-config")
                  (apply invoke
                         (let ((inpts (or native-inputs inputs)))
                           (with-exception-handler (lambda (e)
                                                     (if (search-error? e)
                                                         (search-input-file
                                                          inpts "/bin/python3")
                                                         (raise-exception e)))
                                                   (lambda ()
                                                     (search-input-file inpts
                                                      "/bin/python"))
                                                   #:unwind? #t)) "configure"
                         flags))))))))
    (inputs (modify-inputs (package-inputs node)
              (replace "icu4c" icu4c-78)
              (replace "libuv" libuv-for-node-next)))
    (native-inputs (modify-inputs (package-native-inputs node)
                     (replace "icu4c" icu4c-78)
                     (replace "libuv" libuv-for-node-next)))))

(define-public node-nodable-entities-2.1.0
  (package
    (name "node-nodable-entities")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@nodable/entities/-/entities-2.1.0.tgz")
       (sha256
        (base32 "13aiwcxyma79khgisbsna0i0rg886fg8a42na3jyq64syl2l71lw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest"))))))))
    (home-page "https://github.com/nodable/val-parsers#readme")
    (synopsis
     "Entity parser for XML, HTML, External entites with security and NCR control")
    (description
     "Entity parser for XML, HTML, External entites with security and NCR control")
    (license license:expat)))

(define-public node-node-abi-3.92.0
  (package
    (name "node-node-abi")
    (version "3.92.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/node-abi/-/node-abi-3.92.0.tgz")
       (sha256
        (base32 "1i3hfbfw0xqxfjzpcf4sckr3znvdl9apdw6fj77zaa038c54klkk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape" "semantic-release"
                                                  "@semantic-release/npm"))))))))
    (inputs (list node-semver-7.8.1))
    (home-page "https://github.com/electron/node-abi#readme")
    (synopsis
     "Get the Node ABI for a given target and runtime, and vice versa.")
    (description
     "Get the Node ABI for a given target and runtime, and vice versa.")
    (license license:expat)))

(define-public node-node-addon-api-7.1.1
  (package
    (name "node-node-addon-api")
    (version "7.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/node-addon-api/-/node-addon-api-7.1.1.tgz")
       (sha256
        (base32 "10hzqyn8vxz16gmh4hwdxzw3kn83krkypc0wgb8hqz4pbb8ma15i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("path" "eslint"
                                                  "bindings"
                                                  "fs-extra"
                                                  "benchmark"
                                                  "pre-commit"
                                                  "safe-buffer"
                                                  "clang-format"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-config-semistandard"))))))))
    (home-page "https://github.com/nodejs/node-addon-api")
    (synopsis "Node.js API (Node-API)")
    (description "Node.js API (Node-API)")
    (license license:expat)))

(define-public node-node-addon-api-8.8.0
  (package
    (name "node-node-addon-api")
    (version "8.8.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/node-addon-api/-/node-addon-api-8.8.0.tgz")
       (sha256
        (base32 "1ibnphalbrf7gn477yla6dvc4ll2w9dcq8ayhlcvra1mh8d8ylkj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("benchmark" "bindings"
                                                  "clang-format"
                                                  "eslint"
                                                  "fs-extra"
                                                  "neostandard"
                                                  "pre-commit"
                                                  "semver"))))))))
    (home-page "https://github.com/nodejs/node-addon-api")
    (synopsis "Node.js API (Node-API)")
    (description "Node.js API (Node-API)")
    (license license:expat)))

(define-public node-node-domexception-1.0.0
  (package
    (name "node-node-domexception")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/node-domexception/-/node-domexception-1.0.0.tgz")
       (sha256
        (base32 "0wf9c2mxlzvr2cjwdlg1kgml40idsdyccaswp03pi2ch909k98pb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/jimmywarting/node-domexception#readme")
    (synopsis "An implementation of the DOMException class from NodeJS")
    (description "An implementation of the DOMException class from NodeJS")
    (license license:expat)))

(define-public node-node-edge-tts-1.2.10
  (package
    (name "node-node-edge-tts")
    (version "1.2.10")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/node-edge-tts/-/node-edge-tts-1.2.10.tgz")
       (sha256
        (base32 "00dy98cnjrcd5l3mkk075lgi8n4m8yvh47cdbx9g5ycndhsdljp4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/ws" "@types/yargs"
                                                  "typescript"))))))))
    (inputs (list node-yargs-17.7.2 node-ws-8.21.0
                  node-https-proxy-agent))
    (home-page "https://github.com/SchneeHertz/node-edge-tts")
    (synopsis
     "node-edge-tts is a module that using Microsoft Edge's online TTS (Text-to-Speech) service on the Node.js")
    (description
     "node-edge-tts is a module that using Microsoft Edge's online TTS (Text-to-Speech) service on the Node.js")
    (license license:expat)))

(define-public node-node-fetch-3.3.2
  (package
    (name "node-node-fetch")
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/node-fetch/-/node-fetch-3.3.2.tgz")
       (sha256
        (base32 "1ardip9x9gicwpbpv1nqw7f8cdngcg0ydy2l9dmjg3rz6q7gjnk1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("abort-controller"
                                                  "abortcontroller-polyfill"
                                                  "busboy"
                                                  "c8"
                                                  "chai"
                                                  "chai-as-promised"
                                                  "chai-iterator"
                                                  "chai-string"
                                                  "coveralls"
                                                  "form-data"
                                                  "formdata-node"
                                                  "mocha"
                                                  "p-timeout"
                                                  "stream-consumers"
                                                  "tsd"
                                                  "xo"))))))))
    (inputs (list node-formdata-polyfill-4.0.10 node-fetch-blob-3.2.0
                  node-data-uri-to-buffer-4.0.1))
    (home-page "https://github.com/node-fetch/node-fetch")
    (synopsis "A light-weight module that brings Fetch API to node.js")
    (description "A light-weight module that brings Fetch API to node.js")
    (license license:expat)))

(define-public node-nth-check-2.1.1
  (package
    (name "node-nth-check")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/nth-check/-/nth-check-2.1.1.tgz")
       (sha256
        (base32 "0hin0v480d3nzk4p96vk1ss72jy04p3p90xy1xn6a1a2qcmhmp25"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "eslint"
                                                  "ts-jest"
                                                  "prettier"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@types/boolbase"
                                                  "eslint-config-prettier"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-boolbase-1.0.0))
    (home-page "https://github.com/fb55/nth-check")
    (synopsis
     "Parses and compiles CSS nth-checks to highly optimized functions.")
    (description
     "Parses and compiles CSS nth-checks to highly optimized functions.")
    (license license:bsd-2)))

(define-public node-openai-6.26.0
  (package
    (name "node-openai")
    (version "6.26.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/openai/-/openai-6.26.0.tgz")
       (sha256
        (base32 "0j01q7kqkb24si3hm4p1aynr1bkh5iri454l15bli6i2bsyi54wq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ws" "zod"))))))))
    (home-page "https://www.npmjs.com/package/node-openai")
    (synopsis "The official TypeScript library for the OpenAI API")
    (description "The official TypeScript library for the OpenAI API")
    (license license:asl2.0)))

(define-public node-openai-6.38.0
  (package
    (name "node-openai")
    (version "6.38.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/openai/-/openai-6.38.0.tgz")
       (sha256
        (base32 "0b7mxr9s6bpi61m70pshma0g0s9xph483xvv14syj1rrgcpqiw0m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ws" "zod"))))))))
    (home-page "https://github.com/openai/openai-node#readme")
    (synopsis "The official TypeScript library for the OpenAI API")
    (description "The official TypeScript library for the OpenAI API")
    (license license:asl2.0)))

(define-public node-openclaw
  (package
    (name "node-openclaw")
    (version "2026.5.20")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/openclaw/-/openclaw-2026.5.20.tgz")
       (sha256
        (base32 "0qwg22q75zx3h3r64i8d6cd6dp242p5xmz935ibm80xh1hrviki8"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'allow-guix-node
            (lambda _
              (substitute* (cons "openclaw.mjs" (find-files "dist" "\\.js$"))
                (("22\\.19") "22.14")
                (("minor: 19") "minor: 14")
                (("const MIN_NODE_MINOR = 19;")
                 "const MIN_NODE_MINOR = 14;"))
              (modify-json
               (lambda (pkg-meta)
                 (assoc-set! pkg-meta "engines"
                             (assoc-set!
                              (or (assoc-ref pkg-meta "engines") '())
                              "node" ">=22.14.0"))))))
          (add-after 'set-home 'set-compiler
            (lambda _
              (setenv "CC" "gcc")
              (setenv "CXX" "g++")))
          (add-after 'patch-dependencies 'patch-overrides
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((fast-uri (string-append (assoc-ref inputs "node-fast-uri")
                                             "/lib/node_modules/fast-uri"))
                    (ip-address (string-append
                                 (assoc-ref inputs "node-ip-address")
                                 "/lib/node_modules/ip-address"))
                    (node-domexception
                     (string-append
                      (assoc-ref inputs "node-node-domexception")
                      "/lib/node_modules/node-domexception")))
                (modify-json
                 (lambda (pkg-meta)
                   (let ((overrides (assoc-ref pkg-meta "overrides")))
                     (assoc-set! pkg-meta "overrides"
                                 (assoc-set!
                                  (assoc-set!
                                   (assoc-set! overrides
                                               "fast-uri" fast-uri)
                                   "ip-address" ip-address)
                                  "node-domexception"
                                  node-domexception))))))))
          (add-after 'patch-dependencies 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("lit" "tsx"
                                                  "jscpd"
                                                  "jsdom"
                                                  "oxfmt"
                                                  "unrun"
                                                  "oxlint"
                                                  "tsdown"
                                                  "vitest"
                                                  "@a2ui/lit"
                                                  "@types/ws"
                                                  "@mdx-js/mdx"
                                                  "@types/node"
                                                  "@lit/context"
                                                  "signal-utils"
                                                  "@types/express"
                                                  "@grammyjs/types"
                                                  "oxlint-tsgolint"
                                                  "@lit-labs/signals"
                                                  "@copilotkit/aimock"
                                                  "@types/markdown-it"
                                                  "@vitest/coverage-v8"
                                                  "@typescript/native-preview"))))))))
    (native-inputs (list python))
    (inputs (list node-grammyjs-transformer-throttler-1.2.1
                  node-earendil-works-pi-coding-agent-0.75.4
                  node-earendil-works-pi-agent-core-0.75.4
                  node-modelcontextprotocol-sdk-1.29.0
                  node-agentclientprotocol-sdk-0.22.1
                  node-earendil-works-pi-tui-0.75.4
                  node-earendil-works-pi-ai-0.75.4
                  node-mozilla-readability-0.6.0
                  node-openclaw-proxyline-0.3.3
                  node-openclaw-fs-safe-0.2.7
                  node-tree-sitter-bash-0.25.1
                  node-lydell-node-pty-1.2.0-beta.12
                  node-homebridge-ciao-1.3.8
                  node-grammyjs-runner-2.0.3
                  node-web-tree-sitter-0.26.9
                  node-playwright-core-1.60.0
                  node-clack-prompts-1.4.0
                  node-node-edge-tts-1.2.10
                  node-google-genai-2.5.0
                  node-quickjs-wasi-2.2.0
                  node-markdown-it-14.1.1
                  node-clack-core-1.3.1
                  node-typescript-6.0.3
                  node-tokenjuice-0.7.1
                  node-pdfjs-dist-5.7.284
                  node-ipaddr-js-2.4.0
                  node-file-type-22.0.1
                  node-commander-14.0.3
                  node-web-push-3.6.7
                  node-linkedom-0.18.12
                  node-chokidar-5.0.0
                  node-typebox-1.1.38
                  node-express-5.2.1
                  node-undici-8.3.0
                  node-qrcode-1.5.4
                  node-openai-6.38.0
                  node-kysely-0.29.2
                  node-grammy-1.43.0
                  node-dotenv-17.4.2
                  node-croner-10.0.1
                  node-tslog-4.10.2
                  node-jszip-3.10.1
                  node-json5-2.2.3
                  node-chalk-5.6.2
                  node-yaml-2.9.0
                  node-jiti-2.7.0
                  node-zod-4.4.3
                  node-tar-7.5.15
                  node-fast-uri-3.1.2
                  node-ajv-8.20.0
                  node-ip-address-10.2.0
                  node-node-domexception-1.0.0
                  node-ws-8.20.1))
    (home-page "https://github.com/openclaw/openclaw#readme")
    (synopsis
     "Multi-channel AI gateway with extensible messaging integrations")
    (description
     "Multi-channel AI gateway with extensible messaging integrations")
    (license license:expat)))

(define-public node-openclaw-fs-safe-0.2.7
  (package
    (name "node-openclaw-fs-safe")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@openclaw/fs-safe/-/fs-safe-0.2.7.tgz")
       (sha256
        (base32 "0vz4p6f9iixsgw28981ic3lfw0l24dxcg7zsz8bkhcimgh80jsk6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("vitest" "typescript"
                                                  "@types/node"
                                                  "@vitest/coverage-v8"))))))))
    (home-page "https://github.com/openclaw/fs-safe#readme")
    (synopsis
     "Capability-style filesystem roots for Node.js apps that handle untrusted relative paths.")
    (description
     "Capability-style filesystem roots for Node.js apps that handle untrusted relative paths.")
    (license license:expat)))

(define-public node-openclaw-proxyline-0.3.3
  (package
    (name "node-openclaw-proxyline")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@openclaw/proxyline/-/proxyline-0.3.3.tgz")
       (sha256
        (base32 "1czxlkbd8875k3wjmv7zd9vfrxc447plwgbw5yagv76i3i0qcxr3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "@types/ws"
                                                  "tsx" "typescript" "undici"
                                                  "ws"))))))))
    (home-page "https://proxyline.dev")
    (synopsis "Process-global proxy routing for Node.js.")
    (description "Process-global proxy routing for Node.js.")
    (license license:expat)))

(define-public node-p-limit-2.3.0
  (package
    (name "node-p-limit")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/p-limit/-/p-limit-2.3.0.tgz")
       (sha256
        (base32 "15djin88kfxjdvzd7f2gnwblgclqljzqxiidm1pmrsyg14j4ajrq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"
                                                  "delay"
                                                  "in-range"
                                                  "time-span"
                                                  "tsd-check"
                                                  "random-int"))))))))
    (inputs (list node-p-try-2.2.0))
    (home-page "https://github.com/sindresorhus/p-limit#readme")
    (synopsis
     "Run multiple promise-returning & async functions with limited concurrency")
    (description
     "Run multiple promise-returning & async functions with limited concurrency")
    (license license:expat)))

(define-public node-p-locate-4.1.0
  (package
    (name "node-p-locate")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/p-locate/-/p-locate-4.1.0.tgz")
       (sha256
        (base32 "1w55dykp8ysc41xx4cl8ln3gxsxvqfhvsl62n3g6gng3cbj6lnnr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd" "delay"
                                                  "in-range" "time-span"))))))))
    (inputs (list node-p-limit-2.3.0))
    (home-page "https://github.com/sindresorhus/p-locate#readme")
    (synopsis
     "Get the first fulfilled promise that satisfies the provided testing function")
    (description
     "Get the first fulfilled promise that satisfies the provided testing function")
    (license license:expat)))

(define-public node-p-retry-4.6.2
  (package
    (name "node-p-retry")
    (version "4.6.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/p-retry/-/p-retry-4.6.2.tgz")
       (sha256
        (base32 "0n5mgwrr69i01n5y8ah793dcyphjcrmbw7jzx3lj0cfyhjs2n491"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd" "delay"))))))))
    (inputs (list node-types-retry-0.12.0 node-retry-0.13.1))
    (home-page "https://github.com/sindresorhus/p-retry#readme")
    (synopsis "Retry a promise-returning or async function")
    (description "Retry a promise-returning or async function")
    (license license:expat)))

(define-public node-p-try-2.2.0
  (package
    (name "node-p-try")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/p-try/-/p-try-2.2.0.tgz")
       (sha256
        (base32 "141pf5z1f3xmm5c0fdrfddsf7xfigjxfl103zh59bpwrk2wb5453"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/p-try#readme")
    (synopsis "`Start a promise chain")
    (description "`Start a promise chain")
    (license license:expat)))

(define-public node-pako-1.0.11
  (package
    (name "node-pako")
    (version "1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pako/-/pako-1.0.11.tgz")
       (sha256
        (base32 "0h9rmpkzyav4qxpb185z89nrhi17gy8p5mxz1k1l19sj0gf2hh0d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ansi" "benchmark"
                                                  "browserify"
                                                  "buffer-from"
                                                  "eslint"
                                                  "istanbul"
                                                  "mocha"
                                                  "multiparty"
                                                  "ndoc"
                                                  "uglify-js"
                                                  "zlibjs"))))))))
    (home-page "https://github.com/nodeca/pako")
    (synopsis
     "zlib port to javascript - fast, modularized, with browser support")
    (description
     "zlib port to javascript - fast, modularized, with browser support")
    (license #f)))

(define-public node-partial-json-0.1.7
  (package
    (name "node-partial-json")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/partial-json/-/partial-json-0.1.7.tgz")
       (sha256
        (base32 "08k35xv5dhx2k0mlamp1yl5qzyfrjrvw6d2gl8ngn9fwdznzxsih"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@vitest/coverage-istanbul"
                                                  "@vitest/ui" "typescript"
                                                  "vitest"))))))))
    (home-page "https://promplate.dev/partial-json-parser")
    (synopsis "Parse partial JSON generated by LLM")
    (description "Parse partial JSON generated by LLM")
    (license license:expat)))

(define-public node-path-exists-4.0.0
  (package
    (name "node-path-exists")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/path-exists/-/path-exists-4.0.0.tgz")
       (sha256
        (base32 "0p3pzdvfy2il8p0dvpp1l688in68bh2zzqzcfzvv7s9c634kbdfv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"))))))))
    (home-page "https://github.com/sindresorhus/path-exists#readme")
    (synopsis "Check if a path exists")
    (description "Check if a path exists")
    (license license:expat)))

(define-public node-path-expression-matcher-1.5.0
  (package
    (name "node-path-expression-matcher")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/path-expression-matcher/-/path-expression-matcher-1.5.0.tgz")
       (sha256
        (base32 "0wxix71mpn0wk4nam7wqxbd4024z216bfvyg4w46dr6s6dzzyl8k"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/plugin-transform-runtime"
                                                  "@babel/preset-env"
                                                  "@babel/register"
                                                  "@types/node"
                                                  "babel-loader"
                                                  "c8"
                                                  "eslint"
                                                  "prettier"
                                                  "typescript"
                                                  "webpack"
                                                  "webpack-cli"))))))))
    (home-page
     "https://github.com/NaturalIntelligence/path-expression-matcher#readme")
    (synopsis
     "Efficient path tracking and pattern matching for XML/JSON parsers")
    (description
     "Efficient path tracking and pattern matching for XML/JSON parsers")
    (license license:expat)))

(define-public node-path-key-3.1.1
  (package
    (name "node-path-key")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/path-key/-/path-key-3.1.1.tgz")
       (sha256
        (base32 "14kvp849wnkg6f3dqgmcb73nnb5k6b3gxf65sgf0x0qlp6n9k2ab"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "ava" "tsd"
                                                  "xo"))))))))
    (home-page "https://github.com/sindresorhus/path-key#readme")
    (synopsis "Get the PATH environment variable key cross-platform")
    (description "Get the PATH environment variable key cross-platform")
    (license license:expat)))

(define-public node-path-scurry-2.0.2
  (package
    (name "node-path-scurry")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/path-scurry/-/path-scurry-2.0.2.tgz")
       (sha256
        (base32 "1h9yq8i7j1hl3vixpqnx23hwfl10n0ri26w7npy2h360n7df78yy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@nodelib/fs.walk"
                                                  "@types/node"
                                                  "mkdirp"
                                                  "prettier"
                                                  "rimraf"
                                                  "tap"
                                                  "ts-node"
                                                  "tshy"
                                                  "typedoc"))))))))
    (inputs (list node-minipass-7.1.3 node-lru-cache-11.5.0))
    (home-page "https://github.com/isaacs/path-scurry#readme")
    (synopsis "walk paths fast and efficiently")
    (description "walk paths fast and efficiently")
    (license license:blue-oak1.0.0)))

(define-public node-pdfjs-dist-5.7.284
  (package
    (name "node-pdfjs-dist")
    (version "5.7.284")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pdfjs-dist/-/pdfjs-dist-5.7.284.tgz")
       (sha256
        (base32 "0ga1l10b6wa45ii0c3hgyf6nkfxr89i283licvk4ziyy60jvfkgi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://mozilla.github.io/pdf.js/")
    (synopsis "Generic build of Mozilla's PDF.js library.")
    (description "Generic build of Mozilla's PDF.js library.")
    (license license:asl2.0)))

(define-public node-pkce-challenge-5.0.1
  (package
    (name "node-pkce-challenge")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/pkce-challenge/-/pkce-challenge-5.0.1.tgz")
       (sha256
        (base32 "0w7a7gzxrn5widngl5w358kfi68njp121ig77p8n4mf0bfpbpz6i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "diverge" "esbuild"
                                                  "typescript" "@types/jest"
                                                  "@types/node"))))))))
    (home-page "https://github.com/crouchcd/pkce-challenge#readme")
    (synopsis
     "Generate or verify a Proof Key for Code Exchange (PKCE) challenge pair")
    (description
     "Generate or verify a Proof Key for Code Exchange (PKCE) challenge pair")
    (license license:expat)))

(define-public node-playwright-core-1.60.0
  (package
    (name "node-playwright-core")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/playwright-core/-/playwright-core-1.60.0.tgz")
       (sha256
        (base32 "1gr5baf9877r718wagk4x8awkhgclw73b5n9nlga942ylwdzh7cb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://playwright.dev")
    (synopsis "A high-level API to automate web browsers")
    (description "A high-level API to automate web browsers")
    (license license:asl2.0)))

(define-public node-pngjs-5.0.0
  (package
    (name "node-pngjs")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pngjs/-/pngjs-5.0.0.tgz")
       (sha256
        (base32 "083gydxgqw4xsq1kc7qiml743iw4z1s2hbl26rx2s0kqw2xhp5jd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "--delay-directory-restore"
                      "--no-same-owner" "--no-same-permissions"
                      "-xvf" source)
              (chmod "package" #o755)
              (invoke "chmod" "-R" "u+rwX" "package")
              (chdir "package")))
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("browserify" "buffer-equal"
                                                  "codecov"
                                                  "connect"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "nyc"
                                                  "prettier"
                                                  "puppeteer"
                                                  "serve-static"
                                                  "tap-dot"
                                                  "tape"))))))))
    (home-page "https://github.com/lukeapage/pngjs")
    (synopsis
     "PNG encoder/decoder in pure JS, supporting any bit size & interlace, async & sync with full test suite.")
    (description
     "PNG encoder/decoder in pure JS, supporting any bit size & interlace, async & sync with full test suite.")
    (license license:expat)))

(define-public node-prebuild-install-7.1.3
  (package
    (name "node-prebuild-install")
    (version "7.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/prebuild-install/-/prebuild-install-7.1.3.tgz")
       (sha256
        (base32 "1azr614z2qdb67swy33ngiwkhns0c0jkdjj84xjhlk62ygnbhhb7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nock" "tape"
                                                  "tempy"
                                                  "rimraf"
                                                  "hallmark"
                                                  "standard"
                                                  "a-native-module"))))))))
    (inputs (list node-github-from-package-0.0.0
                  node-napi-build-utils-2.0.0
                  node-expand-template-2.0.3
                  node-mkdirp-classic-0.5.3
                  node-tunnel-agent-0.6.0
                  node-detect-libc-2.1.2
                  node-simple-get-4.0.1
                  node-node-abi-3.92.0
                  node-minimist
                  node-tar-fs-2.1.4
                  node-pump-3.0.4
                  node-rc-1.2.8))
    (home-page "https://github.com/prebuild/prebuild-install")
    (synopsis
     "A command line tool to easily install prebuilt binaries for multiple version of node/iojs on a specific platform")
    (description
     "A command line tool to easily install prebuilt binaries for multiple version of node/iojs on a specific platform")
    (license license:expat)))

(define-public node-process-nextick-args-2.0.1
  (package
    (name "node-process-nextick-args")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/process-nextick-args/-/process-nextick-args-2.0.1.tgz")
       (sha256
        (base32 "16w8m2ycy5s4ykgdfg97qxa67gfvkh6x3vdwfsncafyj4p3zhns2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap"))))))))
    (home-page "https://github.com/calvinmetcalf/process-nextick-args")
    (synopsis "process.nextTick but always with args")
    (description "process.nextTick but always with args")
    (license license:expat)))

(define-public node-protobufjs-7.6.1
  (package
    (name "node-protobufjs")
    (version "7.6.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/protobufjs/-/protobufjs-7.6.1.tgz")
       (sha256
        (base32 "1j5ql7vqb68yds0vbivb7n5bswabk9j7q1ph4j4x8vwpg8ib7iif"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("benchmark" "browserify"
                                                  "browserify-wrap"
                                                  "bundle-collapser"
                                                  "chalk"
                                                  "escodegen"
                                                  "eslint"
                                                  "espree"
                                                  "estraverse"
                                                  "gh-pages"
                                                  "git-raw-commits"
                                                  "git-semver-tags"
                                                  "google-protobuf"
                                                  "gulp"
                                                  "gulp-header"
                                                  "gulp-if"
                                                  "gulp-sourcemaps"
                                                  "gulp-uglify"
                                                  "jaguarjs-jsdoc"
                                                  "jsdoc"
                                                  "minimist"
                                                  "nyc"
                                                  "reflect-metadata"
                                                  "tape"
                                                  "tslint"
                                                  "typescript"
                                                  "uglify-js"
                                                  "vinyl-buffer"
                                                  "vinyl-fs"
                                                  "vinyl-source-stream"))))))))
    (inputs (list node-long-5.3.2
                  node-types-node-25.9.1
                  node-protobufjs-utf8-1.1.1
                  node-protobufjs-pool-1.1.0
                  node-protobufjs-path-1.1.2
                  node-protobufjs-inquire-1.1.2
                  node-protobufjs-float-1.0.2
                  node-protobufjs-fetch-1.1.1
                  node-protobufjs-eventemitter-1.1.1
                  node-protobufjs-codegen-2.0.5
                  node-protobufjs-base64-1.1.2
                  node-protobufjs-aspromise-1.1.2))
    (home-page "https://protobufjs.github.io/protobuf.js/")
    (synopsis "Protocol Buffers for JavaScript (& TypeScript).")
    (description "Protocol Buffers for JavaScript (& TypeScript).")
    (license license:bsd-3)))

(define-public node-protobufjs-aspromise-1.1.2
  (package
    (name "node-protobufjs-aspromise")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@protobufjs/aspromise/-/aspromise-1.1.2.tgz")
       (sha256
        (base32 "06f1w67bgnw3zr7hbhv0yfbzz5mwbqsnc1jyri5zmwh9ha5qfh80"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "tape"))))))))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis "Returns a promise from a node-style callback function.")
    (description "Returns a promise from a node-style callback function.")
    (license license:bsd-3)))

(define-public node-protobufjs-base64-1.1.2
  (package
    (name "node-protobufjs-base64")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@protobufjs/base64/-/base64-1.1.2.tgz")
       (sha256
        (base32 "0shs0f28zn7q7vprwlh6kzfrnmsg2h94rhyzii3i4ma4w7k6wznn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "tape"))))))))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis "A minimal base64 implementation for number arrays.")
    (description "A minimal base64 implementation for number arrays.")
    (license license:bsd-3)))

(define-public node-protobufjs-codegen-2.0.5
  (package
    (name "node-protobufjs-codegen")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@protobufjs/codegen/-/codegen-2.0.5.tgz")
       (sha256
        (base32 "009sl5cds5r1ikixc091pib9mvysiscl3a2h4hxa5jqv4b8zxfi0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis "A minimalistic code generation utility.")
    (description "A minimalistic code generation utility.")
    (license license:bsd-3)))

(define-public node-protobufjs-eventemitter-1.1.1
  (package
    (name "node-protobufjs-eventemitter")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@protobufjs/eventemitter/-/eventemitter-1.1.1.tgz")
       (sha256
        (base32 "0z3r58vyqxn745pq700l9w1l1gif6n785ldd3lf7fbh6ny6bjdjm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "tape"))))))))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis "A minimal event emitter.")
    (description "A minimal event emitter.")
    (license license:bsd-3)))

(define-public node-protobufjs-fetch-1.1.1
  (package
    (name "node-protobufjs-fetch")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@protobufjs/fetch/-/fetch-1.1.1.tgz")
       (sha256
        (base32 "0s3jicdxhgqjsrdi18qvpgvp6fzbiqsz4f82ig1y3g6ycaf1sj2l"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "tape"))))))))
    (inputs (list node-protobufjs-aspromise-1.1.2))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis "Fetches the contents of a file accross node and browsers.")
    (description "Fetches the contents of a file accross node and browsers.")
    (license license:bsd-3)))

(define-public node-protobufjs-float-1.0.2
  (package
    (name "node-protobufjs-float")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@protobufjs/float/-/float-1.0.2.tgz")
       (sha256
        (base32 "0bbivv9vgs7myqayb6akklb35470lvkahlidc1abg09jsl9ddcr0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("benchmark" "chalk"
                                                  "ieee754" "istanbul" "tape"))))))))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis
     "Reads / writes floats / doubles from / to buffers in both modern and ancient browsers.")
    (description
     "Reads / writes floats / doubles from / to buffers in both modern and ancient browsers.")
    (license license:bsd-3)))

(define-public node-protobufjs-inquire-1.1.2
  (package
    (name "node-protobufjs-inquire")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@protobufjs/inquire/-/inquire-1.1.2.tgz")
       (sha256
        (base32 "1am7m1ikj4b0yi0w7hhiivixrqxjji0h802d89ql51qrw6s85lra"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "tape"))))))))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis
     "Requires a module only if available and hides the require call from bundlers.")
    (description
     "Requires a module only if available and hides the require call from bundlers.")
    (license license:bsd-3)))

(define-public node-protobufjs-path-1.1.2
  (package
    (name "node-protobufjs-path")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@protobufjs/path/-/path-1.1.2.tgz")
       (sha256
        (base32 "131jr8ykzasqh4hjg7r15mnyirxqafxs4da5fgakcg0p23llwdnc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "tape"))))))))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis
     "A minimal path module to resolve Unix, Windows and URL paths alike.")
    (description
     "A minimal path module to resolve Unix, Windows and URL paths alike.")
    (license license:bsd-3)))

(define-public node-protobufjs-pool-1.1.0
  (package
    (name "node-protobufjs-pool")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@protobufjs/pool/-/pool-1.1.0.tgz")
       (sha256
        (base32 "00r7ffp1skf16ad4dzl2z2ffzhrwl3vyj6mzqb8wgll2fb9dn8gp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "tape"))))))))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis "A general purpose buffer pool.")
    (description "A general purpose buffer pool.")
    (license license:bsd-3)))

(define-public node-protobufjs-utf8-1.1.1
  (package
    (name "node-protobufjs-utf8")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@protobufjs/utf8/-/utf8-1.1.1.tgz")
       (sha256
        (base32 "1vvk8s39nfi6sjhaf2fwsqkrh5q33qvv965fl7hak5ar09nlxsj6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "tape"))))))))
    (home-page "https://github.com/dcodeIO/protobuf.js#readme")
    (synopsis "A minimal UTF8 implementation for number arrays.")
    (description "A minimal UTF8 implementation for number arrays.")
    (license license:bsd-3)))

(define-public node-pump-3.0.4
  (package
    (name "node-pump")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pump/-/pump-3.0.4.tgz")
       (sha256
        (base32 "0dlv9kb9py5sgiv935266khkb284iqfyhcqbpgd5q0rh9kycfn1w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-once-1.4.0 node-end-of-stream-1.4.5))
    (home-page "https://github.com/mafintosh/pump#readme")
    (synopsis
     "pipe streams together and close all of them if one of them closes")
    (description
     "pipe streams together and close all of them if one of them closes")
    (license license:expat)))

(define-public node-punycode-js-2.3.1
  (package
    (name "node-punycode-js")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/punycode.js/-/punycode.js-2.3.1.tgz")
       (sha256
        (base32 "1a14fcgs4fcf4v6g3pxy27zxvqv42ig5gfhl00i6949mlsddhs47"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("codecov" "nyc" "mocha"))))))))
    (home-page "https://mths.be/punycode")
    (synopsis
     "A robust Punycode converter that fully complies to RFC 3492 and RFC 5891, and works on nearly all JavaScript platforms.")
    (description
     "A robust Punycode converter that fully complies to RFC 3492 and RFC 5891, and works on nearly all JavaScript platforms.")
    (license license:expat)))

(define-public node-qrcode-1.5.4
  (package
    (name "node-qrcode")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/qrcode/-/qrcode-1.5.4.tgz")
       (sha256
        (base32 "1c724f4xdyk57w5jvh3k05xrg1dp7402wjpmvlprrwwrqbq78whc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "sinon"
                                                  "canvas"
                                                  "colors"
                                                  "rollup"
                                                  "express"
                                                  "standard"
                                                  "browserify"
                                                  "canvasutil"
                                                  "@babel/core"
                                                  "htmlparser2"
                                                  "@babel/preset-env"
                                                  "rollup-plugin-babel"
                                                  "rollup-plugin-terser"
                                                  "@rollup/plugin-commonjs"
                                                  "@rollup/plugin-node-resolve"))))))))
    (inputs (list node-dijkstrajs-1.0.3 node-yargs-15.4.1 node-pngjs-5.0.0))
    (home-page "http://github.com/soldair/node-qrcode")
    (synopsis
     "QRCode / 2d Barcode api with both server side and client side support using canvas")
    (description
     "QRCode / 2d Barcode api with both server side and client side support using canvas")
    (license license:expat)))

(define-public node-qs-6.14.1
  (package
    (name "node-qs")
    (version "6.14.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/qs/-/qs-6.14.1.tgz")
       (sha256
        (base32 "01lf0p7zbv8d50wh1lqvvdb23sagljslxyn5hb1x37dlwf5aq4j7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@browserify/envify"
                                                  "@browserify/uglifyify"
                                                  "@ljharb/eslint-config"
                                                  "browserify"
                                                  "bundle-collapser"
                                                  "common-shakeify"
                                                  "eclint"
                                                  "es-value-fixtures"
                                                  "eslint"
                                                  "evalmd"
                                                  "for-each"
                                                  "glob"
                                                  "has-bigints"
                                                  "has-override-mistake"
                                                  "has-property-descriptors"
                                                  "has-proto"
                                                  "has-symbols"
                                                  "iconv-lite"
                                                  "in-publish"
                                                  "jackspeak"
                                                  "jiti"
                                                  "mkdirp"
                                                  "mock-property"
                                                  "module-deps"
                                                  "npmignore"
                                                  "nyc"
                                                  "object-inspect"
                                                  "qs-iconv"
                                                  "safe-publish-latest"
                                                  "safer-buffer"
                                                  "tape"
                                                  "unassertify"))))))))
    (inputs (list node-side-channel))
    (home-page "https://github.com/ljharb/qs")
    (synopsis
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (description
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (license license:bsd-3)))

(define-public node-qs-6.15.2
  (package
    (name "node-qs")
    (version "6.15.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/qs/-/qs-6.15.2.tgz")
       (sha256
        (base32 "15xqhlhplzsvwyrgfrxb3bc20k054i9d3h99rmdiy632mjrx96g6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@browserify/envify"
                                                  "@browserify/uglifyify"
                                                  "@ljharb/eslint-config"
                                                  "browserify"
                                                  "bundle-collapser"
                                                  "common-shakeify"
                                                  "eclint"
                                                  "es-value-fixtures"
                                                  "eslint"
                                                  "evalmd"
                                                  "for-each"
                                                  "glob"
                                                  "has-bigints"
                                                  "has-override-mistake"
                                                  "has-property-descriptors"
                                                  "has-proto"
                                                  "has-symbols"
                                                  "iconv-lite"
                                                  "in-publish"
                                                  "jackspeak"
                                                  "jiti"
                                                  "mkdirp"
                                                  "mock-property"
                                                  "module-deps"
                                                  "npmignore"
                                                  "nyc"
                                                  "object-inspect"
                                                  "qs-iconv"
                                                  "safe-publish-latest"
                                                  "safer-buffer"
                                                  "tape"
                                                  "unassertify"))))))))
    (inputs (list node-side-channel))
    (home-page "https://github.com/ljharb/qs")
    (synopsis
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (description
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (license license:bsd-3)))

(define-public node-quickjs-wasi-2.2.0
  (package
    (name "node-quickjs-wasi")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/quickjs-wasi/-/quickjs-wasi-2.2.0.tgz")
       (sha256
        (base32 "1lcwhgc73zrp0n6y9bpkirmrrw4g7cgdrbx8ja3gnpvh5rn3ipgl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("bun" "vitest"
                                                  "esbuild"
                                                  "tinybench"
                                                  "typescript"
                                                  "@types/node"
                                                  "@vercel/nft"
                                                  "degenerator"
                                                  "core-js-pure"
                                                  "whatwg-fetch"
                                                  "@changesets/cli"
                                                  "fast-text-encoding"
                                                  "quickjs-emscripten"
                                                  "@changesets/changelog-github"))))))))
    (home-page "https://github.com/vercel-labs/quickjs-wasi#readme")
    (synopsis
     "Snapshotable JavaScript runtime via WebAssembly. QuickJS-NG compiled to WASM with snapshot/restore support.")
    (description
     "Snapshotable JavaScript runtime via WebAssembly. QuickJS-NG compiled to WASM with snapshot/restore support.")
    (license license:expat)))

(define-public node-raw-body-3.0.2
  (package
    (name "node-raw-body")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/raw-body/-/raw-body-3.0.2.tgz")
       (sha256
        (base32 "1nqrhjp2v55z7rd13f1qlgnk6vgc4hmd9c8awnxmipina012mpk6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "bluebird"
                                                  "neostandard"
                                                  "safe-buffer"
                                                  "readable-stream"
                                                  "@stylistic/eslint-plugin"
                                                  "@stylistic/eslint-plugin-js"))))))))
    (inputs (list node-http-errors-2.0.1 node-iconv-lite-0.7.2
                  node-unpipe node-bytes))
    (home-page "https://github.com/stream-utils/raw-body#readme")
    (synopsis "Get and validate the raw body of a readable stream.")
    (description "Get and validate the raw body of a readable stream.")
    (license license:expat)))

(define-public node-rc-1.2.8
  (package
    (name "node-rc")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/rc/-/rc-1.2.8.tgz")
       (sha256
        (base32 "0fz9r8aphj84cvxv8k0m7g008gffz561r6ryvljb8gi0hpjgm983"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-strip-json-comments-2.0.1 node-deep-extend-0.6.0
                  node-minimist node-ini-1.3.8))
    (home-page "https://github.com/dominictarr/rc#readme")
    (synopsis "hardwired configuration loader")
    (description "hardwired configuration loader")
    (license #f)))

(define-public node-react-reconciler
  (package
    (name "node-react-reconciler")
    (version "0.29.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/react-reconciler/-/react-reconciler-0.29.2.tgz")
       (sha256
        (base32 "082adpyk2g9gnz9rx5gz4gn7vrhaavg93ihazzgp4kkxf6xshjl3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("react"))))))))
    (inputs (list node-loose-envify node-scheduler node-react))
    (home-page "https://reactjs.org/")
    (synopsis "React package for creating custom renderers.")
    (description "React package for creating custom renderers.")
    (license license:expat)))

(define-public node-ink
  (package
    (name "node-ink")
    (version "5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ink/-/ink-5.2.1.tgz")
       (sha256
        (base32 "1n4zr8ygvdcdn309cs4xwjjkgkcl0bp3qfj3zm9qi0rj6gcl7ryv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/ink")))
                (mkdir-p lib)
                (copy-recursively "." lib) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ms" "xo"
                                                  "ava"
                                                  "boxen"
                                                  "delay"
                                                  "react"
                                                  "sinon"
                                                  "p-queue"
                                                  "ts-node"
                                                  "node-pty"
                                                  "prettier"
                                                  "@types/ms"
                                                  "@types/ws"
                                                  "strip-ansi"
                                                  "typescript"
                                                  "@types/node"
                                                  "@types/react"
                                                  "@types/sinon"
                                                  "@faker-js/faker"
                                                  "@types/benchmark"
                                                  "@types/scheduler"
                                                  "@types/signal-exit"
                                                  "@types/stack-utils"
                                                  "eslint-plugin-react"
                                                  "react-devtools-core"
                                                  "@sindresorhus/tsconfig"
                                                  "eslint-config-xo-react"
                                                  "@types/react-reconciler"
                                                  "@vdemedes/prettier-config"
                                                  "eslint-plugin-react-hooks"))))))))
    (inputs (list node-alcalzone-ansi-tokenize
                  node-react-reconciler
                  node-patch-console
                  node-indent-string
                  node-string-width
                  node-code-excerpt
                  node-cli-truncate
                  node-ansi-escapes
                  node-yoga-layout
                  node-widest-line
                  node-stack-utils
                  node-signal-exit
                  node-ansi-styles
                  node-slice-ansi
                  node-es-toolkit
                  node-cli-cursor
                  node-wrap-ansi
                  node-type-fest
                  node-scheduler
                  node-cli-boxes
                  node-auto-bind
                  node-is-in-ci
                  node-chalk
                  node-ws
                  node-react-devtools-core
                  node-types-react
                  node-react))
    (home-page "https://github.com/vadimdemedes/ink")
    (synopsis "React for CLI")
    (description "React for CLI")
    (license license:expat)))

(define-public node-diff
  (package
    (name "node-diff")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/diff/-/diff-7.0.0.tgz")
       (sha256
        (base32 "1h9aiz8ffsp9qjxd9m5c1c5hdm0iczcj9wd6lxwfasxx0scff4li"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "grunt"
                                                  "karma"
                                                  "mocha"
                                                  "eslint"
                                                  "rollup"
                                                  "semver"
                                                  "webpack"
                                                  "istanbul"
                                                  "grunt-cli"
                                                  "@babel/cli"
                                                  "grunt-exec"
                                                  "@babel/core"
                                                  "grunt-babel"
                                                  "grunt-karma"
                                                  "karma-mocha"
                                                  "babel-eslint"
                                                  "babel-loader"
                                                  "grunt-eslint"
                                                  "grunt-webpack"
                                                  "karma-webpack"
                                                  "@colors/colors"
                                                  "@babel/register"
                                                  "grunt-mocha-test"
                                                  "@babel/preset-env"
                                                  "grunt-contrib-copy"
                                                  "webpack-dev-server"
                                                  "grunt-contrib-clean"
                                                  "grunt-contrib-watch"
                                                  "rollup-plugin-babel"
                                                  "grunt-contrib-uglify"
                                                  "grunt-mocha-istanbul"
                                                  "karma-mocha-reporter"
                                                  "karma-chrome-launcher"
                                                  "karma-sourcemap-loader"
                                                  "@babel/plugin-transform-modules-commonjs"))))))))
    (home-page "https://www.npmjs.com/package/node-diff")
    (synopsis "A JavaScript text diff implementation.")
    (description "A JavaScript text diff implementation.")
    (license license:bsd-3)))

(define-public node-meow
  (package
    (name "node-meow")
    (version "13.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/meow/-/meow-13.2.0.tgz")
       (sha256
        (base32 "1l7jcffqi8571xacl39brx9s24akbpzdq9qr9h6zmwm1afwcvrqq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"
                                                  "tsd"
                                                  "execa"
                                                  "globby"
                                                  "redent"
                                                  "rollup"
                                                  "read-pkg"
                                                  "type-fest"
                                                  "decamelize"
                                                  "typescript"
                                                  "common-tags"
                                                  "yargs-parser"
                                                  "indent-string"
                                                  "trim-newlines"
                                                  "camelcase-keys"
                                                  "@types/minimist"
                                                  "decamelize-keys"
                                                  "delete_comments"
                                                  "read-package-up"
                                                  "minimist-options"
                                                  "rollup-plugin-dts"
                                                  "@rollup/plugin-json"
                                                  "rollup-plugin-license"
                                                  "normalize-package-data"
                                                  "@rollup/plugin-commonjs"
                                                  "@rollup/plugin-node-resolve"))))))))
    (home-page "https://github.com/sindresorhus/meow")
    (synopsis "CLI app helper")
    (description "CLI app helper")
    (license license:expat)))

(define-public node-readable-stream-2.3.8
  (package
    (name "node-readable-stream")
    (version "2.3.8")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/readable-stream/-/readable-stream-2.3.8.tgz")
       (sha256
        (base32 "0cm5g4a5mfqb5im98mzv5y3dpv377bws6wlgpv5wk2pn90wn2j0n"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "tap"
                                                  "tape"
                                                  "lolex"
                                                  "assert"
                                                  "buffer"
                                                  "babel-polyfill"))))))))
    (inputs (list node-process-nextick-args-2.0.1
                  node-util-deprecate-1.0.2
                  node-string-decoder-1.1.1
                  node-core-util-is-1.0.3
                  node-safe-buffer-5.1.2
                  node-inherits-2.0.4
                  node-isarray-1.0.0))
    (home-page "https://github.com/nodejs/readable-stream#readme")
    (synopsis "Streams3, a user-land copy of the stream library from Node.js")
    (description
     "Streams3, a user-land copy of the stream library from Node.js")
    (license license:expat)))

(define-public node-readable-stream-3.6.2
  (package
    (name "node-readable-stream")
    (version "3.6.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/readable-stream/-/readable-stream-3.6.2.tgz")
       (sha256
        (base32 "0pdb0mrh95ks672ikgj8frx9nh078bfyngknj70ak2iibv06dn7d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("bl" "nyc"
                                                  "tap"
                                                  "glob"
                                                  "pump"
                                                  "tape"
                                                  "lolex"
                                                  "airtap"
                                                  "assert"
                                                  "rimraf"
                                                  "tar-fs"
                                                  "@babel/cli"
                                                  "hyperquest"
                                                  "@babel/core"
                                                  "events.once"
                                                  "gunzip-maybe"
                                                  "util-promisify"
                                                  "@babel/polyfill"
                                                  "@babel/preset-env"
                                                  "deep-strict-equal"))))))))
    (inputs (list node-util-deprecate-1.0.2 node-string-decoder-1.3.0
                  node-inherits-2.0.4))
    (home-page "https://github.com/nodejs/readable-stream#readme")
    (synopsis "Streams3, a user-land copy of the stream library from Node.js")
    (description
     "Streams3, a user-land copy of the stream library from Node.js")
    (license license:expat)))

(define-public node-readdirp-5.0.0
  (package
    (name "node-readdirp")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/readdirp/-/readdirp-5.0.0.tgz")
       (sha256
        (base32 "01q4n1i7m5k7zvk1chxca088814nyvcd28rb8s64pnwgpzbdkv01"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@paulmillr/jsbt"
                                                  "@types/node"
                                                  "c8"
                                                  "chai"
                                                  "chai-subset"
                                                  "prettier"
                                                  "typescript"))))))))
    (home-page "https://github.com/paulmillr/readdirp")
    (synopsis "Recursive version of fs.readdir with small RAM & CPU footprint")
    (description
     "Recursive version of fs.readdir with small RAM & CPU footprint")
    (license license:expat)))

(define-public node-require-from-string-2.0.2
  (package
    (name "node-require-from-string")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/require-from-string/-/require-from-string-2.0.2.tgz")
       (sha256
        (base32 "10ldp2bzb86czf47kmvirn9x2976yh6g0my7l1spg3whcm4llsfb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha"))))))))
    (home-page "https://github.com/floatdrop/require-from-string#readme")
    (synopsis "Require module from string")
    (description "Require module from string")
    (license license:expat)))

(define-public node-require-main-filename-2.0.0
  (package
    (name "node-require-main-filename")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/require-main-filename/-/require-main-filename-2.0.0.tgz")
       (sha256
        (base32 "004f896jmh9di8kd5ypszhpk6yzbdsx0lz5cqb3r2q7v31imdfy5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "standard"
                                                  "standard-version" "tap"))))))))
    (home-page "https://github.com/yargs/require-main-filename#readme")
    (synopsis
     "shim for require.main.filename() that works in as many environments as possible")
    (description
     "shim for require.main.filename() that works in as many environments as possible")
    (license license:isc)))

(define-public node-retry-0.13.1
  (package
    (name "node-retry")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/retry/-/retry-0.13.1.tgz")
       (sha256
        (base32 "1140kg3sia3i6fi3bzpypam1gysa7i3szdyci3l7am44br2dh8bm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("fake" "istanbul" "tape"))))))))
    (home-page "https://github.com/tim-kos/node-retry")
    (synopsis
     "Abstraction for exponential and custom retry strategies for failed operations.")
    (description
     "Abstraction for exponential and custom retry strategies for failed operations.")
    (license license:expat)))

(define-public node-run-applescript
  (package
    (name "node-run-applescript")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/run-applescript/-/run-applescript-7.0.0.tgz")
       (sha256
        (base32 "1sbmpymakf3npd0psjg382m3l30g184p2jqx14lkad7ciypbs5k7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/run-applescript")
    (synopsis "Run AppleScript and get the result")
    (description "Run AppleScript and get the result")
    (license license:expat)))

(define-public node-bundle-name
  (package
    (name "node-bundle-name")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bundle-name/-/bundle-name-4.1.0.tgz")
       (sha256
        (base32 "0489x4n9f6z8v4qpycgakqp30j58xsgi69lrl96bw7698119vr1m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "xo"))))))))
    (inputs (list node-run-applescript))
    (home-page "https://github.com/sindresorhus/bundle-name")
    (synopsis "Get bundle name from a bundle identifier on macOS")
    (description
     "This package extracts the human-readable application name from a macOS
bundle identifier.  For example, it converts 'com.apple.Safari' to 'Safari'.")
    (license license:expat)))

(define-public node-default-browser-id
  (package
    (name "node-default-browser-id")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/default-browser-id/-/default-browser-id-5.0.0.tgz")
       (sha256
        (base32 "0kpdxc9s6z6bsda3j77njysr6iq8aamml4hm80gngl7gx6d55i2z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "xo"))))))))
    (home-page "https://github.com/sindresorhus/default-browser-id")
    (synopsis
     "Get the bundle identifier of the default browser (macOS). Example: com.apple.Safari")
    (description
     "Get the bundle identifier of the default browser (macOS). Example: com.apple.Safari")
    (license license:expat)))

(define-public node-default-browser
  (package
    (name "node-default-browser")
    (version "5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/default-browser/-/default-browser-5.2.1.tgz")
       (sha256
        (base32 "0r57clkxxsnlajyx4yk7lyvdixx66k3awr5746vkgqhavy6qk6yf"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-default-browser-id node-bundle-name))
    (home-page "https://github.com/sindresorhus/default-browser")
    (synopsis "Get the default browser")
    (description "Get the default browser")
    (license license:expat)))

(define-public node-define-lazy-prop
  (package
    (name "node-define-lazy-prop")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/define-lazy-prop/-/define-lazy-prop-3.0.0.tgz")
       (sha256
        (base32 "1da99k4vnnn9bxpgjniai8248bcf4w9cwyn7p3wlzii9l9kzxsdv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/define-lazy-prop")
    (synopsis "Define a lazily evaluated property on an object")
    (description "Define a lazily evaluated property on an object")
    (license license:expat)))

(define-public node-is-docker
  (package
    (name "node-is-docker")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/is-docker/-/is-docker-3.0.0.tgz")
       (sha256
        (base32 "1vnxw8y4p31nx66rbgxhd40jc5zx8akmcf7fpl3gy7n84l5hn8qs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "sinon" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/is-docker")
    (synopsis "Check if the process is running inside a Docker container")
    (description "Check if the process is running inside a Docker container")
    (license license:expat)))

(define-public node-is-inside-container
  (package
    (name "node-is-inside-container")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/is-inside-container/-/is-inside-container-1.0.0.tgz")
       (sha256
        (base32 "0yz0fbbkypqsx5d5cls1ik8928zkqhwl2kcp4iv7bi2g8fw9bpnv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "esmock" "tsd" "xo"))))))))
    (inputs (list node-is-docker))
    (home-page "https://github.com/sindresorhus/is-inside-container")
    (synopsis
     "Check if the process is running inside a container (Docker/Podman)")
    (description
     "Check if the process is running inside a container (Docker/Podman)")
    (license license:expat)))

(define-public node-is-wsl
  (package
    (name "node-is-wsl")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/is-wsl/-/is-wsl-3.1.0.tgz")
       (sha256
        (base32 "1ch2zg7f3dqv2c142lkbygj85j9xiq336kcm509l26k4pikswg6w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "esmock" "tsd" "xo"))))))))
    (inputs (list node-is-inside-container))
    (home-page "https://github.com/sindresorhus/is-wsl")
    (synopsis
     "Check if the process is running inside Windows Subsystem for Linux (Bash on Windows)")
    (description
     "Check if the process is running inside Windows Subsystem for Linux (Bash on Windows)")
    (license license:expat)))

(define-public node-open
  (package
    (name "node-open")
    (version "10.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/open/-/open-10.1.2.tgz")
       (sha256
        (base32 "1b8jhmysf9fnha0b388yv4vlff1mkip9lyzly3irwhwh4hpfcwbn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "ava" "tsd"
                                                  "xo"))))))))
    (inputs (list node-is-wsl node-is-inside-container node-define-lazy-prop
                  node-default-browser))
    (home-page "https://github.com/sindresorhus/open")
    (synopsis "Open stuff like URLs, files, executables. Cross-platform.")
    (description "Open stuff like URLs, files, executables. Cross-platform.")
    (license license:expat)))

(define-public node-react
  (package
    (name "node-react")
    (version "18.3.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/react/-/react-18.3.1.tgz")
       (sha256
        (base32 "0iq3hz3mmzgrc1h3181hk8446skwshd7hbclhzrymrvjlq0yv6wd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-loose-envify))
    (home-page "https://reactjs.org/")
    (synopsis "React is a JavaScript library for building user interfaces.")
    (description "React is a JavaScript library for building user interfaces.")
    (license license:expat)))

(define-public node-dotenv
  (package
    (name "node-dotenv")
    (version "16.5.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/dotenv/-/dotenv-16.5.0.tgz")
       (sha256
        (base32 "02831fia3jppjgg973n3dgp2ffh4waqgpirknnizw5mxyynwyl1z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "decache"
                                                  "sinon"
                                                  "standard"
                                                  "standard-version"
                                                  "tap"
                                                  "typescript"))))))))
    (home-page "https://github.com/motdotla/dotenv")
    (synopsis "Loads environment variables from .env file")
    (description "Loads environment variables from .env file")
    (license license:bsd-2)))

(define-public node-safe-buffer-5.1.2
  (package
    (name "node-safe-buffer")
    (version "5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.1.2.tgz")
       (sha256
        (base32 "08ma0a2a9j537bxl7qd2dn6sjcdhrclpdbslr19bkbyc1z30d4p0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("standard" "tape"))))))))
    (home-page "https://github.com/feross/safe-buffer")
    (synopsis "Safer Node.js Buffer API")
    (description "Safer Node.js Buffer API")
    (license license:expat)))

(define-public node-semver-7.8.1
  (package
    (name "node-semver")
    (version "7.8.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/semver/-/semver-7.8.1.tgz")
       (sha256
        (base32 "06vdrdl8mara56r969jyhyab34qh8dgjf4msvn6my25nzcwi6pjz"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@npmcli/eslint-config"
                                                  "@npmcli/template-oss"
                                                  "benchmark" "tap"))))))))
    (home-page "https://github.com/npm/node-semver#readme")
    (synopsis "The semantic version parser used by npm.")
    (description "The semantic version parser used by npm.")
    (license license:isc)))

(define-public node-send-1.2.1
  (package
    (name "node-send")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/send/-/send-1.2.1.tgz")
       (sha256
        (base32 "0wn46l21fl21yf5abnx3yr6fr2kq4cxm7ggf5g5xs8yx2srly9gs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("after" "eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"
                                                  "supertest"))))))))
    (inputs (list node-statuses-2.0.2
                  node-range-parser
                  node-on-finished
                  node-ms-2.1.3
                  node-mime-types-3.0.2
                  node-http-errors-2.0.1
                  node-fresh
                  node-etag
                  node-escape-html
                  node-encodeurl
                  node-debug-4.4.3))
    (home-page "https://github.com/pillarjs/send#readme")
    (synopsis
     "Better streaming static file server with Range and conditional-GET support")
    (description
     "Better streaming static file server with Range and conditional-GET support")
    (license license:expat)))

(define-public node-serve-static-2.2.1
  (package
    (name "node-serve-static")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/serve-static/-/serve-static-2.2.1.tgz")
       (sha256
        (base32 "0hszcbfcncifwgxk060jy1andx4b7pzjbvig40cfnwlknlmzgm1n"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"
                                                  "supertest"))))))))
    (inputs (list node-send-1.2.1 node-parseurl node-escape-html
                  node-encodeurl))
    (home-page "https://github.com/expressjs/serve-static#readme")
    (synopsis "Serve static files")
    (description "Serve static files")
    (license license:expat)))

(define-public node-set-blocking-2.0.0
  (package
    (name "node-set-blocking")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/set-blocking/-/set-blocking-2.0.0.tgz")
       (sha256
        (base32 "0gb9mvv8bjfavsxlzq56189qis7z2lrp893px04xl2cyvgkswd6r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "coveralls" "mocha"
                                                  "nyc" "standard"
                                                  "standard-version"))))))))
    (home-page "https://github.com/yargs/set-blocking#readme")
    (synopsis
     "set blocking stdio and stderr ensuring that terminal output does not truncate")
    (description
     "set blocking stdio and stderr ensuring that terminal output does not truncate")
    (license license:isc)))

(define-public node-setimmediate-1.0.5
  (package
    (name "node-setimmediate")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/setimmediate/-/setimmediate-1.0.5.tgz")
       (sha256
        (base32 "17icj9sgsg9fcyclds1a8mlgmspza3fa6sidq11fsr43d4igrfaw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jshint" "mocha"
                                                  "http-server" "opener"
                                                  "zuul"))))))))
    (home-page "https://github.com/yuzujs/setImmediate#readme")
    (synopsis "A shim for the setImmediate efficient script yielding API")
    (description "A shim for the setImmediate efficient script yielding API")
    (license license:expat)))

(define-public node-shebang-command-2.0.0
  (package
    (name "node-shebang-command")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/shebang-command/-/shebang-command-2.0.0.tgz")
       (sha256
        (base32 "0vjmdpwcz23glkhlmxny8hc3x01zyr6hwf4qb3grq7m532ysbjws"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "xo"))))))))
    (inputs (list node-shebang-regex-3.0.0))
    (home-page "https://github.com/kevva/shebang-command#readme")
    (synopsis "Get the command from a shebang")
    (description "Get the command from a shebang")
    (license license:expat)))

(define-public node-shebang-regex-3.0.0
  (package
    (name "node-shebang-regex")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/shebang-regex/-/shebang-regex-3.0.0.tgz")
       (sha256
        (base32 "13wmb23w5srjpn9xx1c85yk5jbc5z9ypg0iz33h6nv5jdnmapnzy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/shebang-regex#readme")
    (synopsis "Regular expression for matching a shebang line")
    (description "Regular expression for matching a shebang line")
    (license license:expat)))

(define-public node-silvia-odwyer-photon-node-0.3.4
  (package
    (name "node-silvia-odwyer-photon-node")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@silvia-odwyer/photon-node/-/photon-node-0.3.4.tgz")
       (sha256
        (base32 "02n55fjxr7nj92whzzyja8pvjisjk0lnkn9d94y8xhyj3if028ss"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://silvia-odwyer.github.io/photon/")
    (synopsis
     "High-performance image processing library for native use and the web")
    (description
     "High-performance image processing library for native use and the web")
    (license license:asl2.0)))

(define-public node-simple-concat-1.0.1
  (package
    (name "node-simple-concat")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/simple-concat/-/simple-concat-1.0.1.tgz")
       (sha256
        (base32 "0gknha4csvbwagam34g0qx67nzlfg9wm22nki8ak77xbkiscpz3a"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("standard" "tape"))))))))
    (home-page "https://github.com/feross/simple-concat")
    (synopsis
     "Super-minimalist version of `concat-stream`. Less than 15 lines!")
    (description
     "Super-minimalist version of `concat-stream`. Less than 15 lines!")
    (license license:expat)))

(define-public node-simple-get-4.0.1
  (package
    (name "node-simple-get")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/simple-get/-/simple-get-4.0.1.tgz")
       (sha256
        (base32 "0m4p8n4wai9dwykpbcjh0g1dz06lvhyq650rcd5l0z5vc7pa20pr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("self-signed-https"
                                                  "standard"
                                                  "string-to-stream" "tape"))))))))
    (inputs (list node-simple-concat-1.0.1 node-once-1.4.0
                  node-decompress-response-6.0.0))
    (home-page "https://github.com/feross/simple-get")
    (synopsis
     "Simplest way to make http get requests. Supports HTTPS, redirects, gzip/deflate, streams in < 100 lines.")
    (description
     "Simplest way to make http get requests. Supports HTTPS, redirects, gzip/deflate, streams in < 100 lines.")
    (license license:expat)))

(define-public node-sisteransi-1.0.5
  (package
    (name "node-sisteransi")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/sisteransi/-/sisteransi-1.0.5.tgz")
       (sha256
        (base32 "1cvl75j0rn1nzsan50rl8nq0px0njyqa2z3nl7jisq8m8fr2jkcy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap-spec" "tape"))))))))
    (home-page "https://github.com/terkelg/sisteransi#readme")
    (synopsis "ANSI escape codes for some terminal swag")
    (description "ANSI escape codes for some terminal swag")
    (license license:expat)))

(define-public node-smithy-core-3.24.4
  (package
    (name "node-smithy-core")
    (version "3.24.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@smithy/core/-/core-3.24.4.tgz")
       (sha256
        (base32 "1pazbhq9vrcv7h0m2i3d5l0zrvc37ag3faa5s6yr7rg7bnswmv80"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "concurrently"
                                                  "downlevel-dts"
                                                  "hash-test-vectors"
                                                  "json-bigint"
                                                  "premove"
                                                  "typedoc"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-aws-crypto-crc32-5.2.0))
    (home-page
     "https://github.com/smithy-lang/smithy-typescript/tree/main/packages/core")
    (synopsis
     "[![NPM version](https://img.shields.io/npm/v/@smithy/core/latest.svg)](https://www.npmjs.com/package/@smithy/core) [![NPM downloads](https://img.shields.io/npm/dm/@smithy/core.svg)](https://www.npmjs.com/package/@smithy/core)")
    (description
     "[![NPM version](https://img.shields.io/npm/v/@smithy/core/latest.svg)](https://www.npmjs.com/package/@smithy/core) [![NPM downloads](https://img.shields.io/npm/dm/@smithy/core.svg)](https://www.npmjs.com/package/@smithy/core)")
    (license license:asl2.0)))

(define-public node-smithy-credential-provider-imds-4.3.4
  (package
    (name "node-smithy-credential-provider-imds")
    (version "4.3.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@smithy/credential-provider-imds/-/credential-provider-imds-4.3.4.tgz")
       (sha256
        (base32 "0sig4pgkcdvz3hp686sgjz5icznxk7jyb7yq4ra44gf8ns241apq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typedoc"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-smithy-core-3.24.4))
    (home-page
     "https://github.com/smithy-lang/smithy-typescript/tree/main/packages/credential-provider-imds")
    (synopsis
     "AWS credential provider that sources credentials from the EC2 instance metadata service and ECS container metadata service")
    (description
     "AWS credential provider that sources credentials from the EC2 instance metadata service and ECS container metadata service")
    (license license:asl2.0)))

(define-public node-smithy-fetch-http-handler-5.4.4
  (package
    (name "node-smithy-fetch-http-handler")
    (version "5.4.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@smithy/fetch-http-handler/-/fetch-http-handler-5.4.4.tgz")
       (sha256
        (base32 "0sbdsgjr3x1llqi0xf7ajwkyx8bifp2wblpd6hrak6cy9n47cqs7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@smithy/abort-controller"
                                                  "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typedoc"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-smithy-core-3.24.4))
    (home-page
     "https://github.com/smithy-lang/smithy-typescript/tree/main/packages/fetch-http-handler")
    (synopsis "Provides a way to make requests")
    (description "Provides a way to make requests")
    (license license:asl2.0)))

(define-public node-smithy-is-array-buffer-2.2.0
  (package
    (name "node-smithy-is-array-buffer")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@smithy/is-array-buffer/-/is-array-buffer-2.2.0.tgz")
       (sha256
        (base32 "1ggpdqnyl7yrvmskk8n624awp9wj9br8kfqw4p0plvbmn03jdx1l"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("rimraf" "typedoc"
                                                  "concurrently"
                                                  "downlevel-dts"
                                                  "@tsconfig/recommended"))))))))
    (inputs (list node-tslib-2.8.1))
    (home-page
     "https://github.com/awslabs/smithy-typescript/tree/main/packages/is-array-buffer")
    (synopsis
     "Provides a function for detecting if an argument is an ArrayBuffer")
    (description
     "Provides a function for detecting if an argument is an ArrayBuffer")
    (license license:asl2.0)))

(define-public node-smithy-node-http-handler-4.7.4
  (package
    (name "node-smithy-node-http-handler")
    (version "4.7.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@smithy/node-http-handler/-/node-http-handler-4.7.4.tgz")
       (sha256
        (base32 "1vvwpxfmfby8y3b92a6b8j0wbnlahfzm1mnf2ywjpfrfps5h9ig3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@smithy/abort-controller"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typedoc"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-smithy-core-3.24.4))
    (home-page
     "https://github.com/smithy-lang/smithy-typescript/tree/main/packages/node-http-handler")
    (synopsis "Provides a way to make requests")
    (description "Provides a way to make requests")
    (license license:asl2.0)))

(define-public node-smithy-signature-v4-5.4.4
  (package
    (name "node-smithy-signature-v4")
    (version "5.4.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@smithy/signature-v4/-/signature-v4-5.4.4.tgz")
       (sha256
        (base32 "0l70yf31l7qgakrk513qz7ryvq9yvcxcsj4lxwkbmz76zrwmvfam"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@aws-crypto/sha256-js"
                                                  "concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typedoc"))))))))
    (inputs (list node-tslib-2.8.1 node-smithy-types-4.14.2
                  node-smithy-core-3.24.4))
    (home-page
     "https://github.com/smithy-lang/smithy-typescript/tree/main/packages/signature-v4")
    (synopsis
     "A standalone implementation of the AWS Signature V4 request signing algorithm")
    (description
     "A standalone implementation of the AWS Signature V4 request signing algorithm")
    (license license:asl2.0)))

(define-public node-smithy-types-4.14.2
  (package
    (name "node-smithy-types")
    (version "4.14.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@smithy/types/-/types-4.14.2.tgz")
       (sha256
        (base32 "0ia2gj6splzym92rsfccs5ax1d27pck9jk06jghipmbffmgf2604"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("concurrently"
                                                  "downlevel-dts" "premove"
                                                  "typedoc"))))))))
    (inputs (list node-tslib-2.8.1))
    (home-page
     "https://github.com/smithy-lang/smithy-typescript/tree/main/packages/types")
    (synopsis
     "[![NPM version](https://img.shields.io/npm/v/@smithy/types/latest.svg)](https://www.npmjs.com/package/@smithy/types) [![NPM downloads](https://img.shields.io/npm/dm/@smithy/types.svg)](https://www.npmjs.com/package/@smithy/types)")
    (description
     "[![NPM version](https://img.shields.io/npm/v/@smithy/types/latest.svg)](https://www.npmjs.com/package/@smithy/types) [![NPM downloads](https://img.shields.io/npm/dm/@smithy/types.svg)](https://www.npmjs.com/package/@smithy/types)")
    (license license:asl2.0)))

(define-public node-smithy-util-buffer-from-2.2.0
  (package
    (name "node-smithy-util-buffer-from")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@smithy/util-buffer-from/-/util-buffer-from-2.2.0.tgz")
       (sha256
        (base32 "1y5ifi0nicvi35k0vf8663limqa387vq0qyywi8qq64b6v9in515"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("rimraf" "typedoc"
                                                  "@types/node" "concurrently"
                                                  "downlevel-dts"
                                                  "@tsconfig/recommended"))))))))
    (inputs (list node-smithy-is-array-buffer-2.2.0 node-tslib-2.8.1))
    (home-page
     "https://github.com/awslabs/smithy-typescript/tree/main/packages/util-buffer-from")
    (synopsis
     "[![NPM version](https://img.shields.io/npm/v/@smithy/util-buffer-from/latest.svg)](https://www.npmjs.com/package/@smithy/util-buffer-from) [![NPM downloads](https://img.shields.io/npm/dm/@smithy/util-buffer-from.svg)](https://www.npmjs.com/package/@smithy")
    (description
     "[![NPM version](https://img.shields.io/npm/v/@smithy/util-buffer-from/latest.svg)](https://www.npmjs.com/package/@smithy/util-buffer-from) [![NPM downloads](https://img.shields.io/npm/dm/@smithy/util-buffer-from.svg)](https://www.npmjs.com/package/@smithy")
    (license license:asl2.0)))

(define-public node-smithy-util-utf8-2.3.0
  (package
    (name "node-smithy-util-utf8")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@smithy/util-utf8/-/util-utf8-2.3.0.tgz")
       (sha256
        (base32 "06h2zai4w7sv0d8cd5830bbza4zl9g0y2ch37n49r91j79ngadxi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("rimraf" "typedoc"
                                                  "concurrently"
                                                  "downlevel-dts"
                                                  "@tsconfig/recommended"))))))))
    (inputs (list node-smithy-util-buffer-from-2.2.0 node-tslib-2.8.1))
    (home-page
     "https://github.com/awslabs/smithy-typescript/tree/main/packages/util-utf8")
    (synopsis "A UTF-8 string <-> UInt8Array converter")
    (description "A UTF-8 string <-> UInt8Array converter")
    (license license:asl2.0)))

(define-public node-statuses-2.0.2
  (package
    (name "node-statuses")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/statuses/-/statuses-2.0.2.tgz")
       (sha256
        (base32 "1q4zjhjprvhdjc32px6b4b0i3ffn1mbqygp7yilbb204f4j0m06a"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("csv-parse" "eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"
                                                  "raw-body"
                                                  "stream-to-array"))))))))
    (home-page "https://github.com/jshttp/statuses#readme")
    (synopsis "HTTP status utility")
    (description "HTTP status utility")
    (license license:expat)))

(define-public node-string-decoder-1.1.1
  (package
    (name "node-string-decoder")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/string_decoder/-/string_decoder-1.1.1.tgz")
       (sha256
        (base32 "0fln2r91b8gj845j7jl76fvsp7nij13fyzvz82985yh88m1n50mg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("babel-polyfill"
                                                  "core-util-is" "inherits"
                                                  "tap"))))))))
    (inputs (list node-safe-buffer-5.1.2))
    (home-page "https://github.com/nodejs/string_decoder")
    (synopsis "The string_decoder module from Node core")
    (description "The string_decoder module from Node core")
    (license license:expat)))

(define-public node-strip-json-comments-2.0.1
  (package
    (name "node-strip-json-comments")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/strip-json-comments/-/strip-json-comments-2.0.1.tgz")
       (sha256
        (base32 "16aq89q4gbs10fgy3a5n5miqphvs1sy44ckk4mf2dxqvmzmmzr6v"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"))))))))
    (home-page "https://github.com/sindresorhus/strip-json-comments#readme")
    (synopsis
     "Strip comments from JSON. Lets you use comments in your JSON files!")
    (description
     "Strip comments from JSON. Lets you use comments in your JSON files!")
    (license license:expat)))

(define-public node-strnum-2.3.0
  (package
    (name "node-strnum")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/strnum/-/strnum-2.3.0.tgz")
       (sha256
        (base32 "0lvsrf8p7dnkmg2g5p2nky8x66rfim1kk5bddi15smmxbkgzcb4y"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@byspec/numbers" "jasmine"))))))))
    (home-page "https://github.com/NaturalIntelligence/strnum#readme")
    (synopsis "Parse String to Number based on configuration")
    (description "Parse String to Number based on configuration")
    (license license:expat)))

(define-public node-strtok3-10.3.5
  (package
    (name "node-strtok3")
    (version "10.3.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/strtok3/-/strtok3-10.3.5.tgz")
       (sha256
        (base32 "15kwz94dlifpxqwcqq6hqi4m13jdpkiblkf7l5752h13bgk5ynaq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@biomejs/biome"
                                                  "@types/chai"
                                                  "@types/chai-as-promised"
                                                  "@types/debug"
                                                  "@types/mocha"
                                                  "@types/node"
                                                  "c8"
                                                  "chai"
                                                  "chai-as-promised"
                                                  "del-cli"
                                                  "mocha"
                                                  "node-readable-to-web-readable-stream"
                                                  "remark-cli"
                                                  "remark-preset-lint-recommended"
                                                  "token-types"
                                                  "ts-node"
                                                  "typescript"
                                                  "uint8array-extras"))))))))
    (inputs (list node-tokenizer-token))
    (home-page "https://github.com/Borewit/strtok3#readme")
    (synopsis "A promise based streaming tokenizer")
    (description "A promise based streaming tokenizer")
    (license license:expat)))

(define-public node-tar-7.5.15
  (package
    (name "node-tar")
    (version "7.5.15")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/tar/-/tar-7.5.15.tgz")
       (sha256
        (base32 "0748cm6i3yvs6vpv2migqhdq8qgg0qznd1hc3n9nvmackf6b8pl6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "chmodr"
                                                  "end-of-stream"
                                                  "esbuild"
                                                  "events-to-array"
                                                  "mutate-fs"
                                                  "nock"
                                                  "oxlint"
                                                  "oxlint-tsgolint"
                                                  "prettier"
                                                  "rimraf"
                                                  "tap"
                                                  "tshy"
                                                  "typedoc"))))))))
    (inputs (list node-yallist-5.0.0 node-minizlib-3.1.0 node-minipass-7.1.3
                  node-chownr-3.0.0 node-isaacs-fs-minipass-4.0.1))
    (home-page "https://github.com/isaacs/node-tar#readme")
    (synopsis "tar for node")
    (description "tar for node")
    (license license:blue-oak1.0.0)))

(define-public node-tar-fs-2.1.4
  (package
    (name "node-tar-fs")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/tar-fs/-/tar-fs-2.1.4.tgz")
       (sha256
        (base32 "17qkddvagnzw9ab1anrmp5qwndj18pknxzy6dr86bs20qkllcj3c"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape" "rimraf" "standard"))))))))
    (inputs (list node-mkdirp-classic-0.5.3 node-tar-stream-2.2.0
                  node-chownr-1.1.4 node-pump-3.0.4))
    (home-page "https://github.com/mafintosh/tar-fs")
    (synopsis "filesystem bindings for tar-stream")
    (description "filesystem bindings for tar-stream")
    (license license:expat)))

(define-public node-tar-stream-2.2.0
  (package
    (name "node-tar-stream")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/tar-stream/-/tar-stream-2.2.0.tgz")
       (sha256
        (base32 "0nrrl6sgl5yazgllc8ryxpg083432xwqvqbrlqdl16sfszgq73rs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape" "standard"
                                                  "concat-stream"))))))))
    (inputs (list node-readable-stream-3.6.2 node-end-of-stream-1.4.5
                  node-fs-constants-1.0.0 node-inherits-2.0.4 node-bl-4.1.0))
    (home-page "https://github.com/mafintosh/tar-stream")
    (synopsis
     "tar-stream is a streaming tar parser and generator and nothing else. It is streams2 and operates purely using streams which means you can easily extract/parse tarballs without ever hitting the file system.")
    (description
     "tar-stream is a streaming tar parser and generator and nothing else. It is streams2 and operates purely using streams which means you can easily extract/parse tarballs without ever hitting the file system.")
    (license license:expat)))

(define-public node-token-types-6.1.2
  (package
    (name "node-token-types")
    (version "6.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/token-types/-/token-types-6.1.2.tgz")
       (sha256
        (base32 "0hazs8mvy9r00c52888428xsw6f99svyg4nk97cxmxi89mqj0j7b"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@biomejs/biome"
                                                  "@types/chai"
                                                  "@types/mocha"
                                                  "@types/node"
                                                  "c8"
                                                  "chai"
                                                  "del-cli"
                                                  "mocha"
                                                  "remark-cli"
                                                  "remark-preset-lint-recommended"
                                                  "source-map-support"
                                                  "ts-node"
                                                  "typescript"))))))))
    (inputs (list node-ieee754-1.2.1 node-tokenizer-token
                  node-borewit-text-codec-0.2.2))
    (home-page "https://github.com/Borewit/token-types#readme")
    (synopsis
     "Common token types for decoding and encoding numeric and string values")
    (description
     "Common token types for decoding and encoding numeric and string values")
    (license license:expat)))

(define-public node-tokenizer-inflate-0.4.1
  (package
    (name "node-tokenizer-inflate")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@tokenizer/inflate/-/inflate-0.4.1.tgz")
       (sha256
        (base32 "1h4xglb51m6jjgplyvnm45vx9wcgqqyj48v7s2yywafg2c5sdxj1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "mocha"
                                                  "del-cli"
                                                  "strtok3"
                                                  "ts-node"
                                                  "file-type"
                                                  "typescript"
                                                  "@types/chai"
                                                  "@types/node"
                                                  "@types/debug"
                                                  "@types/mocha"
                                                  "@tokenizer/s3"
                                                  "@biomejs/biome"
                                                  "@aws-sdk/client-s3"))))))))
    (inputs (list node-token-types-6.1.2 node-debug-4.4.3))
    (home-page "https://github.com/Borewit/tokenizer-inflate#readme")
    (synopsis "Tokenized zip support")
    (description "Tokenized zip support")
    (license license:expat)))

(define-public node-tokenjuice-0.7.1
  (package
    (name "node-tokenjuice")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/tokenjuice/-/tokenjuice-0.7.1.tgz")
       (sha256
        (base32 "044rn0cardhbvaaz7wnvp5z2kr0zfwg612hmm2gc7zzrw7invlsy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "esbuild"
                                                  "madge"
                                                  "oxlint"
                                                  "publint"
                                                  "typescript"
                                                  "vitest"))))))))
    (home-page "https://github.com/vincentkoc/tokenjuice")
    (synopsis "Lean output compaction for terminal-heavy agent workflows.")
    (description "Lean output compaction for terminal-heavy agent workflows.")
    (license license:expat)))

(define-public node-tree-sitter-0.25.0
  (package
    (name "node-tree-sitter")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/tree-sitter/-/tree-sitter-0.25.0.tgz")
       (sha256
        (base32 "0jmakazk0vb0yfrr8wvg8gk2dl3css612p0mw7bsw8rcivp4hr01"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'set-home 'set-compiler
            (lambda _
              (setenv "CC" "gcc")
              (setenv "CXX" "g++")))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "@types/tmp"
                                                  "glob"
                                                  "node-gyp"
                                                  "prebuildify"
                                                  "tmp"
                                                  "tree-sitter-c"
                                                  "tree-sitter-embedded-template"
                                                  "tree-sitter-html"
                                                  "tree-sitter-java"
                                                  "tree-sitter-javascript"
                                                  "tree-sitter-json"
                                                  "tree-sitter-python"
                                                  "tree-sitter-ruby"
                                                  "tree-sitter-rust"
                                                  "typedoc"
                                                  "typedoc-plugin-rename-defaults"
                                                  "typescript"))))))))
    (native-inputs (list python))
    (inputs (list node-node-gyp-build node-node-addon-api-8.8.0))
    (home-page "https://github.com/tree-sitter/node-tree-sitter#readme")
    (synopsis "Node.js bindings to the Tree-sitter parsing library")
    (description "Node.js bindings to the Tree-sitter parsing library")
    (license license:expat)))

(define-public node-tree-sitter-bash-0.25.1
  (package
    (name "node-tree-sitter-bash")
    (version "0.25.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/tree-sitter-bash/-/tree-sitter-bash-0.25.1.tgz")
       (sha256
        (base32 "1yd6wj59mphm7bqci5fkgc047m0hlq2f7xzzaf4wp5za12aq3cnl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-prebuilds
            (lambda _
              (delete-file-recursively "prebuilds")))
          (add-after 'set-home 'set-compiler
            (lambda _
              (setenv "CC" "gcc")
              (setenv "CXX" "g++")))
          (add-after 'patch-dependencies 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-treesitter"
                                                  "tree-sitter-cli"
                                                  "prebuildify" "tree-sitter"))))))))
    (native-inputs (list python))
    (inputs (list node-node-gyp-build node-node-addon-api-8.8.0
                  node-tree-sitter-0.25.0))
    (home-page "https://github.com/tree-sitter/tree-sitter-bash#readme")
    (synopsis "Bash grammar for tree-sitter")
    (description "Bash grammar for tree-sitter")
    (license license:expat)))

(define-public node-ts-algebra-2.0.0
  (package
    (name "node-ts-algebra")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ts-algebra/-/ts-algebra-2.0.0.tgz")
       (sha256
        (base32 "0p669fivm6k85ip9n54rv6bh70lcalrv43l6jhp9bdqyv27rdhzl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@trivago/prettier-plugin-sort-imports"
                                                  "@types/node"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "@zerollup/ts-transform-paths"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-import-resolver-typescript"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-jsdoc"
                                                  "eslint-plugin-prefer-arrow"
                                                  "eslint-plugin-prettier"
                                                  "eslint-plugin-unused-imports"
                                                  "prettier"
                                                  "rollup"
                                                  "rollup-plugin-dts"
                                                  "rollup-plugin-import-map"
                                                  "ts-node"
                                                  "ts-toolbelt"
                                                  "ts-unused-exports"
                                                  "ttypescript"
                                                  "typescript"))))))))
    (home-page "https://github.com/ThomasAribart/ts-algebra#readme")
    (synopsis "Types on steroids ð")
    (description "Types on steroids ð")
    (license license:expat)))

(define-public node-tslib-2.8.1
  (package
    (name "node-tslib")
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/tslib/-/tslib-2.8.1.tgz")
       (sha256
        (base32 "17hiw9pawyczkhsnhlq4k9dn3kq2l49nk5rlfn049bmbxvakbxk6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis "Runtime library for TypeScript helper functions")
    (description "Runtime library for TypeScript helper functions")
    (license license:bsd-0)))

(define-public node-tslog-4.10.2
  (package
    (name "node-tslog")
    (version "4.10.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/tslog/-/tslog-4.10.2.tgz")
       (sha256
        (base32 "0srkixbhzms9rlh4d6nljfxvgbg5f5gandcjdiyxwpfki41pkblr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://tslog.js.org")
    (synopsis "Extensible TypeScript Logger for Node.js and Browser.")
    (description "Extensible TypeScript Logger for Node.js and Browser.")
    (license license:expat)))

(define-public node-tunnel-agent-0.6.0
  (package
    (name "node-tunnel-agent")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/tunnel-agent/-/tunnel-agent-0.6.0.tgz")
       (sha256
        (base32 "04jhbjld99zavh1rvik2bayrgxwj2zx69xsbcm0gmlnna15c1qyk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-safe-buffer-5.2.1))
    (home-page "https://github.com/mikeal/tunnel-agent#readme")
    (synopsis
     "HTTP proxy tunneling agent. Formerly part of mikeal/request, now a standalone module.")
    (description
     "HTTP proxy tunneling agent. Formerly part of mikeal/request, now a standalone module.")
    (license license:asl2.0)))

(define-public node-type-is-2.1.0
  (package
    (name "node-type-is")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/type-is/-/type-is-2.1.0.tgz")
       (sha256
        (base32 "1z02ncgm0xqw2q9g4k20jbac129hk1dfxz5l5h68wj6dd66hhlws"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"))))))))
    (inputs (list node-mime-types-3.0.2 node-media-typer
                  node-content-type-2.0.0))
    (home-page "https://github.com/jshttp/type-is#readme")
    (synopsis "Infer the content-type of a request.")
    (description "Infer the content-type of a request.")
    (license license:expat)))

(define-public node-typebox-1.1.38
  (package
    (name "node-typebox")
    (version "1.1.38")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/typebox/-/typebox-1.1.38.tgz")
       (sha256
        (base32 "0dbdrq584m9lqypzfp94pv47s6z3xpnjk573fbi2f2zv2d2f6373"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/sinclairzx81/typebox#readme")
    (synopsis
     "Json Schema Type Builder with Static Type Resolution for TypeScript")
    (description
     "Json Schema Type Builder with Static Type Resolution for TypeScript")
    (license license:expat)))

(define-public node-types-node-25.9.1
  (package
    (name "node-types-node")
    (version "25.9.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@types/node/-/node-25.9.1.tgz")
       (sha256
        (base32 "0m2710wz6dz6kdr7wbiv8d3ppm5qfrm2cl0xiqhzhg6wbmgvshhh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-undici-types-7.24.6))
    (home-page
     "https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/node")
    (synopsis "TypeScript definitions for node")
    (description "TypeScript definitions for node")
    (license license:expat)))

(define-public node-types-retry-0.12.0
  (package
    (name "node-types-retry")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@types/retry/-/retry-0.12.0.tgz")
       (sha256
        (base32 "1j7qm574gpf1favz78qzn79ky5g6xbflkwwz3f8wps51mdsxp5vw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://www.npmjs.com/package/node-types-retry")
    (synopsis "TypeScript definitions for retry")
    (description "TypeScript definitions for retry")
    (license license:expat)))

(define-public node-typescript-6.0.3
  (package
    (name "node-typescript")
    (version "6.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/typescript/-/typescript-6.0.3.tgz")
       (sha256
        (base32 "0anjcm0xk05wkls2sz1dlhs4rsnxc9n87nm92nfrx35apvhhxk9k"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@dprint/formatter"
                                                  "@dprint/typescript"
                                                  "@esfx/canceltoken"
                                                  "@eslint/js"
                                                  "@octokit/rest"
                                                  "@types/chai"
                                                  "@types/minimist"
                                                  "@types/mocha"
                                                  "@types/ms"
                                                  "@types/node"
                                                  "@types/source-map-support"
                                                  "@types/which"
                                                  "@typescript-eslint/rule-tester"
                                                  "@typescript-eslint/type-utils"
                                                  "@typescript-eslint/utils"
                                                  "azure-devops-node-api"
                                                  "c8"
                                                  "chai"
                                                  "chokidar"
                                                  "diff"
                                                  "dprint"
                                                  "esbuild"
                                                  "eslint"
                                                  "eslint-plugin-regexp"
                                                  "fast-xml-parser"
                                                  "glob"
                                                  "globals"
                                                  "hereby"
                                                  "jsonc-parser"
                                                  "knip"
                                                  "minimist"
                                                  "mocha"
                                                  "mocha-fivemat-progress-reporter"
                                                  "monocart-coverage-reports"
                                                  "ms"
                                                  "picocolors"
                                                  "playwright"
                                                  "source-map-support"
                                                  "tslib"
                                                  "typescript"
                                                  "typescript-eslint"
                                                  "which"))))))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis
     "TypeScript is a language for application scale JavaScript development")
    (description
     "TypeScript is a language for application scale JavaScript development")
    (license license:asl2.0)))

(define-public node-uc-micro-2.1.0
  (package
    (name "node-uc-micro")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/uc.micro/-/uc.micro-2.1.0.tgz")
       (sha256
        (base32 "16znmdypk3b6s28ar54iz6ag139vlyiyczxiwh7kgb6xj33605m3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@unicode/unicode-15.1.0"
                                                  "mocha" "rollup" "shelljs"))))))))
    (home-page "https://github.com/markdown-it/uc.micro#readme")
    (synopsis "Micro subset of unicode data files for markdown-it projects.")
    (description
     "Micro subset of unicode data files for markdown-it projects.")
    (license license:expat)))

(define-public node-uhyphen-0.2.0
  (package
    (name "node-uhyphen")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/uhyphen/-/uhyphen-0.2.0.tgz")
       (sha256
        (base32 "1lda3fxckan5lqgzqxbx3i7721fml07pfgivfnifz857kq4l5yzk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/preset-env"
                                                  "ascjs"
                                                  "coveralls"
                                                  "nyc"
                                                  "rollup"
                                                  "rollup-plugin-babel"
                                                  "rollup-plugin-node-resolve"
                                                  "rollup-plugin-terser"
                                                  "uglify-js"))))))))
    (home-page "https://github.com/WebReflection/uhyphen#readme")
    (synopsis "A micro utility to hyphenize strings")
    (description "A micro utility to hyphenize strings")
    (license license:isc)))

(define-public node-uint8array-extras-1.5.0
  (package
    (name "node-uint8array-extras")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/uint8array-extras/-/uint8array-extras-1.5.0.tgz")
       (sha256
        (base32 "0xyn0knwx5iipyc4cdh9bmkby34nhmn7j52a6gsfzk3yrv4lv0v5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "typescript" "xo"
                                                  "benchmark"))))))))
    (home-page "https://github.com/sindresorhus/uint8array-extras#readme")
    (synopsis "Useful utilities for working with Uint8Array (and Buffer)")
    (description "Useful utilities for working with Uint8Array (and Buffer)")
    (license license:expat)))

(define-public node-undici-8.3.0
  (package
    (name "node-undici")
    (version "8.3.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/undici/-/undici-8.3.0.tgz")
       (sha256
        (base32 "1k92ck2f8x79vai2lbyz8z8pzglwmf5qhrh2cgyqw8r9w8anj530"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@fastify/busboy"
                                                  "@matteo.collina/tspl"
                                                  "@metcoder95/https-pem"
                                                  "@sinonjs/fake-timers"
                                                  "@types/node"
                                                  "abort-controller"
                                                  "borp"
                                                  "c8"
                                                  "cross-env"
                                                  "dns-packet"
                                                  "esbuild"
                                                  "eslint"
                                                  "fast-check"
                                                  "husky"
                                                  "jest"
                                                  "jsondiffpatch"
                                                  "neostandard"
                                                  "node-forge"
                                                  "proxy"
                                                  "tsd"
                                                  "typescript"
                                                  "ws"))))))))
    (home-page "https://undici.nodejs.org")
    (synopsis "An HTTP/1.1 client, written from scratch for Node.js")
    (description "An HTTP/1.1 client, written from scratch for Node.js")
    (license license:expat)))

(define-public node-undici-types-7.24.6
  (package
    (name "node-undici-types")
    (version "7.24.6")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/undici-types/-/undici-types-7.24.6.tgz")
       (sha256
        (base32 "1x50j292z2sbq1np2mp6n26h376q9bn4i211a4lx4zdqhrd5kgra"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://undici.nodejs.org")
    (synopsis "A stand-alone types package for Undici")
    (description "A stand-alone types package for Undici")
    (license license:expat)))

(define-public node-utf-8-validate-6.0.6
  (package
    (name "node-utf-8-validate")
    (version "6.0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/utf-8-validate/-/utf-8-validate-6.0.6.tgz")
       (sha256
        (base32 "03x0qhiickjrw4vqdg6ji54k3vay1nkvgpdmcyykfj8pn7z0appn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "node-gyp"
                                                  "prebuildify"
                                                  "prebuildify-cross"))))))))
    (inputs (list node-node-gyp-build))
    (home-page "https://github.com/websockets/utf-8-validate")
    (synopsis "Check if a buffer contains valid UTF-8")
    (description "Check if a buffer contains valid UTF-8")
    (license license:expat)))

(define-public node-web-push-3.6.7
  (package
    (name "node-web-push")
    (version "3.6.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/web-push/-/web-push-3.6.7.tgz")
       (sha256
        (base32 "0gj62garzvaswicidxirhbsgcg0bwaxcxssc13g0pxq9xvzlyxw5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chromedriver" "del"
                                                  "eslint"
                                                  "eslint-config-airbnb"
                                                  "eslint-plugin-import"
                                                  "geckodriver"
                                                  "nyc"
                                                  "mkdirp"
                                                  "mocha"
                                                  "portfinder"
                                                  "selenium-assistant"
                                                  "sinon"))))))))
    (inputs (list node-minimist node-jws-4.0.1
                  node-https-proxy-agent node-http-ece-1.2.0
                  node-asn1-js-5.4.1))
    (home-page "https://github.com/web-push-libs/web-push#readme")
    (synopsis "Web Push library for Node.js")
    (description "Web Push library for Node.js")
    (license license:mpl2.0)))

(define-public node-web-streams-polyfill-3.3.3
  (package
    (name "node-web-streams-polyfill")
    (version "3.3.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/web-streams-polyfill/-/web-streams-polyfill-3.3.3.tgz")
       (sha256
        (base32 "0m5v1r411b7vlziw4bgk1vxc8mkxbnklsq20bk9ylqq2vk9kiq8y"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tslib" "eslint"
                                                  "rollup"
                                                  "jasmine"
                                                  "minimist"
                                                  "ts-morph"
                                                  "micromatch"
                                                  "playwright"
                                                  "typescript"
                                                  "wpt-runner"
                                                  "@types/node"
                                                  "downlevel-dts"
                                                  "recursive-readdir"
                                                  "@rollup/plugin-strip"
                                                  "@rollup/plugin-inject"
                                                  "@rollup/plugin-terser"
                                                  "@rollup/plugin-replace"
                                                  "@microsoft/api-extractor"
                                                  "@rollup/plugin-typescript"
                                                  "@typescript-eslint/parser"
                                                  "@ungap/promise-all-settled"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://github.com/MattiasBuelens/web-streams-polyfill#readme")
    (synopsis "Web Streams, based on the WHATWG spec reference implementation")
    (description
     "Web Streams, based on the WHATWG spec reference implementation")
    (license license:expat)))

(define-public node-web-tree-sitter-0.26.9
  (package
    (name "node-web-tree-sitter")
    (version "0.26.9")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/web-tree-sitter/-/web-tree-sitter-0.26.9.tgz")
       (sha256
        (base32 "06v92if20282gc1y4ds6qsq5dg3a8x9p4vaj67jk5hcg0nv8mkda"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@eslint/js"
                                                  "@types/emscripten"
                                                  "@types/node"
                                                  "@vitest/coverage-v8"
                                                  "dts-buddy"
                                                  "esbuild"
                                                  "eslint"
                                                  "source-map"
                                                  "tsx"
                                                  "typescript"
                                                  "typescript-eslint"
                                                  "vitest"))))))))
    (home-page "https://github.com/tree-sitter/tree-sitter#readme")
    (synopsis "Tree-sitter bindings for the web")
    (description "Tree-sitter bindings for the web")
    (license license:expat)))

(define-public node-which-2.0.2
  (package
    (name "node-which")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/which/-/which-2.0.2.tgz")
       (sha256
        (base32 "1p2fkm4lr36s85gdjxmyr6wh86dizf0iwmffxmarcxpbvmgxyfm1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "mkdirp" "rimraf"))))))))
    (inputs (list node-isexe-2.0.0))
    (home-page "https://github.com/isaacs/node-which#readme")
    (synopsis
     "Like which(1) unix command. Find the first instance of an executable in the PATH.")
    (description
     "Like which(1) unix command. Find the first instance of an executable in the PATH.")
    (license license:isc)))

(define-public node-which-module-2.0.1
  (package
    (name "node-which-module")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/which-module/-/which-module-2.0.1.tgz")
       (sha256
        (base32 "0ws98arh0dxzsxijq17b6hk4qyh2277kmifn6d56qa49z4ifz3pz"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "coveralls" "nyc"
                                                  "standard"
                                                  "standard-version"))))))))
    (home-page "https://github.com/nexdrew/which-module#readme")
    (synopsis "Find the module object for something that was require()d")
    (description "Find the module object for something that was require()d")
    (license license:isc)))

(define-public node-wrap-ansi-6.2.0
  (package
    (name "node-wrap-ansi")
    (version "6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-6.2.0.tgz")
       (sha256
        (base32 "1y3qsslrq9zlxgpmr0p1g5wnq39csj39dg2pll03g1sdy09c8vyl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "nyc" "chalk"
                                                  "has-ansi" "coveralls"))))))))
    (inputs (list node-string-width node-ansi-styles
                  node-strip-ansi))
    (home-page "https://github.com/chalk/wrap-ansi#readme")
    (synopsis "Wordwrap a string with ANSI escape codes")
    (description "Wordwrap a string with ANSI escape codes")
    (license license:expat)))

(define-public node-ws-8.20.1
  (package
    (name "node-ws")
    (version "8.20.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ws/-/ws-8.20.1.tgz")
       (sha256
        (base32 "0a7clyqr5zdgc0c0q55rppp1isi1wdd2ig9wgpy8x4m2if1v317v"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "globals"
                                                  "prettier"
                                                  "benchmark"
                                                  "@eslint/js"
                                                  "bufferutil"
                                                  "utf-8-validate"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-prettier"))))))))
    (home-page "https://github.com/websockets/ws")
    (synopsis
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (description
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (license license:expat)))

(define-public node-ws-8.21.0
  (package
    (name "node-ws")
    (version "8.21.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ws/-/ws-8.21.0.tgz")
       (sha256
        (base32 "1kxl9vlkizjmvdqr4fj5mpa7b3dcnajdk80qabnhyfmf79mp52yh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@eslint/js" "benchmark"
                                                  "bufferutil"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-prettier"
                                                  "globals"
                                                  "mocha"
                                                  "nyc"
                                                  "prettier"
                                                  "utf-8-validate"))))))))
    (home-page "https://github.com/websockets/ws")
    (synopsis
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (description
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (license license:expat)))

(define-public node-xml-naming-0.1.0
  (package
    (name "node-xml-naming")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/xml-naming/-/xml-naming-0.1.0.tgz")
       (sha256
        (base32 "0msggwgl9w627xkvaccird3hmqzgk1clxki70hj9z7j2pay7qd0r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jasmine"))))))))
    (home-page "https://github.com/NaturalIntelligence/xml-naming#readme")
    (synopsis
     "Validates XML name productions â Name, NCName, QName, NMToken, NMTokens â for XML 1.0 and 1.1")
    (description
     "Validates XML name productions â Name, NCName, QName, NMToken, NMTokens â for XML 1.0 and 1.1")
    (license license:expat)))

(define-public node-y18n-4.0.3
  (package
    (name "node-y18n")
    (version "4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/y18n/-/y18n-4.0.3.tgz")
       (sha256
        (base32 "1kqfcsvcf3va9nsfs4c1x6jyk9svw01nz8i8fakrnhh1k1270jdw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "coveralls"
                                                  "mocha"
                                                  "nyc"
                                                  "rimraf"
                                                  "standard"
                                                  "standard-version"))))))))
    (home-page "https://github.com/yargs/y18n")
    (synopsis "the bare-bones internationalization library used by yargs")
    (description "the bare-bones internationalization library used by yargs")
    (license license:isc)))

(define-public node-yallist-5.0.0
  (package
    (name "node-yallist")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/yallist/-/yallist-5.0.0.tgz")
       (sha256
        (base32 "15ffxgcsvvq8yp4rxcqnacl84i012jpsbrmh6cqkpavwmgdl77bw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("prettier" "tap" "tshy"
                                                  "typedoc"))))))))
    (home-page "https://github.com/isaacs/yallist#readme")
    (synopsis "Yet Another Linked List")
    (description "Yet Another Linked List")
    (license license:blue-oak1.0.0)))

(define-public node-yaml-2.9.0
  (package
    (name "node-yaml")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/yaml/-/yaml-2.9.0.tgz")
       (sha256
        (base32 "14ggs4m6rb3wm5mi9s2n6kkgz9h9pymlb81b4zh019qvrc2a53q0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/plugin-transform-typescript"
                                                  "@babel/preset-env"
                                                  "@eslint/js"
                                                  "@rollup/plugin-babel"
                                                  "@rollup/plugin-replace"
                                                  "@rollup/plugin-typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "babel-jest"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "fast-check"
                                                  "jest"
                                                  "jest-resolve"
                                                  "jest-ts-webcompat-resolver"
                                                  "prettier"
                                                  "rollup"
                                                  "tslib"
                                                  "typescript"
                                                  "typescript-eslint"))))))))
    (home-page "https://eemeli.org/yaml/")
    (synopsis "JavaScript parser and stringifier for YAML")
    (description "JavaScript parser and stringifier for YAML")
    (license license:isc)))

(define-public node-yargs-15.4.1
  (package
    (name "node-yargs")
    (version "15.4.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/yargs/-/yargs-15.4.1.tgz")
       (sha256
        (base32 "0dg5bmc7rx648h9b16s35fdnm86zcyw36amg62ilzysclz3gxas1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "cpr"
                                                  "gts"
                                                  "chai"
                                                  "chalk"
                                                  "mocha"
                                                  "which"
                                                  "eslint"
                                                  "rimraf"
                                                  "hashish"
                                                  "coveralls"
                                                  "standardx"
                                                  "typescript"
                                                  "@types/chai"
                                                  "@types/node"
                                                  "cross-spawn"
                                                  "es6-promise"
                                                  "@types/mocha"
                                                  "@types/decamelize"
                                                  "eslint-plugin-node"
                                                  "yargs-test-extends"
                                                  "eslint-plugin-import"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-require-main-filename-2.0.0
                  node-require-directory
                  node-get-caller-file
                  node-yargs-parser-18.1.3
                  node-which-module-2.0.1
                  node-string-width
                  node-set-blocking-2.0.0
                  node-decamelize-1.2.0
                  node-find-up-4.1.0
                  node-cliui-6.0.0
                  node-y18n-4.0.3))
    (home-page "https://yargs.js.org/")
    (synopsis "yargs the modern, pirate-themed, successor to optimist.")
    (description "yargs the modern, pirate-themed, successor to optimist.")
    (license license:expat)))

(define-public node-yargs-17.7.2
  (package
    (name "node-yargs")
    (version "17.7.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/yargs/-/yargs-17.7.2.tgz")
       (sha256
        (base32 "1jdfhg5pvp8834zsbm0vibjxx9rpwargqw1619i61mxm21gr7v28"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "cpr"
                                                  "gts"
                                                  "chai"
                                                  "chalk"
                                                  "mocha"
                                                  "which"
                                                  "eslint"
                                                  "rimraf"
                                                  "rollup"
                                                  "hashish"
                                                  "coveralls"
                                                  "cross-env"
                                                  "typescript"
                                                  "@types/chai"
                                                  "@types/node"
                                                  "cross-spawn"
                                                  "@types/mocha"
                                                  "rollup-plugin-ts"
                                                  "yargs-test-extends"
                                                  "rollup-plugin-terser"
                                                  "rollup-plugin-cleanup"))))))))
    (inputs (list node-require-directory
                  node-get-caller-file
                  node-yargs-parser-21.1.1
                  node-string-width
                  node-escalade
                  node-cliui-8.0.1
                  node-y18n))
    (home-page "https://yargs.js.org/")
    (synopsis "yargs the modern, pirate-themed, successor to optimist.")
    (description "yargs the modern, pirate-themed, successor to optimist.")
    (license license:expat)))

(define-public node-yargs-parser-18.1.3
  (package
    (name "node-yargs-parser")
    (version "18.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/yargs-parser/-/yargs-parser-18.1.3.tgz")
       (sha256
        (base32 "0n5yxf9hcd2in09hrx6xv1fjyabvka6iplgy37f9rz9h4gp3b48c"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "chai" "mocha"
                                                  "standard"))))))))
    (inputs (list node-decamelize-1.2.0 node-camelcase-5.3.1))
    (home-page "https://github.com/yargs/yargs-parser#readme")
    (synopsis "the mighty option parser used by yargs")
    (description "the mighty option parser used by yargs")
    (license license:isc)))

(define-public node-yargs-parser-21.1.1
  (package
    (name "node-yargs-parser")
    (version "21.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/yargs-parser/-/yargs-parser-21.1.1.tgz")
       (sha256
        (base32 "0lcif2zbzw2jfj63paq2wdgc1hs9fd8i2xzjcidjcwjagm97sarl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "gts"
                                                  "chai"
                                                  "mocha"
                                                  "serve"
                                                  "eslint"
                                                  "rimraf"
                                                  "rollup"
                                                  "cross-env"
                                                  "puppeteer"
                                                  "standardx"
                                                  "typescript"
                                                  "@types/chai"
                                                  "@types/node"
                                                  "@types/mocha"
                                                  "rollup-plugin-ts"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "rollup-plugin-cleanup"
                                                  "start-server-and-test"
                                                  "@typescript-eslint/parser"
                                                  "ts-transform-default-export"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://github.com/yargs/yargs-parser#readme")
    (synopsis "the mighty option parser used by yargs")
    (description "the mighty option parser used by yargs")
    (license license:isc)))

(define-public node-zod
  (package
    (name "node-zod")
    (version "3.25.51")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/zod/-/zod-3.25.51.tgz")
       (sha256
        (base32 "0g6d3yvdqna8iala2fqj3plhnzn2kk223si19q1rq4xj2psvabcq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://zod.dev")
    (synopsis
     "TypeScript-first schema declaration and validation library with static type inference")
    (description
     "TypeScript-first schema declaration and validation library with static type inference")
    (license license:expat)))

(define-public node-encoding
  (package
    (name "node-encoding")
    (version "0.1.13")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/encoding/-/encoding-0.1.13.tgz")
       (sha256
        (base32 "116gipr1y0hc9zvflsvd39psbbf2j62zvpnw099pqf7fl0nb5xbc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nodeunit"))))))))
    (inputs (list node-iconv-lite))
    (home-page "https://github.com/andris9/encoding")
    (synopsis "Convert encodings, uses iconv-lite")
    (description "Convert encodings, uses iconv-lite")
    (license license:expat)))

(define-public node-tr46
  (package
    (name "node-tr46")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/tr46/-/tr46-0.0.3.tgz")
       (sha256
        (base32 "02ia19bsjr545jlkgv35psmzzr5avic96zxw3dam78yf6bmy2jhn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "request"))))))))
    (home-page "https://github.com/Sebmaster/tr46.js")
    (synopsis "An implementation of the Unicode TR46 spec")
    (description "An implementation of the Unicode TR46 spec")
    (license license:expat)))

(define-public node-webidl-conversions
  (package
    (name "node-webidl-conversions")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/webidl-conversions/-/webidl-conversions-3.0.1.tgz")
       (sha256
        (base32 "1a1dwb1ga1cj2s7av9r46b4xmx11vsk5zncc0gq2qz4l815w7pz4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha"))))))))
    (home-page "https://github.com/jsdom/webidl-conversions")
    (synopsis
     "Implements the WebIDL algorithms for converting to and from JavaScript values")
    (description
     "Implements the WebIDL algorithms for converting to and from JavaScript values")
    (license license:bsd-2)))

(define-public node-whatwg-url
  (package
    (name "node-whatwg-url")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/whatwg-url/-/whatwg-url-5.0.0.tgz")
       (sha256
        (base32 "1lvyrf4ry4bgl2jgpim2pkdmrbv2vb0vh0irmkp7da3kymqw97dh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "eslint" "recast"
                                                  "request" "istanbul"
                                                  "webidl2js"))))))))
    (inputs (list node-webidl-conversions node-tr46))
    (home-page "https://github.com/jsdom/whatwg-url")
    (synopsis
     "An implementation of the WHATWG URL Standard's URL API and parsing machinery")
    (description
     "An implementation of the WHATWG URL Standard's URL API and parsing machinery")
    (license license:expat)))

(define-public node-node-fetch
  (package
    (name "node-node-fetch")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bitinn/node-fetch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nj5n90zrh8jyqy09qn6ih4hq8zbp8iwkyp4sq5c7006yw7l52p6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ungap/url-search-params"
                                                  "abort-controller"
                                                  "abortcontroller-polyfill"
                                                  "babel-core"
                                                  "babel-plugin-istanbul"
                                                  "babel-plugin-transform-async-generator-functions"
                                                  "babel-polyfill"
                                                  "babel-preset-env"
                                                  "babel-register"
                                                  "chai"
                                                  "chai-as-promised"
                                                  "chai-iterator"
                                                  "chai-string"
                                                  "codecov"
                                                  "cross-env"
                                                  "form-data"
                                                  "is-builtin-module"
                                                  "mocha"
                                                  "nyc"
                                                  "parted"
                                                  "promise"
                                                  "resumer"
                                                  "rollup"
                                                  "rollup-plugin-babel"
                                                  "string-to-arraybuffer"
                                                  "teeny-request"
                                                  "encoding"))))))))
    (inputs (list node-whatwg-url node-encoding))
    (home-page "https://github.com/bitinn/node-fetch")
    (synopsis "A light-weight module that brings window.fetch to node.js")
    (description "A light-weight module that brings window.fetch to node.js")
    (license license:expat)))

(define-public node-undici-types
  (package
    (name "node-undici-types")
    (version "5.26.5")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/undici-types/-/undici-types-5.26.5.tgz")
       (sha256
        (base32 "0rflkr8qlh2bvawdxaqz4lpp2ppf06na8xdcn9pm5q5yl7ggyfsh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://undici.nodejs.org")
    (synopsis "A stand-alone types package for Undici")
    (description "A stand-alone types package for Undici")
    (license license:expat)))

(define-public node-types-node
  (package
    (name "node-types-node")
    (version "18.19.110")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@types/node/-/node-18.19.110.tgz")
       (sha256
        (base32 "0yz9wwn2ybcnzlzc183qzjn1an64gnpa9vj3bdqm05yr42c2k7gq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-undici-types))
    (home-page
     "https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/node")
    (synopsis "TypeScript definitions for node")
    (description "TypeScript definitions for node")
    (license license:expat)))

(define-public node-node-domexception
  (package
    (name "node-node-domexception")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jimmywarting/node-domexception")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yawaambf2vixgm9whfyk336ay68x7d16c3n03y6517ckkgpfah0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/jimmywarting/node-domexception")
    (synopsis "An implementation of the DOMException class from NodeJS")
    (description "An implementation of the DOMException class from NodeJS")
    (license license:expat)))

(define-public node-web-streams-polyfill
  (package
    (name "node-web-streams-polyfill")
    (version "4.0.0-beta.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/web-streams-polyfill/-/web-streams-polyfill-4.0.0-beta.3.tgz")
       (sha256
        (base32 "1c5kg7wz2x1s7xgi91llfh80mhk9qnad6m88ba5q8n8a0qr52lxh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tslib" "eslint"
                                                  "rollup"
                                                  "jasmine"
                                                  "minimist"
                                                  "micromatch"
                                                  "playwright"
                                                  "typescript"
                                                  "wpt-runner"
                                                  "@types/node"
                                                  "@types/jasmine"
                                                  "abort-controller"
                                                  "recursive-readdir"
                                                  "@rollup/plugin-strip"
                                                  "rollup-plugin-terser"
                                                  "@rollup/plugin-inject"
                                                  "@rollup/plugin-replace"
                                                  "@microsoft/api-extractor"
                                                  "@rollup/plugin-typescript"
                                                  "@typescript-eslint/parser"
                                                  "@ungap/promise-all-settled"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://github.com/MattiasBuelens/web-streams-polyfill")
    (synopsis "Web Streams, based on the WHATWG spec reference implementation")
    (description
     "Web Streams, based on the WHATWG spec reference implementation")
    (license license:expat)))

(define-public node-formdata-node
  (package
    (name "node-formdata-node")
    (version "4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/formdata-node/-/formdata-node-4.4.1.tgz")
       (sha256
        (base32 "1i4g8r7g2mns70d1fj4jzgjbkgp7nb9mnqwyvw796jcf470nivw9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/formdata-node")))
                (mkdir-p lib)
                (copy-recursively "." lib) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@octetstream/eslint-config"
                                                  "@types/node"
                                                  "@types/sinon"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "@zoltu/typescript-transformer-append-js-extension"
                                                  "ava"
                                                  "c8"
                                                  "eslint"
                                                  "eslint-config-airbnb-typescript"
                                                  "eslint-import-resolver-typescript"
                                                  "eslint-plugin-ava"
                                                  "eslint-plugin-jsx-a11y"
                                                  "eslint-plugin-react"
                                                  "husky"
                                                  "lint-staged"
                                                  "rimraf"
                                                  "sinon"
                                                  "ts-node"
                                                  "ttypescript"
                                                  "typescript"))))))))
    (inputs (list node-web-streams-polyfill node-node-domexception))
    (home-page "https://github.com/octet-stream/form-data")
    (synopsis "Spec-compliant FormData implementation for Node.js")
    (description "Spec-compliant FormData implementation for Node.js")
    (license license:expat)))

(define-public node-humanize-ms
  (package
    (name "node-humanize-ms")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/humanize-ms/-/humanize-ms-1.2.1.tgz")
       (sha256
        (base32 "09sy84kkmvq1la85iws84w2jv1phdsf5jswg7mk7qjbr47dyc3sl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("autod" "mocha" "should"
                                                  "istanbul" "benchmark"
                                                  "beautify-benchmark"))))))))
    (inputs (list node-ms))
    (home-page "https://github.com/node-modules/humanize-ms")
    (synopsis "transform humanize time to ms")
    (description "transform humanize time to ms")
    (license license:expat)))

(define-public node-agentkeepalive
  (package
    (name "node-agentkeepalive")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/agentkeepalive/-/agentkeepalive-4.6.0.tgz")
       (sha256
        (base32 "16jns5qpamw5p7xkjmp5gffwa9d1800ld81brngzhv14z49pbpn3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("coffee" "cross-env"
                                                  "egg-bin"
                                                  "eslint"
                                                  "eslint-config-egg"
                                                  "git-contributor"
                                                  "mm"
                                                  "pedding"
                                                  "typescript"))))))))
    (inputs (list node-humanize-ms))
    (home-page "https://github.com/node-modules/agentkeepalive")
    (synopsis "Missing keepalive http.Agent")
    (description "Missing keepalive http.Agent")
    (license license:expat)))

(define-public node-event-target-shim
  (package
    (name "node-event-target-shim")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/event-target-shim/-/event-target-shim-5.0.1.tgz")
       (sha256
        (base32 "1g16bd7qlk5q6v23k52fq4xc4na9f8ind4365swh7iz5rni7swgh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/plugin-transform-modules-commonjs"
                                                  "@babel/preset-env"
                                                  "@babel/register"
                                                  "@mysticatea/eslint-plugin"
                                                  "@mysticatea/spy"
                                                  "assert"
                                                  "codecov"
                                                  "eslint"
                                                  "karma"
                                                  "karma-chrome-launcher"
                                                  "karma-coverage"
                                                  "karma-firefox-launcher"
                                                  "karma-growl-reporter"
                                                  "karma-ie-launcher"
                                                  "karma-mocha"
                                                  "karma-rollup-preprocessor"
                                                  "mocha"
                                                  "npm-run-all"
                                                  "nyc"
                                                  "opener"
                                                  "rimraf"
                                                  "rollup"
                                                  "rollup-plugin-babel"
                                                  "rollup-plugin-babel-minify"
                                                  "rollup-plugin-commonjs"
                                                  "rollup-plugin-json"
                                                  "rollup-plugin-node-resolve"
                                                  "rollup-watch"
                                                  "type-tester"
                                                  "typescript"))))))))
    (home-page "https://github.com/mysticatea/event-target-shim")
    (synopsis "An implementation of WHATWG EventTarget interface.")
    (description "An implementation of WHATWG EventTarget interface.")
    (license license:expat)))

(define-public node-abort-controller
  (package
    (name "node-abort-controller")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/abort-controller/-/abort-controller-3.0.0.tgz")
       (sha256
        (base32 "0bxba0gy4rglq44q0dx5s2fzm8rw7vb11ihjgkjiisqhs3alkqnj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/plugin-transform-modules-commonjs"
                                                  "@babel/preset-env"
                                                  "@babel/register"
                                                  "@mysticatea/eslint-plugin"
                                                  "@mysticatea/spy"
                                                  "@types/mocha"
                                                  "@types/node"
                                                  "assert"
                                                  "codecov"
                                                  "dts-bundle-generator"
                                                  "eslint"
                                                  "karma"
                                                  "karma-chrome-launcher"
                                                  "karma-coverage"
                                                  "karma-firefox-launcher"
                                                  "karma-growl-reporter"
                                                  "karma-ie-launcher"
                                                  "karma-mocha"
                                                  "karma-rollup-preprocessor"
                                                  "mocha"
                                                  "npm-run-all"
                                                  "nyc"
                                                  "opener"
                                                  "rimraf"
                                                  "rollup"
                                                  "rollup-plugin-babel"
                                                  "rollup-plugin-babel-minify"
                                                  "rollup-plugin-commonjs"
                                                  "rollup-plugin-node-resolve"
                                                  "rollup-plugin-sourcemaps"
                                                  "rollup-plugin-typescript"
                                                  "rollup-watch"
                                                  "ts-node"
                                                  "type-tester"
                                                  "typescript"))))))))
    (inputs (list node-event-target-shim))
    (home-page "https://github.com/mysticatea/abort-controller")
    (synopsis "An implementation of WHATWG AbortController interface.")
    (description "An implementation of WHATWG AbortController interface.")
    (license license:expat)))

(define-public node-asynckit
  (package
    (name "node-asynckit")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/asynckit/-/asynckit-0.4.0.tgz")
       (sha256
        (base32 "1kvxnmjbjwqc8gvp4ms7d8w8x7y41rcizmz4898694h7ywq4y9cc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("browserify"
                                                  "browserify-istanbul"
                                                  "coveralls"
                                                  "eslint"
                                                  "istanbul"
                                                  "obake"
                                                  "phantomjs-prebuilt"
                                                  "pre-commit"
                                                  "reamde"
                                                  "rimraf"
                                                  "size-table"
                                                  "tap-spec"
                                                  "tape"))))))))
    (home-page "https://github.com/alexindigo/asynckit")
    (synopsis "Minimal async jobs utility library, with streams support")
    (description "Minimal async jobs utility library, with streams support")
    (license license:expat)))

(define-public node-delayed-stream
  (package
    (name "node-delayed-stream")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/delayed-stream/-/delayed-stream-1.0.0.tgz")
       (sha256
        (base32 "1lr98585rayrc5xfj599hg6mxqvks38diir74ivivyvx47jgqf5c"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("fake" "far"))))))))
    (home-page "https://github.com/felixge/node-delayed-stream")
    (synopsis
     "Buffers events from a stream until you are ready to handle them.")
    (description
     "Buffers events from a stream until you are ready to handle them.")
    (license license:expat)))

(define-public node-combined-stream
  (package
    (name "node-combined-stream")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/combined-stream/-/combined-stream-1.0.8.tgz")
       (sha256
        (base32 "04hm5rrkwda2qgy1afwhrz42asmflw5hxkbpxddn741ywnmmmgmn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("far"))))))))
    (inputs (list node-delayed-stream))
    (home-page "https://github.com/felixge/node-combined-stream")
    (synopsis "A stream that emits multiple other streams one after another.")
    (description
     "A stream that emits multiple other streams one after another.")
    (license license:expat)))

(define-public node-has-tostringtag
  (package
    (name "node-has-tostringtag")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/has-tostringtag/-/has-tostringtag-1.0.2.tgz")
       (sha256
        (base32 "0yzgy2kkf5z4w3x68r1ymm3aln7ji4ndfis7z1qn56hpy7ip876w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ljharb/eslint-config"
                                                  "@types/has-symbols"
                                                  "@types/tape"
                                                  "aud"
                                                  "auto-changelog"
                                                  "core-js"
                                                  "eslint"
                                                  "get-own-property-symbols"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-has-symbols))
    (home-page "https://github.com/inspect-js/has-tostringtag")
    (synopsis
     "Determine if the JS environment has `Symbol.toStringTag` support. Supports spec, or shams.")
    (description
     "Determine if the JS environment has `Symbol.toStringTag` support. Supports spec, or shams.")
    (license license:expat)))

(define-public node-es-set-tostringtag
  (package
    (name "node-es-set-tostringtag")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/es-set-tostringtag/-/es-set-tostringtag-2.1.0.tgz")
       (sha256
        (base32 "0gdrn28p1xnj8j98b6f1sf15xy4h0cd38alazwml1vikbhdgaxan"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/get-intrinsic"
                                                  "@types/has-symbols"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "encoding"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-hasown node-has-tostringtag node-get-intrinsic
                  node-es-errors))
    (home-page "https://github.com/es-shims/es-set-tostringtag")
    (synopsis
     "A helper to optimistically set Symbol.toStringTag, when possible.")
    (description
     "A helper to optimistically set Symbol.toStringTag, when possible.")
    (license license:expat)))

(define-public node-mime-db
  (package
    (name "node-mime-db")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/mime-db/-/mime-db-1.52.0.tgz")
       (sha256
        (base32 "0fwyiyqi3w03w3xwy2jhm8rsa0y9wgkc0j6q3q6mvk9asns0prxq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("co" "nyc"
                                                  "gnode"
                                                  "mocha"
                                                  "cogent"
                                                  "eslint"
                                                  "bluebird"
                                                  "raw-body"
                                                  "csv-parse"
                                                  "media-typer"
                                                  "stream-to-array"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/mime-db")
    (synopsis "Media Type Database")
    (description "Media Type Database")
    (license license:expat)))

(define-public node-mime-types
  (package
    (name "node-mime-types")
    (version "2.1.35")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/mime-types/-/mime-types-2.1.35.tgz")
       (sha256
        (base32 "1hyi043kcqyfz82w19s357klvj54f0s94d40rymbms86i74lyws9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (inputs (list node-mime-db))
    (home-page "https://github.com/jshttp/mime-types")
    (synopsis "The ultimate javascript content-type utility.")
    (description "The ultimate javascript content-type utility.")
    (license license:expat)))

(define-public node-form-data
  (package
    (name "node-form-data")
    (version "4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/form-data/-/form-data-4.0.2.tgz")
       (sha256
        (base32 "0h7m2ma2h4cwz8lx64v6x8dcfykmpykbdi0lbgpy6rimm68ianyg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/combined-stream"
                                                  "@types/mime-types"
                                                  "@types/node"
                                                  "browserify"
                                                  "browserify-istanbul"
                                                  "coveralls"
                                                  "cross-spawn"
                                                  "eslint"
                                                  "fake"
                                                  "far"
                                                  "formidable"
                                                  "in-publish"
                                                  "is-node-modern"
                                                  "istanbul"
                                                  "obake"
                                                  "pkgfiles"
                                                  "pre-commit"
                                                  "puppeteer"
                                                  "request"
                                                  "rimraf"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-mime-types node-es-set-tostringtag node-combined-stream
                  node-asynckit))
    (home-page "https://github.com/form-data/form-data")
    (synopsis
     "A library to create readable \"multipart/form-data\" streams. Can be used to submit forms and file uploads to other web applications.")
    (description
     "A library to create readable \"multipart/form-data\" streams. Can be used to submit forms and file uploads to other web applications.")
    (license license:expat)))

(define-public node-undici-types
  (package
    (name "node-undici-types")
    (version "6.21.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/undici-types/-/undici-types-6.21.0.tgz")
       (sha256
        (base32 "0vfidx8iwc7ab1p13fxy6bf35njqkii6nkwgah78b249w6jsk8fg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://undici.nodejs.org")
    (synopsis "A stand-alone types package for Undici")
    (description "A stand-alone types package for Undici")
    (license license:expat)))

(define-public node-types-node
  (package
    (name "node-types-node")
    (version "22.15.29")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@types/node/-/node-22.15.29.tgz")
       (sha256
        (base32 "0i9qmvp69gakcraly26c4968c23wphzgg7x9qgf8dj18yjpvf0n3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-undici-types))
    (home-page
     "https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/node")
    (synopsis "TypeScript definitions for node")
    (description "TypeScript definitions for node")
    (license license:expat)))

(define-public node-types-node-fetch
  (package
    (name "node-types-node-fetch")
    (version "2.6.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@types/node-fetch/-/node-fetch-"
             version ".tgz"))
       (sha256
        (base32 "0dnyg4lvdp2fymiw6bz9a3yzkma40sgchhirs0dkm0b5mp59iz4p"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t)))))
    (inputs (list node-types-node node-form-data))
    (home-page
     "https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/node-fetch")
    (synopsis "TypeScript definitions for node-fetch")
    (description "TypeScript definitions for node-fetch")
    (license license:expat)))

(define-public node-form-data-encoder
  (package
    (name "node-form-data-encoder")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/form-data-encoder/-/form-data-encoder-1.7.2.tgz")
       (sha256
        (base32 "1gnv4l5d3i8fvpwvjk5dk1mmhk6j87zh4ifq1hrdahzi7r3h1zhh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "ava"
                                                  "husky"
                                                  "pinst"
                                                  "eslint"
                                                  "ts-node"
                                                  "typescript"
                                                  "@types/node"
                                                  "lint-staged"
                                                  "ttypescript"
                                                  "formdata-node"
                                                  "@types/mime-types"
                                                  "eslint-plugin-ava"
                                                  "eslint-plugin-react"
                                                  "eslint-plugin-jsx-a11y"
                                                  "@typescript-eslint/parser"
                                                  "@octetstream/eslint-config"
                                                  "eslint-config-airbnb-typescript"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@zoltu/typescript-transformer-append-js-extension"))))))))
    (home-page "https://github.com/octet-stream/form-data-encoder")
    (synopsis "Encode FormData content into the multipart/form-data format")
    (description "Encode FormData content into the multipart/form-data format")
    (license license:expat)))

(define-public node-openai
  (package
    (name "node-openai")
    (version "4.104.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/openai/-/openai-4.104.0.tgz")
       (sha256
        (base32 "1l556md71g41h6n876zb4vvvi6gvfzzf5chbxnj7z4zkk8n4g6i4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/openai")))
                (mkdir-p lib)
                (copy-recursively "." lib)
                (let ((bin (string-append out "/bin")))
                  (mkdir-p bin)
                  (symlink (string-append lib "/bin/cli")
                           (string-append bin "/openai"))) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ws" "zod"))))))))
    (inputs (list node-form-data-encoder
                  node-types-node-fetch
                  node-abort-controller
                  node-agentkeepalive
                  node-formdata-node
                  node-types-node
                  node-node-fetch
                  node-zod
                  node-ws))
    (home-page "https://www.npmjs.com/package/node-openai")
    (synopsis "The official TypeScript library for the OpenAI API")
    (description "The official TypeScript library for the OpenAI API")
    (license license:asl2.0)))

(define-public node-vary
  (package
    (name "node-vary")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/vary/-/vary-1.1.2.tgz")
       (sha256
        (base32 "0wbf4kmfyzc23dc0vjcmymkd1ks50z5gvv23lkkkayipf438cy3k"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "eslint"
                                                  "istanbul"
                                                  "benchmark"
                                                  "supertest"
                                                  "beautify-benchmark"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/vary")
    (synopsis "Manipulate the HTTP Vary header")
    (description "Manipulate the HTTP Vary header")
    (license license:expat)))

(define-public node-cookie
  (package
    (name "node-cookie")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cookie/-/cookie-0.7.2.tgz")
       (sha256
        (base32 "084ymsdgqj3jc00gh39cbfbmh1vval1wy2ifd88hlqqw4pw61cbn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "benchmark"
                                                  "top-sites"
                                                  "safe-buffer"
                                                  "beautify-benchmark"
                                                  "eslint-plugin-markdown"))))))))
    (home-page "https://github.com/jshttp/cookie")
    (synopsis "HTTP server cookie parsing and serialization")
    (description "HTTP server cookie parsing and serialization")
    (license license:expat)))

(define-public node-is-promise
  (package
    (name "node-is-promise")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/is-promise/-/is-promise-4.0.0.tgz")
       (sha256
        (base32 "19s5njn24k6ra9c4skkzjhjfaq0d1izkxxicfsw07ykn70br2f45"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/then/is-promise")
    (synopsis "Test whether an object looks like a promises-a+ promise")
    (description "Test whether an object looks like a promises-a+ promise")
    (license license:expat)))

(define-public node-path-to-regexp
  (package
    (name "node-path-to-regexp")
    (version "8.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pillarjs/path-to-regexp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nk9yd8bhv1r25csichpc7d5cw7rf36ynzx5hjj62w7lp5ap6akn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'drop-ts-scripts-config
            (lambda _
              (substitute* "tsconfig.json"
                (("\"extends\": \"@borderless/ts-scripts/configs/tsconfig.json\",")
                 "")
                (("\"compilerOptions\": \\{")
                 "  \"compilerOptions\": {\n    \"declaration\": true,\n    \"strict\": true,\n    \"skipLibCheck\": true,\n"))))
          (replace 'build
            (lambda _
              (invoke "tsc" "--project" "tsconfig.build.json")))
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("recheck" "size-limit"
                                                  "typescript"
                                                  "@types/node"
                                                  "@types/semver"
                                                  "@vitest/coverage-v8"
                                                  "@borderless/ts-scripts"
                                                  "@size-limit/preset-small-lib"))))))))
    (native-inputs (list node-typescript))
    (home-page "https://github.com/pillarjs/path-to-regexp")
    (synopsis "Express style path to RegExp utility")
    (description "Express style path to RegExp utility")
    (license license:expat)))

(define-public node-router
  (package
    (name "node-router")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pillarjs/router")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hl36hn4pbp5wifbb2jaxgyglbb7s7na4j2y8w6wz0kz5rjkls9i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/router")))
                (mkdir-p lib)
                (copy-recursively "." lib)
                (delete-file-recursively (string-append lib "/test")) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("finalhandler" "mocha" "nyc"
                                                  "run-series" "standard"
                                                  "supertest"))))))))
    (inputs (list node-typescript
                  node-path-to-regexp
                  node-parseurl
                  node-is-promise
                  node-depd
                  node-debug))
    (home-page "https://github.com/pillarjs/router")
    (synopsis "Simple middleware-style router")
    (description "Simple middleware-style router")
    (license license:expat)))

(define-public node-negotiator
  (package
    (name "node-negotiator")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/negotiator/-/negotiator-1.0.0.tgz")
       (sha256
        (base32 "015w5p5p4sb02cd9zq20mp7l32jspq206p6d4g355b603ppdz8mm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha" "eslint"
                                                  "eslint-plugin-markdown"))))))))
    (home-page "https://github.com/jshttp/negotiator")
    (synopsis "HTTP content negotiation")
    (description "HTTP content negotiation")
    (license license:expat)))

(define-public node-accepts
  (package
    (name "node-accepts")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/accepts/-/accepts-2.0.0.tgz")
       (sha256
        (base32 "0hi56wcavwsv8s4mpvks7gywmjdiqcqa0a91vga8rpw8gmgr2g8p"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "deep-equal"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (inputs (list node-negotiator node-mime-types))
    (home-page "https://github.com/jshttp/accepts")
    (synopsis "Higher-level content negotiation")
    (description "Higher-level content negotiation")
    (license license:expat)))

(define-public node-forwarded
  (package
    (name "node-forwarded")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/forwarded/-/forwarded-0.2.0.tgz")
       (sha256
        (base32 "168w8dhfqp12llh2w802dm3z1fcarsacsksyvdccnpxqbzlmsnlv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "benchmark"
                                                  "deep-equal"
                                                  "beautify-benchmark"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/forwarded")
    (synopsis "Parse HTTP X-Forwarded-For header")
    (description "Parse HTTP X-Forwarded-For header")
    (license license:expat)))

(define-public node-ipaddr-js
  (package
    (name "node-ipaddr-js")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ipaddr.js/-/ipaddr.js-1.9.1.tgz")
       (sha256
        (base32 "1vlg9vgdlx13dvh6h6sg3rgdbp04lkljmn6gxih43zk77xidjhbl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("coffee-script" "nodeunit"
                                                  "uglify-js"))))))))
    (home-page "https://github.com/whitequark/ipaddr.js")
    (synopsis
     "A library for manipulating IPv4 and IPv6 addresses in JavaScript.")
    (description
     "A library for manipulating IPv4 and IPv6 addresses in JavaScript.")
    (license license:expat)))

(define-public node-proxy-addr
  (package
    (name "node-proxy-addr")
    (version "2.0.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/proxy-addr/-/proxy-addr-2.0.7.tgz")
       (sha256
        (base32 "1na6xrmlga7qjd55gfhnp7m8qg43nynzg5ds54s76kkd9zrvdld0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "benchmark"
                                                  "deep-equal"
                                                  "beautify-benchmark"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (inputs (list node-ipaddr-js node-forwarded))
    (home-page "https://github.com/jshttp/proxy-addr")
    (synopsis "Determine address of proxied request")
    (description "Determine address of proxied request")
    (license license:expat)))

(define-public node-side-channel-list
  (package
    (name "node-side-channel-list")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/side-channel-list/-/side-channel-list-1.0.0.tgz")
       (sha256
        (base32 "1k8wgnr29504nxwmh9p5d88462zdvc2y9nswjjlsrj7bqaq4w1sq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/object-inspect"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "eclint"
                                                  "encoding"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-object-inspect node-es-errors))
    (home-page "https://github.com/ljharb/side-channel-list")
    (synopsis
     "Store information about any JS value in a side channel, using a linked list")
    (description
     "Store information about any JS value in a side channel, using a linked list")
    (license license:expat)))

(define-public node-call-bound
  (package
    (name "node-call-bound")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/call-bound/-/call-bound-1.0.4.tgz")
       (sha256
        (base32 "0cmxdglg3lrrz7apqgvqbkd57jicr98fxwhi92rvkwgd5x4ny21j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/call-bind"
                                                  "@types/get-intrinsic"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "encoding"
                                                  "es-value-fixtures"
                                                  "eslint"
                                                  "evalmd"
                                                  "for-each"
                                                  "gopd"
                                                  "has-strict-mode"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "object-inspect"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-get-intrinsic node-call-bind-apply-helpers))
    (home-page "https://github.com/ljharb/call-bound")
    (synopsis
     "Robust call-bound JavaScript intrinsics, using `call-bind` and `get-intrinsic`.")
    (description
     "Robust call-bound JavaScript intrinsics, using `call-bind` and `get-intrinsic`.")
    (license license:expat)))

(define-public node-es-define-property
  (package
    (name "node-es-define-property")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/es-define-property/-/es-define-property-1.0.1.tgz")
       (sha256
        (base32 "1xw50gnqd3d7nyfcl5a5lzrhxa0sjq7qyzaw1n5hld1166qvi1jr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/gopd"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "encoding"
                                                  "eslint"
                                                  "evalmd"
                                                  "gopd"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (home-page "https://github.com/ljharb/es-define-property")
    (synopsis "`Object.defineProperty`, but not IE 8's broken one.")
    (description "`Object.defineProperty`, but not IE 8's broken one.")
    (license license:expat)))

(define-public node-call-bind-apply-helpers
  (package
    (name "node-call-bind-apply-helpers")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/call-bind-apply-helpers/-/call-bind-apply-helpers-1.0.2.tgz")
       (sha256
        (base32 "0b3xqfkmcxhancq8h4cd3282ryg9h5rnf2h2530zbvdbvgwrygh7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/for-each"
                                                  "@types/function-bind"
                                                  "@types/object-inspect"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "encoding"
                                                  "es-value-fixtures"
                                                  "eslint"
                                                  "evalmd"
                                                  "for-each"
                                                  "has-strict-mode"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "object-inspect"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-function-bind node-es-errors))
    (home-page "https://github.com/ljharb/call-bind-apply-helpers")
    (synopsis
     "Helper functions around Function call/apply/bind, for use in `call-bind`")
    (description
     "Helper functions around Function call/apply/bind, for use in `call-bind`")
    (license license:expat)))

(define-public node-dunder-proto
  (package
    (name "node-dunder-proto")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/dunder-proto/-/dunder-proto-1.0.1.tgz")
       (sha256
        (base32 "1nyg4r9qjc33kmgixdi5xpb0qsjivy2dcn8wjbwhvhc2ihi444zd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "encoding"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-gopd node-es-errors node-call-bind-apply-helpers))
    (home-page "https://github.com/es-shims/dunder-proto")
    (synopsis
     "If available, the `Object.prototype.__proto__` accessor and mutator, call-bound")
    (description
     "If available, the `Object.prototype.__proto__` accessor and mutator, call-bound")
    (license license:expat)))

(define-public node-es-errors
  (package
    (name "node-es-errors")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/es-errors/-/es-errors-1.3.0.tgz")
       (sha256
        (base32 "14q935xgv4cblmy8lk3brx4ypwxpgrid77r1lfnbilsbbg1x2kfi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ljharb/eslint-config"
                                                  "@types/tape"
                                                  "aud"
                                                  "auto-changelog"
                                                  "eclint"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (home-page "https://github.com/ljharb/es-errors")
    (synopsis "A simple cache for a few of the JS Error constructors.")
    (description "A simple cache for a few of the JS Error constructors.")
    (license license:expat)))

(define-public node-es-object-atoms
  (package
    (name "node-es-object-atoms")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/es-object-atoms/-/es-object-atoms-1.1.1.tgz")
       (sha256
        (base32 "1kkrwpp6nz2nc32zxin52xnngyg7qg38c5ljy5xyx2l1azby861y"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "eclint"
                                                  "encoding"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-es-errors))
    (home-page "https://github.com/ljharb/es-object-atoms")
    (synopsis
     "ES Object-related atoms: Object, ToObject, RequireObjectCoercible")
    (description
     "ES Object-related atoms: Object, ToObject, RequireObjectCoercible")
    (license license:expat)))

(define-public node-get-proto
  (package
    (name "node-get-proto")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/get-proto/-/get-proto-1.0.1.tgz")
       (sha256
        (base32 "1086swsp92367j7m6canvgf6zwghh0iqr9f2bwndh7qzzcmcab7b"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-es-object-atoms node-dunder-proto))
    (home-page "https://github.com/ljharb/get-proto")
    (synopsis "Robustly get the [[Prototype]] of an object")
    (description "Robustly get the [[Prototype]] of an object")
    (license license:expat)))

(define-public node-gopd
  (package
    (name "node-gopd")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/gopd/-/gopd-1.2.0.tgz")
       (sha256
        (base32 "10nskxn4cwbfd9i9nzy2r7pf0zm46j9z7dzvd0adr1fj9pgd0dnm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "encoding"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "npmignore"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (home-page "https://github.com/ljharb/gopd")
    (synopsis
     "`Object.getOwnPropertyDescriptor`, but accounts for IE's broken implementation.")
    (description
     "`Object.getOwnPropertyDescriptor`, but accounts for IE's broken implementation.")
    (license license:expat)))

(define-public node-has-symbols
  (package
    (name "node-has-symbols")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/has-symbols/-/has-symbols-1.1.0.tgz")
       (sha256
        (base32 "1ig7dbwgg0kbjg2wc7arp7a28g6l2rwc27lsvhnxzf185x9wfq24"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/core-js"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "core-js"
                                                  "encoding"
                                                  "eslint"
                                                  "get-own-property-symbols"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (home-page "https://github.com/ljharb/has-symbols")
    (synopsis
     "Determine if the JS environment has Symbol support. Supports spec, or shams.")
    (description
     "Determine if the JS environment has Symbol support. Supports spec, or shams.")
    (license license:expat)))

(define-public node-function-bind
  (package
    (name "node-function-bind")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/function-bind/-/function-bind-1.1.2.tgz")
       (sha256
        (base32 "1ah7x13hmwwfslk72h2rs21c5vqnsxyzqifl2x7lb8823djh4i3h"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ljharb/eslint-config"
                                                  "aud"
                                                  "auto-changelog"
                                                  "eslint"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"))))))))
    (home-page "https://github.com/Raynos/function-bind")
    (synopsis "Implementation of Function.prototype.bind")
    (description "Implementation of Function.prototype.bind")
    (license license:expat)))

(define-public node-hasown
  (package
    (name "node-hasown")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/hasown/-/hasown-2.0.2.tgz")
       (sha256
        (base32 "0zc0za6zy8y2iwy31ayzwmi4j912j382iwr9xsv09bhirp9c9kah"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/function-bind"
                                                  "@types/mock-property"
                                                  "@types/tape"
                                                  "aud"
                                                  "auto-changelog"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "mock-property"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-function-bind))
    (home-page "https://github.com/inspect-js/hasOwn")
    (synopsis "A robust, ES3 compatible, \"has own property\" predicate.")
    (description "A robust, ES3 compatible, \"has own property\" predicate.")
    (license license:expat)))

(define-public node-math-intrinsics
  (package
    (name "node-math-intrinsics")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/math-intrinsics/-/math-intrinsics-1.1.0.tgz")
       (sha256
        (base32 "19s3yi9ziz007ymq0r1k2xk1nrg2m5lc9kw8vy3c0ga9fmaw7hmq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/for-each"
                                                  "@types/object-inspect"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "eclint"
                                                  "es-value-fixtures"
                                                  "eslint"
                                                  "evalmd"
                                                  "for-each"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "object-inspect"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (home-page "https://github.com/es-shims/math-intrinsics")
    (synopsis "ES Math-related intrinsics and helpers, robustly cached.")
    (description "ES Math-related intrinsics and helpers, robustly cached.")
    (license license:expat)))

(define-public node-get-intrinsic
  (package
    (name "node-get-intrinsic")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/get-intrinsic/-/get-intrinsic-1.3.0.tgz")
       (sha256
        (base32 "0i05g3xvqgv16ss19k3jprfnkqsln2n4m7wgn3xldzh09vjjfbk6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ljharb/eslint-config"
                                                  "auto-changelog"
                                                  "call-bound"
                                                  "encoding"
                                                  "es-abstract"
                                                  "es-value-fixtures"
                                                  "eslint"
                                                  "evalmd"
                                                  "for-each"
                                                  "make-async-function"
                                                  "make-async-generator-function"
                                                  "make-generator-function"
                                                  "mock-property"
                                                  "npmignore"
                                                  "nyc"
                                                  "object-inspect"
                                                  "safe-publish-latest"
                                                  "tape"))))))))
    (inputs (list node-math-intrinsics
                  node-hasown
                  node-has-symbols
                  node-gopd
                  node-get-proto
                  node-function-bind
                  node-es-object-atoms
                  node-es-errors
                  node-es-define-property
                  node-call-bind-apply-helpers))
    (home-page "https://github.com/ljharb/get-intrinsic")
    (synopsis
     "Get and robustly cache all JS language-level intrinsics at first require time")
    (description
     "Get and robustly cache all JS language-level intrinsics at first require time")
    (license license:expat)))

(define-public node-object-inspect
  (package
    (name "node-object-inspect")
    (version "1.13.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/object-inspect/-/object-inspect-1.13.4.tgz")
       (sha256
        (base32 "1gwh5vk75w4crskqjsn4ps9z8hqqpjp58d8y82q4b2px79x9c943"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ljharb/eslint-config"
                                                  "@pkgjs/support"
                                                  "auto-changelog"
                                                  "core-js"
                                                  "error-cause"
                                                  "es-value-fixtures"
                                                  "eslint"
                                                  "for-each"
                                                  "functions-have-names"
                                                  "glob"
                                                  "globalthis"
                                                  "has-symbols"
                                                  "has-tostringtag"
                                                  "in-publish"
                                                  "jackspeak"
                                                  "make-arrow-function"
                                                  "mock-property"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "safer-buffer"
                                                  "semver"
                                                  "string.prototype.repeat"
                                                  "tape"))))))))
    (home-page "https://github.com/inspect-js/object-inspect")
    (synopsis "string representations of objects in node and the browser")
    (description "string representations of objects in node and the browser")
    (license license:expat)))

(define-public node-side-channel-map
  (package
    (name "node-side-channel-map")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/side-channel-map/-/side-channel-map-1.0.1.tgz")
       (sha256
        (base32 "1mhnf4m2zdv1ikvjk74v6d7fhr2bzn41a6w95nbcq2rh45j6n99v"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/get-intrinsic"
                                                  "@types/object-inspect"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "eclint"
                                                  "encoding"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-object-inspect node-get-intrinsic node-es-errors
                  node-call-bound))
    (home-page "https://github.com/ljharb/side-channel-map")
    (synopsis
     "Store information about any JS value in a side channel, using a Map")
    (description
     "Store information about any JS value in a side channel, using a Map")
    (license license:expat)))

(define-public node-side-channel-weakmap
  (package
    (name "node-side-channel-weakmap")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/side-channel-weakmap/-/side-channel-weakmap-1.0.2.tgz")
       (sha256
        (base32 "0vnhs2whvv59nkqdsfpmd1fwrcjzh2ka5z8giy68kbg7qpq58aiv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/call-bind"
                                                  "@types/get-intrinsic"
                                                  "@types/object-inspect"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "eclint"
                                                  "encoding"
                                                  "eslint"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-side-channel-map node-object-inspect node-get-intrinsic
                  node-es-errors node-call-bound))
    (home-page "https://github.com/ljharb/side-channel-weakmap")
    (synopsis
     "Store information about any JS value in a side channel. Uses WeakMap if available.")
    (description
     "Store information about any JS value in a side channel. Uses WeakMap if available.")
    (license license:expat)))

(define-public node-side-channel
  (package
    (name "node-side-channel")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/side-channel/-/side-channel-1.1.0.tgz")
       (sha256
        (base32 "11d40rvhvkj4r1dis1vmzi0gc6qnvw90jkc2ppz2w90bk5xyg4w4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@ljharb/eslint-config"
                                                  "@ljharb/tsconfig"
                                                  "@types/object-inspect"
                                                  "@types/tape"
                                                  "auto-changelog"
                                                  "eclint"
                                                  "encoding"
                                                  "eslint"
                                                  "in-publish"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"
                                                  "typescript"))))))))
    (inputs (list node-side-channel-weakmap node-side-channel-map
                  node-side-channel-list node-object-inspect node-es-errors))
    (home-page "https://github.com/ljharb/side-channel")
    (synopsis
     "Store information about any JS value in a side channel. Uses WeakMap if available.")
    (description
     "Store information about any JS value in a side channel. Uses WeakMap if available.")
    (license license:expat)))

(define-public node-qs
  (package
    (name "node-qs")
    (version "6.14.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/qs/-/qs-6.14.0.tgz")
       (sha256
        (base32 "16saymrsxrwvjbilw5hm313g06a0qfjl497wgm2xhc99wbg4121h"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@browserify/envify"
                                                  "@browserify/uglifyify"
                                                  "@ljharb/eslint-config"
                                                  "browserify"
                                                  "bundle-collapser"
                                                  "common-shakeify"
                                                  "eclint"
                                                  "es-value-fixtures"
                                                  "eslint"
                                                  "evalmd"
                                                  "for-each"
                                                  "glob"
                                                  "has-bigints"
                                                  "has-override-mistake"
                                                  "has-property-descriptors"
                                                  "has-proto"
                                                  "has-symbols"
                                                  "iconv-lite"
                                                  "in-publish"
                                                  "jackspeak"
                                                  "mkdirp"
                                                  "mock-property"
                                                  "module-deps"
                                                  "npmignore"
                                                  "nyc"
                                                  "object-inspect"
                                                  "qs-iconv"
                                                  "safe-publish-latest"
                                                  "safer-buffer"
                                                  "tape"
                                                  "unassertify"))))))))
    (inputs (list node-side-channel))
    (home-page "https://github.com/ljharb/qs")
    (synopsis
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (description
     "A querystring parser that supports nesting and arrays, with a depth limit")
    (license license:bsd-3)))

(define-public node-bytes
  (package
    (name "node-bytes")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bytes/-/bytes-3.1.2.tgz")
       (sha256
        (base32 "10f5wgg4izi14lc425v7ljr1ayk28ycdjckfxpm4bnj0bankfpl3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-plugin-markdown"
                                                  "mocha" "nyc"))))))))
    (home-page "https://github.com/visionmedia/bytes.js")
    (synopsis "Utility to parse a string bytes to bytes and vice-versa")
    (description "Utility to parse a string bytes to bytes and vice-versa")
    (license license:expat)))

(define-public node-safer-buffer
  (package
    (name "node-safer-buffer")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/safer-buffer/-/safer-buffer-2.1.2.tgz")
       (sha256
        (base32 "1cx383s7vchfac8jlg3mnb820hkgcvhcpfn9w4f0g61vmrjjz0bq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("standard" "tape"))))))))
    (home-page "https://github.com/ChALkeR/safer-buffer")
    (synopsis "Modern Buffer API polyfill without footguns")
    (description "Modern Buffer API polyfill without footguns")
    (license license:expat)))

(define-public node-iconv-lite
  (package
    (name "node-iconv-lite")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.6.3.tgz")
       (sha256
        (base32 "1x681ziwavjjn09j4858fl3h3xi90vf512k5zwg06kwriwafq9vi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("async" "c8"
                                                  "errto"
                                                  "iconv"
                                                  "mocha"
                                                  "request"
                                                  "semver"
                                                  "unorm"))))))))
    (inputs (list node-safer-buffer))
    (home-page "https://github.com/ashtuchkin/iconv-lite")
    (synopsis "Convert character encodings in pure javascript.")
    (description "Convert character encodings in pure javascript.")
    (license license:expat)))

(define-public node-unpipe
  (package
    (name "node-unpipe")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/unpipe/-/unpipe-1.0.0.tgz")
       (sha256
        (base32 "1dnzbqfmchls4jyvkw0wnkc09pig98y66zzsy3lizgyls435xyrd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "mocha"
                                                  "readable-stream"))))))))
    (home-page "https://github.com/stream-utils/unpipe")
    (synopsis "Unpipe a stream from all destinations")
    (description "Unpipe a stream from all destinations")
    (license license:expat)))

(define-public node-raw-body
  (package
    (name "node-raw-body")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/raw-body/-/raw-body-3.0.0.tgz")
       (sha256
        (base32 "0qa5vynb44l1f7cx7prjdkywk71izyjmhjlr6rqpsa7wk37wdar7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("bluebird" "eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"
                                                  "readable-stream"
                                                  "safe-buffer"))))))))
    (inputs (list node-unpipe node-iconv-lite node-http-errors node-bytes))
    (home-page "https://github.com/stream-utils/raw-body")
    (synopsis "Get and validate the raw body of a readable stream.")
    (description "Get and validate the raw body of a readable stream.")
    (license license:expat)))

(define-public node-media-typer
  (package
    (name "node-media-typer")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/media-typer/-/media-typer-1.1.0.tgz")
       (sha256
        (base32 "1ghrgjcv59qna3h37himz6p7qsby9vki3gjrnv7r5z0y3lg57p5m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/media-typer")
    (synopsis "Simple RFC 6838 media type parser and formatter")
    (description "Simple RFC 6838 media type parser and formatter")
    (license license:expat)))

(define-public node-type-is
  (package
    (name "node-type-is")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/type-is/-/type-is-2.0.1.tgz")
       (sha256
        (base32 "1pbm95wdl9hmm93gmbdxyn00887l4r7jhia6dzgl99d89sx2kgqq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"))))))))
    (inputs (list node-mime-types node-media-typer node-content-type))
    (home-page "https://github.com/jshttp/type-is")
    (synopsis "Infer the content-type of a request.")
    (description "Infer the content-type of a request.")
    (license license:expat)))

(define-public node-body-parser
  (package
    (name "node-body-parser")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/body-parser/-/body-parser-2.2.0.tgz")
       (sha256
        (base32 "1xpsbhcrhzbgnaxjsw06xadqfnp3qp5kyyqidbqa8nndvmyaqg5n"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"
                                                  "supertest"))))))))
    (inputs (list node-type-is
                  node-raw-body
                  node-qs
                  node-on-finished
                  node-iconv-lite
                  node-http-errors
                  node-debug
                  node-content-type
                  node-bytes))
    (home-page "https://github.com/expressjs/body-parser")
    (synopsis "Node.js body parsing middleware")
    (description "Node.js body parsing middleware")
    (license license:expat)))

(define-public node-content-type
  (package
    (name "node-content-type")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/content-type/-/content-type-1.0.5.tgz")
       (sha256
        (base32 "1j0jpnlxjrdpbnq7s1h1xga2n8562j5g6612f7fl40jz82cd0cdc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "deep-equal"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/content-type")
    (synopsis "Create and parse HTTP Content-Type header")
    (description "Create and parse HTTP Content-Type header")
    (license license:expat)))

(define-public node-finalhandler
  (package
    (name "node-finalhandler")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/finalhandler/-/finalhandler-2.1.0.tgz")
       (sha256
        (base32 "161sk1l9fhvgl6a7617g8niim1gx9hsxxcjhf60h6c8h8yzy3z9z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "supertest"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (inputs (list node-on-finished
                  node-escape-html
                  node-encodeurl
                  node-statuses
                  node-parseurl
                  node-debug))
    (home-page "https://github.com/pillarjs/finalhandler")
    (synopsis "Node.js final http responder")
    (description "Node.js final http responder")
    (license license:expat)))

(define-public node-parseurl
  (package
    (name "node-parseurl")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/parseurl/-/parseurl-1.3.3.tgz")
       (sha256
        (base32 "06h2bx1rilkdir3v9jlg94r1q2fn895s0vxjjs0wx5z027x4pvsn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "eslint"
                                                  "istanbul"
                                                  "benchmark"
                                                  "fast-url-parser"
                                                  "beautify-benchmark"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/pillarjs/parseurl")
    (synopsis "parse a url with memoization")
    (description "parse a url with memoization")
    (license license:expat)))

(define-public node-encodeurl
  (package
    (name "node-encodeurl")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/encodeurl/-/encodeurl-2.0.0.tgz")
       (sha256
        (base32 "0agvpr5psd2nymhp51l5502fzifkshxxlnixl7kzkf2ii25l2blv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "eslint"
                                                  "istanbul"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/pillarjs/encodeurl")
    (synopsis
     "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (description
     "Encode a URL to a percent-encoded form, excluding already-encoded sequences")
    (license license:expat)))

(define-public node-escape-html
  (package
    (name "node-escape-html")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/escape-html/-/escape-html-1.0.3.tgz")
       (sha256
        (base32 "0rh35dvab1wbp87dy1m6rynbcb9rbs5kry7jk17ixyxx7if1a0d1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("benchmark"
                                                  "beautify-benchmark"))))))))
    (home-page "https://github.com/component/escape-html")
    (synopsis "Escape string for use in HTML")
    (description "Escape string for use in HTML")
    (license license:expat)))

(define-public node-etag
  (package
    (name "node-etag")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/etag/-/etag-1.8.1.tgz")
       (sha256
        (base32 "1bqgznlsrqcmxnhmnqkhwzcrqfaalxmfxzly1ikaplkkm5w6ragn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "eslint"
                                                  "istanbul"
                                                  "benchmark"
                                                  "seedrandom"
                                                  "safe-buffer"
                                                  "beautify-benchmark"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/etag")
    (synopsis "Create simple HTTP ETags")
    (description "Create simple HTTP ETags")
    (license license:expat)))

(define-public node-fresh
  (package
    (name "node-fresh")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/fresh/-/fresh-2.0.0.tgz")
       (sha256
        (base32 "1mvs4wihlr6bw05h7q12771qrkwrssm26bk80ysv4qjzn1x3j25d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "benchmark"
                                                  "beautify-benchmark"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/fresh")
    (synopsis "HTTP response freshness testing")
    (description "HTTP response freshness testing")
    (license license:expat)))

(define-public node-depd
  (package
    (name "node-depd")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/depd/-/depd-2.0.0.tgz")
       (sha256
        (base32 "19yl2piwl0ci2lvn5j5sk0z4nbldj6apsrqds3ql2d09aqh8m998"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("benchmark"
                                                  "beautify-benchmark"
                                                  "eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "istanbul"
                                                  "mocha"
                                                  "safe-buffer"
                                                  "uid-safe"))))))))
    (home-page "https://github.com/dougwilson/nodejs-depd")
    (synopsis "Deprecate all the things")
    (description "Deprecate all the things")
    (license license:expat)))

(define-public node-toidentifier
  (package
    (name "node-toidentifier")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/toidentifier/-/toidentifier-1.0.1.tgz")
       (sha256
        (base32 "021fp42m51qbqbqabwhxky8bkfkkwza65lqiz7d2gqwd91vwqvqq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"))))))))
    (home-page "https://github.com/component/toidentifier")
    (synopsis "Convert a string of words to a JavaScript identifier")
    (description "Convert a string of words to a JavaScript identifier")
    (license license:expat)))

(define-public node-setprototypeof
  (package
    (name "node-setprototypeof")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/setprototypeof/-/setprototypeof-1.2.0.tgz")
       (sha256
        (base32 "1qnzx8bl8h1vga28pf59mjd52wvh1hf3ma18d4zpwmijlrpcqfy8"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "standard"))))))))
    (home-page "https://github.com/wesleytodd/setprototypeof")
    (synopsis "A small polyfill for Object.setprototypeof")
    (description "A small polyfill for Object.setprototypeof")
    (license license:isc)))

(define-public node-http-errors
  (package
    (name "node-http-errors")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/http-errors/-/http-errors-2.0.0.tgz")
       (sha256
        (base32 "1dypd936i09cvjyxx338da0nimbm4cqi2rrxhjch3ix2wmwx6ky1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (inputs (list node-setprototypeof node-toidentifier node-statuses
                  node-inherits node-depd))
    (home-page "https://github.com/jshttp/http-errors")
    (synopsis "Create HTTP error objects")
    (description "Create HTTP error objects")
    (license license:expat)))

(define-public node-mime-db
  (package
    (name "node-mime-db")
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/mime-db/-/mime-db-1.54.0.tgz")
       (sha256
        (base32 "0864s7g498w1f95yvgq0ayqlgpb8x49jfb80qmcbvsnhcm70a89b"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "undici"
                                                  "csv-parse"
                                                  "media-typer"
                                                  "stream-to-array"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/mime-db")
    (synopsis "Media Type Database")
    (description "Media Type Database")
    (license license:expat)))

(define-public node-mime-types
  (package
    (name "node-mime-types")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/mime-types/-/mime-types-3.0.1.tgz")
       (sha256
        (base32 "1mp2jh80l7xi3ghdrylibfa89cf5gd15vbspcrcfc2xmj8dcc9fv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"))))))))
    (inputs (list node-mime-db))
    (home-page "https://github.com/jshttp/mime-types")
    (synopsis "The ultimate javascript content-type utility.")
    (description "The ultimate javascript content-type utility.")
    (license license:expat)))

(define-public node-ee-first
  (package
    (name "node-ee-first")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ee-first/-/ee-first-1.1.1.tgz")
       (sha256
        (base32 "175r500n567a04qmswzw5hkgdnika3dvn63n284jlar2gvmyhj2i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "mocha"))))))))
    (home-page "https://github.com/jonathanong/ee-first")
    (synopsis "return the first event in a set of ee/event pairs")
    (description "return the first event in a set of ee/event pairs")
    (license license:expat)))

(define-public node-on-finished
  (package
    (name "node-on-finished")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/on-finished/-/on-finished-2.4.1.tgz")
       (sha256
        (base32 "02mxvpahgv07xaih7lmpn8wic9v4jph3fir0qpd6qf4w0kql4kgn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (inputs (list node-ee-first))
    (home-page "https://github.com/jshttp/on-finished")
    (synopsis "Execute a callback when a request closes, finishes, or errors")
    (description
     "Execute a callback when a request closes, finishes, or errors")
    (license license:expat)))

(define-public node-range-parser
  (package
    (name "node-range-parser")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/range-parser/-/range-parser-1.2.1.tgz")
       (sha256
        (base32 "09prs852snwqr9cfcrybm7ysl0z1wka9dh4dwc4v1415cvi6cllh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "deep-equal"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/range-parser")
    (synopsis "Range header field string parser")
    (description "Range header field string parser")
    (license license:expat)))

(define-public node-statuses
  (package
    (name "node-statuses")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/statuses/-/statuses-2.0.1.tgz")
       (sha256
        (base32 "0nig6ygf53sj8vcqvbcwrzm4ln986rcz16kn5qjv1y4s9m1l164i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "raw-body"
                                                  "csv-parse"
                                                  "stream-to-array"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (home-page "https://github.com/jshttp/statuses")
    (synopsis "HTTP status utility")
    (description "HTTP status utility")
    (license license:expat)))

(define-public node-send
  (package
    (name "node-send")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/send/-/send-1.2.0.tgz")
       (sha256
        (base32 "177y3wdhqivc7lqmw10z0w299264dlv5cnff76lvw8vlqb7qj2w9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("after" "eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"
                                                  "supertest"))))))))
    (inputs (list node-statuses
                  node-range-parser
                  node-on-finished
                  node-ms
                  node-mime-types
                  node-http-errors
                  node-fresh
                  node-etag
                  node-escape-html
                  node-encodeurl
                  node-debug))
    (home-page "https://github.com/pillarjs/send")
    (synopsis
     "Better streaming static file server with Range and conditional-GET support")
    (description
     "Better streaming static file server with Range and conditional-GET support")
    (license license:expat)))

(define-public node-serve-static
  (package
    (name "node-serve-static")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/serve-static/-/serve-static-2.2.0.tgz")
       (sha256
        (base32 "10mlmm9lrw8ijrbznizysa5phgyijwcgsis5mrvqsphd2k8dbr5w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-promise"
                                                  "eslint-plugin-standard"
                                                  "mocha"
                                                  "nyc"
                                                  "supertest"))))))))
    (inputs (list node-send node-parseurl node-escape-html node-encodeurl))
    (home-page "https://github.com/expressjs/serve-static")
    (synopsis "Serve static files")
    (description "Serve static files")
    (license license:expat)))

(define-public node-cookie-signature
  (package
    (name "node-cookie-signature")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/cookie-signature/-/cookie-signature-1.2.2.tgz")
       (sha256
        (base32 "1lsk6l4501i1sil49gdwdkaj0nzr5asm5ybx1ppn17i93jpvlasd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "should"))))))))
    (home-page "https://github.com/visionmedia/node-cookie-signature")
    (synopsis "Sign and unsign cookies")
    (description "Sign and unsign cookies")
    (license license:expat)))

(define-public node-merge-descriptors
  (package
    (name "node-merge-descriptors")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/merge-descriptors/-/merge-descriptors-2.0.0.tgz")
       (sha256
        (base32 "1khx20ml70ll3k69qsq8p9ybz967mykrks8ak79m46f7bwm3525f"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "xo"))))))))
    (home-page "https://github.com/sindresorhus/merge-descriptors")
    (synopsis "Merge objects using their property descriptors")
    (description "Merge objects using their property descriptors")
    (license license:expat)))

(define-public node-content-disposition
  (package
    (name "node-content-disposition")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/content-disposition/-/content-disposition-1.0.0.tgz")
       (sha256
        (base32 "13z85wzrcb7p1pwhcmkf0k13f2x8l849liw91mnp62ra2cjpcdhm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "deep-equal"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-standard"))))))))
    (inputs (list node-safe-buffer))
    (home-page "https://github.com/jshttp/content-disposition")
    (synopsis "Create and parse Content-Disposition header")
    (description "Create and parse Content-Disposition header")
    (license license:expat)))

(define-public node-express
  (package
    (name "node-express")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/express/-/express-5.1.0.tgz")
       (sha256
        (base32 "06ry76cxyh8nxk9511kyhghjnmdflidc7ma3hk3j1mal9r6xnsyx"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/express")))
                (mkdir-p lib)
                (copy-recursively "." lib) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ejs" "hbs"
                                                  "nyc"
                                                  "after"
                                                  "mocha"
                                                  "vhost"
                                                  "eslint"
                                                  "marked"
                                                  "morgan"
                                                  "supertest"
                                                  "connect-redis"
                                                  "cookie-parser"
                                                  "cookie-session"
                                                  "express-session"
                                                  "method-override"
                                                  "pbkdf2-password"))))))))
    (inputs (list node-content-disposition
                  node-merge-descriptors
                  node-cookie-signature
                  node-serve-static
                  node-range-parser
                  node-finalhandler
                  node-content-type
                  node-on-finished
                  node-http-errors
                  node-escape-html
                  node-body-parser
                  node-proxy-addr
                  node-mime-types
                  node-encodeurl
                  node-statuses
                  node-parseurl
                  node-type-is
                  node-accepts
                  node-router
                  node-cookie
                  node-fresh
                  node-debug
                  node-vary
                  node-send
                  node-once
                  node-etag
                  node-qs))
    (home-page "https://expressjs.com/")
    (synopsis "Fast, unopinionated, minimalist web framework")
    (description "Fast, unopinionated, minimalist web framework")
    (license license:expat)))

(define-public node-argparse
  (package
    (name "node-argparse")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/argparse/-/argparse-2.0.1.tgz")
       (sha256
        (base32 "133jjyhcr25rf4vy7bca7x06dfmsyy819s1kbbyfc5c2zi3ki417"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/eslint-parser"
                                                  "@babel/plugin-syntax-class-properties"
                                                  "eslint" "mocha" "nyc"))))))))
    (home-page "https://github.com/nodeca/argparse")
    (synopsis "CLI arguments parser. Native port of python's argparse.")
    (description "CLI arguments parser. Native port of python's argparse.")
    (license #f)))

(define-public node-js-yaml
  (package
    (name "node-js-yaml")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/js-yaml/-/js-yaml-4.1.0.tgz")
       (sha256
        (base32 "1jpj5j4aiyh9sbcw7y8jjkwkyc6qmwrffw7a4qfb48ngb4jk7bhd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@rollup/plugin-commonjs"
                                                  "@rollup/plugin-node-resolve"
                                                  "ansi"
                                                  "benchmark"
                                                  "codemirror"
                                                  "eslint"
                                                  "fast-check"
                                                  "gh-pages"
                                                  "mocha"
                                                  "nyc"
                                                  "rollup"
                                                  "rollup-plugin-node-polyfills"
                                                  "rollup-plugin-terser"
                                                  "shelljs"))))))))
    (inputs (list node-argparse))
    (home-page "https://github.com/nodeca/js-yaml")
    (synopsis "YAML 1.2 parser and serializer")
    (description "YAML 1.2 parser and serializer")
    (license license:expat)))

(define-public node-csstype
  (package
    (name "node-csstype")
    (version "3.1.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/csstype/-/csstype-3.1.3.tgz")
       (sha256
        (base32 "0xv731kpasg28w32mcfjgkklav292hmzlkjbc9dg1653y9y3y21g"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/chokidar"
                                                  "@types/css-tree"
                                                  "@types/jest"
                                                  "@types/jsdom"
                                                  "@types/node"
                                                  "@types/prettier"
                                                  "@types/request"
                                                  "@types/turndown"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "chalk"
                                                  "chokidar"
                                                  "eslint"
                                                  "css-tree"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-prettier"
                                                  "fast-glob"
                                                  "flow-bin"
                                                  "jest"
                                                  "jsdom"
                                                  "mdn-browser-compat-data"
                                                  "mdn-data"
                                                  "prettier"
                                                  "request"
                                                  "ts-jest"
                                                  "ts-node"
                                                  "turndown"
                                                  "typescript"))))))))
    (home-page "https://github.com/frenic/csstype")
    (synopsis "Strict TypeScript and Flow types for style based on MDN data")
    (description
     "Strict TypeScript and Flow types for style based on MDN data")
    (license license:expat)))

(define-public node-types-react
  (package
    (name "node-types-react")
    (version "19.1.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@types/react/-/react-19.1.6.tgz")
       (sha256
        (base32 "1as2hb7h0l70wybw12rx1ygi1cpghm2w36hr5m6k59s82f34mr99"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-csstype))
    (home-page
     "https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/react")
    (synopsis "TypeScript definitions for react")
    (description "TypeScript definitions for react")
    (license license:expat)))

(define-public node-utf-8-validate
  (package
    (name "node-utf")
    (version "5.0.10")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/utf-8-validate/-/utf-8-validate-5.0.10.tgz")
       (sha256
        (base32 "12hjjrqm7r8fk87wlln4yzn9c58vhzz6jnqwzh1pcadccds81q3j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "node-gyp"
                                                  "prebuildify"))))))))
    (inputs (list node-node-gyp-build))
    (home-page "https://github.com/websockets/utf-8-validate")
    (synopsis "Check if a buffer contains valid UTF-8")
    (description "Check if a buffer contains valid UTF-8")
    (license license:expat)))

(define-public node-ws
  (package
    (name "node-ws")
    (version "7.5.10")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ws/-/ws-7.5.10.tgz")
       (sha256
        (base32 "0bfsf9vaw54mlx5fv4lizrj6mibyf7gwhm5mxkrfknl946n0wilp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "prettier"
                                                  "benchmark"
                                                  "bufferutil"
                                                  "utf-8-validate"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-prettier"))))))))
    (home-page "https://github.com/websockets/ws")
    (synopsis
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (description
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (license license:expat)))

(define-public node-react-devtools-core
  (package
    (name "node-react-devtools-core")
    (version "4.28.5")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/react-devtools-core/-/react-devtools-core-4.28.5.tgz")
       (sha256
        (base32 "1bwkgsk59npvsdqg65mc6ygrwvprgpmqmkb1cidvk8k5miaxaz2x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("process" "webpack"
                                                  "cross-env" "webpack-cli"
                                                  "workerize-loader"))))))))
    (inputs (list node-shell-quote node-ws))
    (home-page "https://github.com/facebook/react")
    (synopsis "Use react-devtools outside of the browser")
    (description "Use react-devtools outside of the browser")
    (license license:expat)))

(define-public node-alcalzone-ansi-tokenize
  (package
    (name "node-alcalzone-ansi-tokenize")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@alcalzone/ansi-tokenize/-/ansi-tokenize-0.1.3.tgz")
       (sha256
        (base32 "1d0ajnwb7savxgp3rvqhjzl7hn3mbg74wg4a85jmfpaii1f3vdjq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsx"
                                                  "eslint"
                                                  "ts-node"
                                                  "prettier"
                                                  "typescript"
                                                  "@types/node"
                                                  "@tsconfig/node14"
                                                  "source-map-support"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-prettier"
                                                  "@alcalzone/release-script"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@alcalzone/release-script-plugin-license"))))))))
    (inputs (list node-is-fullwidth-code-point node-ansi-styles))
    (home-page "https://www.npmjs.com/package/node-alcalzone-ansi-tokenize")
    (synopsis "Efficiently modify strings containing ANSI escape codes")
    (description "Efficiently modify strings containing ANSI escape codes")
    (license license:expat)))

(define-public node-auto-bind
  (package
    (name "node-auto-bind")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/auto-bind/-/auto-bind-5.0.1.tgz")
       (sha256
        (base32 "0nl9yyidiky35h062qp1mfbpil43aj6xwq5m6v6lic449c0px8f4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/react" "ava" "tsd"
                                                  "xo"))))))))
    (home-page "https://github.com/sindresorhus/auto-bind")
    (synopsis "Automatically bind methods to their class instance")
    (description "Automatically bind methods to their class instance")
    (license license:expat)))

(define-public node-cli-boxes
  (package
    (name "node-cli-boxes")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cli-boxes/-/cli-boxes-3.0.0.tgz")
       (sha256
        (base32 "0fagggwp9550n6qbndrdlw11p8cdj92h69xkl4h5v2v26mgr6836"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"))))))))
    (home-page "https://github.com/sindresorhus/cli-boxes")
    (synopsis "Boxes for use in the terminal")
    (description "Boxes for use in the terminal")
    (license license:expat)))

(define-public node-mimic-fn
  (package
    (name "node-mimic-fn")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/mimic-fn/-/mimic-fn-2.1.0.tgz")
       (sha256
        (base32 "1gv60if81lf2gkwvgixgsx8p87ddhsf1aswkihmfzi462hk5qw7a"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/mimic-fn")
    (synopsis "Make a function mimic another one")
    (description "Make a function mimic another one")
    (license license:expat)))

(define-public node-onetime
  (package
    (name "node-onetime")
    (version "5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/onetime/-/onetime-5.1.2.tgz")
       (sha256
        (base32 "1kda4mbpk8csafchkakglpfyhsnmdkcl6gv1qi9v5dqwh3mb4ngh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-mimic-fn))
    (home-page "https://github.com/sindresorhus/onetime")
    (synopsis "Ensure a function is only called once")
    (description "Ensure a function is only called once")
    (license license:expat)))

(define-public node-restore-cursor
  (package
    (name "node-restore-cursor")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/restore-cursor/-/restore-cursor-4.0.0.tgz")
       (sha256
        (base32 "01zniac8bhsx6h63k02i45sirh828m5kq3f8l47yxllqfxddd6nr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "tsd"))))))))
    (inputs (list node-signal-exit node-onetime))
    (home-page "https://github.com/sindresorhus/restore-cursor")
    (synopsis "Gracefully restore the CLI cursor on exit")
    (description "Gracefully restore the CLI cursor on exit")
    (license license:expat)))

(define-public node-cli-cursor
  (package
    (name "node-cli-cursor")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cli-cursor/-/cli-cursor-4.0.0.tgz")
       (sha256
        (base32 "1arhp5cf2sr8kw7srm4zr3zrapr9h9zgpdaaagscn6qbzi979p3b"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"
                                                  "@types/node"))))))))
    (inputs (list node-restore-cursor))
    (home-page "https://github.com/sindresorhus/cli-cursor")
    (synopsis "Toggle the CLI cursor")
    (description "Toggle the CLI cursor")
    (license license:expat)))

(define-public node-is-fullwidth-code-point
  (package
    (name "node-is-fullwidth-code-point")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-4.0.0.tgz")
       (sha256
        (base32 "1mhw57bjw8bkyzz15kf1imrizvz3cza31zin1i65m2nvbmdap3cs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/is-fullwidth-code-point")
    (synopsis
     "Check if the character represented by a given Unicode code point is fullwidth")
    (description
     "Check if the character represented by a given Unicode code point is fullwidth")
    (license license:expat)))

(define-public node-slice-ansi
  (package
    (name "node-slice-ansi")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/slice-ansi/-/slice-ansi-5.0.0.tgz")
       (sha256
        (base32 "05lz2bbirhfdz6141z6wcnc3319hll4y0qjwi5a7i2850v6qfgg9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "chalk" "random-item"
                                                  "strip-ansi" "xo"))))))))
    (inputs (list node-is-fullwidth-code-point node-ansi-styles))
    (home-page "https://github.com/chalk/slice-ansi")
    (synopsis "Slice a string with ANSI escape codes")
    (description "Slice a string with ANSI escape codes")
    (license license:expat)))

(define-public node-cli-truncate
  (package
    (name "node-cli-truncate")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cli-truncate/-/cli-truncate-4.0.0.tgz")
       (sha256
        (base32 "08myz2n2na1z5xhr8b0n5hyzgpadqwdc9l9zmcziax34vh1klfi9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-string-width node-slice-ansi))
    (home-page "https://github.com/sindresorhus/cli-truncate")
    (synopsis "Truncate a string to a specific width in the terminal")
    (description "Truncate a string to a specific width in the terminal")
    (license license:expat)))

(define-public node-convert-to-spaces
  (package
    (name "node-convert-to-spaces")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vadimdemedes/convert-to-spaces")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00jpjwkkik3rydx78rlyv5dblgw4ynjms9zhih6cx9wxwb5l9psc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'remove-scripts
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[[:space:]]*\\{[^}]*\\},?")
                 "")
                (("\"postinstall\":[[:space:]]*\"[^\"]*\",?")
                 "")
                (("\"prepare\":[[:space:]]*\"[^\"]*\",?")
                 ""))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@sindresorhus/tsconfig"
                                                  "@vdemedes/prettier-config"
                                                  "ava" "prettier"
                                                  "typescript" "xo"))))))))
    (home-page "https://github.com/vadimdemedes/convert-to-spaces")
    (synopsis "Convert tabs to spaces in a string")
    (description "Convert tabs to spaces in a string")
    (license license:expat)))

(define-public node-typescript
  (package
    (name "node-typescript")
    (version "5.8.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/typescript/-/typescript-5.8.3.tgz")
       (sha256
        (base32 "0x43l3anaqaz824llcm069pyxhmmz9s9vdaclfwnwbicp6z5vrvj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "ms"
                                                  "chai"
                                                  "diff"
                                                  "glob"
                                                  "knip"
                                                  "chalk"
                                                  "mocha"
                                                  "tslib"
                                                  "which"
                                                  "dprint"
                                                  "eslint"
                                                  "hereby"
                                                  "esbuild"
                                                  "globals"
                                                  "chokidar"
                                                  "minimist"
                                                  "@types/ms"
                                                  "@eslint/js"
                                                  "playwright"
                                                  "typescript"
                                                  "@types/chai"
                                                  "@types/diff"
                                                  "@types/node"
                                                  "@types/mocha"
                                                  "@types/which"
                                                  "jsonc-parser"
                                                  "@octokit/rest"
                                                  "@types/minimist"
                                                  "fast-xml-parser"
                                                  "@dprint/formatter"
                                                  "@esfx/canceltoken"
                                                  "typescript-eslint"
                                                  "@dprint/typescript"
                                                  "source-map-support"
                                                  "eslint-plugin-regexp"
                                                  "azure-devops-node-api"
                                                  "@typescript-eslint/utils"
                                                  "@types/source-map-support"
                                                  "monocart-coverage-reports"
                                                  "@typescript-eslint/type-utils"
                                                  "@typescript-eslint/rule-tester"
                                                  "mocha-fivemat-progress-reporter"
                                                  "eslint-formatter-autolinkable-stylish"))))))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis
     "TypeScript is a language for application scale JavaScript development")
    (description
     "TypeScript is a language for application scale JavaScript development")
    (license license:asl2.0)))

(define-public node-code-excerpt
  (package
    (name "node-code-excerpt")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vadimdemedes/code-excerpt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0smjigknqdrvbv3m03s19jv5c0ynsa3hija36f4qcvsssgwxh2hi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'remove-scripts
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[[:space:]]*\\{[^}]*\\},?")
                 "")
                (("\"postinstall\":[[:space:]]*\"[^\"]*\",?")
                 "")
                (("\"prepare\":[[:space:]]*\"[^\"]*\",?")
                 ""))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@sindresorhus/tsconfig"
                                                  "@vdemedes/prettier-config"
                                                  "ava" "prettier"
                                                  "typescript" "xo"))))))))
    (native-inputs (list node-typescript))
    (inputs (list node-convert-to-spaces))
    (home-page "https://github.com/vadimdemedes/code-excerpt")
    (synopsis "Extract code excerpts")
    (description "Extract code excerpts")
    (license license:expat)))

(define-public node-es-toolkit
  (package
    (name "node-es-toolkit")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/es-toolkit/-/es-toolkit-1.39.0.tgz")
       (sha256
        (base32 "1cjyvkf5qc9jahjqvqpbddmw7ciysypyqasvjxxd6q7wbscn9wlh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@changesets/changelog-github"
                                                  "@changesets/cli"
                                                  "@eslint/js"
                                                  "@rollup/plugin-terser"
                                                  "@rollup/plugin-typescript"
                                                  "@trivago/prettier-plugin-sort-imports"
                                                  "@types/broken-link-checker"
                                                  "@types/eslint"
                                                  "@types/jscodeshift"
                                                  "@types/node"
                                                  "@types/tar"
                                                  "@typescript-eslint/parser"
                                                  "@vitest/coverage-istanbul"
                                                  "@vue/compiler-sfc"
                                                  "broken-link-checker"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-no-for-of-array"
                                                  "eslint-plugin-prettier"
                                                  "eslint-plugin-vue"
                                                  "execa"
                                                  "globals"
                                                  "happy-dom"
                                                  "jscodeshift"
                                                  "prettier"
                                                  "prettier-plugin-sort-re-exports"
                                                  "rollup"
                                                  "rollup-plugin-dts"
                                                  "tar"
                                                  "tslib"
                                                  "tsx"
                                                  "typescript"
                                                  "typescript-eslint"
                                                  "vercel"
                                                  "vitest"))))))))
    (home-page "https://es-toolkit.slash.page")
    (synopsis
     "A state-of-the-art, high-performance JavaScript utility library with a small bundle size and strong type annotations.")
    (description
     "A state-of-the-art, high-performance JavaScript utility library with a small bundle size and strong type annotations.")
    (license license:expat)))

(define-public node-indent-string
  (package
    (name "node-indent-string")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/indent-string/-/indent-string-5.0.0.tgz")
       (sha256
        (base32 "1rppkdg82z148nzmli0hdyhy3w7ba5sgzi397m47cbilwpd8li6x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/indent-string")
    (synopsis "Indent each line in a string")
    (description "Indent each line in a string")
    (license license:expat)))

(define-public node-is-in-ci
  (package
    (name "node-is-in-ci")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/is-in-ci/-/is-in-ci-1.0.0.tgz")
       (sha256
        (base32 "1r6sckjvd0xpbnw0l4q4fxvi1p8d7agf002c2gxx993jhrf4bwk2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "typescript" "xo"))))))))
    (home-page "https://github.com/sindresorhus/is-in-ci")
    (synopsis
     "Check if the process is running in a Continuous Integration (CI) environment")
    (description
     "Check if the process is running in a Continuous Integration (CI) environment")
    (license license:expat)))

(define-public node-patch-console
  (package
    (name "node-patch-console")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/patch-console/-/patch-console-2.0.0.tgz")
       (sha256
        (base32 "0rxp7jx30hv8hrhy71h76cz2z7n7jb2r9s38fsbd1rg5j4qqnvd1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'remove-scripts
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[[:space:]]*\\{[^}]*\\},?")
                 "")
                (("\"postinstall\":[[:space:]]*\"[^\"]*\",?")
                 "")
                (("\"prepare\":[[:space:]]*\"[^\"]*\",?")
                 ""))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@sindresorhus/tsconfig"
                                                  "@types/node"
                                                  "@vdemedes/prettier-config"
                                                  "ava"
                                                  "prettier"
                                                  "sinon"
                                                  "typescript"
                                                  "xo"))))))))
    (home-page "https://github.com/vadimdemedes/patch-console")
    (synopsis "Patch console methods to intercept output")
    (description "Patch console methods to intercept output")
    (license license:expat)))

(define-public node-scheduler
  (package
    (name "node-scheduler")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/scheduler/-/scheduler-0.26.0.tgz")
       (sha256
        (base32 "0anpg716fh3x79459c3dzz6rbk4rgpvzim3n5a588nyj04s5vg0v"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://react.dev/")
    (synopsis "Cooperative scheduler for the browser environment.")
    (description "Cooperative scheduler for the browser environment.")
    (license license:expat)))

(define-public node-react-reconciler
  (package
    (name "node-react-reconciler")
    (version "0.32.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/react-reconciler/-/react-reconciler-0.32.0.tgz")
       (sha256
        (base32 "1l4fs54g18fjxmim33b8c0a93xlzg5sg6nbffwrkkaa6zgj6z4l5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("react"))))))))
    (inputs (list node-scheduler node-react))
    (home-page "https://react.dev/")
    (synopsis "React package for creating custom renderers.")
    (description "React package for creating custom renderers.")
    (license license:expat)))

(define-public node-js-tokens
  (package
    (name "node-js-tokens")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/js-tokens/-/js-tokens-4.0.0.tgz")
       (sha256
        (base32 "0lrw3qvcfmxrwwi7p7ng4r17yw32ki7jpnbj2a65ddddv2icg16q"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "esprima"
                                                  "coffeescript"
                                                  "everything.js"))))))))
    (home-page "https://github.com/lydell/js-tokens")
    (synopsis "A regex that tokenizes JavaScript.")
    (description "A regex that tokenizes JavaScript.")
    (license license:expat)))

(define-public node-loose-envify
  (package
    (name "node-loose-envify")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/loose-envify/-/loose-envify-1.4.0.tgz")
       (sha256
        (base32 "1p5b3ca0b2jkxalyg7h9bss6aspa8plkh0ak1mrlz2jkjc58660j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("browserify" "envify" "tap"))))))))
    (inputs (list node-js-tokens))
    (home-page "https://github.com/zertosh/loose-envify")
    (synopsis
     "Fast (and loose) selective `process.env` replacer using js-tokens instead of an AST")
    (description
     "Fast (and loose) selective `process.env` replacer using js-tokens instead of an AST")
    (license license:expat)))

(define-public node-scheduler
  (package
    (name "node-scheduler")
    (version "0.23.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/scheduler/-/scheduler-0.23.2.tgz")
       (sha256
        (base32 "1bi6dzp0zb1w0plxa6vzpynjc4wwmz8hlay9cvpr7ax9cgzdiwg4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-loose-envify))
    (home-page "https://reactjs.org/")
    (synopsis "Cooperative scheduler for the browser environment.")
    (description "Cooperative scheduler for the browser environment.")
    (license license:expat)))

(define-public node-signal-exit
  (package
    (name "node-signal-exit")
    (version "3.0.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/signal-exit/-/signal-exit-3.0.7.tgz")
       (sha256
        (base32 "1a10ixkiak24yy6s7p9m7c6v9jkz2fm7wxgc2l3614dbdbx275j3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "coveralls" "nyc"
                                                  "standard-version" "tap"))))))))
    (home-page "https://github.com/tapjs/signal-exit")
    (synopsis "when you want to fire an event no matter how a process exits.")
    (description
     "when you want to fire an event no matter how a process exits.")
    (license license:isc)))

(define-public node-is-fullwidth-code-point
  (package
    (name "node-is-fullwidth-code-point")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-5.0.0.tgz")
       (sha256
        (base32 "05bvj6y5774jvww8r9in17qwj3w72rvyjcq6fl2fz3mylidhlf2y"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-get-east-asian-width))
    (home-page "https://github.com/sindresorhus/is-fullwidth-code-point")
    (synopsis
     "Check if the character represented by a given Unicode code point is fullwidth")
    (description
     "Check if the character represented by a given Unicode code point is fullwidth")
    (license license:expat)))

(define-public node-slice-ansi
  (package
    (name "node-slice-ansi")
    (version "7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/slice-ansi/-/slice-ansi-7.1.0.tgz")
       (sha256
        (base32 "0634nw284w2jx5539bgvsww5nqhc7yd0a8jy6nla86fdyxivy88x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "chalk" "random-item"
                                                  "strip-ansi" "xo"))))))))
    (inputs (list node-is-fullwidth-code-point node-ansi-styles))
    (home-page "https://github.com/chalk/slice-ansi")
    (synopsis "Slice a string with ANSI escape codes")
    (description "Slice a string with ANSI escape codes")
    (license license:expat)))

(define-public node-escape-string-regexp
  (package
    (name "node-escape-string-regexp")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-2.0.0.tgz")
       (sha256
        (base32 "05ivsfsg6yc214fw0r8kaf3590jfvgl81m5l1n5ivxcpyv0xpn3m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/escape-string-regexp")
    (synopsis "Escape RegExp special characters")
    (description "Escape RegExp special characters")
    (license license:expat)))

(define-public node-stack-utils
  (package
    (name "node-stack-utils")
    (version "2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/stack-utils/-/stack-utils-2.0.6.tgz")
       (sha256
        (base32 "1zrf548x1j12fk93q95iqx1hmpvc87l09n3jsjaa03iqmd8shcpb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("bluebird" "coveralls"
                                                  "nested-error-stacks"
                                                  "pify"
                                                  "q"
                                                  "source-map-support"
                                                  "tap"))))))))
    (inputs (list node-escape-string-regexp))
    (home-page "https://github.com/tapjs/stack-utils")
    (synopsis "Captures and cleans stack traces")
    (description "Captures and cleans stack traces")
    (license license:expat)))

(define-public node-type-fest
  (package
    (name "node-type-fest")
    (version "4.41.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/type-fest/-/type-fest-4.41.0.tgz")
       (sha256
        (base32 "0cl98zyggxrvx5w8adbyr4wvg3l4fmzcqrgia8q1kjfxlvh6kqbx"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("expect-type" "npm-run-all2"
                                                  "tsd" "typescript" "xo"))))))))
    (home-page "https://github.com/sindresorhus/type-fest")
    (synopsis "A collection of essential TypeScript types")
    (description "A collection of essential TypeScript types")
    (license #f)))

(define-public node-widest-line
  (package
    (name "node-widest-line")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/widest-line/-/widest-line-5.0.0.tgz")
       (sha256
        (base32 "1bkh8yaqjfavwli3kssvdwnzczgjf1nb3kh7vqjwzlpsv0wldgs5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-string-width))
    (home-page "https://github.com/sindresorhus/widest-line")
    (synopsis
     "Get the visual width of the widest line in a string - the number of columns required to display it")
    (description
     "Get the visual width of the widest line in a string - the number of columns required to display it")
    (license license:expat)))

(define-public node-ansi-styles
  (package
    (name "node-ansi-styles")
    (version "6.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ansi-styles/-/ansi-styles-6.2.1.tgz")
       (sha256
        (base32 "062x7k7hi7ngipb8r95wl1rh49wql7lz9iwd7157k6rf7wyygw38"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "svg-term-cli" "tsd"
                                                  "xo"))))))))
    (home-page "https://github.com/chalk/ansi-styles")
    (synopsis "ANSI escape codes for styling strings in the terminal")
    (description "ANSI escape codes for styling strings in the terminal")
    (license license:expat)))

(define-public node-emoji-regex
  (package
    (name "node-emoji-regex")
    (version "10.4.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/emoji-regex/-/emoji-regex-10.4.0.tgz")
       (sha256
        (base32 "0fwvypqrypyjm8ks7lbbkj16b9d7fjii85bzx7apirwpl8133pmq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@unicode/unicode-16.0.0"
                                                  "emoji-test-regex-pattern"
                                                  "mocha"))))))))
    (home-page "https://mths.be/emoji-regex")
    (synopsis
     "A regular expression to match all Emoji-only symbols as per the Unicode Standard.")
    (description
     "A regular expression to match all Emoji-only symbols as per the Unicode Standard.")
    (license license:expat)))

(define-public node-get-east-asian-width
  (package
    (name "node-get-east-asian-width")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/get-east-asian-width/-/get-east-asian-width-1.3.0.tgz")
       (sha256
        (base32 "03642lmlrrs0609dwdmiik6y3s2g1ir69y6rw6m1g1m96hvgzi97"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "indent-string"
                                                  "outdent" "simplify-ranges"
                                                  "typescript" "xo"))))))))
    (home-page "https://github.com/sindresorhus/get-east-asian-width")
    (synopsis "Determine the East Asian Width of a Unicode character")
    (description "Determine the East Asian Width of a Unicode character")
    (license license:expat)))

(define-public node-string-width
  (package
    (name "node-string-width")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/string-width/-/string-width-7.2.0.tgz")
       (sha256
        (base32 "1wzipf9yyzrklrv5g1f7h46p6j8z31nkx5g2dycgdpnak0d9141y"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-strip-ansi node-get-east-asian-width node-emoji-regex))
    (home-page "https://github.com/sindresorhus/string-width")
    (synopsis
     "Get the visual width of a string - the number of columns required to display it")
    (description
     "Get the visual width of a string - the number of columns required to display it")
    (license license:expat)))

(define-public node-wrap-ansi
  (package
    (name "node-wrap-ansi")
    (version "9.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-9.0.0.tgz")
       (sha256
        (base32 "0wlh7pd73jki26qgfr7047s0vc2pasybhxd2z0jd9zgg1gxhscg4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "chalk"
                                                  "coveralls"
                                                  "has-ansi"
                                                  "nyc"
                                                  "tsd"
                                                  "xo"))))))))
    (inputs (list node-strip-ansi node-string-width node-ansi-styles))
    (home-page "https://github.com/chalk/wrap-ansi")
    (synopsis "Wordwrap a string with ANSI escape codes")
    (description "Wordwrap a string with ANSI escape codes")
    (license license:expat)))

(define-public node-bufferutil
  (package
    (name "node-bufferutil")
    (version "4.0.9")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bufferutil/-/bufferutil-4.0.9.tgz")
       (sha256
        (base32 "1fghdd0bp7z04y90gh536g96xdwi2v2v4g1rivpxpz3pv5f0bjj9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "node-gyp"
                                                  "prebuildify"))))))))
    (inputs (list node-node-gyp-build))
    (home-page "https://github.com/websockets/bufferutil")
    (synopsis "WebSocket buffer utils")
    (description "WebSocket buffer utils")
    (license license:expat)))

(define-public node-node-gyp-build
  (package
    (name "node-node-gyp-build")
    (version "4.8.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/node-gyp-build/-/node-gyp-build-4.8.4.tgz")
       (sha256
        (base32 "095c5d6s3c5kc6f66nsqdamxg91qffk34r0mmqixrgaq87xm014l"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("array-shuffle" "standard"
                                                  "tape"))))))))
    (home-page "https://github.com/prebuild/node-gyp-build")
    (synopsis
     "Build tool and bindings loader for node-gyp that supports prebuilds")
    (description
     "Build tool and bindings loader for node-gyp that supports prebuilds")
    (license license:expat)))

(define-public node-utf-8-validate
  (package
    (name "node-utf")
    (version "6.0.5")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/utf-8-validate/-/utf-8-validate-6.0.5.tgz")
       (sha256
        (base32 "068499ci706d117rk8fda9pqn5kf9k8ci4gjbgngcghli8iyrv5l"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "node-gyp"
                                                  "prebuildify"
                                                  "prebuildify-cross"))))))))
    (inputs (list node-node-gyp-build))
    (home-page "https://github.com/websockets/utf-8-validate")
    (synopsis "Check if a buffer contains valid UTF-8")
    (description "Check if a buffer contains valid UTF-8")
    (license license:expat)))

(define-public node-ws
  (package
    (name "node-ws")
    (version "8.18.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ws/-/ws-8.18.2.tgz")
       (sha256
        (base32 "0cgdgrvn0clx61j5llns5b7p83d75z7r5v1rg52r7bfxwza7n2xr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("benchmark" "bufferutil"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-prettier"
                                                  "globals"
                                                  "mocha"
                                                  "nyc"
                                                  "prettier"
                                                  "utf-8-validate"))))))))
    (home-page "https://github.com/websockets/ws")
    (synopsis
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (description
     "Simple to use, blazing fast and thoroughly tested websocket client and server for Node.js")
    (license license:expat)))

(define-public node-yoga-layout
  (package
    (name "node-yoga-layout")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/yoga-layout/-/yoga-layout-3.2.1.tgz")
       (sha256
        (base32 "1zvzgraw9i0a0gfgbs9bnbh3hz2l5s33iy53lqmpaskki9hpgg28"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/cli" "@babel/core"
                                                  "@babel/preset-env"
                                                  "@babel/preset-typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@types/which"
                                                  "@yogalayout/cmake-bin"
                                                  "babel-register-esm"
                                                  "clang-format"
                                                  "glob"
                                                  "jest"
                                                  "just-scripts"
                                                  "ninja-binaries"
                                                  "which"))))))))
    (home-page "https://yogalayout.dev/")
    (synopsis
     "An embeddable and performant flexbox layout engine with bindings for multiple languages")
    (description
     "An embeddable and performant flexbox layout engine with bindings for multiple languages")
    (license license:expat)))

(define-public node-ink
  (package
    (name "node-ink")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ink/-/ink-6.0.0.tgz")
       (sha256
        (base32 "19sqwq3qqkwy9rwm2mg4sq7mjviy0fm5rxl0d36gs5y5nhy8rpka"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/ink")))
                (mkdir-p lib)
                (copy-recursively "." lib) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@faker-js/faker"
                                                  "@sindresorhus/tsconfig"
                                                  "@types/benchmark"
                                                  "@types/ms"
                                                  "@types/node"
                                                  "@types/react"
                                                  "@types/react-reconciler"
                                                  "@types/scheduler"
                                                  "@types/signal-exit"
                                                  "@types/sinon"
                                                  "@types/stack-utils"
                                                  "@types/ws"
                                                  "@vdemedes/prettier-config"
                                                  "ava"
                                                  "boxen"
                                                  "delay"
                                                  "eslint-config-xo-react"
                                                  "eslint-plugin-react"
                                                  "eslint-plugin-react-hooks"
                                                  "ms"
                                                  "node-pty"
                                                  "p-queue"
                                                  "prettier"
                                                  "react"
                                                  "react-devtools-core"
                                                  "sinon"
                                                  "strip-ansi"
                                                  "ts-node"
                                                  "typescript"
                                                  "xo"))))))))
    (inputs (list node-yoga-layout
                  node-ws
                  node-wrap-ansi
                  node-widest-line
                  node-type-fest
                  node-string-width
                  node-stack-utils
                  node-slice-ansi
                  node-signal-exit
                  node-scheduler
                  node-react-reconciler
                  node-patch-console
                  node-is-in-ci
                  node-indent-string
                  node-es-toolkit
                  node-code-excerpt
                  node-cli-truncate
                  node-cli-cursor
                  node-cli-boxes
                  node-chalk
                  node-auto-bind
                  node-ansi-styles
                  node-ansi-escapes
                  node-alcalzone-ansi-tokenize
                  node-react-devtools-core
                  node-react
                  node-types-react))
    (home-page "https://github.com/vadimdemedes/ink")
    (synopsis "React for CLI")
    (description "React for CLI")
    (license license:expat)))

(define-public node-cli-spinners
  (package
    (name "node-cli-spinners")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cli-spinners/-/cli-spinners-3.2.0.tgz")
       (sha256
        (base32 "0nd97wkhhwh3wnrdch16hmz9c2327dn4an7fb5ba0n6fdm30z3v4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "log-update"
                                                  "string-length" "typescript"
                                                  "xo"))))))))
    (home-page "https://github.com/sindresorhus/cli-spinners")
    (synopsis "Spinners for use in the terminal")
    (description "Spinners for use in the terminal")
    (license license:expat)))

(define-public node-deepmerge
  (package
    (name "node-deepmerge")
    (version "4.3.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/deepmerge/-/deepmerge-4.3.1.tgz")
       (sha256
        (base32 "0p0xixxkxlld05k9vddvmhlqd5mmyrhzasd97xa9asi792is00s0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node"
                                                  "is-mergeable-object"
                                                  "is-plain-object"
                                                  "jsmd"
                                                  "rollup"
                                                  "rollup-plugin-commonjs"
                                                  "rollup-plugin-node-resolve"
                                                  "tape"
                                                  "ts-node"
                                                  "typescript"
                                                  "uglify-js"))))))))
    (home-page "https://github.com/TehShrike/deepmerge")
    (synopsis "A library for deep (recursive) merging of Javascript objects")
    (description
     "A library for deep (recursive) merging of Javascript objects")
    (license license:expat)))

(define-public node-is-unicode-supported
  (package
    (name "node-is-unicode-supported")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/is-unicode-supported/-/is-unicode-supported-2.1.0.tgz")
       (sha256
        (base32 "0qnkzlhkxra98ahzbpcr0w3sd2h22wrffn26gg5zjbz61r0c09jr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/is-unicode-supported")
    (synopsis "Detect whether the terminal supports Unicode")
    (description "Detect whether the terminal supports Unicode")
    (license license:expat)))

(define-public node-figures
  (package
    (name "node-figures")
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/figures/-/figures-6.1.0.tgz")
       (sha256
        (base32 "1z0xwlwg029qbdrfvawjhh4b995px8b848qxc7sxv498fxf1byrh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-is-unicode-supported))
    (home-page "https://github.com/sindresorhus/figures")
    (synopsis "Unicode symbols with fallbacks for older terminals")
    (description "Unicode symbols with fallbacks for older terminals")
    (license license:expat)))

(define-public node-inkjs-ui
  (package
    (name "node-inkjs-ui")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@inkjs/ui/-/ui-2.0.0.tgz")
       (sha256
        (base32 "1ykjcl6rcvsgd29p0w63xsgassfwhsj7f5l202mzzhf2k73cja1z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/@inkjs/ui")))
                (mkdir-p lib)
                (copy-recursively "." lib) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@sindresorhus/tsconfig"
                                                  "@types/react"
                                                  "@vdemedes/prettier-config"
                                                  "ava"
                                                  "boxen"
                                                  "cat-names"
                                                  "delay"
                                                  "eslint-config-xo-react"
                                                  "eslint-plugin-react"
                                                  "eslint-plugin-react-hooks"
                                                  "ink"
                                                  "ink-testing-library"
                                                  "prettier"
                                                  "react"
                                                  "tsimp"
                                                  "typescript"
                                                  "xo"))))))))
    (inputs (list node-figures node-deepmerge node-cli-spinners node-chalk
                  node-ink))
    (home-page "https://github.com/vadimdemedes/ink-ui")
    (synopsis
     "Collection of customizable UI components for CLIs made with Ink")
    (description
     "Collection of customizable UI components for CLIs made with Ink")
    (license license:expat)))

(define-public node-peek-readable
  (package
    (name "node-peek-readable")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/peek-readable/-/peek-readable-7.0.0.tgz")
       (sha256
        (base32 "0rgzz9bl4hm72m3nwhy7fq8jlzky4lwl6lkfyvpyhjqpwdqkxip7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@biomejs/biome"
                                                  "@types/chai"
                                                  "@types/chai-as-promised"
                                                  "@types/mocha"
                                                  "@types/node"
                                                  "c8"
                                                  "chai"
                                                  "chai-as-promised"
                                                  "del-cli"
                                                  "mocha"
                                                  "node-readable-to-web-readable-stream"
                                                  "remark-cli"
                                                  "remark-preset-lint-recommended"
                                                  "source-map-support"
                                                  "ts-node"
                                                  "typescript"))))))))
    (home-page "https://github.com/Borewit/peek-readable")
    (synopsis "Read and peek from a readable stream")
    (description "Read and peek from a readable stream")
    (license license:expat)))

(define-public node-strtok3
  (package
    (name "node-strtok3")
    (version "10.2.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/strtok3/-/strtok3-10.2.2.tgz")
       (sha256
        (base32 "1y8cnibh3kk10avlk5s81kzh87k146yvn92bzqwwg0nc96hflwhl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@biomejs/biome"
                                                  "@types/chai"
                                                  "@types/chai-as-promised"
                                                  "@types/debug"
                                                  "@types/mocha"
                                                  "@types/node"
                                                  "c8"
                                                  "chai"
                                                  "chai-as-promised"
                                                  "del-cli"
                                                  "mocha"
                                                  "node-readable-to-web-readable-stream"
                                                  "remark-cli"
                                                  "remark-preset-lint-recommended"
                                                  "token-types"
                                                  "ts-node"
                                                  "typescript"
                                                  "uint8array-extras"))))))))
    (inputs (list node-peek-readable node-tokenizer-token))
    (home-page "https://github.com/Borewit/strtok3")
    (synopsis "A promise based streaming tokenizer")
    (description "A promise based streaming tokenizer")
    (license license:expat)))

(define-public node-uint8array-extras
  (package
    (name "node-uint8array-extras")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/uint8array-extras/-/uint8array-extras-1.4.0.tgz")
       (sha256
        (base32 "1ah4sfv1r16v92gdw2xq7ss9dm4h2kx021b6vg5b3bh6hhyy8alc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "typescript" "xo"
                                                  "benchmark"))))))))
    (home-page "https://github.com/sindresorhus/uint8array-extras")
    (synopsis "Useful utilities for working with Uint8Array (and Buffer)")
    (description "Useful utilities for working with Uint8Array (and Buffer)")
    (license license:expat)))

(define-public node-fflate
  (package
    (name "node-fflate")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/fflate/-/fflate-0.8.2.tgz")
       (sha256
        (base32 "1p21s6c9kk613jzyqjxzw49s6y5xqzzg5xr964z5x3pww9hm1zb1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@parcel/service-worker"
                                                  "@types/node"
                                                  "@types/pako"
                                                  "@types/react"
                                                  "@types/react-dom"
                                                  "jszip"
                                                  "pako"
                                                  "parcel"
                                                  "preact"
                                                  "react"
                                                  "react-dom"
                                                  "simple-git"
                                                  "terser"
                                                  "tiny-inflate"
                                                  "ts-node"
                                                  "typedoc"
                                                  "typedoc-plugin-markdown"
                                                  "typescript"
                                                  "uvu"
                                                  "uzip"))))))))
    (home-page "https://101arrowz.github.io/fflate")
    (synopsis "High performance (de)compression in an 8kB package")
    (description "High performance (de)compression in an 8kB package")
    (license license:expat)))

(define-public node-tokenizer-token
  (package
    (name "node-tokenizer-token")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@tokenizer/token/-/token-0.3.0.tgz")
       (sha256
        (base32 "02f2hw274vlgch5azd8pq79icksippxys3khf99qx713rb75h5wi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node"))))))))
    (home-page "https://github.com/Borewit/tokenizer-token")
    (synopsis "TypeScript definition for strtok3 token")
    (description "TypeScript definition for strtok3 token")
    (license license:expat)))

(define-public node-token-types
  (package
    (name "node-token-types")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/token-types/-/token-types-6.0.0.tgz")
       (sha256
        (base32 "0yk1b3spmnz7yzcky49lychr12n6cc4i86mrqs4ydx05rgwxqxna"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/chai" "@types/mocha"
                                                  "@types/node"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "c8"
                                                  "chai"
                                                  "del-cli"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-import-resolver-typescript"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-jsdoc"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-unicorn"
                                                  "mocha"
                                                  "remark-cli"
                                                  "remark-preset-lint-recommended"
                                                  "source-map-support"
                                                  "ts-node"
                                                  "typescript"))))))))
    (inputs (list node-ieee754 node-tokenizer-token))
    (home-page "https://github.com/Borewit/token-types")
    (synopsis
     "Common token types for decoding and encoding numeric and string values")
    (description
     "Common token types for decoding and encoding numeric and string values")
    (license license:expat)))

(define-public node-tokenizer-inflate
  (package
    (name "node-tokenizer-inflate")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@tokenizer/inflate/-/inflate-0.2.7.tgz")
       (sha256
        (base32 "199v02px6n80gmgvhqbm0sjh7yzn4252hnsijqzzxqb05xpb8vwz"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@aws-sdk/client-s3"
                                                  "@biomejs/biome"
                                                  "@tokenizer/s3"
                                                  "@types/chai"
                                                  "@types/debug"
                                                  "@types/mocha"
                                                  "@types/node"
                                                  "chai"
                                                  "del-cli"
                                                  "file-type"
                                                  "mocha"
                                                  "strtok3"
                                                  "ts-node"
                                                  "typescript"))))))))
    (inputs (list node-token-types node-fflate node-debug))
    (home-page "https://github.com/Borewit/tokenizer-inflate")
    (synopsis "Tokenized zip support")
    (description "Tokenized zip support")
    (license license:expat)))

(define-public node-file-type
  (package
    (name "node-file-type")
    (version "20.5.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/file-type/-/file-type-20.5.0.tgz")
       (sha256
        (base32 "1zk9nlga5havh9ixs4z61s1hmi95x45y8lazljdcz9w5kjzgy7bi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"
                                                  "tsd"
                                                  "commonmark"
                                                  "get-stream"
                                                  "@types/node"
                                                  "noop-stream"
                                                  "@tokenizer/token"))))))))
    (inputs (list node-tokenizer-inflate node-uint8array-extras
                  node-token-types node-strtok3))
    (home-page "https://github.com/sindresorhus/file-type")
    (synopsis "Detect the file type of a file, stream, or data")
    (description "Detect the file type of a file, stream, or data")
    (license license:expat)))

(define-public node-strip-ansi
  (package
    (name "node-strip-ansi")
    (version "7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/strip-ansi/-/strip-ansi-7.1.0.tgz")
       (sha256
        (base32 "0wckndx1a7226mikg5989dl2sv8rh31inigm0qdqnmch8q4s7i4l"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-ansi-regex))
    (home-page "https://github.com/chalk/strip-ansi")
    (synopsis "Strip ANSI escape codes from a string")
    (description "Strip ANSI escape codes from a string")
    (license license:expat)))

(define-public node-to-rotated
  (package
    (name "node-to-rotated")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/to-rotated/-/to-rotated-1.0.0.tgz")
       (sha256
        (base32 "08ghsx0p3mac0bcmzmcnlx8lwi9rkq7dk50r9yaywpl3224vy0pj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "xo"))))))))
    (home-page "https://github.com/sindresorhus/to-rotated")
    (synopsis "Rotate an array by a given number of steps")
    (description "Rotate an array by a given number of steps")
    (license license:expat)))

(define-public node-shell-quote
  (package
    (name "node-shell-quote")
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/shell-quote/-/shell-quote-1.8.3.tgz")
       (sha256
        (base32 "1h92gr88jggrii0xg6q7fd5y0dqmjm82wan7sqiji3mg9cshnr6z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@ljharb/eslint-config"
                                                  "auto-changelog"
                                                  "encoding"
                                                  "eslint"
                                                  "evalmd"
                                                  "in-publish"
                                                  "jackspeak"
                                                  "npmignore"
                                                  "nyc"
                                                  "safe-publish-latest"
                                                  "tape"))))))))
    (home-page "https://github.com/ljharb/shell-quote")
    (synopsis "quote and parse shell commands")
    (description "quote and parse shell commands")
    (license license:expat)))

(define-public node-react
  (package
    (name "node-react")
    (version "19.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/react/-/react-19.1.0.tgz")
       (sha256
        (base32 "0mgzwz34nipl15h9q69sxhhnglgf919dpp3ld9hrn5dkwpbk6m7i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://react.dev/")
    (synopsis "React is a JavaScript library for building user interfaces.")
    (description "React is a JavaScript library for building user interfaces.")
    (license license:expat)))

(define-public node-use-interval
  (package
    (name "node-use-interval")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/use-interval/-/use-interval-1.4.0.tgz")
       (sha256
        (base32 "09fbw63n8pxwgdzfybd4r7308vi3832grrx7wr6q4d7g5bn8sin1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/runtime"
                                                  "@types/jest"
                                                  "@types/react-dom"
                                                  "@types/react"
                                                  "babel-preset-react-app"
                                                  "cross-env"
                                                  "gh-pages"
                                                  "prettier"
                                                  "react"
                                                  "react-dom"
                                                  "react-scripts"
                                                  "rollup"
                                                  "rollup-plugin-babel"
                                                  "rollup-plugin-commonjs"
                                                  "rollup-plugin-node-resolve"
                                                  "rollup-plugin-peer-deps-external"
                                                  "rollup-plugin-typescript2"
                                                  "rollup-plugin-url"
                                                  "typescript"))))))))
    (home-page "https://github.com/Hermanya/use-interval")
    (synopsis "React hook for setting an interval as posted on overreacted.io")
    (description
     "React hook for setting an interval as posted on overreacted.io")
    (license license:expat)))

(define-public node-fast-npm-meta
  (package
    (name "node-fast-npm-meta")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/fast-npm-meta/-/fast-npm-meta-0.4.3.tgz")
       (sha256
        (base32 "0vrk6c9j9d5j2xsr1hl2j5c52vzadrc91h4lnaxsaahywpncwzwy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/antfu/fast-npm-meta")
    (synopsis "Get npm package metadata")
    (description "Get npm package metadata")
    (license license:expat)))

(define-public node-fast-deep-equal
  (package
    (name "node-fast-deep-equal")
    (version "3.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/fast-deep-equal/-/fast-deep-equal-3.1.3.tgz")
       (sha256
        (base32 "13vvwib6za4zh7054n3fg86y127ig3jb0djqz31qsqr71yca06dh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("coveralls" "dot"
                                                  "eslint"
                                                  "mocha"
                                                  "nyc"
                                                  "pre-commit"
                                                  "react"
                                                  "react-test-renderer"
                                                  "sinon"
                                                  "typescript"))))))))
    (home-page "https://github.com/epoberezkin/fast-deep-equal")
    (synopsis "Fast deep equal")
    (description "Fast deep equal")
    (license license:expat)))

(define-public node-marked
  (package
    (name "node-marked")
    (version "15.0.12")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/marked/-/marked-15.0.12.tgz")
       (sha256
        (base32 "1aisg55lfmih6c86w8swlmayl6a68ifsazcjdy2b45dgjsl4ga1r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@arethetypeswrong/cli"
                                                  "@markedjs/eslint-config"
                                                  "@markedjs/testutils"
                                                  "@semantic-release/commit-analyzer"
                                                  "@semantic-release/git"
                                                  "@semantic-release/github"
                                                  "@semantic-release/npm"
                                                  "@semantic-release/release-notes-generator"
                                                  "cheerio"
                                                  "commonmark"
                                                  "cross-env"
                                                  "dts-bundle-generator"
                                                  "esbuild"
                                                  "esbuild-plugin-umd-wrapper"
                                                  "eslint"
                                                  "highlight.js"
                                                  "markdown-it"
                                                  "marked-highlight"
                                                  "marked-man"
                                                  "recheck"
                                                  "semantic-release"
                                                  "titleize"
                                                  "tslib"
                                                  "typescript"))))))))
    (home-page "https://marked.js.org")
    (synopsis "A markdown parser built for speed")
    (description "A markdown parser built for speed")
    (license license:expat)))

(define-public node-environment
  (package
    (name "node-environment")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/environment/-/environment-1.1.0.tgz")
       (sha256
        (base32 "0zw1fsaq56rvcjls4639m897k2ynbx85cm23y9sps3gqhvg0zjxj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "typescript" "xo"))))))))
    (home-page "https://github.com/sindresorhus/environment")
    (synopsis
     "Check which JavaScript environment your code is running in at runtime: browser, Node.js, Bun, etc")
    (description
     "Check which JavaScript environment your code is running in at runtime: browser, Node.js, Bun, etc")
    (license license:expat)))

(define-public node-ansi-escapes
  (package
    (name "node-ansi-escapes")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ansi-escapes/-/ansi-escapes-7.0.0.tgz")
       (sha256
        (base32 "1sgs57gbsn17lhzn2hyd42p52jnrc49vvh2wycs36k9h30ywcrjw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "ava" "tsd"
                                                  "xo"))))))))
    (inputs (list node-environment))
    (home-page "https://github.com/sindresorhus/ansi-escapes")
    (synopsis "ANSI escape codes for manipulating the terminal")
    (description "ANSI escape codes for manipulating the terminal")
    (license license:expat)))

(define-public node-ansi-regex
  (package
    (name "node-ansi-regex")
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ansi-regex/-/ansi-regex-6.1.0.tgz")
       (sha256
        (base32 "0g2gd7xf3n73wd8vlr9kjdbyaxba0kb97q9daam8ddh2fvhnyvq4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ansi-escapes" "ava" "tsd"
                                                  "xo"))))))))
    (home-page "https://github.com/chalk/ansi-regex")
    (synopsis "Regular expression for matching ANSI escape codes")
    (description "Regular expression for matching ANSI escape codes")
    (license license:expat)))

(define-public node-chalk
  (package
    (name "node-chalk")
    (version "5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/chalk/-/chalk-5.4.1.tgz")
       (sha256
        (base32 "1nnhzx3mpy8mkscl71iq2liqbfiis51ayy9293s82rx7hvrvkf44"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "ava"
                                                  "c8"
                                                  "color-convert"
                                                  "execa"
                                                  "log-update"
                                                  "matcha"
                                                  "tsd"
                                                  "xo"
                                                  "yoctodelay"))))))))
    (home-page "https://github.com/chalk/chalk")
    (synopsis "Terminal string styling done right")
    (description "Terminal string styling done right")
    (license license:expat)))

(define-public node-chalk
  (package
    (name "node-chalk")
    (version "4.1.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/chalk/-/chalk-4.1.2.tgz")
       (sha256
        (base32 "02prgl8d52k2vgxnssx06ha2sjm2vp6v6s6kqgkar1ryllx68k78"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"
                                                  "nyc"
                                                  "tsd"
                                                  "execa"
                                                  "matcha"
                                                  "coveralls"
                                                  "import-fresh"
                                                  "resolve-from"))))))))
    (inputs (list node-supports-color node-ansi-styles))
    (home-page "https://github.com/chalk/chalk")
    (synopsis "Terminal string styling done right")
    (description "Terminal string styling done right")
    (license license:expat)))

(define-public node-highlight-js
  (package
    (name "node-highlight-js")
    (version "10.7.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/highlight.js/-/highlight.js-10.7.3.tgz")
       (sha256
        (base32 "07m9prr3bz7vygcl80kfbwg65pwvinlp6k9fvl7r2vqh3p1nha99"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("del" "glob"
                                                  "refa"
                                                  "jsdom"
                                                  "mocha"
                                                  "colors"
                                                  "eslint"
                                                  "lodash"
                                                  "rollup"
                                                  "should"
                                                  "terser"
                                                  "clean-css"
                                                  "cli-table"
                                                  "commander"
                                                  "handlebars"
                                                  "typescript"
                                                  "tiny-worker"
                                                  "@types/mocha"
                                                  "glob-promise"
                                                  "deep-freeze-es6"
                                                  "eslint-plugin-node"
                                                  "@rollup/plugin-json"
                                                  "dependency-resolver"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-promise"
                                                  "eslint-config-standard"
                                                  "@rollup/plugin-commonjs"
                                                  "@typescript-eslint/parser"
                                                  "@rollup/plugin-node-resolve"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://highlightjs.org/")
    (synopsis "Syntax highlighting with language autodetection.")
    (description "Syntax highlighting with language autodetection.")
    (license license:bsd-3)))

(define-public node-object-assign
  (package
    (name "node-object-assign")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/object-assign/-/object-assign-4.1.1.tgz")
       (sha256
        (base32 "1v999sycxcp74j2pikdhyinm2d80p2bsy4nnrrnb59rv4rm74bbq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "lodash" "matcha"))))))))
    (home-page "https://github.com/sindresorhus/object-assign")
    (synopsis "ES2015 `Object.assign()` ponyfill")
    (description "ES2015 `Object.assign()` ponyfill")
    (license license:expat)))

(define-public node-any-promise
  (package
    (name "node-any-promise")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/any-promise/-/any-promise-1.3.0.tgz")
       (sha256
        (base32 "0fjn1ckk3ds74szpk1lh2g7jjw7mzyr9ci6w12v6ckb273ssz8cv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "bluebird"
                                                  "es6-promise"
                                                  "is-promise"
                                                  "lie"
                                                  "mocha"
                                                  "native-promise-only"
                                                  "phantomjs-prebuilt"
                                                  "pinkie"
                                                  "promise"
                                                  "q"
                                                  "rsvp"
                                                  "vow"
                                                  "when"
                                                  "zuul"))))))))
    (home-page "http://github.com/kevinbeaty/any-promise")
    (synopsis "Resolve any installed ES6 compatible promise")
    (description "Resolve any installed ES6 compatible promise")
    (license license:expat)))

(define-public node-thenify
  (package
    (name "node-thenify")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/thenify/-/thenify-3.3.1.tgz")
       (sha256
        (base32 "10mlld6q2z3h03dfrwnxr3qv5lyxk4f60ff3xiglkml90g373q7j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("bluebird" "istanbul"
                                                  "mocha"))))))))
    (inputs (list node-any-promise))
    (home-page "https://github.com/thenables/thenify")
    (synopsis "Promisify a callback-based function")
    (description "Promisify a callback-based function")
    (license license:expat)))

(define-public node-thenify-all
  (package
    (name "node-thenify-all")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/thenify-all/-/thenify-all-1.6.0.tgz")
       (sha256
        (base32 "1fzcliy88iz2qzkh99jicjzf0j3snvx6jg93ga64ps08b4l6hqdh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("bluebird" "istanbul"
                                                  "mocha"))))))))
    (inputs (list node-thenify))
    (home-page "https://github.com/thenables/thenify-all")
    (synopsis "Promisifies all the selected functions in an object")
    (description "Promisifies all the selected functions in an object")
    (license license:expat)))

(define-public node-mz
  (package
    (name "node-mz")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/mz/-/mz-2.7.0.tgz")
       (sha256
        (base32 "0cxmswx4xgi5pv90mcx08apdg86da52r7dhv8c90l9llz18frl77"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("istanbul" "bluebird"
                                                  "mocha"))))))))
    (inputs (list node-thenify-all node-object-assign node-any-promise))
    (home-page "https://github.com/normalize/mz")
    (synopsis "modernize node.js to current ECMAScript standards")
    (description "modernize node.js to current ECMAScript standards")
    (license license:expat)))

(define-public node-parse5
  (package
    (name "node-parse5")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/parse5/-/parse5-5.1.1.tgz")
       (sha256
        (base32 "0x9yhqny7n39bsa1xy52xsc6yx1zs5v8mn79bv67x2mpf3zpwwlj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/inikulin/parse5")
    (synopsis "HTML parser and serializer.")
    (description "HTML parser and serializer.")
    (license license:expat)))

(define-public node-parse5
  (package
    (name "node-parse5")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/parse5/-/parse5-6.0.1.tgz")
       (sha256
        (base32 "1x4nysnnip9vfgy5gianq85v7xsgdzsidmwp1jlf7nddkhq2lmjs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/inikulin/parse5")
    (synopsis "HTML parser and serializer.")
    (description "HTML parser and serializer.")
    (license license:expat)))

(define-public node-parse5-htmlparser2-tree-adapter
  (package
    (name "node-parse5-htmlparser2-tree-adapter")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/parse5-htmlparser2-tree-adapter/-/parse5-htmlparser2-tree-adapter-6.0.1.tgz")
       (sha256
        (base32 "0qpkkiqv9999sn0p3j6qljh1vbqn7731a4kbmla4mfvn9vq941z7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (inputs (list node-parse5))
    (home-page "https://github.com/inikulin/parse5")
    (synopsis "htmlparser2 tree adapter for parse5.")
    (description "htmlparser2 tree adapter for parse5.")
    (license license:expat)))

(define-public node-y18n
  (package
    (name "node-y18n")
    (version "5.0.8")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/y18n/-/y18n-5.0.8.tgz")
       (sha256
        (base32 "0sd7mpg2c0fxqq4q2574hvm26fgy6byp1gxnsgskmjx7sfx46dyl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node"
                                                  "@wessberg/rollup-plugin-ts"
                                                  "c8"
                                                  "chai"
                                                  "cross-env"
                                                  "gts"
                                                  "mocha"
                                                  "rimraf"
                                                  "rollup"
                                                  "standardx"
                                                  "ts-transform-default-export"
                                                  "typescript")))))
          (add-after 'delete-dev-dependencies 'remove-scripts
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[[:space:]]*\\{[^}]*\\},?")
                 "\"scripts\": {},")
                (("\"postinstall\":[[:space:]]*\"[^\"]*\",?")
                 "")
                (("\"prepare\":[[:space:]]*\"[^\"]*\",?")
                 "")))))))
    (home-page "https://github.com/yargs/y18n")
    (synopsis "the bare-bones internationalization library used by yargs")
    (description "the bare-bones internationalization library used by yargs")
    (license license:isc)))

(define-public node-color-name
  (package
    (name "node-color-name")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/color-name/-/color-name-1.1.4.tgz")
       (sha256
        (base32 "020p7x7k8rlph38lhsqpqvkx0b70lzlmk6mgal9r9sz8c527qysh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/colorjs/color-name")
    (synopsis "A list of color names and its values")
    (description "A list of color names and its values")
    (license license:expat)))

(define-public node-color-convert
  (package
    (name "node-color-convert")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/color-convert/-/color-convert-2.0.1.tgz")
       (sha256
        (base32 "1qbw9rwfzcp7y0cpa8gmwlj7ccycf9pwn15zvf2s06f070ss83wj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "chalk"))))))))
    (inputs (list node-color-name))
    (home-page "https://github.com/Qix-/color-convert")
    (synopsis "Plain color conversion functions")
    (description "Plain color conversion functions")
    (license license:expat)))

(define-public node-ansi-styles
  (package
    (name "node-ansi-styles")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ansi-styles/-/ansi-styles-4.3.0.tgz")
       (sha256
        (base32 "0zwqsx67hr7m4a8dpd0jzkp2rjm5v7938x4rhcqh7djsv139llrc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/color-convert" "ava"
                                                  "svg-term-cli" "tsd" "xo"))))))))
    (inputs (list node-color-convert))
    (home-page "https://github.com/chalk/ansi-styles")
    (synopsis "ANSI escape codes for styling strings in the terminal")
    (description "ANSI escape codes for styling strings in the terminal")
    (license license:expat)))

(define-public node-wrap-ansi
  (package
    (name "node-wrap-ansi")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-7.0.0.tgz")
       (sha256
        (base32 "0bx4bkwfli34343rl97qy5pdg3zmskinsf8mlkv3isfj1d8v7587"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "chalk" "coveralls"
                                                  "has-ansi" "nyc" "xo"))))))))
    (inputs (list node-strip-ansi node-string-width node-ansi-styles))
    (home-page "https://github.com/chalk/wrap-ansi")
    (synopsis "Wordwrap a string with ANSI escape codes")
    (description "Wordwrap a string with ANSI escape codes")
    (license license:expat)))

(define-public node-cliui
  (package
    (name "node-cliui")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cliui/-/cliui-7.0.4.tgz")
       (sha256
        (base32 "1fq3ak5alvaybsz9asc0fx7xj7wgwwp05h8k8dm85s43hrdq7x57"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "gts"
                                                  "chai"
                                                  "chalk"
                                                  "mocha"
                                                  "eslint"
                                                  "rimraf"
                                                  "rollup"
                                                  "cross-env"
                                                  "standardx"
                                                  "typescript"
                                                  "@types/node"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "@typescript-eslint/parser"
                                                  "@wessberg/rollup-plugin-ts"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-string-width node-strip-ansi node-wrap-ansi))
    (home-page "https://github.com/yargs/cliui")
    (synopsis "easily create complex multi-column command-line-interfaces")
    (description "easily create complex multi-column command-line-interfaces")
    (license license:isc)))

(define-public node-escalade
  (package
    (name "node-escalade")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/escalade/-/escalade-3.2.0.tgz")
       (sha256
        (base32 "1rxy0388xsgxam8limnbxhksa1z1xccd47vi4r9p1ip6q2pma9xs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("bundt" "esm" "uvu"))))))))
    (home-page "https://github.com/lukeed/escalade")
    (synopsis
     "A tiny (183B to 210B) and fast utility to ascend parent directories")
    (description
     "A tiny (183B to 210B) and fast utility to ascend parent directories")
    (license license:expat)))

(define-public node-yargs-parser
  (package
    (name "node-yargs-parser")
    (version "20.2.9")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/yargs-parser/-/yargs-parser-20.2.9.tgz")
       (sha256
        (base32 "018imjlbdcnkc7j4s510h1vkzi8gyqkajfay88i3nmq4wvr4ch4r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "gts"
                                                  "chai"
                                                  "mocha"
                                                  "serve"
                                                  "eslint"
                                                  "rimraf"
                                                  "rollup"
                                                  "cross-env"
                                                  "puppeteer"
                                                  "standardx"
                                                  "typescript"
                                                  "@types/chai"
                                                  "@types/node"
                                                  "@types/mocha"
                                                  "eslint-plugin-node"
                                                  "eslint-plugin-import"
                                                  "rollup-plugin-cleanup"
                                                  "start-server-and-test"
                                                  "@typescript-eslint/parser"
                                                  "@wessberg/rollup-plugin-ts"
                                                  "ts-transform-default-export"
                                                  "@typescript-eslint/eslint-plugin")))))
          (add-after 'delete-dev-dependencies 'remove-scripts
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[[:space:]]*\\{[^}]*\\},?")
                 "\"scripts\": {},")
                (("\"postinstall\":[[:space:]]*\"[^\"]*\",?")
                 "")
                (("\"prepare\":[[:space:]]*\"[^\"]*\",?")
                 "")))))))
    (home-page "https://github.com/yargs/yargs-parser")
    (synopsis "the mighty option parser used by yargs")
    (description "the mighty option parser used by yargs")
    (license license:isc)))

(define-public node-get-caller-file
  (package
    (name "node-get-caller-file")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/get-caller-file/-/get-caller-file-2.0.5.tgz")
       (sha256
        (base32 "0pwk4r8iyyq2j0xpxavdm3ldb4h4k4s4xb74m8dlrzs9374f24vv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'delete-scripts
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (filter (lambda (field)
                           (not (equal? (car field) "scripts")))
                         pkg-meta)))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/chai"
                                                  "@types/ensure-posix-path"
                                                  "@types/mocha"
                                                  "@types/node"
                                                  "chai"
                                                  "ensure-posix-path"
                                                  "mocha"
                                                  "typescript"))))))))
    (home-page "https://github.com/stefanpenner/get-caller-file")
    (synopsis
     "[![Build Status](https://travis-ci.org/stefanpenner/get-caller-file.svg?branch=master)](https://travis-ci.org/stefanpenner/get-caller-file) [![Build status](https://ci.appveyor.com/api/projects/status/ol2q94g1932cy14a/branch/master?svg=true)](https://ci.a")
    (description
     "[![Build Status](https://travis-ci.org/stefanpenner/get-caller-file.svg?branch=master)](https://travis-ci.org/stefanpenner/get-caller-file) [![Build status](https://ci.appveyor.com/api/projects/status/ol2q94g1932cy14a/branch/master?svg=true)](https://ci.a")
    (license license:isc)))

(define-public node-require-directory
  (package
    (name "node-require-directory")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/require-directory/-/require-directory-2.1.1.tgz")
       (sha256
        (base32 "1j46ydacaai73mx5krskl0k78r32lnjx94l79bz860rn8h4fwfvh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jshint" "mocha"))))))))
    (home-page "https://github.com/troygoode/node-require-directory/")
    (synopsis
     "Recursively iterates over specified directory, require()'ing each file, and returning a nested hash structure containing those modules.")
    (description
     "Recursively iterates over specified directory, require()'ing each file, and returning a nested hash structure containing those modules.")
    (license license:expat)))

(define-public node-yargs
  (package
    (name "node-yargs")
    (version "16.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/yargs/-/yargs-16.2.0.tgz")
       (sha256
        (base32 "1xdbfvf4vfrj78vwvl494n09z6gxixmjx4n4mbmdlrd8ihags7a6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/yargs")))
                (mkdir-p lib)
                (copy-recursively "." lib) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "cpr"
                                                  "gts"
                                                  "chai"
                                                  "chalk"
                                                  "mocha"
                                                  "which"
                                                  "rimraf"
                                                  "rollup"
                                                  "hashish"
                                                  "coveralls"
                                                  "cross-env"
                                                  "standardx"
                                                  "typescript"
                                                  "@types/chai"
                                                  "@types/node"
                                                  "cross-spawn"
                                                  "@types/mocha"
                                                  "yargs-test-extends"
                                                  "rollup-plugin-cleanup"
                                                  "@wessberg/rollup-plugin-ts"))))))))
    (inputs (list node-require-directory
                  node-get-caller-file
                  node-yargs-parser
                  node-string-width
                  node-escalade
                  node-cliui
                  node-y18n))
    (home-page "https://yargs.js.org/")
    (synopsis "yargs the modern, pirate-themed, successor to optimist.")
    (description "yargs the modern, pirate-themed, successor to optimist.")
    (license license:expat)))

(define-public node-cli-highlight
  (package
    (name "node-cli-highlight")
    (version "2.1.11")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/cli-highlight/-/cli-highlight-2.1.11.tgz")
       (sha256
        (base32 "07sqbbapkdy0dhk6pwk6r8axbncbwd4m3k1xa94mmkcgpfifcy3v"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/cli-highlight")))
                (mkdir-p lib)
                (copy-recursively "." lib)
                (let ((bin (string-append out "/bin")))
                  (mkdir-p bin)
                  (symlink (string-append lib "/bin/highlight")
                           (string-append bin "/highlight"))) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@commitlint/cli"
                                                  "@commitlint/config-conventional"
                                                  "@eclass/semantic-release-surge"
                                                  "@sourcegraph/eslint-config"
                                                  "@sourcegraph/prettierrc"
                                                  "@types/jest"
                                                  "@types/mz"
                                                  "@types/node"
                                                  "@types/parse5"
                                                  "@types/parse5-htmlparser2-tree-adapter"
                                                  "@types/yargs"
                                                  "eslint"
                                                  "husky"
                                                  "jest"
                                                  "prettier"
                                                  "semantic-release"
                                                  "ts-jest"
                                                  "typedoc"
                                                  "typescript"))))))))
    (inputs (list node-yargs
                  node-parse5-htmlparser2-tree-adapter
                  node-parse5
                  node-mz
                  node-highlight-js
                  node-chalk))
    (home-page "https://github.com/felixfbecker/cli-highlight")
    (synopsis "Syntax highlighting in your terminal")
    (description "Syntax highlighting in your terminal")
    (license license:isc)))

(define-public node-ansi-regex
  (package
    (name "node-ansi-regex")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ansi-regex/-/ansi-regex-5.0.1.tgz")
       (sha256
        (base32 "1ng0r2k4mcz7b2bfr6g1dschnxm0vifaslsvv2smv06smb6ss3hf"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"))))))))
    (home-page "https://github.com/chalk/ansi-regex")
    (synopsis "Regular expression for matching ANSI escape codes")
    (description "Regular expression for matching ANSI escape codes")
    (license license:expat)))

(define-public node-strip-ansi
  (package
    (name "node-strip-ansi")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/strip-ansi/-/strip-ansi-6.0.1.tgz")
       (sha256
        (base32 "1jh81jj6cn1lli1c7m6xi0ynra9zdghb1g63v1nib7zlpz87bnwv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-ansi-regex))
    (home-page "https://github.com/chalk/strip-ansi")
    (synopsis "Strip ANSI escape codes from a string")
    (description "Strip ANSI escape codes from a string")
    (license license:expat)))

(define-public node-emoji-regex
  (package
    (name "node-emoji-regex")
    (version "8.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/emoji-regex/-/emoji-regex-8.0.0.tgz")
       (sha256
        (base32 "01xi3ikahnlj77h2gqs3jb7kmnxn1nsb9dmnpvpqw288zgxxkk5m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("mocha" "regexgen"
                                                  "@babel/cli"
                                                  "@babel/core"
                                                  "unicode-12.0.0"
                                                  "@babel/preset-env"
                                                  "@babel/plugin-proposal-unicode-property-regex"))))))))
    (home-page "https://mths.be/emoji-regex")
    (synopsis
     "A regular expression to match all Emoji-only symbols as per the Unicode Standard.")
    (description
     "A regular expression to match all Emoji-only symbols as per the Unicode Standard.")
    (license license:expat)))

(define-public node-is-fullwidth-code-point
  (package
    (name "node-is-fullwidth-code-point")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-3.0.0.tgz")
       (sha256
        (base32 "0jmw03rxmbwbrkx0a8wq05qsjhdrx9jn3vns88dhy1y6bnp5shbg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd-check" "xo"))))))))
    (home-page "https://github.com/sindresorhus/is-fullwidth-code-point")
    (synopsis
     "Check if the character represented by a given Unicode code point is fullwidth")
    (description
     "Check if the character represented by a given Unicode code point is fullwidth")
    (license license:expat)))

(define-public node-string-width
  (package
    (name "node-string-width")
    (version "4.2.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/string-width/-/string-width-4.2.3.tgz")
       (sha256
        (base32 "0d19spdisrqxd6311fc7z1yg34ww6rwh1zxdk6pnk03fnaqlzfxd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"))))))))
    (inputs (list node-is-fullwidth-code-point node-emoji-regex
                  node-strip-ansi))
    (home-page "https://github.com/sindresorhus/string-width")
    (synopsis
     "Get the visual width of a string - the number of columns required to display it")
    (description
     "Get the visual width of a string - the number of columns required to display it")
    (license license:expat)))

(define-public node-cli-table3
  (package
    (name "node-cli-table3")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/cli-table3/-/cli-table3-0.6.5.tgz")
       (sha256
        (base32 "10f7cik7yfcxjrvzxm39qv37wx633f27llcl9s6qixyzn46fv8x4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("cli-table" "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-prettier"
                                                  "jest"
                                                  "jest-runner-eslint"
                                                  "lerna-changelog"
                                                  "prettier"))))))))
    (inputs (list node-string-width))
    (home-page "https://github.com/cli-table/cli-table3")
    (synopsis
     "Pretty unicode tables for the command line. Based on the original cli-table.")
    (description
     "Pretty unicode tables for the command line. Based on the original cli-table.")
    (license license:expat)))

(define-public node-sindresorhus-is
  (package
    (name "node-sindresorhus-is")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@sindresorhus/is/-/is-4.6.0.tgz")
       (sha256
        (base32 "1xr4dqjza7wlh436rkm44ywd00skdjsld343rbrqc6vlh36fqw1r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'remove-scripts
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[[:space:]]*\\{[^}]*\\},?")
                 ""))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"
                                                  "rxjs"
                                                  "jsdom"
                                                  "tempy"
                                                  "del-cli"
                                                  "ts-node"
                                                  "typescript"
                                                  "@types/node"
                                                  "@types/jsdom"
                                                  "zen-observable"
                                                  "@types/zen-observable"
                                                  "@sindresorhus/tsconfig"
                                                  "@typescript-eslint/parser"
                                                  "eslint-config-xo-typescript"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://github.com/sindresorhus/is")
    (synopsis "Type check values")
    (description "Type check values")
    (license license:expat)))

(define-public node-char-regex
  (package
    (name "node-char-regex")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Richienb/char-regex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fzbwdgiafcpxqwhlb5gp3d8v5s9xg22mp3gdwwdgfk3a1dq05fb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"
                                                  "array-uniq"
                                                  "emoji.json"
                                                  "@babel/core"
                                                  "unicode-chars"
                                                  "eslint-config-richienb"
                                                  "@babel/plugin-proposal-unicode-property-regex"))))))))
    (home-page "https://github.com/Richienb/char-regex")
    (synopsis
     "A regex to match any full character, considering weird character ranges.")
    (description
     "A regex to match any full character, considering weird character ranges.")
    (license license:expat)))

(define-public node-emojilib
  (package
    (name "node-emojilib")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/emojilib/-/emojilib-2.4.0.tgz")
       (sha256
        (base32 "1gm63ygkkjfm2wxbsgzy80irhr2r8k09lgv91h9j31x2a8aw01dh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape" "tap-spec"
                                                  "JSONStream"
                                                  "format-json-stream"))))))))
    (home-page "https://github.com/muan/emojilib")
    (synopsis "Emoji keyword library.")
    (description "Emoji keyword library.")
    (license license:expat)))

(define-public node-unicode-emoji-modifier-base
  (package
    (name "node-unicode-emoji-modifier-base")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/unicode-emoji-modifier-base/-/unicode-emoji-modifier-base-1.0.0.tgz")
       (sha256
        (base32 "1rglbwhwhb53c51imzszvjzqk6p75hyy50ny2nsg4m0zz87qclxp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("codecov" "istanbul" "jsesc"
                                                  "lodash.range" "mocha"))))))))
    (home-page "https://github.com/mathiasbynens/unicode-emoji-modifier-base")
    (synopsis
     "The set of Unicode symbols that can serve as a base for emoji modifiers, i.e. those with the `Emoji_Modifier_Base` property set to `Yes`.")
    (description
     "The set of Unicode symbols that can serve as a base for emoji modifiers, i.e. those with the `Emoji_Modifier_Base` property set to `Yes`.")
    (license license:expat)))

(define-public node-skin-tone
  (package
    (name "node-skin-tone")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/skin-tone/-/skin-tone-2.0.0.tgz")
       (sha256
        (base32 "0zsf8y42swy8rs12yars2nm3savplzaj12hfc05ch1j2m6dgnibl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (inputs (list node-unicode-emoji-modifier-base))
    (home-page "https://github.com/sindresorhus/skin-tone")
    (synopsis "Change the skin tone of emoji characters")
    (description
     "This package provides functionality to modify the skin tone of emoji
characters using Unicode emoji modifier bases.")
    (license license:expat)))

(define-public node-node-emoji
  (package
    (name "node-node-emoji")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/omnidan/node-emoji")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ydahvbmydds8f6c9ab0pl5xw4a8rpv4sxsv52mp9habq30kxfvj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/node-emoji")))
                (mkdir-p lib)
                (copy-recursively "." lib)
                (for-each (lambda (dir)
                            (let ((path (string-append lib "/" dir)))
                              (when (file-exists? path)
                                (delete-file-recursively path))))
                          '(".github" ".husky" ".vscode" "src")) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@release-it/conventional-changelog"
                                                  "@swc/core"
                                                  "@types/eslint"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "@vitest/coverage-v8"
                                                  "console-fail-test"
                                                  "cspell"
                                                  "eslint"
                                                  "eslint-plugin-deprecation"
                                                  "eslint-plugin-eslint-comments"
                                                  "eslint-plugin-jsdoc"
                                                  "eslint-plugin-jsonc"
                                                  "eslint-plugin-markdown"
                                                  "eslint-plugin-n"
                                                  "eslint-plugin-no-only-tests"
                                                  "eslint-plugin-perfectionist"
                                                  "eslint-plugin-regexp"
                                                  "eslint-plugin-vitest"
                                                  "eslint-plugin-yml"
                                                  "husky"
                                                  "jsonc-eslint-parser"
                                                  "knip"
                                                  "lint-staged"
                                                  "markdownlint"
                                                  "markdownlint-cli"
                                                  "npm-package-json-lint"
                                                  "npm-package-json-lint-config-default"
                                                  "prettier"
                                                  "prettier-plugin-curly"
                                                  "prettier-plugin-packagejson"
                                                  "release-it"
                                                  "sentences-per-line"
                                                  "should-semantic-release"
                                                  "tsup"
                                                  "typescript"
                                                  "vitest"
                                                  "yaml-eslint-parser"))))))))
    (inputs (list node-skin-tone node-emojilib node-char-regex
                  node-sindresorhus-is))
    (home-page "https://github.com/omnidan/node-emoji")
    (synopsis "Friendly emoji lookups and parsing utilities for Node.js")
    (description "Friendly emoji lookups and parsing utilities for Node.js")
    (license license:expat)))

(define-public node-has-flag
  (package
    (name "node-has-flag")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/has-flag/-/has-flag-4.0.0.tgz")
       (sha256
        (base32 "1cdmvliwz8h02nwg0ipli0ydd1l82sz9s1m7bj5bn9yr24afp9vp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "tsd" "xo"))))))))
    (home-page "https://github.com/sindresorhus/has-flag")
    (synopsis "Check if argv has a specific flag")
    (description "Check if argv has a specific flag")
    (license license:expat)))

(define-public node-supports-color
  (package
    (name "node-supports-color")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/supports-color/-/supports-color-7.2.0.tgz")
       (sha256
        (base32 "0jjyglzdzscmhgidn43zc218q5jf9h03hmaaq9h4wqil2vywlspi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "import-fresh"))))))))
    (inputs (list node-has-flag))
    (home-page "https://github.com/chalk/supports-color")
    (synopsis "Detect whether a terminal supports color")
    (description "Detect whether a terminal supports color")
    (license license:expat)))

(define-public node-supports-hyperlinks
  (package
    (name "node-supports-hyperlinks")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/supports-hyperlinks/-/supports-hyperlinks-3.2.0.tgz")
       (sha256
        (base32 "1nqyhzi7w4q59mzhmxw17ry27qbi3v8cpsfvj1hg9pgcsckn4rz5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "codecov"
                                                  "typescript"
                                                  "@tsconfig/node14"
                                                  "@types/supports-color"))))))))
    (inputs (list node-supports-color node-has-flag))
    (home-page "https://github.com/chalk/supports-hyperlinks")
    (synopsis "Detect whether a terminal supports hyperlinks")
    (description "Detect whether a terminal supports hyperlinks")
    (license license:expat)))

(define-public node-marked-terminal
  (package
    (name "node-marked-terminal")
    (version "7.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/marked-terminal/-/marked-terminal-7.3.0.tgz")
       (sha256
        (base32 "101pa6v4587as0xa02z2p3zs29j71x0pgjzvkzxn55fmw6j62f8x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out
                                         "/lib/node_modules/marked-terminal")))
                (mkdir-p lib)
                (copy-recursively "." lib) #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@rollup/plugin-commonjs"
                                                  "@rollup/plugin-node-resolve"
                                                  "cross-env" "marked" "mocha"
                                                  "rollup"))))))))
    (inputs (list node-supports-hyperlinks
                  node-node-emoji
                  node-cli-table3
                  node-cli-highlight
                  node-chalk
                  node-ansi-regex
                  node-ansi-escapes
                  node-marked))
    (home-page "https://github.com/mikaelbr/marked-terminal")
    (synopsis "A custom render for marked to output to the Terminal")
    (description "A custom render for marked to output to the Terminal")
    (license license:expat)))

(define-public node-agent-base
  (package
    (name "node-agent-base")
    (version "7.1.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/agent-base/-/agent-base-7.1.3.tgz")
       (sha256
        (base32 "0r2c4v0gxv4pvzjz1m97z4ffipdacqsqgmpx99ghxy65cqf8njcr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/debug" "@types/jest"
                                                  "@types/node"
                                                  "@types/semver"
                                                  "@types/ws"
                                                  "async-listen"
                                                  "jest"
                                                  "ts-jest"
                                                  "typescript"
                                                  "ws"
                                                  "tsconfig"))))))))
    (home-page "https://github.com/TooTallNate/proxy-agents")
    (synopsis "Turn a function into an `http.Agent` instance")
    (description "Turn a function into an `http.Agent` instance")
    (license license:expat)))

(define-public node-debug
  (package
    (name "node-debug")
    (version "4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/debug/-/debug-4.4.1.tgz")
       (sha256
        (base32 "1knsscg6xbni9rj66a0p27x1zxsaixvwljgq930vk2ahy33afp16"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("brfs" "browserify"
                                                  "coveralls"
                                                  "karma"
                                                  "karma-browserify"
                                                  "karma-chrome-launcher"
                                                  "karma-mocha"
                                                  "mocha"
                                                  "mocha-lcov-reporter"
                                                  "sinon"
                                                  "xo"))))))))
    (inputs (list node-ms))
    (home-page "https://github.com/debug-js/debug")
    (synopsis "Lightweight debugging utility for Node.js and the browser")
    (description "Lightweight debugging utility for Node.js and the browser")
    (license license:expat)))

(define-public node-https-proxy-agent
  (package
    (name "node-https-proxy-agent")
    (version "7.0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/https-proxy-agent/-/https-proxy-agent-7.0.6.tgz")
       (sha256
        (base32 "1q603cjw6z348j2i7k7s5zb3kp91hi9lml298bv84214wpl8j3wn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/async-retry"
                                                  "@types/debug"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "async-listen"
                                                  "async-retry"
                                                  "jest"
                                                  "ts-jest"
                                                  "typescript"
                                                  "proxy"
                                                  "tsconfig"))))))))
    (inputs (list node-debug node-agent-base))
    (home-page "https://github.com/TooTallNate/proxy-agents")
    (synopsis "An HTTP(s) proxy `http.Agent` implementation for HTTPS")
    (description "An HTTP(s) proxy `http.Agent` implementation for HTTPS")
    (license license:expat)))

(define-public node-package-manager-detector
  (package
    (name "node-package-manager-detector")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/package-manager-detector/-/package-manager-detector-1.3.0.tgz")
       (sha256
        (base32 "0d39zvjvgy697y42y6y7qbr4dlvr17dbxha8hkr9zdf3aq1g38kh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@antfu/eslint-config"
                                                  "@types/fs-extra"
                                                  "@types/node"
                                                  "bumpp"
                                                  "eslint"
                                                  "fs-extra"
                                                  "typescript"
                                                  "unbuild"
                                                  "vitest"
                                                  "vitest-package-exports"))))))))
    (home-page "https://github.com/antfu-collective/package-manager-detector")
    (synopsis "Package manager detector")
    (description "Package manager detector")
    (license license:expat)))

(define-public node-openai-codex
  (package
    (name "node-openai-codex")
    (version "0.133.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://registry.npmjs.org/@openai/codex/-/codex-"
             version ".tgz"))
       (sha256
        (base32 "0fl9adv1s2fz5wqk1clp87jky787rwfhbspy4by2sjwzy8qsda9n"))))
    (build-system node-build-system)
    (native-inputs (list `("platform-source" ,(origin
                                                (method url-fetch)
                                                (uri (let ((system (or (%current-target-system)
                                                                       (%current-system))))
                                                       (cond
                                                         ((string=? system
                                                           "x86_64-linux")
                                                          (string-append
                                                           "https://registry.npmjs.org/@openai/codex/-/codex-"
                                                           version
                                                           "-linux-x64.tgz"))
                                                         ((string=? system
                                                           "aarch64-linux")
                                                          (string-append
                                                           "https://registry.npmjs.org/@openai/codex/-/codex-"
                                                           version
                                                           "-linux-arm64.tgz"))
                                                         (else (error
                                                                "unsupported system for node-openai-codex"
                                                                system)))))
                                                (sha256 (base32 (let ((system (or
                                                                               (%current-target-system)
                                                                               (%current-system))))
                                                                  (cond
                                                                    ((string=?
                                                                      system
                                                                      "x86_64-linux")
                                                                     "078rwgp9j7mb02mj63y3cpxnaqwyilviazxizz04nnv5nzz1k199")
                                                                    ((string=?
                                                                      system
                                                                      "aarch64-linux")
                                                                     "1v8nii56x0dw06g9wmq9icrnzjhhd65wp24jagv8v4zkv07zpsm1")
                                                                    (else (error
                                                                           "unsupported system for node-openai-codex"
                                                                           system))))))))))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'add-platform-vendor
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "tar" "-xf"
                      (assoc-ref inputs "platform-source")
                      "--strip-components=1" "package/vendor") #t))
          (add-after 'add-platform-vendor 'remove-precompiled-binaries
            (lambda _
              ;; Avoid bundled network-fetching ripgrep.
              (when (file-exists? "bin/rg")
                (delete-file "bin/rg"))
              (for-each delete-file
                        (find-files "vendor" "/path/rg$")) #t)))))
    (propagated-inputs (list node ripgrep))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://github.com/openai/codex")
    (synopsis
     "Codex CLI is a coding agent from OpenAI that runs locally on your computer.")
    (description
     "Codex CLI is a coding agent from OpenAI that runs locally on your computer.")
    (license license:asl2.0)))

(define-public node-pyright
  (package
    (name "node-pyright")
    (version "1.1.403")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pyright/-/pyright-1.1.403.tgz")
       (sha256
        (base32 "17gcgvaa7fydhddf54s5f45qwai3phc5cny2ryw6fpqazzy3cayn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node"
                                                  "copy-webpack-plugin"
                                                  "esbuild-loader"
                                                  "shx"
                                                  "ts-loader"
                                                  "typescript"
                                                  "webpack"
                                                  "webpack-cli"))))))))
    (home-page "https://github.com/Microsoft/pyright")
    (synopsis "Type checker for the Python language")
    (description "Type checker for the Python language")
    (license license:expat)))

(define-public node-anthropic-ai-claude-code
  (package
    (name "node-anthropic-ai-claude-code")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-"
             version ".tgz"))
       (sha256
        (base32 "1qsh2sa95ykqpgrccj9z5pw179v5q5nz4sq5182p7pcx868x2fbc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'remove-precompiled-binaries
            (lambda _
              (delete-file-recursively "vendor/ripgrep")
              (delete-file-recursively "vendor/claude-code-jetbrains-plugin")
              #t)))))
    (propagated-inputs (list node))
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis
     "Use Claude, Anthropic's AI assistant, right from your terminal. Claude can understand your codebase, edit files, run terminal commands, and handle entire workflows for you.")
    (description
     "Use Claude, Anthropic's AI assistant, right from your terminal. Claude can understand your codebase, edit files, run terminal commands, and handle entire workflows for you.")
    (license #f)))
(define-public node-zod-4.4.3
  (package
    (name "node-zod")
    (version "4.4.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/zod/-/zod-4.4.3.tgz")
       (sha256
        (base32 "17171zbchqs56621d99kxgs2cg215yp879450rhh1m9zadzz2f7f"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://zod.dev")
    (synopsis
     "TypeScript-first schema declaration and validation library with static type inference")
    (description
     "TypeScript-first schema declaration and validation library with static type inference")
    (license license:expat)))

(define-public node-zod-to-json-schema-3.25.2
  (package
    (name "node-zod-to-json-schema")
    (version "3.25.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/zod-to-json-schema/-/zod-to-json-schema-3.25.2.tgz")
       (sha256
        (base32 "1x4kiprzcpvwmzyw9i1h1m4lqjyxy23z3j8rq7b4zzralfsrm4d4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/json-schema"
                                                  "@types/node"
                                                  "ajv"
                                                  "ajv-errors"
                                                  "ajv-formats"
                                                  "fast-diff"
                                                  "local-ref-resolver"
                                                  "rimraf"
                                                  "tsx"
                                                  "typescript"
                                                  "zod"))))))))
    (home-page "https://github.com/StefanTerdell/zod-to-json-schema#readme")
    (synopsis "Converts Zod schemas to Json Schemas")
    (description "Converts Zod schemas to Json Schemas")
    (license license:isc)))
