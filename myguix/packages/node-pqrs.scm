(define-module (myguix packages node-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages node)
  #:use-module (guix build-system node)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/ink")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/formdata-node")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
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
    (version "2.6.11") ;Use a proper npm version
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
              ;; Skip npm install for TypeScript definitions
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/openai")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
                (copy-recursively "." lib)
                ;; Make the CLI executable
                (let ((bin (string-append out "/bin")))
                  (mkdir-p bin)
                  (symlink (string-append lib "/bin/cli")
                           (string-append bin "/openai")))
                #t)))
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
          (delete 'build)
          (add-after 'unpack 'remove-scripts
            (lambda _
              ;; Remove the scripts section from package.json
              (substitute* "package.json"
                (("\"scripts\":[[:space:]]*\\{[^}]*\\},?")
                 "")
                (("\"postinstall\":[[:space:]]*\"[^\"]*\",?")
                 ""))))
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/router")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
                (copy-recursively "." lib)
                ;; Remove test files to save space
                (delete-file-recursively (string-append lib "/test"))
                #t)))
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/express")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
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
              ;; Remove all scripts to prevent postinstall issues
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
              ;; Remove all scripts to prevent postinstall issues
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
              ;; Remove all scripts to prevent postinstall issues
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
       (uri "https://registry.npmjs.org/node-gyp-build/-/node-gyp-build")
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/ink")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/@inkjs/ui")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
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
              ;; Remove all scripts to prevent postinstall issues
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
              ;; Remove all scripts to prevent postinstall issues
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/yargs")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/cli-highlight")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
                (copy-recursively "." lib)
                ;; Create CLI executable
                (let ((bin (string-append out "/bin")))
                  (mkdir-p bin)
                  (symlink (string-append lib "/bin/highlight")
                           (string-append bin "/highlight")))
                #t)))
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
              ;; Remove all scripts to prevent postinstall issues
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/node-emoji")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
                (copy-recursively "." lib)
                ;; Remove unnecessary files
                (for-each (lambda (dir)
                            (let ((path (string-append lib "/" dir)))
                              (when (file-exists? path)
                                (delete-file-recursively path))))
                          '(".github" ".husky" ".vscode" "src"))
                #t)))
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
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out
                                         "/lib/node_modules/marked-terminal")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
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
    (version "0.1.2505291658")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@openai/codex/-/codex-0.1.2505291658.tgz")
       (sha256
        (base32 "0sm8rhifpafdq937b40dljqdw0cnpihg631kk43nhpc15qr12sh6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'configure
            (lambda _
              ;; Skip npm install since dependencies are handled by Guix
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules/@openai/codex")))
                ;; Create the target directory
                (mkdir-p lib)
                ;; Copy all files to the target directory
                (copy-recursively "." lib)
                ;; Remove the use-native file to force JavaScript implementation
                (delete-file (string-append lib "/bin/use-native"))
                ;; Remove all native binaries since we're using JS implementation
                (with-directory-excursion (string-append lib "/bin")
                  (for-each delete-file
                            (find-files "."
                                        (lambda (file stat)
                                          (and (not (string-suffix? ".js" file))
                                               (string-prefix? "codex-"
                                                               (basename file)))))))
                ;; Create CLI executable
                (let ((bin (string-append out "/bin")))
                  (mkdir-p bin)
                  (symlink (string-append lib "/bin/codex.js")
                           (string-append bin "/codex")))
                #t)))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("vite" "boxen"
                                                  "husky"
                                                  "which"
                                                  "semver"
                                                  "vitest"
                                                  "esbuild"
                                                  "ts-node"
                                                  "prettier"
                                                  "punycode"
                                                  "@eslint/js"
                                                  "typescript"
                                                  "whatwg-url"
                                                  "@types/diff"
                                                  "@types/react"
                                                  "@types/which"
                                                  "@types/semver"
                                                  "@types/express"
                                                  "@types/js-yaml"
                                                  "@types/shell-quote"
                                                  "eslint-plugin-react"
                                                  "ink-testing-library"
                                                  "eslint-plugin-import"
                                                  "@types/marked-terminal"
                                                  "@typescript-eslint/parser"
                                                  "eslint-plugin-react-hooks"
                                                  "eslint-plugin-react-refresh"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-package-manager-detector
                  node-https-proxy-agent
                  node-marked-terminal
                  node-fast-deep-equal
                  node-fast-npm-meta
                  node-use-interval
                  node-shell-quote
                  node-to-rotated
                  node-strip-ansi
                  node-file-type
                  node-inkjs-ui
                  node-js-yaml
                  node-figures
                  node-express
                  node-openai
                  node-marked
                  node-dotenv
                  node-react
                  node-chalk
                  node-open
                  node-meow
                  node-diff
                  node-zod
                  node-ink))
    (home-page "https://github.com/openai/codex")
    (synopsis
     "OpenAI Codex CLI - Lightweight coding agent that runs in your terminal")
    (description
     "OpenAI Codex CLI - Lightweight coding agent that runs in your terminal")
    (license license:asl2.0)))

(define-public node-pyright
  (package
    (name "node-pyright")
    (version "1.1.401")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pyright/-/pyright-1.1.401.tgz")
       (sha256
        (base32 "03rxk89ql404ks5sd4r12ihy2px15cshhk3gv8hc3yb32xsfwjii"))))
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
    (version "1.0.43")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-"
             version ".tgz"))
       (sha256
        (base32 "05br5ziwcqych07g6n5df91ncc4mbq5y170p9bfwids46affxidp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'remove-precompiled-binaries
            (lambda _
              ;; Remove all precompiled binaries
              (delete-file-recursively "vendor/ripgrep")
              ;; Also remove other vendor binaries if not needed
              (delete-file-recursively "vendor/claude-code-jetbrains-plugin")
              #t)))))
    (propagated-inputs (list node)) ;Add node as a propagated input
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis
     "Use Claude, Anthropic's AI assistant, right from your terminal. Claude can understand your codebase, edit files, run terminal commands, and handle entire workflows for you.")
    (description
     "Use Claude, Anthropic's AI assistant, right from your terminal. Claude can understand your codebase, edit files, run terminal commands, and handle entire workflows for you.")
    (license #f)))
