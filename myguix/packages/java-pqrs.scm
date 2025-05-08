(define-module (myguix packages java-pqrs)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:hide (openssl))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages java)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages tls))

(define-public cypher-shell
  (package
    (name "cypher-shell")
    (version "5.26.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dist.neo4j.org/cypher-shell/cypher-shell-"
             version ".zip"))
       (sha256
        (base32 "1pf9m9fs0b0z1cdix3ggzhs5hy3lbxbxxjnmyhvpbyssq1n4n4xv"))))
    (build-system trivial-build-system)
    (inputs (list openjdk21 unzip bash-minimal))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((unzip (search-input-file %build-inputs "/bin/unzip"))
                 (java (search-input-file %build-inputs "/bin/java"))
                 (jhome (dirname (dirname java))) ;…/openjdk21
                 (bash (search-input-file %build-inputs "/bin/bash"))
                 (out #$output)
                 (srcdir (string-append "tmp/cypher-shell-"
                                        #$version)))
            ;; 1. unpack
            (invoke unzip
                    #$source "-d" "tmp")
            ;; 2. install expected layout
            (mkdir-p (string-append out "/bin"))
            (mkdir-p (string-append out "/lib"))
            (copy-recursively (string-append srcdir "/bin")
                              (string-append out "/bin"))
            (copy-recursively (string-append srcdir "/lib")
                              (string-append out "/lib"))
            ;; 3. rename upstream script → .real
            (let* ((orig (string-append out "/bin/cypher-shell"))
                   (real (string-append orig ".real")))
              (rename-file orig real)
              ;; 4. write quiet wrapper
              (call-with-output-file orig
                (lambda (p)
                  (format p
                   "#!~a~%
export JAVA_HOME=\"~a\"~%
export JAVA_OPTS=\"--enable-native-access=ALL-UNNAMED\"~%
export LD_LIBRARY_PATH=\"${JAVA_HOME}/lib:${JAVA_HOME}/lib/server${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH\"~%
exec \"~a\" \"$@\"~%"
                   bash jhome real)))
              (chmod orig #o755))) #t)))
    (home-page "https://neo4j.com")
    (synopsis "Command-line Cypher shell for Neo4j")
    (description
     "Cypher Shell lets you execute Cypher statements and administrative
commands against a Neo4j graph database over the Bolt protocol.")
    (license gpl3+)))

(define-public apache-arrow-flight
  (let* ( ;Patched gRPC that guards against the duplicate “re2::re2”
         

         ;; target.
         (grpc-re2fix (package
                        (inherit grpc)
                        (name "grpc-re2fix")
                        (arguments
                         (substitute-keyword-arguments (package-arguments grpc)
                           ((#:phases phases)
                            #~(modify-phases #$phases
                                (add-after 'unpack 'patch-re2-target
                                  (lambda _
                                    ;; Avoid “target already exists” error
                                    (substitute* "cmake/modules/Findre2.cmake"
                                      (("add_library\\(re2::re2 INTERFACE IMPORTED\\)")
                                       "if(NOT TARGET re2::re2)
  add_library(re2::re2 INTERFACE IMPORTED)
endif()"))))))))))
         (libcares c-ares/cmake))
    (package
      (inherit apache-arrow)
      (name "apache-arrow-flight")
      (inputs (modify-inputs (package-inputs apache-arrow)
                (replace "grpc" grpc-re2fix)
                (append libcares ;provides “c-ares::cares” target
                        openssl abseil-cpp re2)))
      (arguments
       (substitute-keyword-arguments (package-arguments apache-arrow)
         ((#:configure-flags orig
           #~'())
          #~(let* ((plugin (search-input-file %build-inputs
                                              "/bin/grpc_cpp_plugin"))
                   (grpc-dir (dirname (dirname (search-input-file
                                                %build-inputs
                                                "/lib/libgrpc.so")))))
              (append (list "-DARROW_FLIGHT=ON" "-DPYARROW_WITH_FLIGHT=ON"
                            "-DARROW_GRPC=ON"
                            (string-append "-DgRPC_DIR=" grpc-dir)
                            (string-append "-DARROW_GRPC_CPP_PLUGIN=" plugin))
                      #$orig))))))))
