(define-module (myguix packages java-pqrs)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (gnu packages java)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bash))

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
