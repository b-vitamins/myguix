(define-module (myguix packages perl-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages web)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (guix build-system perl)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public perl-ntlm
  (package
    (name "perl-ntlm")
    (version "1.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NB/NBEBOUT/NTLM-"
                           version ".tar.gz"))
       (sha256
        (base32 "118z2n9qhjalfj7l9hhpjppyxdg2c34h4hsqdriibg3nv86f68y8"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-digest-hmac))
    (home-page "https://metacpan.org/release/NTLM")
    (synopsis "An NTLM authentication module")
    (description
     "This module provides methods to use NTLM authentication.  It can
be used as an authenticate method with the @code{Mail::IMAPClient} module
to perform the challenge/response mechanism for NTLM connections
or it can be used on its own for NTLM authentication with other
protocols (eg. HTTP).
 
The implementation is a direct port of the code from F<fetchmail>
which, itself, has based its NTLM implementation on F<samba>.  As
such, this code is not especially efficient, however it will still
take a fraction of a second to negotiate a login on a PII which is
likely to be good enough for most situations.")
    (license license:artistic2.0)))

(define-public perl-test-http-server-simple
  (package
    (name "perl-test-http-server-simple")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AL/ALEXMV/Test-HTTP-Server-Simple-"
             version ".tar.gz"))
       (sha256
        (base32 "0y5vjslgf29wzgi1r6zq4j7j41w8lhnh6xvjn68m507b9nypxjc5"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-http-server-simple))
    (home-page "https://metacpan.org/release/Test-HTTP-Server-Simple")
    (synopsis "Test::More functions for HTTP::Server::Simple")
    (description
     "This mixin class provides methods to test an @code{HTTP::Server::Simple}-based web server. Currently, it provides only one such method: @code{started_ok}.

@code{started_ok} takes an optional test description. The server needs to have been configured (specifically, its port needs to have been set), but it should not have been run or backgrounded. @code{started_ok} calls background on the server, which forks it to run in the background. @code{Test::HTTP::Server::Simple} takes care of killing the server when your test script dies, even if you kill your test script with an interrupt. @code{started_ok} returns the URL @code{http://localhost:$port} which you can use to connect to your server.

Note that if the child process dies, or never gets around to listening for connections, this just hangs. (This may be fixed in a future version.)

Also, it probably won't work if you use a custom @code{Net::Server} in your server.")
    (license license:artistic2.0)))

(define-public perl-net-https-nb
  (package
    (name "perl-net-https-nb")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OL/OLEG/Net-HTTPS-NB-"
                           version ".tar.gz"))
       (sha256
        (base32 "0kwc4z8pqnbc396wjnlgdmri10zdh91f2bi6saxkpfjzlm7wysba"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-io-socket-ssl perl-net-http))
    (home-page "https://metacpan.org/release/Net-HTTPS-NB")
    (synopsis "Non-blocking HTTPS client")
    (description
     "Same interface as @code{Net::HTTPS} but it will never try multiple reads when the @code{read_response_headers()} or @code{read_entity_body()} methods are invoked. In addition allows non-blocking connect.

If @code{read_response_headers()} did not see enough data to complete the headers an empty list is returned.
If @code{read_entity_body()} did not see new entity data in its read the value -1 is returned.")
    (license license:artistic2.0)))

(define-public perl-libwww-perl
  (package
    (name "perl-libwww-perl")
    (version "6.77")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/O/OA/OALDERS/libwww-perl-" version
             ".tar.gz"))
       (sha256
        (base32 "1r30y2jn0f20zndlh02c5ca00m8zld7zvvrxyip9d3gangb0gacl"))))
    (build-system perl-build-system)
    (native-inputs (list perl-http-cookiejar
                         perl-http-daemon
                         perl-test-fatal
                         perl-test-leaktrace
                         perl-test-needs
                         perl-test-requiresinternet))
    (propagated-inputs (list perl-data-dump
                             perl-encode-locale
                             perl-file-listing
                             perl-html-parser
                             perl-http-cookies
                             perl-http-date
                             perl-http-message
                             perl-http-negotiate
                             perl-lwp-mediatypes
                             perl-lwp-protocol-https
                             perl-net-http
                             perl-ntlm
                             perl-try-tiny
                             perl-uri
                             perl-www-robotrules))
    (home-page "https://metacpan.org/release/libwww-perl")
    (synopsis "The World-Wide Web library for Perl")
    (description
     "The @code{libwww-perl} collection is a set of Perl modules that provides a simple, consistent application programming interface to the World-Wide Web. The main focus of the library is providing classes and functions allowing to write WWW clients. It also contains modules that are of more general use and even classes to help implement simple HTTP servers.")
    (license license:artistic2.0)))

(define-public perl-http-async
  (package
    (name "perl-http-async")
    (version "0.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KA/KAORU/HTTP-Async-"
                           version ".tar.gz"))
       (sha256
        (base32 "1m4n256jmmsw2scfqc144f9ra2lx7c0h5az6vlvcryj54rba3nsy"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-http-message
                             perl-http-server-simple
                             perl-libwww-perl
                             perl-net-http
                             perl-net-https-nb
                             perl-test-fatal
                             perl-test-http-server-simple
                             perl-test-tcp
                             perl-uri))
    (home-page "https://metacpan.org/release/HTTP-Async")
    (synopsis "process multiple HTTP requests in parallel without blocking.")
    (description
     "Although using the conventional @code{LWP::UserAgent} is fast and easy it does have some drawbacks - the code execution blocks until the request has been completed and it is only possible to process one request at a time. @code{HTTP::Async} attempts to address these limitations.

It gives you a 'Async' object that you can add requests to, and then get the requests off as they finish. The actual sending and receiving of the requests is abstracted. As soon as you add a request it is transmitted, if there are too many requests in progress at the moment they are queued. There is no concept of starting or stopping - it runs continuously.

Whilst it is waiting to receive data it returns control to the code that called it meaning that you can carry out processing whilst fetching data from the network. All without forking or threading - it is actually done using select lists.")
    (license license:artistic2.0)))

(define-public perl-anyevent-http
  (package
    (name "perl-anyevent-http")
    (version "2.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/ML/MLEHMANN/AnyEvent-HTTP-" version
             ".tar.gz"))
       (sha256
        (base32 "04shwa3gw16di4wnhn0jymfjv6m7ikm00ayk9ipny5r4c50m7yjw"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-anyevent perl-common-sense perl-uri))
    (home-page "https://metacpan.org/release/AnyEvent-HTTP")
    (synopsis "simple but non-blocking HTTP/HTTPS client")
    (description
     "This module is an @code{AnyEvent} user, you need to make sure that you use and run a supported event loop.

This module implements a simple, stateless and non-blocking HTTP client. It supports GET, POST and other request methods, cookies and more, all on a very low level. It can follow redirects, supports proxies, and automatically limits the number of connections to the values specified in the RFC.

It should generally be a good client that is enough for most HTTP tasks. Simple tasks should be simple, but complex tasks should still be possible as the user retains control over request and response headers.

The caller is responsible for authentication management, cookies (if the simplistic implementation in this module doesn't suffice), referer and other high-level protocol details for which this module offers only limited support.")
    (license license:artistic2.0)))
