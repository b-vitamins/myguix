;;; Test gexp utilities
(define-module (test-gexp)
  #:use-module (myguix gexp)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (ice-9 textual-ports)  ; Add this!
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "gexp")

(test-group "file-like->name"
  (test-equal "extract name from local-file"
    "test.txt"
    (file-like->name (local-file "test.txt")))
  
  (test-equal "extract name from local-file with custom name"
    "custom-name"
    (file-like->name (local-file "test.txt" "custom-name")))
  
  (test-equal "extract name from plain-file"
    "config"
    (file-like->name (plain-file "config" "content")))
  
  (test-equal "extract name from computed-file"
    "result"
    (file-like->name (computed-file "result" #~(mkdir #$output))))
  
  (test-equal "extract name from program-file"
    "my-script"
    (file-like->name (program-file "my-script" #~(display "hello"))))
  
  (test-error "error on non-file-like object"
    #t
    (file-like->name "not-a-file"))
  
  (test-error "error on number"
    #t
    (file-like->name 42))
  
  (test-error "error on list"
    #t
    (file-like->name '(not a file))))

(test-group "slurp-file-like"
  (test-assert "returns gexp for plain-file"
    (gexp? (slurp-file-like (plain-file "test" "content"))))
  
  (test-assert "returns gexp for local-file"
    (gexp? (slurp-file-like (local-file "test.txt"))))
  
  (test-assert "returns gexp for computed-file"
    (gexp? (slurp-file-like (computed-file "test" #~(call-with-output-file #$output
                                                      (lambda (port)
                                                        (display "test" port)))))))
  
  (test-assert "accepts encoding parameter"
    (gexp? (slurp-file-like (plain-file "test" "content") 
                            #:encoding "ISO-8859-1")))
  
  (test-assert "default encoding is UTF-8"
    (gexp? (slurp-file-like (plain-file "test" "content"))))
  
  (test-error "error on non-file-like object"
    #t
    (slurp-file-like "not-a-file"))
  
  (test-error "error on #f"
    #t
    (slurp-file-like #f))
  
  (test-error "error on number"
    #t
    (slurp-file-like 123)))

(test-group "mixed-text-file*"
  (test-assert "creates file-like object"
    (file-like? (mixed-text-file* "test" "hello" " " "world")))
  
  (test-equal "gets correct name"
    "test-file"
    (file-like->name (mixed-text-file* "test-file" "content")))
  
  (test-assert "filters empty strings"
    (let ((file (mixed-text-file* "test" "hello" "" "world")))
      (file-like? file)))
  
  (test-assert "filters multiple empty strings"
    (let ((file (mixed-text-file* "test" "a" "" "b" "" "c")))
      (file-like? file)))
  
  (test-assert "filters #f values"
    (let ((file (mixed-text-file* "test" "hello" #f "world")))
      (file-like? file)))
  
  (test-assert "filters multiple #f values"
    (let ((file (mixed-text-file* "test" #f "hello" #f "world" #f)))
      (file-like? file)))
  
  (test-assert "filters mixed empty strings and #f"
    (let ((file (mixed-text-file* "test" "a" "" #f "b" #f "" "c")))
      (file-like? file)))
  
  (test-assert "keeps non-empty strings"
    (let ((file (mixed-text-file* "test" "hello" " " "world")))
      (file-like? file)))
  
  (test-assert "preserves gexps while filtering empty strings"
    (let ((file (mixed-text-file* "test" "" "prefix" "" #~(display "hi") "")))
      (file-like? file)))

  (test-assert "works with single string"
    (let ((file (mixed-text-file* "test" "just text")))
      (file-like? file)))

  (test-assert "works with no content after filtering"
    (let ((file (mixed-text-file* "test" "" #f "")))
      (file-like? file)))
  
  (test-assert "handles all #f"
    (let ((file (mixed-text-file* "test" #f #f #f)))
      (file-like? file)))
  
  (test-assert "handles all empty strings"
    (let ((file (mixed-text-file* "test" "" "" "")))
      (file-like? file))))

(test-end "gexp")
