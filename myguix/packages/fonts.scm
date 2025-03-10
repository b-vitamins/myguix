;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Korytov Pavel <thexcloud@gmail.com>
;;; Copyright © 2021 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2023 Krzysztof Baranowski <pharcosyle@gmail.com>

(define-module (myguix packages fonts)
  #:use-module (ice-9 string-fun)
  #:use-module (gnu packages compression)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system font)
  #:use-module (guix build-system copy)
  #:use-module (myguix licenses))

(define mscorefont
  (lambda* (file #:key version font-name hash)
    (package
      ;; Downcase and replace " " with - to get "font-microsoft-times-new-roman"
      ;; from "Times New Roman"
      (name (string-append "font-microsoft-"
                           (string-replace-substring (string-downcase
                                                      font-name) " " "-")))
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://deac-ams.dl.sourceforge.net/project/corefonts/the%20fonts/final/"
               file ".exe"))
         (sha256
          (base32 hash))))
      (build-system font-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    ;; Unpack EXE and replace .TTF with .ttf
                    (replace 'unpack
                      (lambda* (#:key inputs #:allow-other-keys)
                        (use-modules (ice-9 string-fun))
                        (let ((source (assoc-ref inputs "source")))
                          (system (format #f "7z e ~a -ofont" source))
                          (map (lambda (oldname)
                                 (rename-file oldname
                                              (string-replace-substring
                                               oldname "TTF" "ttf")))
                               (find-files "./font" "\\.TTF$"))))))))
      (native-inputs `(("p7zip" ,p7zip)))
      (home-page "http://corefonts.sourceforge.net/")
      (synopsis (string-append font-name " font"))
      (description (string-append "This package provides " font-name
                    "
font from the pack of Microsoft core fonts for the web."))
      (license (nonfree "http://corefonts.sourceforge.net/eula.htm")))))

(define-public font-microsoft-andale-mono
  (mscorefont "andale32"
              #:version "2.00"
              #:font-name "Andale Mono"
              #:hash "0w7927hlwayqf3vvanf8f3qp2g1i404jzqvhp1z3mp0sjm1gw905"))

(define-public font-microsoft-arial
  (mscorefont "arial32"
              #:version "2.82"
              #:font-name "Arial"
              #:hash "1xkqyivbyb3z9dcalzidf8m4npzfpls2g0kldyn8g73f2i6plac5"))

(define-public font-microsoft-arial-black
  (mscorefont "arialb32"
              #:version "2.35"
              #:font-name "Arial Black"
              #:hash "1a60zqrg63kjnykh5hz7dbpzvx7lyivn3vbrp7jyv9d1nvzz09d4"))

(define-public font-microsoft-comic-sans-ms
  (mscorefont "comic32"
              #:version "2.10"
              #:font-name "Comic Sans MS"
              #:hash "0ki0rljjc1pxkbsxg515fwx15yc95bdyaksa3pjd89nyxzzg6vcw"))

(define-public font-microsoft-courier-new
  (mscorefont "courie32"
              #:version "2.82"
              #:font-name "Courier New"
              #:hash "111k3waxki9yyxpjwl2qrdkswvsd2dmvhbjmmrwyipam2s31sldv"))

(define-public font-microsoft-couirer-new
  (deprecated-package "font-microsoft-couirer-new" font-microsoft-courier-new))

(define-public font-microsoft-georgia
  (mscorefont "georgi32"
              #:version "2.05"
              #:font-name "Georgia"
              #:hash "0083jcpd837j2c06kp1q8glfjn9k7z6vg3wi137savk0lv6psb1c"))

(define-public font-microsoft-impact
  (mscorefont "impact32"
              #:version "2.35"
              #:font-name "Impact"
              #:hash "1yyc5z7zmm3s418hmrkmc8znc55afsrz5dgxblpn9n81fhxyyqb0"))

(define-public font-microsoft-trebuchet-ms
  (mscorefont "trebuc32"
              #:version "1.22"
              #:font-name "Trebuchet MS"
              #:hash "1jfsgz80pvyqvpfpaiz5pd8zwlcn67rg2jgynjwf22sip2dhssas"))

(define-public font-microsoft-times-new-roman
  (mscorefont "times32"
              #:version "2.82"
              #:font-name "Times New Roman"
              #:hash "1aq7z3l46vwgqljvq9zfgkii6aivy00z1529qbjkspggqrg5jmnv"))

(define-public font-microsoft-verdana
  (mscorefont "verdan32"
              #:version "2.35"
              #:font-name "Verdana"
              #:hash "15mdbbfqbyp25a6ynik3rck3m3mg44plwrj79rwncc9nbqjn3jy1"))

(define-public font-microsoft-webdings
  (mscorefont "webdin32"
              #:version "1.03"
              #:font-name "Webdings"
              #:hash "0nnp2znmnmx87ijq9zma0vl0hd46npx38p0cc6lgp00hpid5nnb4"))

(define-public font-microsoft-web-core-fonts
  (package
    (inherit font-microsoft-times-new-roman)
    (name "font-microsoft-web-core-fonts")
    (version "1.0.0")
    (synopsis "Collection of widely spread Microsoft TrueType fonts")
    (description "This package provides fonts from the collection of Microsoft
True Type Core Fonts for the Web.

Included fonts:
@itemize
@item Andale Mono
@item Arial Black
@item Arial (Bold, Italic, Bold Italic)
@item Comic Sans MS (Bold)
@item Courier New (Bold, Italic, Bold Italic)
@item Georgia (Bold, Italic, Bold Italic)
@item Impact
@item Times New Roman (Bold, Italic, Bold Italic)
@item Trebuchet (Bold, Italic, Bold Italic)
@item Verdana (Bold, Italic, Bold Italic)
@item Webdings
@end itemize")
    (propagated-inputs `(("font-microsoft-andale-mono" ,font-microsoft-andale-mono)
                         ("font-microsoft-arial" ,font-microsoft-arial)
                         ("font-microsoft-arial-black" ,font-microsoft-arial-black)
                         ("font-microsoft-comic-sans-ms" ,font-microsoft-comic-sans-ms)
                         ("font-microsoft-couirer-new" ,font-microsoft-couirer-new)
                         ("font-microsoft-georgia" ,font-microsoft-georgia)
                         ("font-microsoft-impact" ,font-microsoft-impact)
                         ("font-microsoft-times-new-roman" ,font-microsoft-times-new-roman)
                         ("font-microsoft-trebuchet-ms" ,font-microsoft-trebuchet-ms)
                         ("font-microsoft-verdana" ,font-microsoft-verdana)
                         ("font-microsoft-webdings" ,font-microsoft-webdings)))))

(define* (apple-font #:key font-name
                     file
                     hash
                     version
                     synopsis
                     description)
  (package
    (name (string-append "font-apple-"
                         (string-replace-substring (string-downcase font-name)
                                                   " " "-")))
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://devimages-cdn.apple.com/design/resources/download/" file
             ".dmg"))
       (sha256
        (base32 hash))))
    (build-system font-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source inputs #:allow-other-keys)
              (let ((7z-exe (search-input-file inputs "/bin/7z")))
                (invoke 7z-exe "x" source)
                (invoke 7z-exe "x"
                        (car (find-files "." "\\.pkg$")))
                (invoke 7z-exe "x" "Payload~")))))))
    (native-inputs (list p7zip))
    (home-page "https://developer.apple.com/fonts")
    (synopsis (string-append font-name " typeface by Apple."))
    (description description)
    (license (nonfree "https://www.apple.com"
              "Specific license information is embedded in the font files."))))

(define-public font-apple-sf-pro
  (apple-font #:font-name "SF Pro"
   #:file "SF-Pro"
   #:hash "1krvzxz7kal6y0l5cx9svmgikqdj5v0fl5vnfjig0z4nwp903ir1"
   #:version "20.0d8e1"
   #:description
   "This neutral, flexible, sans-serif typeface is the system
font for iOS, iPad OS, macOS and tvOS.  SF Pro features nine weights, variable
optical sizes for optimal legibility, four widths, and includes a rounded
variant.  SF Pro supports over 150 languages across Latin, Greek, and Cyrillic
scripts."))

(define-public font-apple-sf-compact
  (apple-font #:font-name "SF Compact"
   #:file "SF-Compact"
   #:hash "0ncybkrzqazw13azy2s30ss7ml5pxaia6hbmqq9wn7xhlhrxlniy"
   #:version "20.0d8e1"
   #:description
   "Sharing many features with SF Pro, SF Compact features an
efficient, compact design that is optimized for small sizes and narrow columns.
SF Compact is the system font for watchOS and includes a rounded variant."))

(define-public font-apple-sf-mono
  (apple-font #:font-name "SF Mono"
   #:file "SF-Mono"
   #:hash "0ibrk9fvbq52f5qnv1a8xlsazd3x3jnwwhpn2gwhdkdawdw0njkd"
   #:version "20.0d8e1"
   #:description
   "This monospaced variant of San Francisco enables alignment
between rows and columns of text, and is used in coding environments like Xcode.
SF Mono features six weights and supports Latin, Greek, and Cyrillic scripts."))

(define-public font-apple-sf-arabic
  (apple-font #:font-name "SF Arabic"
   #:file "SF-Arabic"
   #:hash "1fcpa488vx3xj9f9hq70gxj4qbgcjaijwz2i94n02xrba0nwcq17"
   #:version "20.0d8e1"
   #:description
   "A contemporary interpretation of the Naskh style with a
rational and flexible design, this extension of San Francisco is the Arabic
system font on Apple platforms.  Like San Francisco, SF Arabic features nine
weights, variable optical sizes that automatically adjust spacing and contrast
based on the point size, and includes a rounded variant."))

(define-public font-apple-new-york
  (apple-font #:font-name "New York"
   #:file "NY"
   #:hash "1x7qi3dqwq1p4l3md31cd93mcds3ba7rgsmpz0kg7h3caasfsbhw"
   #:version "17.0d5e1"
   #:description
   "A companion to San Francisco, this serif typeface is based on
essential aspects of historical type styles.  New York features six weights,
supports Latin, Greek and Cyrillic scripts, and features variable optical sizes
allowing it to perform as a traditional reading face at small sizes and a
graphic display face at larger sizes."))

;; At the time of this writing, `sf-symbols' is just `sf-pro' and `sf-compact'
;; together plus one extra file, SFSymbolsFallback.otf. This package is
;; probably of limited use on non-macOS but we'll include it for completeness
;; and in case the situation changes in the future.
(define-public font-apple-sf-symbols
  (apple-font #:font-name "SF Symbols"
   #:file "SF-Symbols-6"
   #:hash "1yl41hjn2ckixvv6lpdw1imvihan0v5ggfkkllsd4mjd4z590vl4"
   #:version "6"
   #:description
   "With over 5,000 symbols, SF Symbols is a library of
iconography designed to integrate seamlessly with San Francisco, the system
font for Apple platforms.  Symbols come in nine weights and three scales, and
automatically align with text labels."))

(define-public font-apple-color-emoji
  (package
    (name "font-apple-color-emoji")
    (version "20.0d1e3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pharcosyle/apple-fonts"
                           "/releases/download" "/Apple-Color-Emoji-" version
                           "/Apple.Color.Emoji.ttc"))
       (sha256
        (base32 "06i1ady7b2g1i2hl3f8yxn64g2i82c2ni9vdw25gpdqdj28vyqw3"))))
    (build-system font-build-system)
    (home-page "https://www.apple.com")
    (synopsis "Apple Color Emoji typeface by Apple.")
    (description "Color and black-and-white Apple emoji fonts.")
    (license (nonfree "https://www.apple.com"))))

(define-public font-apple-symbols
  (package
    (name "font-apple-symbols")
    (version "17.0d1e2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pharcosyle/apple-fonts"
                           "/releases/download" "/Apple-Symbols-" version
                           "/Apple.Symbols.ttf"))
       (sha256
        (base32 "14ivhsiszmy1ql69kpr1f33n07qgpaj7w43w2p5qnmlwcrvs3ccr"))))
    (build-system font-build-system)
    (home-page "https://www.apple.com")
    (synopsis "Apple Symbols typeface by Apple.")
    (description "An Apple system font intended to provide coverage for
characters defined as symbols in the Unicode Standard.")
    (license (nonfree "https://www.apple.com"))))
