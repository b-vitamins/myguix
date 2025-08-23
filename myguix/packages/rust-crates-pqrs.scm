;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Ayan Das <bvits@riseup.net>
;;;
;;; This file is NOT part of GNU Guix, but follows the structure
;;; of GNU Guix rust-crates module for external channels.

(define-module (myguix packages rust-crates-pqrs)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix build-system cargo)
                #:select (crate-uri define-cargo-inputs))
  #:export (lookup-myguix-cargo-inputs lookup-cargo-inputs))
 ;; Export with standard name for cargo-inputs function

;;; Helper for crate sources
(define* (crate-source name version hash
                       #:key snippet)
  "Create an origin for a crate from crates.io"
  (origin
    (method url-fetch)
    (uri (crate-uri name version))
    (file-name (string-append name "-" version ".tar.gz"))
    (sha256 (base32 hash))
    (snippet snippet)))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstyle-1.0.11
  (crate-source "anstyle" "1.0.11"
                "1gbbzi0zbgff405q14v8hhpi1kz2drzl9a75r3qhks47lindjbl6"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-base64-0.13.1
  (crate-source "base64" "0.13.1"
                "1s494mqmzjb766fy1kqlccgfg2sdcjb6hzbvzqv2jw65fdi5h6wy"))

(define rust-bit-set-0.5.3
  (crate-source "bit-set" "0.5.3"
                "1wcm9vxi00ma4rcxkl3pzzjli6ihrpn9cfdi0c5b4cvga2mxs007"))

(define rust-bit-vec-0.6.3
  (crate-source "bit-vec" "0.6.3"
                "1ywqjnv60cdh1slhz67psnp422md6jdliji6alq0gmly2xm9p7rl"))

(define rust-bitflags-2.9.3
  (crate-source "bitflags" "2.9.3"
                "0pgjwsd9qgdmsmwpvg47p9ccrsc26yfjqawbhsi9qds5sg6brvrl"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-cc-1.2.34
  (crate-source "cc" "1.2.34"
                "1p5ycww65h7xca03lwdp264qalw8v357rg5h17s7naq3h3m4mg22"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-clap-4.5.45
  (crate-source "clap" "4.5.45"
                "0663m85dd0aq1g3mkwz5b8pkjv4f5k2smlz7bagib4iqf15fgh0z"))

(define rust-clap-builder-4.5.44
  (crate-source "clap_builder" "4.5.44"
                "1a48x3c9q1l7r6wbgy71mq6kfsihpqzxsnbaaamcgwvp88hz9rxk"))

(define rust-clap-derive-4.5.45
  (crate-source "clap_derive" "4.5.45"
                "1xir8wn5d10wpmnzmzjf2k1ib7j5mmzsm6v3yap6qlvx1axk3jql"))

(define rust-clap-lex-0.7.5
  (crate-source "clap_lex" "0.7.5"
                "0xb6pjza43irrl99axbhs12pxq4sr8x7xd36p703j57f5i3n2kxr"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-console-0.15.11
  (crate-source "console" "0.15.11"
                "1n5gmsjk6isbnw6qss043377kln20lfwlmdk3vswpwpr21dwnk05"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-darling-0.14.4
  (crate-source "darling" "0.14.4"
                "0l1qrn805bsxa0iy7x8bmdwr8c10hlw0yiqs8ckv7lbz86rhqxbv"))

(define rust-darling-core-0.14.4
  (crate-source "darling_core" "0.14.4"
                "1w4b2ndxmkwghwq84yphk8x15jnpivm08w596g12ry5pwsk1r70h"))

(define rust-darling-macro-0.14.4
  (crate-source "darling_macro" "0.14.4"
                "13mlyd5w275c815k0ijf6g4c446hs8b3m2h4an5isqgpr7dv9am4"))

(define rust-derive-builder-0.12.0
  (crate-source "derive_builder" "0.12.0"
                "1y4p569zcvpmly5s5hmjp9h83drxvdp6kj6bb61h225mhj3pfrwd"))

(define rust-derive-builder-core-0.12.0
  (crate-source "derive_builder_core" "0.12.0"
                "03vvmw3mfg370swq0dh2h5kcjjb8va2m4asqgp9wfyy4l08xq6y1"))

(define rust-derive-builder-macro-0.12.0
  (crate-source "derive_builder_macro" "0.12.0"
                "17p71qzh7x1q2yxzz3xrg73zw3xl0h479b7ybyjm0s1rg9fa7kgb"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-env-logger-0.10.2
  (crate-source "env_logger" "0.10.2"
                "1005v71kay9kbz1d5907l0y7vh9qn2fqsp2yfgb8bjvin6m0bm2c"))

(define rust-errno-0.3.13
  (crate-source "errno" "0.3.13"
                "1bd5g3srn66zr3bspac0150bvpg1s7zi6zwhwhlayivciz12m3kp"))

(define rust-esaxx-rs-0.1.10
  (crate-source "esaxx-rs" "0.1.10"
                "1rm6vm5yr7s3n5ly7k9x9j6ra5p2l2ld151gnaya8x03qcwf05yq"))

(define rust-fancy-regex-0.13.0
  (crate-source "fancy-regex" "0.13.0"
                "1wjbqjsdj8fkq6z2i9llq25iaqzd9f208vxnwg8mdbr2ba1lc7jk"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.5.2
  (crate-source "hermit-abi" "0.5.2"
                "1744vaqkczpwncfy960j2hxrbjl1q01csm84jpd9dajbdr2yy3zw"))

(define rust-humantime-2.2.0
  (crate-source "humantime" "2.2.0"
                "17rz8jhh1mcv4b03wnknhv1shwq2v9vhkhlfg884pprsig62l4cv"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-indicatif-0.17.11
  (crate-source "indicatif" "0.17.11"
                "0db2b2r79r9x8x4lysq1ci9xm13c0xg0sqn3z960yh2bk2430fqq"))

(define rust-indoc-2.0.6
  (crate-source "indoc" "2.0.6"
                "1gbn2pkx5sgbd9lp05d2bkqpbfgazi0z3nvharh5ajah11d29izl"))

(define rust-is-terminal-0.4.16
  (crate-source "is-terminal" "0.4.16"
                "1acm63whnpwiw1padm9bpqz04sz8msymrmyxc55mvlq8hqqpykg0"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-itertools-0.11.0
  (crate-source "itertools" "0.11.0"
                "0mzyqcc59azx9g5cg6fs8k529gvh4463smmka6jvzs3cd2jp7hdi"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-js-sys-0.3.77
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-linux-raw-sys-0.9.4
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-lock-api-0.4.13
  (crate-source "lock_api" "0.4.13"
                "0rd73p4299mjwl4hhlfj9qr88v3r0kc8s1nszkfmnq2ky43nb4wn"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-macro-rules-attribute-0.2.2
  (crate-source "macro_rules_attribute" "0.2.2"
                "0835cx5bdsj06yffaspqqlids57bn3cwxp0x1g6l10394dwrs135"))

(define rust-macro-rules-attribute-proc-macro-0.2.2
  (crate-source "macro_rules_attribute-proc_macro" "0.2.2"
                "0c1s3lgkrdl5l2zmz6jc5g90zkq5w9islgn19alc86vmi7ddy3v7"))

(define rust-matrixmultiply-0.3.10
  (crate-source "matrixmultiply" "0.3.10"
                "020sqwg3cvprfasbszqbnis9zx6c3w9vlkfidyimgblzdq0y6vd0"))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

(define rust-memmap2-0.9.8
  (crate-source "memmap2" "0.9.8"
                "1dqxjs89krh8cin0k7ksqc9myw9yni9kn8d8cllwq4fn1isrhfl4"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-monostate-0.1.14
  (crate-source "monostate" "0.1.14"
                "1vpv8d9j8i7wachlcrpbwsy1rvzimpncgv8gwpil4mn7s3lipzma"))

(define rust-monostate-impl-0.1.14
  (crate-source "monostate-impl" "0.1.14"
                "1db3jrnbriivny6cahvhcc9af7w38q846mg1r4r4y82y5l4s80n4"))

(define rust-ndarray-0.15.6
  (crate-source "ndarray" "0.15.6"
                "0cpsm28hyk8qfjs4g9649dprv3hm53z12qqwyyjqbi3yjr72vcdd"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-num-complex-0.4.6
  (crate-source "num-complex" "0.4.6"
                "15cla16mnw12xzf5g041nxbjjm9m85hdgadd5dl5d0b30w9qmy3k"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-number-prefix-0.4.0
  (crate-source "number_prefix" "0.4.0"
                "1wvh13wvlajqxkb1filsfzbrnq0vrmrw298v2j3sy82z1rm282w3"))

(define rust-numpy-0.20.0
  (crate-source "numpy" "0.20.0"
                "0cfkj99lqjc9i1bxl2r43jrkkbznrq6f6naja8q3pa3y86xirx5y"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-onig-6.5.1
  (crate-source "onig" "6.5.1"
                "1w63vbzamn2v9jpnlj3wkglapqss0fcvhhd8pqafzkis8iirqsrk"))

(define rust-onig-sys-69.9.1
  ;; TODO: Check bundled sources.
  (crate-source "onig_sys" "69.9.1"
                "1p17cxzqnpqzpzamh7aqwpagxlnbhzs6myxw4dgz2v9xxxp6ry67"))

(define rust-parking-lot-0.12.4
  (crate-source "parking_lot" "0.12.4"
                "04sab1c7304jg8k0d5b2pxbj1fvgzcf69l3n2mfpkdb96vs8pmbh"))

(define rust-parking-lot-core-0.9.11
  (crate-source "parking_lot_core" "0.9.11"
                "19g4d6m5k4ggacinqprnn8xvdaszc3y5smsmbz1adcdmaqm8v0xw"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-portable-atomic-1.11.1
  (crate-source "portable-atomic" "1.11.1"
                "10s4cx9y3jvw0idip09ar52s2kymq8rq9a668f793shn1ar6fhpq"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-pyo3-0.20.3
  (crate-source "pyo3" "0.20.3"
                "0cw2pgab0pq5gd98nhv18xgxvyigygspla6c8mfycmwisjbbpgak"))

(define rust-pyo3-0.22.6
  (crate-source "pyo3" "0.22.6"
                "110qrq9yibfv40zzind9p2i87617lhzs379ix0m2065b2qk0c0pl"))

(define rust-pyo3-0.23.5
  (crate-source "pyo3" "0.23.5"
                "0wm8z6jgg18z2cgr99wc34mbkffhcnb50igmq5d1ff6ghpyvyy3p"))

(define rust-pyo3-build-config-0.20.3
  (crate-source "pyo3-build-config" "0.20.3"
                "1ms83n1qa81989c6pakpznifalvxv5fiyyji23732lizvr2mgany"))

(define rust-pyo3-build-config-0.22.6
  (crate-source "pyo3-build-config" "0.22.6"
                "0f4w8waba9cyzllq0dpxpw7qmgic05wdf4k20p8nsi7znmsmfjxi"))

(define rust-pyo3-build-config-0.23.5
  (crate-source "pyo3-build-config" "0.23.5"
                "1yqhw1k466k65rqvy2d4xz2shl0hzkry1xlxinciigzkdvlcpxll"))

(define rust-pyo3-ffi-0.20.3
  (crate-source "pyo3-ffi" "0.20.3"
                "1yja1npmzh4i73jn5dv2rnw7idif8bns51bf3zpx821ys0qjbd32"))

(define rust-pyo3-ffi-0.22.6
  (crate-source "pyo3-ffi" "0.22.6"
                "0dl6zj806rkvs67q2mdgjbnzjhzm8glms46nqx8bpp1c9bqbrdcs"))

(define rust-pyo3-ffi-0.23.5
  (crate-source "pyo3-ffi" "0.23.5"
                "13fxvxijl59vilv39akdzwqd1l7fb6c70f53n27irfy0672b9wg9"))

(define rust-pyo3-macros-0.20.3
  (crate-source "pyo3-macros" "0.20.3"
                "0n61s98qb2qc1wlda3bz4r0wi0vsr9p4lj2yr5g0bf01z8hcf1bk"))

(define rust-pyo3-macros-0.22.6
  (crate-source "pyo3-macros" "0.22.6"
                "0lylczfabgylnfldns6m36vsw98m9sini0wn1gcfda83g64lvlhg"))

(define rust-pyo3-macros-0.23.5
  (crate-source "pyo3-macros" "0.23.5"
                "1nm9i19aff7zn245v35qb0lbr3cxr19zdgcayq84fg7n509j1hpv"))

(define rust-pyo3-macros-backend-0.20.3
  (crate-source "pyo3-macros-backend" "0.20.3"
                "11b1z7qnbdnd9hy74ds3xcjx3mjkz43mvpnan32ljccwpdl9nzkw"))

(define rust-pyo3-macros-backend-0.22.6
  (crate-source "pyo3-macros-backend" "0.22.6"
                "1gmz3i0sr4fdg7qjvd8ylbkrgbbch9wv955kni903rd17fh13h1n"))

(define rust-pyo3-macros-backend-0.23.5
  (crate-source "pyo3-macros-backend" "0.23.5"
                "0a10yxj41kvjhh9vywzd2zj3h6iwm4bg3mlkw2frrnpks1m759pw"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rawpointer-0.2.1
  (crate-source "rawpointer" "0.2.1"
                "1qy1qvj17yh957vhffnq6agq0brvylw27xgks171qrah75wmg8v0"))

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-cond-0.3.0
  (crate-source "rayon-cond" "0.3.0"
                "1ybxppq84p3q60h9rng9j3dm79f6970hn4wljyf31lpgan5m77q5"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-redox-syscall-0.5.17
  (crate-source "redox_syscall" "0.5.17"
                "0xrvpchkaxph3r5ww2i04v9nwg3843fp3prf8kqlh1gv01b4c1sl"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustix-1.0.8
  (crate-source "rustix" "1.0.8"
                "1j6ajqi61agdnh1avr4bplrsgydjw1n4mycdxw3v8g94pyx1y60i"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-safetensors-0.5.3
  (crate-source "safetensors" "0.5.3"
                "1s50s455akpz4s8sri6h271i4m0prd1fz3yzyq8s2f6pk1qxn36c"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-derive-1.0.219
  (crate-source "serde_derive" "1.0.219"
                "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))

(define rust-serde-json-1.0.143
  (crate-source "serde_json" "1.0.143"
                "0njabwzldvj13ykrf1aaf4gh5cgl25kf9hzbpafbv3qh3ppsn0fl"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-spm-precompiled-0.1.4
  (crate-source "spm_precompiled" "0.1.4"
                "09pkdk2abr8xf4pb9kq3rk80dgziq6vzfk7aywv3diik82f6jlaq"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-tempfile-3.21.0
  (crate-source "tempfile" "3.21.0"
                "07kx58ibjk3ydq1gcb7q637fs5zkxaa550lxckhgg9p3427izdhm"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-tokenizers-0.15.2
  (crate-source "tokenizers" "0.15.2"
                "0kb5sfrgrdd8yaxn4080fhagsdniahbvz3si6gyyfdmsn1i7km1x"))

(define rust-unicode-categories-0.1.1
  (crate-source "unicode_categories" "0.1.1"
                "0kp1d7fryxxm7hqywbk88yb9d1avsam9sg76xh36k5qx2arj9v1r"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"))

(define rust-unicode-normalization-alignments-0.1.12
  (crate-source "unicode-normalization-alignments" "0.1.12"
                "1pk2f3arh3qvdsmrsiri0gr5y5vqpk2gv1yjin0njvh4zbj17xj3"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-width-0.2.1
  (crate-source "unicode-width" "0.2.1"
                "0k0mlq7xy1y1kq6cgv1r2rs2knn6rln3g3af50rhi0dkgp60f6ja"))

(define rust-unindent-0.2.4
  (crate-source "unindent" "0.2.4"
                "1wvfh815i6wm6whpdz1viig7ib14cwfymyr1kn3sxk2kyl3y2r3j"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasi-0.14.2+wasi-0.2.4
  (crate-source "wasi" "0.14.2+wasi-0.2.4"
                "1cwcqjr3dgdq8j325awgk8a715h0hg0f7jqzsb077n4qm6jzk0wn"))

(define rust-wasm-bindgen-0.2.100
  (crate-source "wasm-bindgen" "0.2.100"
                "1x8ymcm6yi3i1rwj78myl1agqv2m86i648myy3lc97s9swlqkp0y"))

(define rust-wasm-bindgen-backend-0.2.100
  (crate-source "wasm-bindgen-backend" "0.2.100"
                "1ihbf1hq3y81c4md9lyh6lcwbx6a5j0fw4fygd423g62lm8hc2ig"))

(define rust-wasm-bindgen-macro-0.2.100
  (crate-source "wasm-bindgen-macro" "0.2.100"
                "01xls2dvzh38yj17jgrbiib1d3nyad7k2yw9s0mpklwys333zrkz"))

(define rust-wasm-bindgen-macro-support-0.2.100
  (crate-source "wasm-bindgen-macro-support" "0.2.100"
                "1plm8dh20jg2id0320pbmrlsv6cazfv6b6907z19ys4z1jj7xs4a"))

(define rust-wasm-bindgen-shared-0.2.100
  (crate-source "wasm-bindgen-shared" "0.2.100"
                "0gffxvqgbh9r9xl36gprkfnh3w9gl8wgia6xrin7v11sjcxxf18s"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-winapi-util-0.1.10
  (crate-source "winapi-util" "0.1.10"
                "08hb8rj3aq9lcrfmliqs4l7v9zh6srbcn0376yn0pndkf5qvyy09"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-sys-0.59.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.3
  (crate-source "windows-targets" "0.53.3"
                "14fwwm136dhs3i1impqrrip7nvkra3bdxa4nqkblj604qhqn1znm"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-wit-bindgen-rt-0.39.0
  (crate-source "wit-bindgen-rt" "0.39.0"
                "1hd65pa5hp0nl664m94bg554h4zlhrzmkjsf6lsgsb7yc4734hkg"))

(define rust-zerocopy-0.8.26
  (crate-source "zerocopy" "0.8.26"
                "0bvsj0qzq26zc6nlrm3z10ihvjspyngs7n0jw1fz031i7h6xsf8h"))

(define rust-zerocopy-derive-0.8.26
  (crate-source "zerocopy-derive" "0.8.26"
                "10aiywi5qkha0mpsnb1zjwi44wl2rhdncaf3ykbp4i9nqm65pkwy"))

(define-cargo-inputs lookup-myguix-cargo-inputs
                     (python-safetensors =>
                                         (list rust-autocfg-1.5.0
                                          rust-cfg-if-1.0.3
                                          rust-heck-0.5.0
                                          rust-indoc-2.0.6
                                          rust-itoa-1.0.15
                                          rust-libc-0.2.175
                                          rust-memchr-2.7.5
                                          rust-memmap2-0.9.8
                                          rust-memoffset-0.9.1
                                          rust-once-cell-1.21.3
                                          rust-portable-atomic-1.11.1
                                          rust-proc-macro2-1.0.101
                                          rust-pyo3-0.23.5
                                          rust-pyo3-build-config-0.23.5
                                          rust-pyo3-ffi-0.23.5
                                          rust-pyo3-macros-0.23.5
                                          rust-pyo3-macros-backend-0.23.5
                                          rust-quote-1.0.40
                                          rust-ryu-1.0.20
                                          rust-safetensors-0.5.3
                                          rust-serde-1.0.219
                                          rust-serde-derive-1.0.219
                                          rust-serde-json-1.0.143
                                          rust-syn-2.0.106
                                          rust-target-lexicon-0.12.16
                                          rust-unicode-ident-1.0.18
                                          rust-unindent-0.2.4))
                     (python-tiktoken =>
                                      (list rust-aho-corasick-1.1.3
                                            rust-autocfg-1.5.0
                                            rust-bit-set-0.5.3
                                            rust-bit-vec-0.6.3
                                            rust-bstr-1.12.0
                                            rust-cfg-if-1.0.3
                                            rust-fancy-regex-0.13.0
                                            rust-heck-0.5.0
                                            rust-indoc-2.0.6
                                            rust-libc-0.2.175
                                            rust-memchr-2.7.5
                                            rust-memoffset-0.9.1
                                            rust-once-cell-1.21.3
                                            rust-portable-atomic-1.11.1
                                            rust-proc-macro2-1.0.101
                                            rust-pyo3-0.22.6
                                            rust-pyo3-build-config-0.22.6
                                            rust-pyo3-ffi-0.22.6
                                            rust-pyo3-macros-0.22.6
                                            rust-pyo3-macros-backend-0.22.6
                                            rust-quote-1.0.40
                                            rust-regex-1.11.1
                                            rust-regex-automata-0.4.9
                                            rust-regex-syntax-0.8.5
                                            rust-rustc-hash-1.1.0
                                            rust-serde-1.0.219
                                            rust-serde-derive-1.0.219
                                            rust-syn-2.0.106
                                            rust-target-lexicon-0.12.16
                                            rust-unicode-ident-1.0.18
                                            rust-unindent-0.2.4))
                     (python-tokenizers-for-nougat =>
                                                   (list
                                                    rust-aho-corasick-1.1.3
                                                    rust-anstream-0.6.20
                                                    rust-anstyle-1.0.11
                                                    rust-anstyle-parse-0.2.7
                                                    rust-anstyle-query-1.1.4
                                                    rust-anstyle-wincon-3.0.10
                                                    rust-autocfg-1.5.0
                                                    rust-base64-0.13.1
                                                    rust-bitflags-2.9.3
                                                    rust-bumpalo-3.19.0
                                                    rust-cc-1.2.34
                                                    rust-cfg-if-1.0.3
                                                    rust-clap-4.5.45
                                                    rust-clap-builder-4.5.44
                                                    rust-clap-derive-4.5.45
                                                    rust-clap-lex-0.7.5
                                                    rust-colorchoice-1.0.4
                                                    rust-console-0.15.11
                                                    rust-crossbeam-deque-0.8.6
                                                    rust-crossbeam-epoch-0.9.18
                                                    rust-crossbeam-utils-0.8.21
                                                    rust-darling-0.14.4
                                                    rust-darling-core-0.14.4
                                                    rust-darling-macro-0.14.4
                                                    rust-derive-builder-0.12.0
                                                    rust-derive-builder-core-0.12.0
                                                    rust-derive-builder-macro-0.12.0
                                                    rust-either-1.15.0
                                                    rust-encode-unicode-1.0.0
                                                    rust-env-logger-0.10.2
                                                    rust-errno-0.3.13
                                                    rust-esaxx-rs-0.1.10
                                                    rust-fastrand-2.3.0
                                                    rust-fnv-1.0.7
                                                    rust-getrandom-0.2.16
                                                    rust-getrandom-0.3.3
                                                    rust-heck-0.4.1
                                                    rust-heck-0.5.0
                                                    rust-hermit-abi-0.5.2
                                                    rust-humantime-2.2.0
                                                    rust-ident-case-1.0.1
                                                    rust-indicatif-0.17.11
                                                    rust-indoc-2.0.6
                                                    rust-is-terminal-0.4.16
                                                    rust-is-terminal-polyfill-1.70.1
                                                    rust-itertools-0.11.0
                                                    rust-itertools-0.12.1
                                                    rust-itoa-1.0.15
                                                    rust-js-sys-0.3.77
                                                    rust-lazy-static-1.5.0
                                                    rust-libc-0.2.175
                                                    rust-linux-raw-sys-0.9.4
                                                    rust-lock-api-0.4.13
                                                    rust-log-0.4.27
                                                    rust-macro-rules-attribute-0.2.2
                                                    rust-macro-rules-attribute-proc-macro-0.2.2
                                                    rust-matrixmultiply-0.3.10
                                                    rust-memchr-2.7.5
                                                    rust-memoffset-0.9.1
                                                    rust-minimal-lexical-0.2.1
                                                    rust-monostate-0.1.14
                                                    rust-monostate-impl-0.1.14
                                                    rust-ndarray-0.15.6
                                                    rust-nom-7.1.3
                                                    rust-num-complex-0.4.6
                                                    rust-num-integer-0.1.46
                                                    rust-num-traits-0.2.19
                                                    rust-number-prefix-0.4.0
                                                    rust-numpy-0.20.0
                                                    rust-once-cell-1.21.3
                                                    rust-once-cell-polyfill-1.70.1
                                                    rust-onig-6.5.1
                                                    rust-onig-sys-69.9.1
                                                    rust-parking-lot-0.12.4
                                                    rust-parking-lot-core-0.9.11
                                                    rust-paste-1.0.15
                                                    rust-pkg-config-0.3.32
                                                    rust-portable-atomic-1.11.1
                                                    rust-ppv-lite86-0.2.21
                                                    rust-proc-macro2-1.0.101
                                                    rust-pyo3-0.20.3
                                                    rust-pyo3-build-config-0.20.3
                                                    rust-pyo3-ffi-0.20.3
                                                    rust-pyo3-macros-0.20.3
                                                    rust-pyo3-macros-backend-0.20.3
                                                    rust-quote-1.0.40
                                                    rust-r-efi-5.3.0
                                                    rust-rand-0.8.5
                                                    rust-rand-chacha-0.3.1
                                                    rust-rand-core-0.6.4
                                                    rust-rawpointer-0.2.1
                                                    rust-rayon-1.11.0
                                                    rust-rayon-cond-0.3.0
                                                    rust-rayon-core-1.13.0
                                                    rust-redox-syscall-0.5.17
                                                    rust-regex-1.11.1
                                                    rust-regex-automata-0.4.9
                                                    rust-regex-syntax-0.8.5
                                                    rust-rustc-hash-1.1.0
                                                    rust-rustix-1.0.8
                                                    rust-ryu-1.0.20
                                                    rust-scopeguard-1.2.0
                                                    rust-serde-1.0.219
                                                    rust-serde-derive-1.0.219
                                                    rust-serde-json-1.0.143
                                                    rust-shlex-1.3.0
                                                    rust-smallvec-1.15.1
                                                    rust-spm-precompiled-0.1.4
                                                    rust-strsim-0.10.0
                                                    rust-strsim-0.11.1
                                                    rust-syn-1.0.109
                                                    rust-syn-2.0.106
                                                    rust-target-lexicon-0.12.16
                                                    rust-tempfile-3.21.0
                                                    rust-termcolor-1.4.1
                                                    rust-thiserror-1.0.69
                                                    rust-thiserror-impl-1.0.69
                                                    rust-unicode-ident-1.0.18
                                                    rust-unicode-normalization-alignments-0.1.12
                                                    rust-unicode-segmentation-1.12.0
                                                    rust-unicode-width-0.2.1
                                                    rust-unicode-categories-0.1.1
                                                    rust-unindent-0.2.4
                                                    rust-utf8parse-0.2.2
                                                    rust-wasi-0.11.1+wasi-snapshot-preview1
                                                    rust-wasi-0.14.2+wasi-0.2.4
                                                    rust-wasm-bindgen-0.2.100
                                                    rust-wasm-bindgen-backend-0.2.100
                                                    rust-wasm-bindgen-macro-0.2.100
                                                    rust-wasm-bindgen-macro-support-0.2.100
                                                    rust-wasm-bindgen-shared-0.2.100
                                                    rust-web-time-1.1.0
                                                    rust-winapi-util-0.1.10
                                                    rust-windows-link-0.1.3
                                                    rust-windows-sys-0.59.0
                                                    rust-windows-sys-0.60.2
                                                    rust-windows-targets-0.52.6
                                                    rust-windows-targets-0.53.3
                                                    rust-windows-aarch64-gnullvm-0.52.6
                                                    rust-windows-aarch64-gnullvm-0.53.0
                                                    rust-windows-aarch64-msvc-0.52.6
                                                    rust-windows-aarch64-msvc-0.53.0
                                                    rust-windows-i686-gnu-0.52.6
                                                    rust-windows-i686-gnu-0.53.0
                                                    rust-windows-i686-gnullvm-0.52.6
                                                    rust-windows-i686-gnullvm-0.53.0
                                                    rust-windows-i686-msvc-0.52.6
                                                    rust-windows-i686-msvc-0.53.0
                                                    rust-windows-x86-64-gnu-0.52.6
                                                    rust-windows-x86-64-gnu-0.53.0
                                                    rust-windows-x86-64-gnullvm-0.52.6
                                                    rust-windows-x86-64-gnullvm-0.53.0
                                                    rust-windows-x86-64-msvc-0.52.6
                                                    rust-windows-x86-64-msvc-0.53.0
                                                    rust-wit-bindgen-rt-0.39.0
                                                    rust-zerocopy-0.8.26
                                                    rust-zerocopy-derive-0.8.26
                                                    rust-tokenizers-0.15.2)))
