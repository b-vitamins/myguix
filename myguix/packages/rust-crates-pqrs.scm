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

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-android-tzdata-0.1.1
  (crate-source "android-tzdata" "0.1.1"
                "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))

(define rust-anes-0.1.6
  (crate-source "anes" "0.1.6"
                "16bj1ww1xkwzbckk32j2pnbn5vk6wgsl3q4p3j9551xbcarwnijb"))

(define rust-annotate-snippets-0.11.5
  (crate-source "annotate-snippets" "0.11.5"
                "1i1bmr5vy957l8fvivj9x1xs24np0k56rdgwj0bxqk45b2p8w3ki"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstyle-1.0.11
  (crate-source "anstyle" "1.0.11"
                "1gbbzi0zbgff405q14v8hhpi1kz2drzl9a75r3qhks47lindjbl6"))

(define rust-anstyle-lossy-1.1.4
  (crate-source "anstyle-lossy" "1.1.4"
                "07x0kqkklc0124cbn49fc21d9wzp9w2vhaw827md113ghbfablq4"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-svg-0.1.10
  (crate-source "anstyle-svg" "0.1.10"
                "1abg68rnik8qbzfihhmp2k7j6r8fmhh62iqfdhwy2vshxxqaf0yw"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anyhow-1.0.99
  (crate-source "anyhow" "1.0.99"
                "001icqvkfl28rxxmk99rm4gvdzxqngj5v50yg2bh3dzcvqfllrxh"))

(define rust-append-only-vec-0.1.7
  (crate-source "append-only-vec" "0.1.7"
                "0wg596rw1dhw8wjgd5dvd4cx7sx2jpabycfxj9lykkrmq1g0i4kr"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-argfile-0.2.1
  (crate-source "argfile" "0.2.1"
                "1s756chhwp69ha23i17hvqfriqxf5k7zfrjccx0dnmyyd6xc070a"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-assert-fs-1.1.3
  (crate-source "assert_fs" "1.1.3"
                "1a97vn1dcr2szr7552dqzhnfdbdrg30abrz5zvy8cs2i3z5zclm6"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-aws-lc-rs-1.14.1
  (crate-source "aws-lc-rs" "1.14.1"
                "03a4f2h4h724pjwbbjwii73ra5zxiysyc2nwli5l1srdb64nr6w7"))

(define rust-aws-lc-sys-0.32.3
  ;; TODO: Check bundled sources.
  (crate-source "aws-lc-sys" "0.32.3"
                "134gmaf47gaa0pnxbfm8z63skwi51vp8vfw49vh676dbkjflwyhh"))

(define rust-base64-0.13.1
  (crate-source "base64" "0.13.1"
                "1s494mqmzjb766fy1kqlccgfg2sdcjb6hzbvzqv2jw65fdi5h6wy"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-bencher-0.1.5
  (crate-source "bencher" "0.1.5"
                "1x8p2xblgqssay8cdykp5pkfc0np0jk5bs5cx4f5av097aav9zbx"))

(define rust-bincode-1.3.3
  (crate-source "bincode" "1.3.3"
                "1bfw3mnwzx5g1465kiqllp5n4r10qrqy88kdlp3jfwnq2ya5xx5i"))

(define rust-bincode-2.0.1
  (crate-source "bincode" "2.0.1"
                "0h5pxp3dqkigjwy926a03sl69n9wv7aq4142a20kw9lhn3bzbsin"))

(define rust-bincode-derive-2.0.1
  (crate-source "bincode_derive" "2.0.1"
                "029wmh26hq3hhs1gq629y0frn2pkl7ld061rk23fji8g8jd715dz"))

(define rust-bindgen-0.72.1
  (crate-source "bindgen" "0.72.1"
                "15bq73y3wd3x3vxh3z3g72hy08zs8rxg1f0i1xsrrd6g16spcdwr"))

(define rust-bit-set-0.5.3
  (crate-source "bit-set" "0.5.3"
                "1wcm9vxi00ma4rcxkl3pzzjli6ihrpn9cfdi0c5b4cvga2mxs007"))

(define rust-bit-vec-0.6.3
  (crate-source "bit-vec" "0.6.3"
                "1ywqjnv60cdh1slhz67psnp422md6jdliji6alq0gmly2xm9p7rl"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.10.0
  (crate-source "bitflags" "2.10.0"
                "1lqxwc3625lcjrjm5vygban9v8a6dlxisp1aqylibiaw52si4bl1"))

(define rust-bitflags-2.9.3
  (crate-source "bitflags" "2.9.3"
                "0pgjwsd9qgdmsmwpvg47p9ccrsc26yfjqawbhsi9qds5sg6brvrl"))

(define rust-bitvec-1.0.1
  (crate-source "bitvec" "1.0.1"
                "173ydyj2q5vwj88k6xgjnfsshs4x9wbvjjv7sm0h36r34hn87hhv"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-boxcar-0.2.14
  (crate-source "boxcar" "0.2.14"
                "0vksx6zjnkqwxsm2bp21vhmc35dqlmhjgzr69cdxm10awkm4pxin"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-bytes-1.10.1
  (crate-source "bytes" "1.10.1"
                "0smd4wi2yrhp5pmq571yiaqx84bjqlm1ixqhnvfwzzc6pqkn26yp"))

(define rust-cachedir-0.3.1
  (crate-source "cachedir" "0.3.1"
                "0wyqx30crm2qsq4ny57hhljyq6iw6j4qfg7fbfiqznvpf29z60s7"))

(define rust-camino-1.1.11
  (crate-source "camino" "1.1.11"
                "1h2150limbipfx5w59cw797nsgwjxn5mjpf33gvpc35hjfdal1sx"))

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-castaway-0.2.4
  (crate-source "castaway" "0.2.4"
                "0nn5his5f8q20nkyg1nwb40xc19a08yaj4y76a8q2y3mdsmm3ify"))

(define rust-cc-1.2.34
  (crate-source "cc" "1.2.34"
                "1p5ycww65h7xca03lwdp264qalw8v357rg5h17s7naq3h3m4mg22"))

(define rust-cc-1.2.43
  (crate-source "cc" "1.2.43"
                "1hpg1f1srgd5bfivvln1s3kcajdxpqvjsvd8m4y4nmap8pwv17kk"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-chrono-0.4.41
  (crate-source "chrono" "0.4.41"
                "0k8wy2mph0mgipq28vv3wirivhb31pqs7jyid0dzjivz0i9djsf4"))

(define rust-ciborium-0.2.2
  (crate-source "ciborium" "0.2.2"
                "03hgfw4674im1pdqblcp77m7rc8x2v828si5570ga5q9dzyrzrj2"))

(define rust-ciborium-io-0.2.2
  (crate-source "ciborium-io" "0.2.2"
                "0my7s5g24hvp1rs1zd1cxapz94inrvqpdf1rslrvxj8618gfmbq5"))

(define rust-ciborium-ll-0.2.2
  (crate-source "ciborium-ll" "0.2.2"
                "1n8g4j5rwkfs3rzfi6g1p7ngmz6m5yxsksryzf5k72ll7mjknrjp"))

(define rust-clang-sys-1.8.1
  ;; TODO: Check bundled sources.
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-4.5.45
  (crate-source "clap" "4.5.45"
                "0663m85dd0aq1g3mkwz5b8pkjv4f5k2smlz7bagib4iqf15fgh0z"))

(define rust-clap-builder-4.5.44
  (crate-source "clap_builder" "4.5.44"
                "1a48x3c9q1l7r6wbgy71mq6kfsihpqzxsnbaaamcgwvp88hz9rxk"))

(define rust-clap-complete-4.5.57
  (crate-source "clap_complete" "4.5.57"
                "1bbixanlxdsb47qhk9fp1jl31vbk218rmnh1xsxzf2az7yyh35ad"))

(define rust-clap-complete-command-0.6.1
  (crate-source "clap_complete_command" "0.6.1"
                "0qhv99j7msqyw7j17hswqwpqbdvqawy8l7ip6rnnh5930n61k3ns"))

(define rust-clap-complete-nushell-4.5.8
  (crate-source "clap_complete_nushell" "4.5.8"
                "1kixnzc8rjqjhk189s1jjvg24v21d1ymj7a2knzna7k9jhb9a30a"))

(define rust-clap-derive-4.5.45
  (crate-source "clap_derive" "4.5.45"
                "1xir8wn5d10wpmnzmzjf2k1ib7j5mmzsm6v3yap6qlvx1axk3jql"))

(define rust-clap-lex-0.7.5
  (crate-source "clap_lex" "0.7.5"
                "0xb6pjza43irrl99axbhs12pxq4sr8x7xd36p703j57f5i3n2kxr"))

(define rust-clearscreen-4.0.2
  (crate-source "clearscreen" "4.0.2"
                "1bmi736xxhy07bld3y81ysr66vn3gnff08kvb4ahqay0l5rspa45"))

(define rust-cmake-0.1.54
  (crate-source "cmake" "0.1.54"
                "1w41ma28yzad9x757s9sfq1wigjs9j902hbzc0nbxpc9vvws7jp7"))

(define rust-codspeed-2.10.1
  (crate-source "codspeed" "2.10.1"
                "0v3cg65nbh4m9p6vg63l5vgsjhbghaqypzpz07qw8jbwqblwrx4k"))

(define rust-codspeed-bencher-compat-2.10.1
  (crate-source "codspeed-bencher-compat" "2.10.1"
                "0016b2pp449cjzpbbg0r3002xjqrgcdm1wnjdcicjvnjm9737nw7"))

(define rust-codspeed-criterion-compat-2.10.1
  (crate-source "codspeed-criterion-compat" "2.10.1"
                "09cp974b67fvnsb2358aglaqfcbsvn0q9jiq5nssm8i81a43vhn3"))

(define rust-codspeed-criterion-compat-walltime-2.10.1
  (crate-source "codspeed-criterion-compat-walltime" "2.10.1"
                "1fwg5jj339gqdki24kim1a88kg7pkdlfmsb75brg8iz3cmrjy2kv"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-colored-2.2.0
  (crate-source "colored" "2.2.0"
                "0g6s7j2qayjd7i3sivmwiawfdg8c8ldy0g2kl4vwk1yk16hjaxqi"))

(define rust-colored-3.0.0
  (crate-source "colored" "3.0.0"
                "0plizddhxc4vgkzdbzky5zggyaqfrmyim2d0n6sb7py9j3nf1q7x"))

(define rust-compact-str-0.8.1
  (crate-source "compact_str" "0.8.1"
                "0cmgp61hw4fwaakhilwznfgncw2p4wkbvz6dw3i7ibbckh3c8y9v"))

(define rust-console-0.15.11
  (crate-source "console" "0.15.11"
                "1n5gmsjk6isbnw6qss043377kln20lfwlmdk3vswpwpr21dwnk05"))

(define rust-console-0.16.0
  (crate-source "console" "0.16.0"
                "17f6rgdjz29wdgf4sld4bi6fa370y8hxh4slqss67jxwxgbww29f"))

(define rust-console-error-panic-hook-0.1.7
  (crate-source "console_error_panic_hook" "0.1.7"
                "1g5v8s0ndycc10mdn6igy914k645pgpcl8vjpz6nvxkhyirynsm0"))

(define rust-console-log-1.0.0
  (crate-source "console_log" "1.0.0"
                "03rwzvpg384y68j6hxm4h1bhzi7xcc5jdari8hxlvgzdwi0fv2my"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-countme-3.0.1
  (crate-source "countme" "3.0.1"
                "0dn62hhvgmwyxslh14r4nlbvz8h50cp5mnn1qhqsw63vs7yva13p"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-criterion-0.5.1
  (crate-source "criterion" "0.5.1"
                "0bv9ipygam3z8kk6k771gh9zi0j0lb9ir0xi1pc075ljg80jvcgj"))

(define rust-criterion-plot-0.5.0
  (crate-source "criterion-plot" "0.5.0"
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))

(define rust-crossbeam-0.8.4
  (crate-source "crossbeam" "0.8.4"
                "1a5c7yacnk723x0hfycdbl91ks2nxhwbwy46b8y5vyy0gxzcsdqi"))

(define rust-crossbeam-channel-0.5.15
  (crate-source "crossbeam-channel" "0.5.15"
                "1cicd9ins0fkpfgvz9vhz3m9rpkh6n8d3437c3wnfsdkd3wgif42"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-queue-0.3.12
  (crate-source "crossbeam-queue" "0.3.12"
                "059igaxckccj6ndmg45d5yf7cm4ps46c18m21afq3pwiiz1bnn0g"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crunchy-0.2.4
  (crate-source "crunchy" "0.2.4"
                "1mbp5navim2qr3x48lyvadqblcxc1dm0lqr0swrkkwy2qblvw3s6"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-ctrlc-3.4.7
  (crate-source "ctrlc" "3.4.7"
                "0wvf4w2wbpdnhp828jqw435x5ly4k7k1y1vzxxbdddsrlj03gya6"))

(define rust-darling-0.14.4
  (crate-source "darling" "0.14.4"
                "0l1qrn805bsxa0iy7x8bmdwr8c10hlw0yiqs8ckv7lbz86rhqxbv"))

(define rust-darling-0.20.11
  (crate-source "darling" "0.20.11"
                "1vmlphlrlw4f50z16p4bc9p5qwdni1ba95qmxfrrmzs6dh8lczzw"))

(define rust-darling-core-0.14.4
  (crate-source "darling_core" "0.14.4"
                "1w4b2ndxmkwghwq84yphk8x15jnpivm08w596g12ry5pwsk1r70h"))

(define rust-darling-core-0.20.11
  (crate-source "darling_core" "0.20.11"
                "0bj1af6xl4ablnqbgn827m43b8fiicgv180749f5cphqdmcvj00d"))

(define rust-darling-macro-0.14.4
  (crate-source "darling_macro" "0.14.4"
                "13mlyd5w275c815k0ijf6g4c446hs8b3m2h4an5isqgpr7dv9am4"))

(define rust-darling-macro-0.20.11
  (crate-source "darling_macro" "0.20.11"
                "1bbfbc2px6sj1pqqq97bgqn6c8xdnb2fmz66f7f40nrqrcybjd7w"))

(define rust-dashmap-5.5.3
  (crate-source "dashmap" "5.5.3"
                "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))

(define rust-dashmap-6.1.0
  (crate-source "dashmap" "6.1.0"
                "1kvnw859xvrqyd1lk89na6797yvl5bri4wi9j0viz2a4j54wqhah"))

(define rust-derive-builder-0.12.0
  (crate-source "derive_builder" "0.12.0"
                "1y4p569zcvpmly5s5hmjp9h83drxvdp6kj6bb61h225mhj3pfrwd"))

(define rust-derive-builder-0.20.2
  (crate-source "derive_builder" "0.20.2"
                "0is9z7v3kznziqsxa5jqji3ja6ay9wzravppzhcaczwbx84znzah"))

(define rust-derive-builder-core-0.12.0
  (crate-source "derive_builder_core" "0.12.0"
                "03vvmw3mfg370swq0dh2h5kcjjb8va2m4asqgp9wfyy4l08xq6y1"))

(define rust-derive-builder-core-0.20.2
  (crate-source "derive_builder_core" "0.20.2"
                "1s640r6q46c2iiz25sgvxw3lk6b6v5y8hwylng7kas2d09xwynrd"))

(define rust-derive-builder-macro-0.12.0
  (crate-source "derive_builder_macro" "0.12.0"
                "17p71qzh7x1q2yxzz3xrg73zw3xl0h479b7ybyjm0s1rg9fa7kgb"))

(define rust-derive-builder-macro-0.20.2
  (crate-source "derive_builder_macro" "0.20.2"
                "0g1zznpqrmvjlp2w7p0jzsjvpmw5rvdag0rfyypjhnadpzib0qxb"))

(define rust-derive-new-0.6.0
  (crate-source "derive-new" "0.6.0"
                "1b8jv6jx0b8jgkz9kmz0ciqmnf74xkk0mmvkb5z1c87932kdwl6i"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-difflib-0.4.0
  (crate-source "difflib" "0.4.0"
                "1s7byq4d7jgf2hcp2lcqxi2piqwl8xqlharfbi8kf90n8csy7131"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dir-test-0.4.1
  (crate-source "dir-test" "0.4.1"
                "0yaav4f0r6bsmz8shg7i0db4z9x73xn44dizb7jg6r2qhbz17h32"))

(define rust-dir-test-macros-0.4.1
  (crate-source "dir-test-macros" "0.4.1"
                "0zns8frffpsxkks24lrphmsphdd3s4sqwcsvzq029g56nkbm8byl"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-6.0.0
  (crate-source "dirs" "6.0.0"
                "0knfikii29761g22pwfrb8d0nqpbgw77sni9h2224haisyaams63"))

(define rust-dirs-sys-0.4.1
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

(define rust-dirs-sys-0.5.0
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.5.0"
                "1aqzpgq6ampza6v012gm2dppx9k35cdycbj54808ksbys9k366p0"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-dissimilar-1.0.10
  (crate-source "dissimilar" "1.0.10"
                "08b94x25x3ba6vg79i53wspxyagqr43crg9dw2zn2dpgl3dgyxc9"))

(define rust-doc-comment-0.3.3
  (crate-source "doc-comment" "0.3.3"
                "043sprsf3wl926zmck1bm7gw0jq50mb76lkpk49vasfr6ax1p97y"))

(define rust-drop-bomb-0.1.5
  (crate-source "drop_bomb" "0.1.5"
                "1qc59a53ngwxpnbvl8xidp2cmwrl671dhbzw7zijmjjaq0hqxnlv"))

(define rust-dunce-1.0.5
  (crate-source "dunce" "1.0.5"
                "04y8wwv3vvcqaqmqzssi6k0ii9gs6fpz96j5w9nky2ccsl23axwj"))

(define rust-dyn-clone-1.0.20
  (crate-source "dyn-clone" "1.0.20"
                "0m956cxcg8v2n8kmz6xs5zl13k2fak3zkapzfzzp7pxih6hix26h"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-env-filter-0.1.3
  (crate-source "env_filter" "0.1.3"
                "1l4p6f845cylripc3zkxa0lklk8rn2q86fqm522p6l2cknjhavhq"))

(define rust-env-home-0.1.0
  (crate-source "env_home" "0.1.0"
                "1zn08mk95rjh97831rky1n944k024qrwjhbcgb0xv9zhrh94xy67"))

(define rust-env-logger-0.10.2
  (crate-source "env_logger" "0.10.2"
                "1005v71kay9kbz1d5907l0y7vh9qn2fqsp2yfgb8bjvin6m0bm2c"))

(define rust-env-logger-0.11.8
  (crate-source "env_logger" "0.11.8"
                "17q6zbjam4wq75fa3m4gvvmv3rj3ch25abwbm84b28a0j3q67j0k"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.13
  (crate-source "errno" "0.3.13"
                "1bd5g3srn66zr3bspac0150bvpg1s7zi6zwhwhlayivciz12m3kp"))

(define rust-esaxx-rs-0.1.10
  (crate-source "esaxx-rs" "0.1.10"
                "1rm6vm5yr7s3n5ly7k9x9j6ra5p2l2ld151gnaya8x03qcwf05yq"))

(define rust-escape8259-0.5.3
  (crate-source "escape8259" "0.5.3"
                "1in2zrdak794x8v11lkrrgyqxjjqbijvg0yfn2paay0rb9xxv4jn"))

(define rust-escargot-0.5.15
  (crate-source "escargot" "0.5.15"
                "1kyhkrxdbxlw839h3l838ck5ir96i9v2p9x6kh650yy95fisxhqi"))

(define rust-etcetera-0.8.0
  (crate-source "etcetera" "0.8.0"
                "0hxrsn75dirbjhwgkdkh0pnpqrnq17ypyhjpjaypgax1hd91nv8k"))

(define rust-expect-test-1.5.1
  (crate-source "expect-test" "1.5.1"
                "1c5c081ykm4k5rlsam9jw56w4wgs2h7r4aj78zxlis1i8kzl7bv3"))

(define rust-eyre-0.6.12
  (crate-source "eyre" "0.6.12"
                "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))

(define rust-fancy-regex-0.13.0
  (crate-source "fancy-regex" "0.13.0"
                "1wjbqjsdj8fkq6z2i9llq25iaqzd9f208vxnwg8mdbr2ba1lc7jk"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fern-0.7.1
  (crate-source "fern" "0.7.1"
                "0a9v59vcq2fgd6bwgbfl7q6b0zzgxn85y6g384z728wvf1gih5j3"))

(define rust-filetime-0.2.26
  (crate-source "filetime" "0.2.26"
                "1vb3vz83saxr084wjf2032hspx7wfc5ggggnhc15i9kg3g6ha1dw"))

(define rust-find-msvc-tools-0.1.4
  (crate-source "find-msvc-tools" "0.1.4"
                "09x1sfinrz86bkm6i2d85lpsfnxn0w797g5zisv1nwhaz1w1h1aj"))

(define rust-flate2-1.1.2
  (crate-source "flate2" "1.1.2"
                "07abz7v50lkdr5fjw8zaw2v8gm2vbppc0f7nqm8x3v3gb6wpsgaa"))

(define rust-flate2-1.1.5
  (crate-source "flate2" "1.1.5"
                "1yrvxgxyg7mzksmmcd9i7vc3023kbv3zhdsf8mkjm8c5ivfkxqxz"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fs-err-2.11.0
  (crate-source "fs-err" "2.11.0"
                "0hdajzh5sjvvdjg0n15j91mv8ydvb7ff6m909frvdmg1bw81z948"))

(define rust-fs-extra-1.3.0
  (crate-source "fs_extra" "1.3.0"
                "075i25z70j2mz9r7i9p9r521y8xdj81q7skslyb7zhqnnw33fw22"))

(define rust-fsevent-sys-4.1.0
  ;; TODO: Check bundled sources.
  (crate-source "fsevent-sys" "4.1.0"
                "1liz67v8b0gcs8r31vxkvm2jzgl9p14i78yfqx81c8sdv817mvkn"))

(define rust-funty-2.0.0
  (crate-source "funty" "2.0.0"
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-getopts-0.2.23
  (crate-source "getopts" "0.2.23"
                "1ha8a3l3w68yrw3qjfzj0pak0rppf1yghign03iri1llxdisx9nb"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-glob-0.3.3
  (crate-source "glob" "0.3.3"
                "106jpd3syfzjfj2k70mwm0v436qbx96wig98m4q8x071yrq35hhc"))

(define rust-globset-0.4.16
  (crate-source "globset" "0.4.16"
                "1xa9ivqs74imf1q288spxh49g6iw2mn3x9snibdgapazzj6h58al"))

(define rust-globwalk-0.9.1
  (crate-source "globwalk" "0.9.1"
                "0mz7bsa66p2rrgnz3l94ac4kbklh7mq8j30iizyxjy4qyvmn1xqb"))

(define rust-half-2.6.0
  (crate-source "half" "2.6.0"
                "1j83v0xaqvrw50ppn0g33zig0zsbdi7xiqbzgn7sd5al57nrd4a5"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-hashlink-0.9.1
  (crate-source "hashlink" "0.9.1"
                "1byq4nyrflm5s6wdx5qwp96l1qbp2d0nljvrr5yqrsfy51qzz93b"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.5.2
  (crate-source "hermit-abi" "0.5.2"
                "1744vaqkczpwncfy960j2hxrbjl1q01csm84jpd9dajbdr2yy3zw"))

(define rust-hf-hub-0.4.1
  (crate-source "hf-hub" "0.4.1"
                "1dn608nydckazl8fv4igbq2js0rpwjqv8f5rw6wibf2ampva4bqi"))

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

(define rust-html-escape-0.2.13
  (crate-source "html-escape" "0.2.13"
                "0xml3hswv0205fbm5iq7dqiwjkr6d245xkfppwi7wqjdfr4x86kd"))

(define rust-http-1.3.1
  (crate-source "http" "1.3.1"
                "0r95i5h7dr1xadp1ac9453w0s62s27hzkam356nyx2d9mqqmva7l"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.3
  (crate-source "http-body-util" "0.1.3"
                "0jm6jv4gxsnlsi1kzdyffjrj8cfr3zninnxpw73mvkxy4qzdj8dh"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-humantime-2.2.0
  (crate-source "humantime" "2.2.0"
                "17rz8jhh1mcv4b03wnknhv1shwq2v9vhkhlfg884pprsig62l4cv"))

(define rust-hyper-1.7.0
  (crate-source "hyper" "1.7.0"
                "07n59pxzlq621z611cbpvh7p4h9h15v0r7m5wgxygpx02d5aafpb"))

(define rust-hyper-rustls-0.27.7
  (crate-source "hyper-rustls" "0.27.7"
                "0n6g8998szbzhnvcs1b7ibn745grxiqmlpg53xz206v826v3xjg3"))

(define rust-hyper-util-0.1.17
  (crate-source "hyper-util" "0.1.17"
                "1a5fcnz0alrg4lx9xf6ja66ihaab58jnm5msnky804wg39cras9w"))

(define rust-iana-time-zone-0.1.63
  (crate-source "iana-time-zone" "0.1.63"
                "1n171f5lbc7bryzmp1h30zw86zbvl5480aq02z92lcdwvvjikjdh"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-icu-collections-2.0.0
  (crate-source "icu_collections" "2.0.0"
                "0izfgypv1hsxlz1h8fc2aak641iyvkak16aaz5b4aqg3s3sp4010"))

(define rust-icu-locale-core-2.0.0
  (crate-source "icu_locale_core" "2.0.0"
                "02phv7vwhyx6vmaqgwkh2p4kc2kciykv2px6g4h8glxfrh02gphc"))

(define rust-icu-normalizer-2.0.0
  (crate-source "icu_normalizer" "2.0.0"
                "0ybrnfnxx4sf09gsrxri8p48qifn54il6n3dq2xxgx4dw7l80s23"))

(define rust-icu-normalizer-data-2.0.0
  (crate-source "icu_normalizer_data" "2.0.0"
                "1lvjpzxndyhhjyzd1f6vi961gvzhj244nribfpdqxjdgjdl0s880"))

(define rust-icu-properties-2.0.1
  (crate-source "icu_properties" "2.0.1"
                "0az349pjg8f18lrjbdmxcpg676a7iz2ibc09d2wfz57b3sf62v01"))

(define rust-icu-properties-data-2.0.1
  (crate-source "icu_properties_data" "2.0.1"
                "0cnn3fkq6k88w7p86w7hsd1254s4sl783rpz4p6hlccq74a5k119"))

(define rust-icu-provider-2.0.0
  (crate-source "icu_provider" "2.0.0"
                "1bz5v02gxv1i06yhdhs2kbwxkw3ny9r2vvj9j288fhazgfi0vj03"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-ignore-0.4.23
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-imara-diff-0.1.8
  (crate-source "imara-diff" "0.1.8"
                "1lmk5dpha2fhahrnsrgavxn1qz6ydp1w8jz8fpvlb28p89ylplqp"))

(define rust-imperative-1.0.6
  (crate-source "imperative" "1.0.6"
                "1aclnvya7k1adh2q12yhk4rpz97wxfw7mvffr6pgj8gpd99gd899"))

(define rust-indenter-0.3.4
  (crate-source "indenter" "0.3.4"
                "1maq7yl2px9y40f68c2g2gjsq93rabphzp5shinj8nsldplfckcn"))

(define rust-indexmap-2.11.0
  (crate-source "indexmap" "2.11.0"
                "1sb3nmhisf9pdwfcxzqlvx97xifcvlh5g0rqj9j7i7qg8f01jj7j"))

(define rust-indexmap-2.12.0
  (crate-source "indexmap" "2.12.0"
                "17xs7cqf9nzv8iw8yzpvpjh43lcf9492i8a3xfia2ad9lp9ah5v7"))

(define rust-indicatif-0.17.11
  (crate-source "indicatif" "0.17.11"
                "0db2b2r79r9x8x4lysq1ci9xm13c0xg0sqn3z960yh2bk2430fqq"))

(define rust-indicatif-0.18.0
  (crate-source "indicatif" "0.18.0"
                "1kg1wi3x9x15f22q99spfzcg5fzlmhcc5i6aqjxyssyh8vcld9kh"))

(define rust-indoc-2.0.6
  (crate-source "indoc" "2.0.6"
                "1gbn2pkx5sgbd9lp05d2bkqpbfgazi0z3nvharh5ajah11d29izl"))

(define rust-indoc-2.0.7
  (crate-source "indoc" "2.0.7"
                "01np60qdq6lvgh8ww2caajn9j4dibx9n58rvzf7cya1jz69mrkvr"))

(define rust-inotify-0.11.0
  (crate-source "inotify" "0.11.0"
                "1wq8m657rl085cg59p38sc5y62xy9yhhpvxbkd7n1awi4zzwqzgk"))

(define rust-inotify-0.9.6
  (crate-source "inotify" "0.9.6"
                "1zxb04c4qccp8wnr3v04l503qpxzxzzzph61amlqbsslq4z9s1pq"))

(define rust-inotify-sys-0.1.5
  ;; TODO: Check bundled sources.
  (crate-source "inotify-sys" "0.1.5"
                "1syhjgvkram88my04kv03s0zwa66mdwa5v7ddja3pzwvx2sh4p70"))

(define rust-insta-1.43.1
  (crate-source "insta" "1.43.1"
                "0wa3iz1bafg3jwaqkjcs63spyalcm6chnhyxassm9065f3m38j8m"))

(define rust-insta-cmd-0.6.0
  (crate-source "insta-cmd" "0.6.0"
                "1rix5nmswns1p5p5f7pj5l9wvm69awzby0fbkkacwp4j4ylyzvpz"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

(define rust-iri-string-0.7.8
  (crate-source "iri-string" "0.7.8"
                "1cl0wfq97wq4s1p4dl0ix5cfgsc5fn7l22ljgw9ab9x1qglypifv"))

(define rust-is-docker-0.2.0
  (crate-source "is-docker" "0.2.0"
                "1cyibrv6817cqcpf391m327ss40xlbik8wxcv5h9pj9byhksx2wj"))

(define rust-is-macro-0.3.7
  (crate-source "is-macro" "0.3.7"
                "1r5hvxy697qrrp284qg1f9pyrq7i3mzn1r1qfxj24k728zja6mqx"))

(define rust-is-terminal-0.4.16
  (crate-source "is-terminal" "0.4.16"
                "1acm63whnpwiw1padm9bpqz04sz8msymrmyxc55mvlq8hqqpykg0"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-is-wsl-0.4.0
  (crate-source "is-wsl" "0.4.0"
                "19bs5pq221d4bknnwiqqkqrnsx2in0fsk8fylxm1747iim4hjdhp"))

(define rust-itertools-0.10.5
  (crate-source "itertools" "0.10.5"
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))

(define rust-itertools-0.11.0
  (crate-source "itertools" "0.11.0"
                "0mzyqcc59azx9g5cg6fs8k529gvh4463smmka6jvzs3cd2jp7hdi"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-jiff-0.2.15
  (crate-source "jiff" "0.2.15"
                "0jby6kbs2ra33ji0rx4swcp66jzmcvgszc5v4izwfsgbn6w967xy"))

(define rust-jiff-static-0.2.15
  (crate-source "jiff-static" "0.2.15"
                "1d4l4pvlhz3w487gyhnzvagpbparspv4c8f35qk6g5w9zx8k8d03"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-jod-thread-0.1.2
  (crate-source "jod-thread" "0.1.2"
                "1bj7g6l59ybcf33znf80ccqbxvs1cmd8ynd4m8h7ywdqk473c8wb"))

(define rust-js-sys-0.3.77
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-js-sys-0.3.81
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.81"
                "01ckbf16iwh7qj92fax9zh8vf2y9sk60cli6999cn7a1jxx96j7c"))

(define rust-kqueue-1.1.1
  (crate-source "kqueue" "1.1.1"
                "0sjrsnza8zxr1zfpv6sa0zapd54kx9wlijrz9apqvs6wsw303hza"))

(define rust-kqueue-sys-1.0.4
  ;; TODO: Check bundled sources.
  (crate-source "kqueue-sys" "1.0.4"
                "12w3wi90y4kwis4k9g6fp0kqjdmc6l00j16g8mgbhac7vbzjb5pd"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lexical-parse-float-0.8.5
  (crate-source "lexical-parse-float" "0.8.5"
                "0py0gp8hlzcrlvjqmqlpl2v1as65iiqxq2xsabxvhc01pmg3lfv8"))

(define rust-lexical-parse-integer-0.8.6
  (crate-source "lexical-parse-integer" "0.8.6"
                "1sayji3mpvb2xsjq56qcq3whfz8px9a6fxk5v7v15hyhbr4982bd"))

(define rust-lexical-util-0.8.5
  (crate-source "lexical-util" "0.8.5"
                "1z73qkv7yxhsbc4aiginn1dqmsj8jarkrdlyxc88g2gz2vzvjmaj"))

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-libc-0.2.177
  (crate-source "libc" "0.2.177"
                "0xjrn69cywaii1iq2lib201bhlvan7czmrm604h5qcm28yps4x18"))

(define rust-libcst-1.8.2
  (crate-source "libcst" "1.8.2"
                "0k386nxrl6ppvmjwy547mlrdkj6bjnh1q18xl9332ghcp72xsa5f"))

(define rust-libcst-derive-1.8.2
  (crate-source "libcst_derive" "1.8.2"
                "18q0kcqw3m7n7vpa7c8xiyvc8qmji11qbcchf97qmkrbyv1fabfw"))

(define rust-libfuzzer-sys-0.4.10
  ;; TODO: Check bundled sources.
  (crate-source "libfuzzer-sys" "0.4.10"
                "0124z86582vyzl8gbadqscjgf9i94jcpa9mxcpsyxjvh3w71jdsh"))

(define rust-libloading-0.8.9
  (crate-source "libloading" "0.8.9"
                "0mfwxwjwi2cf0plxcd685yxzavlslz7xirss3b9cbrzyk4hv1i6p"))

(define rust-libmimalloc-sys-0.1.43
  ;; TODO: Check bundled sources.
  (crate-source "libmimalloc-sys" "0.1.43"
                "0kbdw7d5vp7v70n8l6w3mvm6dbd3l50zdqnvh4biq9fyx5kwv25z"))

(define rust-libredox-0.1.10
  (crate-source "libredox" "0.1.10"
                "1jswil4ai90s4rh91fg8580x8nikni1zl3wnch4h01nvidqpwvs1"))

(define rust-libredox-0.1.9
  (crate-source "libredox" "0.1.9"
                "1qqczzfqcc3sw3bl7la6qv7i9hy1s7sxhxmdvpxkfgdd3c9904ir"))

(define rust-libtest-mimic-0.7.3
  (crate-source "libtest-mimic" "0.7.3"
                "0n4vdf4wz4zglammhdzgwxqal9v1a8gbj6rc4q22jfjvxm2xl2yc"))

(define rust-linux-raw-sys-0.9.4
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-litemap-0.8.0
  (crate-source "litemap" "0.8.0"
                "0mlrlskwwhirxk3wsz9psh6nxcy491n0dh8zl02qgj0jzpssw7i4"))

(define rust-lock-api-0.4.13
  (crate-source "lock_api" "0.4.13"
                "0rd73p4299mjwl4hhlfj9qr88v3r0kc8s1nszkfmnq2ky43nb4wn"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

(define rust-lru-slab-0.1.2
  (crate-source "lru-slab" "0.1.2"
                "0m2139k466qj3bnpk66bwivgcx3z88qkxvlzk70vd65jq373jaqi"))

(define rust-lsp-server-0.7.9
  (crate-source "lsp-server" "0.7.9"
                "0bmsw09xcp9hrfmj9741l8ylvy6g0pd2syv3wz5h69xwilsdlskx"))

(define rust-lsp-types-0.95.1.3512a9f
  ;; TODO: Define standalone package if this is a workspace.
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/astral-sh/lsp-types.git")
                        (commit "3512a9f33eadc5402cfab1b8f7340824c8ca1439")))
    (file-name (git-file-name "rust-lsp-types" "0.95.1.3512a9f"))
    (sha256 (base32 "05275sxngzx4z0rszjf1qnkmy8r1rq1a9z8dx1i7andiszvpds7h"))))

(define rust-macro-rules-attribute-0.2.2
  (crate-source "macro_rules_attribute" "0.2.2"
                "0835cx5bdsj06yffaspqqlids57bn3cwxp0x1g6l10394dwrs135"))

(define rust-macro-rules-attribute-proc-macro-0.2.2
  (crate-source "macro_rules_attribute-proc_macro" "0.2.2"
                "0c1s3lgkrdl5l2zmz6jc5g90zkq5w9islgn19alc86vmi7ddy3v7"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-matches-0.1.10
  (crate-source "matches" "0.1.10"
                "1994402fq4viys7pjhzisj4wcw894l53g798kkm2y74laxk0jci5"))

(define rust-matchit-0.8.6
  (crate-source "matchit" "0.8.6"
                "1y9fb1bndpy5hsgz50cwc0vs844k7gqjnd23mqah45sf1kg6m4ig"))

(define rust-matrixmultiply-0.3.10
  (crate-source "matrixmultiply" "0.3.10"
                "020sqwg3cvprfasbszqbnis9zx6c3w9vlkfidyimgblzdq0y6vd0"))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memmap2-0.9.8
  (crate-source "memmap2" "0.9.8"
                "1dqxjs89krh8cin0k7ksqc9myw9yni9kn8d8cllwq4fn1isrhfl4"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-mimalloc-0.1.47
  (crate-source "mimalloc" "0.1.47"
                "0h5wyqdywhgrpbbgknv9iwazf885fvv20vzhcibsz58y22z1qydi"))

(define rust-minicov-0.3.7
  (crate-source "minicov" "0.3.7"
                "0jsvi62lklfyvdmsiizipkqcfpsc7h4c4illgxlf28iwrkqyjzzj"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-0.8.11
  (crate-source "mio" "0.8.11"
                "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))

(define rust-mio-1.0.4
  (crate-source "mio" "1.0.4"
                "073n3kam3nz8j8had35fd2nn7j6a33pi3y5w3kq608cari2d9gkq"))

(define rust-mio-1.1.0
  (crate-source "mio" "1.1.0"
                "0wr816q3jrjwiajvw807lgi540i9s6r78a5fx4ycz3nwhq03pn39"))

(define rust-monostate-0.1.14
  (crate-source "monostate" "0.1.14"
                "1vpv8d9j8i7wachlcrpbwsy1rvzimpncgv8gwpil4mn7s3lipzma"))

(define rust-monostate-0.1.18
  (crate-source "monostate" "0.1.18"
                "0rzgfqn1p7lfrx6hv6pnkdffkc5sgckbf5wgj3qvxmf9yrrs4h9k"))

(define rust-monostate-impl-0.1.14
  (crate-source "monostate-impl" "0.1.14"
                "1db3jrnbriivny6cahvhcc9af7w38q846mg1r4r4y82y5l4s80n4"))

(define rust-monostate-impl-0.1.18
  (crate-source "monostate-impl" "0.1.18"
                "1sg7wsfnz8smn8wpd3gl9xbiimbgl978s1jr5ycvymxgh1anvnz4"))

(define rust-natord-1.0.9
  (crate-source "natord" "1.0.9"
                "0z75spwag3ch20841pvfwhh3892i2z2sli4pzp1jgizbipdrd39h"))

(define rust-ndarray-0.15.6
  (crate-source "ndarray" "0.15.6"
                "0cpsm28hyk8qfjs4g9649dprv3hm53z12qqwyyjqbi3yjr72vcdd"))

(define rust-newtype-uuid-1.3.0
  (crate-source "newtype-uuid" "1.3.0"
                "12ci3ihq19x7x2yivjb3sx9irlax57sp29kknq2kkcb35a9r614q"))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-nix-0.30.1
  (crate-source "nix" "0.30.1"
                "1dixahq9hk191g0c2ydc0h1ppxj0xw536y6rl63vlnp06lx3ylkl"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-normalize-line-endings-0.3.0
  (crate-source "normalize-line-endings" "0.3.0"
                "1gp52dfn2glz26a352zra8h04351icf0fkqzw1shkwrgh1vpz031"))

(define rust-notify-6.1.1
  (crate-source "notify" "6.1.1"
                "0bad98r0ilkhhq2jg3zs11zcqasgbvxia8224wpasm74n65vs1b2"))

(define rust-notify-8.2.0
  (crate-source "notify" "8.2.0"
                "1hrb83451vm5cpjw83nz5skgwjg5ara28zq8nxsqbzsif690fgad"))

(define rust-notify-debouncer-mini-0.4.1
  (crate-source "notify-debouncer-mini" "0.4.1"
                "0hwxdbzyx01pzwyld1dk7sc7ak5k3xkjz2l59ppwa7rajwhv4h2x"))

(define rust-notify-types-2.0.0
  (crate-source "notify-types" "2.0.0"
                "0pcjm3wnvb7pvzw6mn89csv64ip0xhx857kr8jic5vddi6ljc22y"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-nu-ansi-term-0.50.1
  (crate-source "nu-ansi-term" "0.50.1"
                "16a3isvbxx8pa3lk71h3cq2fsx2d17zzq42j4mhpxy81gl2qx8nl"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-complex-0.4.6
  (crate-source "num-complex" "0.4.6"
                "15cla16mnw12xzf5g041nxbjjm9m85hdgadd5dl5d0b30w9qmy3k"))

(define rust-num-cpus-1.17.0
  (crate-source "num_cpus" "1.17.0"
                "0fxjazlng4z8cgbmsvbzv411wrg7x3hyxdq8nxixgzjswyylppwi"))

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

(define rust-oorandom-11.1.5
  (crate-source "oorandom" "11.1.5"
                "07mlf13z453fq01qff38big1lh83j8l6aaglf63ksqzzqxc0yyfn"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-ordered-float-4.6.0
  (crate-source "ordered-float" "4.6.0"
                "0ldrcgilsiijd141vw51fbkziqmh5fpllil3ydhirjm67wdixdvv"))

(define rust-ordermap-0.5.9
  (crate-source "ordermap" "0.5.9"
                "1i3q4shzddyawgj20mldyj035xc33n0w6p0769wwkj4nv7fgxmig"))

(define rust-os-pipe-1.2.2
  (crate-source "os_pipe" "1.2.2"
                "090jhg0i1pj6l9w9zm0p0r7lv89kpwkj8vqij1iaskmic13mycyv"))

(define rust-os-str-bytes-7.1.1
  (crate-source "os_str_bytes" "7.1.1"
                "0p5dqfb1w2p33h85vxl1qaq7zn0mvcij3sq8gnl12w3mbmxypv33"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-parking-lot-0.12.4
  (crate-source "parking_lot" "0.12.4"
                "04sab1c7304jg8k0d5b2pxbj1fvgzcf69l3n2mfpkdb96vs8pmbh"))

(define rust-parking-lot-core-0.9.11
  (crate-source "parking_lot_core" "0.9.11"
                "19g4d6m5k4ggacinqprnn8xvdaszc3y5smsmbz1adcdmaqm8v0xw"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-path-absolutize-3.1.1
  (crate-source "path-absolutize" "3.1.1"
                "1xc36c5lz187wy452qph3lrr41x8ffgxk1clj2s9b8czwwgkibz4"))

(define rust-path-dedot-3.1.1
  (crate-source "path-dedot" "3.1.1"
                "15wkx8q3vra34fslzlg1lkq7liyxwqrpbxiz44a28wa7w3bhmfh7"))

(define rust-path-slash-0.2.1
  (crate-source "path-slash" "0.2.1"
                "0hjgljv4vy97qqw9gxnwzqhhpysjss2yhdphfccy3c388afhk48y"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-peg-0.8.5
  (crate-source "peg" "0.8.5"
                "0xr42lbgihlwbyfcmjggqv1s3a38agp4j077cd8yqdhv235cya4r"))

(define rust-peg-macros-0.8.5
  (crate-source "peg-macros" "0.8.5"
                "0w8g53c75hz6iy3khc1ja2qm0idpnygjc0xsa9fmpyh2q82ap632"))

(define rust-peg-runtime-0.8.5
  (crate-source "peg-runtime" "0.8.5"
                "1jkgfy5jkjqwqy7h11nn3bmm1qfyn9kq2wnxnlspp4ldhsdwlb8k"))

(define rust-pep440-rs-0.7.3
  (crate-source "pep440_rs" "0.7.3"
                "177vv3fvdsp80x9hi2wigw3hkg7pxq6v4hjzfhrdxqwnyfhmq29i"))

(define rust-pep508-rs-0.9.2
  (crate-source "pep508_rs" "0.9.2"
                "01fcbf9vq8ya3shlsmx04fyz5n7h4vm8ixrgrnnzq8a10qkp5vps"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-pest-2.8.1
  (crate-source "pest" "2.8.1"
                "08s342r6vv6ml5in4jk7pb97wgpf0frcnrvg0sqshn23sdb5zc0x"))

(define rust-pest-derive-2.8.1
  (crate-source "pest_derive" "2.8.1"
                "1g20ma4y29axbjhi3z64ihhpqzmiix71qjn7bs224yd7isg6s1dv"))

(define rust-pest-generator-2.8.1
  (crate-source "pest_generator" "2.8.1"
                "0rj9a20g4bjb4sl3zyzpxqg8mbn8c1kxp0nw08rfp0gp73k09r47"))

(define rust-pest-meta-2.8.1
  (crate-source "pest_meta" "2.8.1"
                "1mf01iln7shbnyxpdfnpf59gzn83nndqjkwiw3yh6n8g2wgi1lgd"))

(define rust-phf-0.11.3
  (crate-source "phf" "0.11.3"
                "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz"))

(define rust-phf-codegen-0.11.3
  (crate-source "phf_codegen" "0.11.3"
                "0si1n6zr93kzjs3wah04ikw8z6npsr39jw4dam8yi9czg2609y5f"))

(define rust-phf-generator-0.11.3
  (crate-source "phf_generator" "0.11.3"
                "0gc4np7s91ynrgw73s2i7iakhb4lzdv1gcyx7yhlc0n214a2701w"))

(define rust-phf-shared-0.11.3
  (crate-source "phf_shared" "0.11.3"
                "1rallyvh28jqd9i916gk5gk2igdmzlgvv5q0l3xbf3m6y8pbrsk7"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-portable-atomic-1.11.1
  (crate-source "portable-atomic" "1.11.1"
                "10s4cx9y3jvw0idip09ar52s2kymq8rq9a668f793shn1ar6fhpq"))

(define rust-portable-atomic-util-0.2.4
  (crate-source "portable-atomic-util" "0.2.4"
                "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))

(define rust-potential-utf-0.1.2
  (crate-source "potential_utf" "0.1.2"
                "11dm6k3krx3drbvhgjw8z508giiv0m09wzl6ghza37176w4c79z5"))

(define rust-potential-utf-0.1.3
  (crate-source "potential_utf" "0.1.3"
                "12mhwvhpvvim6xqp6ifgkh1sniv9j2cmid6axn10fnjvpsnikpw4"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-predicates-3.1.3
  (crate-source "predicates" "3.1.3"
                "0wrm57acvagx0xmh5xffx5xspsr2kbggm698x0vks132fpjrxld5"))

(define rust-predicates-core-1.0.9
  (crate-source "predicates-core" "1.0.9"
                "1yjz144yn3imq2r4mh7k9h0r8wv4yyjjj57bs0zwkscz24mlczkj"))

(define rust-predicates-tree-1.0.12
  (crate-source "predicates-tree" "1.0.12"
                "0p223d9y02ywwxs3yl68kziswz4da4vabz67jfhp7yqx71njvpbj"))

(define rust-pretty-assertions-1.4.1
  (crate-source "pretty_assertions" "1.4.1"
                "0v8iq35ca4rw3rza5is3wjxwsf88303ivys07anc5yviybi31q9s"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-proc-macro2-1.0.103
  (crate-source "proc-macro2" "1.0.103"
                "1s29bz20xl2qk5ffs2mbdqknaj43ri673dz86axdbf47xz25psay"))

(define rust-pyo3-0.20.3
  (crate-source "pyo3" "0.20.3"
                "0cw2pgab0pq5gd98nhv18xgxvyigygspla6c8mfycmwisjbbpgak"))

(define rust-pyo3-0.22.6
  (crate-source "pyo3" "0.22.6"
                "110qrq9yibfv40zzind9p2i87617lhzs379ix0m2065b2qk0c0pl"))

(define rust-pyo3-0.23.5
  (crate-source "pyo3" "0.23.5"
                "0wm8z6jgg18z2cgr99wc34mbkffhcnb50igmq5d1ff6ghpyvyy3p"))

(define rust-pyo3-0.24.2
  (crate-source "pyo3" "0.24.2"
                "06cjzkam3xsxi8q1l4pxi45gya8jb6mhr8isn411mcb6yfc3a875"))

(define rust-pyo3-build-config-0.20.3
  (crate-source "pyo3-build-config" "0.20.3"
                "1ms83n1qa81989c6pakpznifalvxv5fiyyji23732lizvr2mgany"))

(define rust-pyo3-build-config-0.22.6
  (crate-source "pyo3-build-config" "0.22.6"
                "0f4w8waba9cyzllq0dpxpw7qmgic05wdf4k20p8nsi7znmsmfjxi"))

(define rust-pyo3-build-config-0.23.5
  (crate-source "pyo3-build-config" "0.23.5"
                "1yqhw1k466k65rqvy2d4xz2shl0hzkry1xlxinciigzkdvlcpxll"))

(define rust-pyo3-build-config-0.24.2
  (crate-source "pyo3-build-config" "0.24.2"
                "16cr3sxpwgz532a65b6ak1px81l061ck1pmcll7i7jm27x16sqwr"))

(define rust-pyo3-ffi-0.20.3
  (crate-source "pyo3-ffi" "0.20.3"
                "1yja1npmzh4i73jn5dv2rnw7idif8bns51bf3zpx821ys0qjbd32"))

(define rust-pyo3-ffi-0.22.6
  (crate-source "pyo3-ffi" "0.22.6"
                "0dl6zj806rkvs67q2mdgjbnzjhzm8glms46nqx8bpp1c9bqbrdcs"))

(define rust-pyo3-ffi-0.23.5
  (crate-source "pyo3-ffi" "0.23.5"
                "13fxvxijl59vilv39akdzwqd1l7fb6c70f53n27irfy0672b9wg9"))

(define rust-pyo3-ffi-0.24.2
  (crate-source "pyo3-ffi" "0.24.2"
                "0cwyspd1lfnzhwx48b9r48n2vnwhkm0ba19kpiwr4h4wpa9czybq"))

(define rust-pyo3-macros-0.20.3
  (crate-source "pyo3-macros" "0.20.3"
                "0n61s98qb2qc1wlda3bz4r0wi0vsr9p4lj2yr5g0bf01z8hcf1bk"))

(define rust-pyo3-macros-0.22.6
  (crate-source "pyo3-macros" "0.22.6"
                "0lylczfabgylnfldns6m36vsw98m9sini0wn1gcfda83g64lvlhg"))

(define rust-pyo3-macros-0.23.5
  (crate-source "pyo3-macros" "0.23.5"
                "1nm9i19aff7zn245v35qb0lbr3cxr19zdgcayq84fg7n509j1hpv"))

(define rust-pyo3-macros-0.24.2
  (crate-source "pyo3-macros" "0.24.2"
                "1ngcgh20fc0g77f79v1s0kh05pzykyzg3p27n6kgj8fflsqrr68b"))

(define rust-pyo3-macros-backend-0.20.3
  (crate-source "pyo3-macros-backend" "0.20.3"
                "11b1z7qnbdnd9hy74ds3xcjx3mjkz43mvpnan32ljccwpdl9nzkw"))

(define rust-pyo3-macros-backend-0.22.6
  (crate-source "pyo3-macros-backend" "0.22.6"
                "1gmz3i0sr4fdg7qjvd8ylbkrgbbch9wv955kni903rd17fh13h1n"))

(define rust-pyo3-macros-backend-0.23.5
  (crate-source "pyo3-macros-backend" "0.23.5"
                "0a10yxj41kvjhh9vywzd2zj3h6iwm4bg3mlkw2frrnpks1m759pw"))

(define rust-pyo3-macros-backend-0.24.2
  (crate-source "pyo3-macros-backend" "0.24.2"
                "06lixywqwddr0gg4qk5kgh7ryxkl535hpkym0xb784hhgqfcwbl2"))

(define rust-pyproject-toml-0.13.5
  (crate-source "pyproject-toml" "0.13.5"
                "0qs66c4lw8194fr01wlx1g6zd5kz3nnmiffrc298naa8vih623vv"))

(define rust-python3-dll-a-0.2.14
  (crate-source "python3-dll-a" "0.2.14"
                "1n40cyv71pri995yrbbkiz95ran6fgklv2jzz6jls2z778qyz0fk"))

(define rust-quick-junit-0.5.1
  (crate-source "quick-junit" "0.5.1"
                "1mxczpzhcnj3gkd22q448339lln6i1md0fhhaxr325hs769sdl9y"))

(define rust-quick-xml-0.37.5
  (crate-source "quick-xml" "0.37.5"
                "1yxpd7rc2qn6f4agfj47ps2z89vv7lvzxpzawqirix8bmyhrf7ik"))

(define rust-quickcheck-1.0.3
  (crate-source "quickcheck" "1.0.3"
                "1mjhkfqwrb8mdyxdqr4zzbj1rm5dfx25n9zcc25lb6fxwiw673sq"))

(define rust-quickcheck-macros-1.1.0
  (crate-source "quickcheck_macros" "1.1.0"
                "0jn17bziphar3kmn2kw445a2vba1p3wycarnsf49ligq8a5y67pp"))

(define rust-quinn-0.11.9
  (crate-source "quinn" "0.11.9"
                "086gzj666dr3slmlynkvxlndy28hahgl361d6bf93hk3i6ahmqmr"))

(define rust-quinn-proto-0.11.13
  (crate-source "quinn-proto" "0.11.13"
                "0cca3mgja9p4w66f6sl1kfhj8rdf4mwsg1jxzssh9g63n14np47i"))

(define rust-quinn-udp-0.5.14
  (crate-source "quinn-udp" "0.5.14"
                "1gacawr17a2zkyri0r3m0lc9spzmxbq1by3ilyb8v2mdvjhcdpmd"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-quote-1.0.41
  (crate-source "quote" "1.0.41"
                "1lg108nb57lwbqlnpsii89cchk6i8pkcvrv88xh1p7a9gdz7c9ff"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-radium-0.7.0
  (crate-source "radium" "0.7.0"
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.2
  (crate-source "rand" "0.9.2"
                "1lah73ainvrgl7brcxx0pwhpnqa3sm3qaj672034jz8i0q7pgckd"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.9.3
  (crate-source "rand_core" "0.9.3"
                "0f3xhf16yks5ic6kmgxcpv1ngdhp48mmfy4ag82i1wnwh8ws3ncr"))

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

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-redox-users-0.5.2
  (crate-source "redox_users" "0.5.2"
                "1b17q7gf7w8b1vvl53bxna24xl983yn7bd00gfbii74bcg30irm4"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-1.12.2
  (crate-source "regex" "1.12.2"
                "1m14zkg6xmkb0q5ah3y39cmggclsjdr1wpxfa4kf5wvm3wcw0fw4"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.13
  (crate-source "regex-automata" "0.4.13"
                "070z0j23pjfidqz0z89id1fca4p572wxpcr20a0qsv68bbrclxjj"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-regex-syntax-0.8.8
  (crate-source "regex-syntax" "0.8.8"
                "0n7ggnpk0r32rzgnycy5xrc1yp2kq19m6pz98ch3c6dkaxw9hbbs"))

(define rust-reqwest-0.12.24
  (crate-source "reqwest" "0.12.24"
                "0vx3f2n6hfnv81y66v5wayrqh6jlzz4gakky88m0hywz1d0lc2cx"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-ron-0.7.1
  (crate-source "ron" "0.7.1"
                "06iz51r6pyi197jjpfddq8h8884y85myaswfan07cnqylqwkj1w8"))

(define rust-rust-stemmers-1.2.0
  (crate-source "rust-stemmers" "1.2.0"
                "0m6acgdflrrcm17dj7lp7x4sfqqhga24qynv660qinwz04v20sp4"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustix-1.0.8
  (crate-source "rustix" "1.0.8"
                "1j6ajqi61agdnh1avr4bplrsgydjw1n4mycdxw3v8g94pyx1y60i"))

(define rust-rustls-0.23.34
  (crate-source "rustls" "0.23.34"
                "19vzmdybp5rlgr0bjb4fykp28w2d6fkqq150aamqykrbxvlqd5ba"))

(define rust-rustls-pki-types-1.12.0
  (crate-source "rustls-pki-types" "1.12.0"
                "0yawbdpix8jif6s8zj1p2hbyb7y3bj66fhx0y7hyf4qh4964m6i2"))

(define rust-rustls-webpki-0.103.7
  (crate-source "rustls-webpki" "0.103.7"
                "1gqlsd0yqiqch97g0wbsnbmyrp75j6nbzfpf8dmhxa78j50ky2z1"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-safetensors-0.5.3
  (crate-source "safetensors" "0.5.3"
                "1s50s455akpz4s8sri6h271i4m0prd1fz3yzyq8s2f6pk1qxn36c"))

(define rust-salsa-0.18.0.88a1d77
  ;; TODO: Define standalone package if this is a workspace.
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/salsa-rs/salsa.git")
                        (commit "88a1d7774d78f048fbd77d40abca9ebd729fd1f0")))
    (file-name (git-file-name "rust-salsa" "0.18.0.88a1d77"))
    (sha256 (base32 "0j5mhvgam19hpxh4a19pkmk5mpgp8zb8w5ayp92qhw6krp4qvibs"))))

(define rust-salsa-macro-rules-0.1.0.88a1d77
  ;; TODO: Define standalone package if this is a workspace.
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/salsa-rs/salsa.git")
                        (commit "88a1d7774d78f048fbd77d40abca9ebd729fd1f0")))
    (file-name (git-file-name "rust-salsa-macro-rules" "0.1.0.88a1d77"))
    (sha256 (base32 "0j5mhvgam19hpxh4a19pkmk5mpgp8zb8w5ayp92qhw6krp4qvibs"))))

(define rust-salsa-macros-0.18.0.88a1d77
  ;; TODO: Define standalone package if this is a workspace.
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/salsa-rs/salsa.git")
                        (commit "88a1d7774d78f048fbd77d40abca9ebd729fd1f0")))
    (file-name (git-file-name "rust-salsa-macros" "0.18.0.88a1d77"))
    (sha256 (base32 "0j5mhvgam19hpxh4a19pkmk5mpgp8zb8w5ayp92qhw6krp4qvibs"))))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-schemars-0.8.22
  (crate-source "schemars" "0.8.22"
                "05an9nbi18ynyxv1rjmwbg6j08j0496hd64mjggh53mwp3hjmgrz"))

(define rust-schemars-derive-0.8.22
  (crate-source "schemars_derive" "0.8.22"
                "0kakyzrp5801s4i043l4ilv96lzimnlh01pap958h66n99w6bqij"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-seahash-4.1.0
  (crate-source "seahash" "4.1.0"
                "0sxsb64np6bvnppjz5hg4rqpnkczhsl8w8kf2a5lr1c08xppn40w"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.219
  (crate-source "serde_derive" "1.0.219"
                "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-derive-internals-0.29.1
  (crate-source "serde_derive_internals" "0.29.1"
                "04g7macx819vbnxhi52cx0nhxi56xlhrybgwybyy7fb9m4h6mlhq"))

(define rust-serde-json-1.0.143
  (crate-source "serde_json" "1.0.143"
                "0njabwzldvj13ykrf1aaf4gh5cgl25kf9hzbpafbv3qh3ppsn0fl"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-pyobject-0.6.2
  (crate-source "serde-pyobject" "0.6.2"
                "1mxpaca0f11mbs7ripdb9xhkk2jlqz28x09fnwnmy6jylr9mhj2c"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-serde-spanned-1.0.0
  (crate-source "serde_spanned" "1.0.0"
                "10rv91337k8x8zmfir4h8aiwmwgkq07gdv7h0jxhcwwgk10lqws0"))

(define rust-serde-test-1.0.177
  (crate-source "serde_test" "1.0.177"
                "1vgisk4dgwdmz4prc2iwq8lddrp4pbqdbljk0h3b7dnafgjix43z"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-serde-wasm-bindgen-0.6.5
  (crate-source "serde-wasm-bindgen" "0.6.5"
                "0sz1l4v8059hiizf5z7r2spm6ws6sqcrs4qgqwww3p7dy1ly20l3"))

(define rust-serde-with-3.14.0
  (crate-source "serde_with" "3.14.0"
                "1manlm83865xwlvgv8frc472x19b75pd89a54mpxpagg3zb5ri7j"))

(define rust-serde-with-macros-3.14.0
  (crate-source "serde_with_macros" "3.14.0"
                "03xk9ghj2s6n331r565mgh22w0749vnq50094nd0vkk5cmg9946y"))

(define rust-sha2-0.10.9
  (crate-source "sha2" "0.10.9"
                "10xjj843v31ghsksd9sl9y12qfc48157j1xpb8v1ml39jy0psl57"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shellexpand-3.1.1
  (crate-source "shellexpand" "3.1.1"
                "1fwhid2r117rbis2f6lj8mpfjf0w6lq6nqfxjha86cb3vmjxy7wb"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-snapbox-0.6.21
  (crate-source "snapbox" "0.6.21"
                "0ss3nd9ky0fkq7idj7jzr22kvkhxz3ylrq9fmiq5sdg3h52zrp4n"))

(define rust-snapbox-macros-0.3.10
  (crate-source "snapbox-macros" "0.3.10"
                "1bv4lq1kw1vrd9lk7yk79a0z8q8nma2502ifysv1p913r99rymhn"))

(define rust-socket2-0.6.1
  (crate-source "socket2" "0.6.1"
                "109qn0kjhqi5zds84qyqi5wn72g8azjhmf4b04fkgkrkd48rw4hp"))

(define rust-socks-0.3.4
  (crate-source "socks" "0.3.4"
                "12ymihhib0zybm6n4mrvh39hj1dm0ya8mqnqdly63079kayxphzh"))

(define rust-spm-precompiled-0.1.4
  (crate-source "spm_precompiled" "0.1.4"
                "09pkdk2abr8xf4pb9kq3rk80dgziq6vzfk7aywv3diik82f6jlaq"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-strip-ansi-escapes-0.2.1
  (crate-source "strip-ansi-escapes" "0.2.1"
                "0980min1s9f5g47rwlq8l9njks952a0jlz0v7yxrm5p7www813ra"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strum-0.26.3
  (crate-source "strum" "0.26.3"
                "01lgl6jvrf4j28v5kmx9bp480ygf1nhvac8b4p7rcj9hxw50zv4g"))

(define rust-strum-macros-0.26.4
  (crate-source "strum_macros" "0.26.4"
                "1gl1wmq24b8md527cpyd5bw9rkbqldd7k1h38kf5ajd2ln2ywssc"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-syn-2.0.108
  (crate-source "syn" "2.0.108"
                "05z908svb0yw5wzrlv27l2i8j1d8l16hd5r8bjh809146myr2n6s"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-tap-1.0.1
  (crate-source "tap" "1.0.1"
                "0sc3gl4nldqpvyhqi3bbd0l9k7fngrcl4zs47n314nqqk4bpx4sm"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-target-lexicon-0.13.3
  (crate-source "target-lexicon" "0.13.3"
                "0355pbycq0cj29h1rp176l57qnfwmygv7hwzchs7iq15gibn4zyz"))

(define rust-target-triple-0.1.4
  (crate-source "target-triple" "0.1.4"
                "140p6rjx7ychv0sryndziia1w14cfjflmhh7ccjj57ar3wvsmj8s"))

(define rust-tempfile-3.21.0
  (crate-source "tempfile" "3.21.0"
                "07kx58ibjk3ydq1gcb7q637fs5zkxaa550lxckhgg9p3427izdhm"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-terminal-size-0.4.3
  (crate-source "terminal_size" "0.4.3"
                "1l7cicmz49c0cyskfp5a389rsai649xi7y032v73475ikjbwpf30"))

(define rust-terminfo-0.9.0
  (crate-source "terminfo" "0.9.0"
                "0qp6rrzkxcg08vjzsim2bw7mid3vi29mizrg70dzbycj0q7q3snl"))

(define rust-termtree-0.5.1
  (crate-source "termtree" "0.5.1"
                "10s610ax6nb70yi7xfmwcb6d3wi9sj5isd0m63gy2pizr2zgwl4g"))

(define rust-test-case-3.3.1
  (crate-source "test-case" "3.3.1"
                "1a380yzm6787737cw7s09jqmkn9035hghahradl2ikdg2gfm09gb"))

(define rust-test-case-core-3.3.1
  (crate-source "test-case-core" "3.3.1"
                "0krqi0gbi1yyycigyjlak63r8h1n0vms7mg3kckqwlfd87c7zjxd"))

(define rust-test-case-macros-3.3.1
  (crate-source "test-case-macros" "3.3.1"
                "1yvgky3qax73bic6m368q04xc955p4a91mddd6b5fk7d04mfg2aw"))

(define rust-test-log-0.2.18
  (crate-source "test-log" "0.2.18"
                "0yxywma018rfr4mb409b1yz4ppg8ir9rg87bd08vx81fb25bjcqy"))

(define rust-test-log-macros-0.2.18
  (crate-source "test-log-macros" "0.2.18"
                "0djzwzwqnalwf00r81lv0yv71s4sqwmx7y7fn40pc3ck552kf6s5"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.16
  (crate-source "thiserror" "2.0.16"
                "1h30bqyjn5s9ypm668yd9849371rzwk185klwgjg503k2hadcrrl"))

(define rust-thiserror-2.0.17
  (crate-source "thiserror" "2.0.17"
                "1j2gixhm2c3s6g96vd0b01v0i0qz1101vfmw0032mdqj1z58fdgn"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.16
  (crate-source "thiserror-impl" "2.0.16"
                "0q3r1ipr1rhff6cgrcvc0njffw17rpcqz9hdc7p754cbqkhinpkc"))

(define rust-thiserror-impl-2.0.17
  (crate-source "thiserror-impl" "2.0.17"
                "04y92yjwg1a4piwk9nayzjfs07sps8c4vq9jnsfq9qvxrn75rw9z"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-threadpool-1.8.1
  (crate-source "threadpool" "1.8.1"
                "1amgfyzvynbm8pacniivzq9r0fh3chhs7kijic81j76l6c5ycl6h"))

(define rust-tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
  ;; TODO: Check bundled sources.
  (crate-source "tikv-jemalloc-sys"
                "0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7"
                "0baf5vjpg9ipa388md4yxim77rdblnk8r95mnp1akbqjcj860g6d"))

(define rust-tikv-jemallocator-0.6.0
  (crate-source "tikv-jemallocator" "0.6.0"
                "0r985npb7d9hrbs3mb0bkfbv0nvzjpgvzsbpyj21bn0qhpqmzv2c"))

(define rust-tinystr-0.8.1
  (crate-source "tinystr" "0.8.1"
                "12sc6h3hnn6x78iycm5v6wrs2xhxph0ydm43yyn7gdfw8l8nsksx"))

(define rust-tinytemplate-1.2.1
  (crate-source "tinytemplate" "1.2.1"
                "1g5n77cqkdh9hy75zdb01adxn45mkh9y40wdr7l68xpz35gnnkdy"))

(define rust-tinyvec-1.10.0
  (crate-source "tinyvec" "1.10.0"
                "1yhk0qdqyiaa4v2j9h8pzax5gxgwpz4da0lcphfil6g6pk1zv9dz"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokenizers-0.15.2
  (crate-source "tokenizers" "0.15.2"
                "0kb5sfrgrdd8yaxn4080fhagsdniahbvz3si6gyyfdmsn1i7km1x"))

(define rust-tokenizers-0.21.1
  (crate-source "tokenizers" "0.21.1"
                "1mqbli35mfkz4rsj7wlf90c95m1mldw7kvnajp49cm4jbwcv6s9i"))

(define rust-tokio-1.48.0
  (crate-source "tokio" "1.48.0"
                "0244qva5pksy8gam6llf7bd6wbk2vkab9lx26yyf08dix810wdpz"))

(define rust-tokio-rustls-0.26.4
  (crate-source "tokio-rustls" "0.26.4"
                "0qggwknz9w4bbsv1z158hlnpkm97j3w8v31586jipn99byaala8p"))

(define rust-tokio-util-0.7.16
  (crate-source "tokio-util" "0.7.16"
                "1r9wdrg1k5hna3m0kc8kcb8jdb6n52g7vnw93kw2xxw4cyc7qc0l"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-0.9.5
  (crate-source "toml" "0.9.5"
                "1s7n4l40hvpf46jmgidfknnzpyblz4hip7gfkymgn2q0qlfrw4km"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.7.0
  (crate-source "toml_datetime" "0.7.0"
                "1qwivxqkjxxwcqsvfhxnphpwphci0grdfk197wyxfn1gj0z1rpms"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-parser-1.0.2
  (crate-source "toml_parser" "1.0.2"
                "042wp5ni22yqcbrfqq9c63g2vbbp4m59zamxw97hvacs8ipqhldm"))

(define rust-toml-write-0.1.2
  (crate-source "toml_write" "0.1.2"
                "008qlhqlqvljp1gpp9rn5cqs74gwvdgbvs92wnpq8y3jlz4zi6ax"))

(define rust-toml-writer-1.0.2
  (crate-source "toml_writer" "1.0.2"
                "0r7x3m050c66s9lssaq965vmrsxvxj131db4fq0m5vrd3w4l5j7w"))

(define rust-tower-0.5.2
  (crate-source "tower" "0.5.2"
                "1ybmd59nm4abl9bsvy6rx31m4zvzp5rja2slzpn712y9b68ssffh"))

(define rust-tower-http-0.6.6
  (crate-source "tower-http" "0.6.6"
                "1wh51y4rf03f91c6rvli6nwzsarx7097yx6sqlm75ag27pbjzj5d"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-attributes-0.1.30
  (crate-source "tracing-attributes" "0.1.30"
                "00v9bhfgfg3v101nmmy7s3vdwadb7ngc8c1iw6wai9vj9sv3lf41"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

(define rust-tracing-flame-0.2.0
  (crate-source "tracing-flame" "0.2.0"
                "1ad34bhy9gsj0ijn56jsvizydash6zcybbls29g1i2a7w5z13bhb"))

(define rust-tracing-indicatif-0.3.6
  (crate-source "tracing-indicatif" "0.3.6"
                "07cmn4ilw8hdfzc1mirccwkgl160k3x9fhgg7xydj4gy9r181586"))

(define rust-tracing-indicatif-0.3.13
  (crate-source "tracing-indicatif" "0.3.13"
                "0gc2s800fnz7j2ya4zw6i32cvzjmad1yj9vzlhjyzb171qgf3m04"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-tracing-tree-0.4.0
  (crate-source "tracing-tree" "0.4.0"
                "175lqyfp6zq7jbj8m026xdp8p765pzgfdzfxahfggmdhy5wwlngl"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-trybuild-1.0.110
  (crate-source "trybuild" "1.0.110"
                "13ka3x5lyxcplkk06gx3a4hi9m5862rjh2zv2p89yykf4kbmgqij"))

(define rust-tryfn-0.2.3
  (crate-source "tryfn" "0.2.3"
                "15n34ga7a1qyhfz45kph0yqx2gjlx10cb9bkmg4wwsk4kvp45qjz"))

(define rust-typed-arena-2.0.2
  (crate-source "typed-arena" "2.0.2"
                "0shj0jpmglhgw2f1i4b33ycdzwd1z205pbs1rd5wx7ks2qhaxxka"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-ucd-trie-0.1.7
  (crate-source "ucd-trie" "0.1.7"
                "0wc9p07sqwz320848i52nvyjvpsxkx3kv5bfbmm6s35809fdk5i8"))

(define rust-unic-char-property-0.9.0
  (crate-source "unic-char-property" "0.9.0"
                "08g21dn3wwix3ycfl0vrbahn0835nv2q3swm8wms0vwvgm07mid8"))

(define rust-unic-char-range-0.9.0
  (crate-source "unic-char-range" "0.9.0"
                "1g0z7iwvjhqspi6194zsff8vy6i3921hpqcrp3v1813hbwnh5603"))

(define rust-unic-common-0.9.0
  (crate-source "unic-common" "0.9.0"
                "1g1mm954m0zr497dl4kx3vr09yaly290zs33bbl4wrbaba1gzmw0"))

(define rust-unic-ucd-category-0.9.0
  (crate-source "unic-ucd-category" "0.9.0"
                "1h4ixzplc2s441vc8mc4zxliw6qfqh1ziaiv8pa1pzpwyn8lb38v"))

(define rust-unic-ucd-version-0.9.0
  (crate-source "unic-ucd-version" "0.9.0"
                "1i5hnzpfnxkp4ijfk8kvhpvj84bij575ybqx1b6hyigy6wi2zgcn"))

(define rust-unicode-categories-0.1.1
  (crate-source "unicode_categories" "0.1.1"
                "0kp1d7fryxxm7hqywbk88yb9d1avsam9sg76xh36k5qx2arj9v1r"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"))

(define rust-unicode-ident-1.0.20
  (crate-source "unicode-ident" "1.0.20"
                "01lafj17xwizrlvn006zz8ip99hqisf77kjk0a8flfmpmrsynbj6"))

(define rust-unicode-names2-1.3.0
  (crate-source "unicode_names2" "1.3.0"
                "1pdj8zspi52axhq2kmd0icf63cygkl8f90hvz3jlvj42jz53wryi"))

(define rust-unicode-names2-generator-1.3.0
  (crate-source "unicode_names2_generator" "1.3.0"
                "0vn5f32qcjpanfgkrmsc1174yxvnxy9xrmzgjw0i45hhc625n7mr"))

(define rust-unicode-normalization-0.1.24
  (crate-source "unicode-normalization" "0.1.24"
                "0mnrk809z3ix1wspcqy97ld5wxdb31f3xz6nsvg5qcv289ycjcsh"))

(define rust-unicode-normalization-alignments-0.1.12
  (crate-source "unicode-normalization-alignments" "0.1.12"
                "1pk2f3arh3qvdsmrsiri0gr5y5vqpk2gv1yjin0njvh4zbj17xj3"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-width-0.1.14
  (crate-source "unicode-width" "0.1.14"
                "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx"))

(define rust-unicode-width-0.2.1
  (crate-source "unicode-width" "0.2.1"
                "0k0mlq7xy1y1kq6cgv1r2rs2knn6rln3g3af50rhi0dkgp60f6ja"))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-unindent-0.2.4
  (crate-source "unindent" "0.2.4"
                "1wvfh815i6wm6whpdz1viig7ib14cwfymyr1kn3sxk2kyl3y2r3j"))

(define rust-unit-prefix-0.5.1
  (crate-source "unit-prefix" "0.5.1"
                "05rq0asf2f1q5vrcv4bwf0c3y6q20asqkiqpr8wqyrfxyb7h4d1j"))

(define rust-unscanny-0.1.0
  (crate-source "unscanny" "0.1.0"
                "0ivbipc1rnq15fhzgna41p1h01ncq4shycii72f3x5d7czq2mpz9"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-unty-0.0.4
  (crate-source "unty" "0.0.4"
                "1blhyv01qiv5sb72sal3xa1l8nzck3answawxkkiw3fd2x1phjbd"))

(define rust-ureq-2.12.1
  (crate-source "ureq" "2.12.1"
                "07f0qdn6459k4rmdnkivkz0y7j28vxh5c8q8sr0gcxgdfxiadl82"))

(define rust-url-2.5.7
  (crate-source "url" "2.5.7"
                "0nzghdv0kcksyvri0npxbjzyx2ihprks5k590y77bld355m17g08"))

(define rust-urlencoding-2.1.3
  (crate-source "urlencoding" "2.1.3"
                "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8-width-0.1.7
  (crate-source "utf8-width" "0.1.7"
                "1qwj8c0fg8cpn8hq7c9xzz26kdz6ci32bf0madz57a2xi578vgc6"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.18.0
  (crate-source "uuid" "1.18.0"
                "1gn1vlggiwrdpizqcpc5hyxsqz9s5215bbay1b182mqn7rj9ccgk"))

(define rust-uuid-macro-internal-1.18.0
  (crate-source "uuid-macro-internal" "1.18.0"
                "0zmrvbnbvvxx3ksy8xnvc26ip2d7v9wblva3x9gxnxl20q0avdr2"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-ranges-0.1.1
  (crate-source "version-ranges" "0.1.1"
                "0bgl8agnz3k3wsnydiq9qgahf4s0zvdbmbamqczyhazbbi0pkl7q"))

(define rust-virtue-0.0.18
  (crate-source "virtue" "0.0.18"
                "1cgp79pzzs117kjlc3jnnkixbyaqri12j40mx2an41qhrymv27h5"))

(define rust-vt100-0.15.2
  (crate-source "vt100" "0.15.2"
                "1pklc8y984axmxr0cd363srr2d27wd5rj15xlcmkjznvy0xqdkc4"))

(define rust-vte-0.11.1
  (crate-source "vte" "0.11.1"
                "15r1ff4j8ndqj9vsyil3wqwxhhl7jsz5g58f31n0h1wlpxgjn0pm"))

(define rust-vte-0.14.1
  (crate-source "vte" "0.14.1"
                "0xy01fgkzb2080prh2ncd8949hm2248fc5wf1lryhdrhxzbxq7r3"))

(define rust-vte-generate-state-changes-0.1.2
  (crate-source "vte_generate_state_changes" "0.1.2"
                "0biwgpcji3w4llz7h4bi8c2rwqchm9gmyr7dnjki1m853gp9ndif"))

(define rust-wait-timeout-0.2.1
  (crate-source "wait-timeout" "0.2.1"
                "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasi-0.14.2+wasi-0.2.4
  (crate-source "wasi" "0.14.2+wasi-0.2.4"
                "1cwcqjr3dgdq8j325awgk8a715h0hg0f7jqzsb077n4qm6jzk0wn"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasm-bindgen-0.2.100
  (crate-source "wasm-bindgen" "0.2.100"
                "1x8ymcm6yi3i1rwj78myl1agqv2m86i648myy3lc97s9swlqkp0y"))

(define rust-wasm-bindgen-0.2.104
  (crate-source "wasm-bindgen" "0.2.104"
                "0b8f4l6pqm0bz0lj5xgwmchb6977n71vmh7srd0axwg93b011nn1"))

(define rust-wasm-bindgen-backend-0.2.100
  (crate-source "wasm-bindgen-backend" "0.2.100"
                "1ihbf1hq3y81c4md9lyh6lcwbx6a5j0fw4fygd423g62lm8hc2ig"))

(define rust-wasm-bindgen-backend-0.2.104
  (crate-source "wasm-bindgen-backend" "0.2.104"
                "069vnhhn2j4w2gwd8rch6g8d3iwkrgi45fas6i3qm7glcrd9l737"))

(define rust-wasm-bindgen-futures-0.4.50
  (crate-source "wasm-bindgen-futures" "0.4.50"
                "0q8ymi6i9r3vxly551dhxcyai7nc491mspj0j1wbafxwq074fpam"))

(define rust-wasm-bindgen-futures-0.4.54
  (crate-source "wasm-bindgen-futures" "0.4.54"
                "0p5c10vfd7p7c607x3cgyfw9h77z1k33d6zzw2x77k3qwi0qs0vy"))

(define rust-wasm-bindgen-macro-0.2.100
  (crate-source "wasm-bindgen-macro" "0.2.100"
                "01xls2dvzh38yj17jgrbiib1d3nyad7k2yw9s0mpklwys333zrkz"))

(define rust-wasm-bindgen-macro-0.2.104
  (crate-source "wasm-bindgen-macro" "0.2.104"
                "06d1m5bg272h6jabq0snm7c50fifjz6r20f5hqlmz7y5wivh99kw"))

(define rust-wasm-bindgen-macro-support-0.2.100
  (crate-source "wasm-bindgen-macro-support" "0.2.100"
                "1plm8dh20jg2id0320pbmrlsv6cazfv6b6907z19ys4z1jj7xs4a"))

(define rust-wasm-bindgen-macro-support-0.2.104
  (crate-source "wasm-bindgen-macro-support" "0.2.104"
                "1mr18kx7ima1pmsqlkk982q4a0vf3r8s1x6901jb59sd1prd41wz"))

(define rust-wasm-bindgen-shared-0.2.100
  (crate-source "wasm-bindgen-shared" "0.2.100"
                "0gffxvqgbh9r9xl36gprkfnh3w9gl8wgia6xrin7v11sjcxxf18s"))

(define rust-wasm-bindgen-shared-0.2.104
  (crate-source "wasm-bindgen-shared" "0.2.104"
                "1la1xj9v3gmawnlyi7lc3mb3xi447r6frb98hi2fb9m1nb47vmms"))

(define rust-wasm-bindgen-test-0.3.50
  (crate-source "wasm-bindgen-test" "0.3.50"
                "1hsjc60wynlhgw02p32pgb93303pqmsdfxj67gxdkdm37kixbj36"))

(define rust-wasm-bindgen-test-macro-0.3.50
  (crate-source "wasm-bindgen-test-macro" "0.3.50"
                "16znd6wz79v2i3b2sf5n4ld2kcci8br3wcx7z5c9c07sqln09m8p"))

(define rust-wasm-streams-0.4.2
  (crate-source "wasm-streams" "0.4.2"
                "0rddn007hp6k2cm91mm9y33n79b0jxv0c3znzszcvv67hn6ks18m"))

(define rust-web-sys-0.3.77
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.77"
                "1lnmc1ffbq34qw91nndklqqm75rasaffj2g4f8h1yvqqz4pdvdik"))

(define rust-web-sys-0.3.81
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.81"
                "0871ifd79ni9813sp5amk7wb3avznkijlsly2ap4r9r4m4bw8rwk"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-webpki-roots-0.26.11
  (crate-source "webpki-roots" "0.26.11"
                "1agpayg5zzf7m1a01q30jahlgmn5nwggbabdhq0in008pf5c66sj"))

(define rust-webpki-roots-1.0.3
  (crate-source "webpki-roots" "1.0.3"
                "1f49w0s7f3fgczvjri179wh2a9g8jpkmdi5bi5l8p7ylsb031c9j"))

(define rust-which-8.0.0
  (crate-source "which" "8.0.0"
                "07dsqyvvyaqp3dbj4cdl3ib5fxhdf29l6vihm3pcihq666avpynk"))

(define rust-wild-2.2.1
  (crate-source "wild" "2.2.1"
                "1q8hnhmv3fvgx0j7bv8qig00599a15mfsdhgx3hq2ljpiky1l4x3"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.10
  (crate-source "winapi-util" "0.1.10"
                "08hb8rj3aq9lcrfmliqs4l7v9zh6srbcn0376yn0pndkf5qvyy09"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-gnullvm-0.53.1
  (crate-source "windows_aarch64_gnullvm" "0.53.1"
                "0lqvdm510mka9w26vmga7hbkmrw9glzc90l4gya5qbxlm1pl3n59"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-aarch64-msvc-0.53.1
  (crate-source "windows_aarch64_msvc" "0.53.1"
                "01jh2adlwx043rji888b22whx4bm8alrk3khjpik5xn20kl85mxr"))

(define rust-windows-core-0.61.2
  (crate-source "windows-core" "0.61.2"
                "1qsa3iw14wk4ngfl7ipcvdf9xyq456ms7cx2i9iwf406p7fx7zf0"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnu-0.53.1
  (crate-source "windows_i686_gnu" "0.53.1"
                "18wkcm82ldyg4figcsidzwbg1pqd49jpm98crfz0j7nqd6h6s3ln"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-gnullvm-0.53.1
  (crate-source "windows_i686_gnullvm" "0.53.1"
                "030qaxqc4salz6l4immfb6sykc6gmhyir9wzn2w8mxj8038mjwzs"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-i686-msvc-0.53.1
  (crate-source "windows_i686_msvc" "0.53.1"
                "1hi6scw3mn2pbdl30ji5i4y8vvspb9b66l98kkz350pig58wfyhy"))

(define rust-windows-implement-0.60.0
  (crate-source "windows-implement" "0.60.0"
                "0dm88k3hlaax85xkls4gf597ar4z8m5vzjjagzk910ph7b8xszx4"))

(define rust-windows-interface-0.59.1
  (crate-source "windows-interface" "0.59.1"
                "1a4zr8740gyzzhq02xgl6vx8l669jwfby57xgf0zmkcdkyv134mx"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-result-0.3.4
  (crate-source "windows-result" "0.3.4"
                "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))

(define rust-windows-strings-0.4.2
  (crate-source "windows-strings" "0.4.2"
                "0mrv3plibkla4v5kaakc2rfksdd0b14plcmidhbkcfqc78zwkrjn"))

(define rust-windows-sys-0.48.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.3
  (crate-source "windows-targets" "0.53.3"
                "14fwwm136dhs3i1impqrrip7nvkra3bdxa4nqkblj604qhqn1znm"))

(define rust-windows-targets-0.53.5
  (crate-source "windows-targets" "0.53.5"
                "1wv9j2gv3l6wj3gkw5j1kr6ymb5q6dfc42yvydjhv3mqa7szjia9"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnu-0.53.1
  (crate-source "windows_x86_64_gnu" "0.53.1"
                "16d4yiysmfdlsrghndr97y57gh3kljkwhfdbcs05m1jasz6l4f4w"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-gnullvm-0.53.1
  (crate-source "windows_x86_64_gnullvm" "0.53.1"
                "1qbspgv4g3q0vygkg8rnql5c6z3caqv38japiynyivh75ng1gyhg"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-windows-x86-64-msvc-0.53.1
  (crate-source "windows_x86_64_msvc" "0.53.1"
                "0l6npq76vlq4ksn4bwsncpr8508mk0gmznm6wnhjg95d19gzzfyn"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-winsafe-0.0.19
  (crate-source "winsafe" "0.0.19"
                "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-wit-bindgen-rt-0.39.0
  (crate-source "wit-bindgen-rt" "0.39.0"
                "1hd65pa5hp0nl664m94bg554h4zlhrzmkjsf6lsgsb7yc4734hkg"))

(define rust-writeable-0.6.1
  (crate-source "writeable" "0.6.1"
                "1fx29zncvbrqzgz7li88vzdm8zvgwgwy2r9bnjqxya09pfwi0bza"))

(define rust-wyz-0.5.1
  (crate-source "wyz" "0.5.1"
                "1vdrfy7i2bznnzjdl9vvrzljvs4s3qm8bnlgqwln6a941gy61wq5"))

(define rust-yansi-1.0.1
  (crate-source "yansi" "1.0.1"
                "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"))

(define rust-yoke-0.8.0
  (crate-source "yoke" "0.8.0"
                "1k4mfr48vgi7wh066y11b7v1ilakghlnlhw9snzz8vi2p00vnhaz"))

(define rust-yoke-derive-0.8.0
  (crate-source "yoke-derive" "0.8.0"
                "1dha5jrjz9jaq8kmxq1aag86b98zbnm9lyjrihy5sv716sbkrniq"))

(define rust-zerocopy-0.8.26
  (crate-source "zerocopy" "0.8.26"
                "0bvsj0qzq26zc6nlrm3z10ihvjspyngs7n0jw1fz031i7h6xsf8h"))

(define rust-zerocopy-0.8.27
  (crate-source "zerocopy" "0.8.27"
                "0b1870gf2zzlckca69v2k4mqwmf8yh2li37qldnzvvd3by58g508"))

(define rust-zerocopy-derive-0.8.26
  (crate-source "zerocopy-derive" "0.8.26"
                "10aiywi5qkha0mpsnb1zjwi44wl2rhdncaf3ykbp4i9nqm65pkwy"))

(define rust-zerocopy-derive-0.8.27
  (crate-source "zerocopy-derive" "0.8.27"
                "0c9qrylm2p55dvaplxsl24ma48add9qk4y0d6kjbkllaqvcvill8"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.8.2
  (crate-source "zeroize" "1.8.2"
                "1l48zxgcv34d7kjskr610zqsm6j2b4fcr2vfh9jm9j1jgvk58wdr"))

(define rust-zerotrie-0.2.2
  (crate-source "zerotrie" "0.2.2"
                "15gmka7vw5k0d24s0vxgymr2j6zn2iwl12wpmpnpjgsqg3abpw1n"))

(define rust-zerovec-0.11.4
  (crate-source "zerovec" "0.11.4"
                "0fz7j1ns8d86m2fqg2a4bzi5gnh5892bxv4kcr9apwc6a3ajpap7"))

(define rust-zerovec-derive-0.11.1
  (crate-source "zerovec-derive" "0.11.1"
                "13zms8hj7vzpfswypwggyfr4ckmyc7v3di49pmj8r1qcz9z275jv"))

(define rust-zip-0.6.6
  (crate-source "zip" "0.6.6"
                "0qcjbqfvbwxi5g9wbymf2r05cvziic2qqj4xy64q3hp48vi980vn"))

(define rust-zstd-0.11.2+zstd.1.5.2
  (crate-source "zstd" "0.11.2+zstd.1.5.2"
                "1r7xlmgnifhxbfyid8vkcnd5ip16gx9hf89d1l0lzrpc4q1rdk10"))

(define rust-zstd-safe-5.0.2+zstd.1.5.2
  (crate-source "zstd-safe" "5.0.2+zstd.1.5.2"
                "1nzl4q3xl68pq58g9xlym299bvjdii8cl7ix595ym7jgw22maahx"))

(define rust-zstd-sys-2.0.15+zstd.1.5.7
  ;; TODO: Check bundled sources.
  (crate-source "zstd-sys" "2.0.15+zstd.1.5.7"
                "0dx2l7dyw1p7x7g3p1pfd25ip36hr22hvmgixm6cgl4pvlyii0gb"))

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
                                                    rust-tokenizers-0.15.2))
                     (lsp-types =>
                                (list rust-bitflags-1.3.2
                                      rust-displaydoc-0.2.5
                                      rust-form-urlencoded-1.2.2
                                      rust-icu-collections-2.0.0
                                      rust-icu-locale-core-2.0.0
                                      rust-icu-normalizer-2.0.0
                                      rust-icu-normalizer-data-2.0.0
                                      rust-icu-properties-2.0.1
                                      rust-icu-properties-data-2.0.1
                                      rust-icu-provider-2.0.0
                                      rust-idna-1.1.0
                                      rust-idna-adapter-1.2.1
                                      rust-itoa-1.0.15
                                      rust-litemap-0.8.0
                                      rust-memchr-2.7.5
                                      rust-percent-encoding-2.3.2
                                      rust-potential-utf-0.1.2
                                      rust-proc-macro2-1.0.101
                                      rust-quote-1.0.40
                                      rust-ryu-1.0.20
                                      rust-serde-1.0.219
                                      rust-serde-derive-1.0.219
                                      rust-serde-json-1.0.143
                                      rust-serde-repr-0.1.20
                                      rust-smallvec-1.15.1
                                      rust-stable-deref-trait-1.2.0
                                      rust-syn-2.0.106
                                      rust-synstructure-0.13.2
                                      rust-tinystr-0.8.1
                                      rust-unicode-ident-1.0.18
                                      rust-url-2.5.7
                                      rust-utf8-iter-1.0.4
                                      rust-writeable-0.6.1
                                      rust-yoke-0.8.0
                                      rust-yoke-derive-0.8.0
                                      rust-zerofrom-0.1.6
                                      rust-zerofrom-derive-0.1.6
                                      rust-zerotrie-0.2.2
                                      rust-zerovec-0.11.4
                                      rust-zerovec-derive-0.11.1))
                     (ruff =>
                           (list rust-adler2-2.0.1
                            rust-ahash-0.8.12
                            rust-aho-corasick-1.1.3
                            rust-android-system-properties-0.1.5
                            rust-android-tzdata-0.1.1
                            rust-anes-0.1.6
                            rust-annotate-snippets-0.11.5
                            rust-anstream-0.6.20
                            rust-anstyle-1.0.11
                            rust-anstyle-lossy-1.1.4
                            rust-anstyle-parse-0.2.7
                            rust-anstyle-query-1.1.4
                            rust-anstyle-svg-0.1.10
                            rust-anstyle-wincon-3.0.10
                            rust-anyhow-1.0.99
                            rust-append-only-vec-0.1.7
                            rust-arc-swap-1.7.1
                            rust-argfile-0.2.1
                            rust-arrayvec-0.7.6
                            rust-assert-fs-1.1.3
                            rust-autocfg-1.5.0
                            rust-base64-0.13.1
                            rust-bincode-1.3.3
                            rust-bitflags-1.3.2
                            rust-bitflags-2.9.3
                            rust-block-buffer-0.10.4
                            rust-boxcar-0.2.14
                            rust-bstr-1.12.0
                            rust-bumpalo-3.19.0
                            rust-byteorder-1.5.0
                            rust-cachedir-0.3.1
                            rust-camino-1.1.11
                            rust-cast-0.3.0
                            rust-castaway-0.2.4
                            rust-cc-1.2.34
                            rust-cfg-aliases-0.2.1
                            rust-cfg-if-1.0.3
                            rust-chrono-0.4.41
                            rust-ciborium-0.2.2
                            rust-ciborium-io-0.2.2
                            rust-ciborium-ll-0.2.2
                            rust-clap-4.5.45
                            rust-clap-builder-4.5.44
                            rust-clap-complete-4.5.57
                            rust-clap-complete-command-0.6.1
                            rust-clap-complete-nushell-4.5.8
                            rust-clap-derive-4.5.45
                            rust-clap-lex-0.7.5
                            rust-clearscreen-4.0.2
                            rust-codspeed-2.10.1
                            rust-codspeed-criterion-compat-2.10.1
                            rust-codspeed-criterion-compat-walltime-2.10.1
                            rust-colorchoice-1.0.4
                            rust-colored-2.2.0
                            rust-colored-3.0.0
                            rust-compact-str-0.8.1
                            rust-console-0.15.11
                            rust-console-0.16.0
                            rust-console-error-panic-hook-0.1.7
                            rust-console-log-1.0.0
                            rust-core-foundation-sys-0.8.7
                            rust-countme-3.0.1
                            rust-cpufeatures-0.2.17
                            rust-crc32fast-1.5.0
                            rust-criterion-0.5.1
                            rust-criterion-plot-0.5.0
                            rust-crossbeam-0.8.4
                            rust-crossbeam-channel-0.5.15
                            rust-crossbeam-deque-0.8.6
                            rust-crossbeam-epoch-0.9.18
                            rust-crossbeam-queue-0.3.12
                            rust-crossbeam-utils-0.8.21
                            rust-crunchy-0.2.4
                            rust-crypto-common-0.1.6
                            rust-ctrlc-3.4.7
                            rust-darling-0.20.11
                            rust-darling-core-0.20.11
                            rust-darling-macro-0.20.11
                            rust-dashmap-5.5.3
                            rust-dashmap-6.1.0
                            rust-derive-new-0.6.0
                            rust-diff-0.1.13
                            rust-difflib-0.4.0
                            rust-digest-0.10.7
                            rust-dir-test-0.4.1
                            rust-dir-test-macros-0.4.1
                            rust-dirs-6.0.0
                            rust-dirs-sys-0.5.0
                            rust-displaydoc-0.2.5
                            rust-dissimilar-1.0.10
                            rust-doc-comment-0.3.3
                            rust-drop-bomb-0.1.5
                            rust-dunce-1.0.5
                            rust-dyn-clone-1.0.20
                            rust-either-1.15.0
                            rust-encode-unicode-1.0.0
                            rust-env-filter-0.1.3
                            rust-env-home-0.1.0
                            rust-env-logger-0.11.8
                            rust-equivalent-1.0.2
                            rust-errno-0.3.13
                            rust-escape8259-0.5.3
                            rust-escargot-0.5.15
                            rust-etcetera-0.8.0
                            rust-expect-test-1.5.1
                            rust-eyre-0.6.12
                            rust-fastrand-2.3.0
                            rust-fern-0.7.1
                            rust-filetime-0.2.26
                            rust-flate2-1.1.2
                            rust-fnv-1.0.7
                            rust-foldhash-0.1.5
                            rust-form-urlencoded-1.2.2
                            rust-fs-err-2.11.0
                            rust-fsevent-sys-4.1.0
                            rust-generic-array-0.14.7
                            rust-getopts-0.2.23
                            rust-getrandom-0.2.16
                            rust-getrandom-0.3.3
                            rust-glob-0.3.3
                            rust-globset-0.4.16
                            rust-globwalk-0.9.1
                            rust-half-2.6.0
                            rust-hashbrown-0.14.5
                            rust-hashbrown-0.15.5
                            rust-hashlink-0.9.1
                            rust-heck-0.5.0
                            rust-hermit-abi-0.5.2
                            rust-home-0.5.11
                            rust-html-escape-0.2.13
                            rust-iana-time-zone-0.1.63
                            rust-iana-time-zone-haiku-0.1.2
                            rust-icu-collections-2.0.0
                            rust-icu-locale-core-2.0.0
                            rust-icu-normalizer-2.0.0
                            rust-icu-normalizer-data-2.0.0
                            rust-icu-properties-2.0.1
                            rust-icu-properties-data-2.0.1
                            rust-icu-provider-2.0.0
                            rust-ident-case-1.0.1
                            rust-idna-1.1.0
                            rust-idna-adapter-1.2.1
                            rust-ignore-0.4.23
                            rust-imara-diff-0.1.8
                            rust-imperative-1.0.6
                            rust-indenter-0.3.4
                            rust-indexmap-2.11.0
                            rust-indicatif-0.17.11
                            rust-indoc-2.0.6
                            rust-inotify-0.11.0
                            rust-inotify-0.9.6
                            rust-inotify-sys-0.1.5
                            rust-insta-1.43.1
                            rust-insta-cmd-0.6.0
                            rust-is-docker-0.2.0
                            rust-is-macro-0.3.7
                            rust-is-terminal-0.4.16
                            rust-is-terminal-polyfill-1.70.1
                            rust-is-wsl-0.4.0
                            rust-itertools-0.10.5
                            rust-itertools-0.13.0
                            rust-itertools-0.14.0
                            rust-itoa-1.0.15
                            rust-jiff-0.2.15
                            rust-jiff-static-0.2.15
                            rust-jobserver-0.1.34
                            rust-jod-thread-0.1.2
                            rust-js-sys-0.3.77
                            rust-kqueue-1.1.1
                            rust-kqueue-sys-1.0.4
                            rust-lazy-static-1.5.0
                            rust-libc-0.2.175
                            rust-libcst-1.8.2
                            rust-libcst-derive-1.8.2
                            rust-libmimalloc-sys-0.1.43
                            rust-libredox-0.1.9
                            rust-libtest-mimic-0.7.3
                            rust-linux-raw-sys-0.9.4
                            rust-litemap-0.8.0
                            rust-lock-api-0.4.13
                            rust-log-0.4.27
                            rust-lsp-server-0.7.9
                            rust-lsp-types-0.95.1.3512a9f
                            rust-matchers-0.1.0
                            rust-matches-0.1.10
                            rust-matchit-0.8.6
                            rust-memchr-2.7.5
                            rust-mimalloc-0.1.47
                            rust-minicov-0.3.7
                            rust-minimal-lexical-0.2.1
                            rust-miniz-oxide-0.8.9
                            rust-mio-0.8.11
                            rust-mio-1.0.4
                            rust-natord-1.0.9
                            rust-newtype-uuid-1.3.0
                            rust-nix-0.29.0
                            rust-nix-0.30.1
                            rust-nom-7.1.3
                            rust-normalize-line-endings-0.3.0
                            rust-notify-6.1.1
                            rust-notify-8.2.0
                            rust-notify-debouncer-mini-0.4.1
                            rust-notify-types-2.0.0
                            rust-nu-ansi-term-0.46.0
                            rust-nu-ansi-term-0.50.1
                            rust-num-cpus-1.17.0
                            rust-num-traits-0.2.19
                            rust-number-prefix-0.4.0
                            rust-once-cell-1.21.3
                            rust-once-cell-polyfill-1.70.1
                            rust-oorandom-11.1.5
                            rust-option-ext-0.2.0
                            rust-ordered-float-4.6.0
                            rust-ordermap-0.5.9
                            rust-os-pipe-1.2.2
                            rust-os-str-bytes-7.1.1
                            rust-overload-0.1.1
                            rust-parking-lot-0.12.4
                            rust-parking-lot-core-0.9.11
                            rust-paste-1.0.15
                            rust-path-absolutize-3.1.1
                            rust-path-dedot-3.1.1
                            rust-path-slash-0.2.1
                            rust-pathdiff-0.2.3
                            rust-peg-0.8.5
                            rust-peg-macros-0.8.5
                            rust-peg-runtime-0.8.5
                            rust-pep440-rs-0.7.3
                            rust-pep508-rs-0.9.2
                            rust-percent-encoding-2.3.2
                            rust-pest-2.8.1
                            rust-pest-derive-2.8.1
                            rust-pest-generator-2.8.1
                            rust-pest-meta-2.8.1
                            rust-phf-0.11.3
                            rust-phf-codegen-0.11.3
                            rust-phf-generator-0.11.3
                            rust-phf-shared-0.11.3
                            rust-pin-project-lite-0.2.16
                            rust-pkg-config-0.3.32
                            rust-portable-atomic-1.11.1
                            rust-portable-atomic-util-0.2.4
                            rust-potential-utf-0.1.2
                            rust-ppv-lite86-0.2.21
                            rust-predicates-3.1.3
                            rust-predicates-core-1.0.9
                            rust-predicates-tree-1.0.12
                            rust-pretty-assertions-1.4.1
                            rust-proc-macro2-1.0.101
                            rust-pyproject-toml-0.13.5
                            rust-quick-junit-0.5.1
                            rust-quick-xml-0.37.5
                            rust-quickcheck-1.0.3
                            rust-quickcheck-macros-1.1.0
                            rust-quote-1.0.40
                            rust-r-efi-5.3.0
                            rust-rand-0.8.5
                            rust-rand-0.9.2
                            rust-rand-chacha-0.3.1
                            rust-rand-chacha-0.9.0
                            rust-rand-core-0.6.4
                            rust-rand-core-0.9.3
                            rust-rayon-1.11.0
                            rust-rayon-core-1.13.0
                            rust-redox-syscall-0.5.17
                            rust-redox-users-0.5.2
                            rust-regex-1.11.1
                            rust-regex-automata-0.1.10
                            rust-regex-automata-0.4.9
                            rust-regex-syntax-0.6.29
                            rust-regex-syntax-0.8.5
                            rust-ron-0.7.1
                            rust-rust-stemmers-1.2.0
                            rust-rustc-hash-1.1.0
                            rust-rustc-hash-2.1.1
                            rust-rustix-1.0.8
                            rust-rustversion-1.0.22
                            rust-ryu-1.0.20
                            rust-salsa-0.18.0.88a1d77
                            rust-salsa-macro-rules-0.1.0.88a1d77
                            rust-salsa-macros-0.18.0.88a1d77
                            rust-same-file-1.0.6
                            rust-schemars-0.8.22
                            rust-schemars-derive-0.8.22
                            rust-scopeguard-1.2.0
                            rust-seahash-4.1.0
                            rust-serde-1.0.219
                            rust-serde-derive-1.0.219
                            rust-serde-derive-internals-0.29.1
                            rust-serde-json-1.0.143
                            rust-serde-repr-0.1.20
                            rust-serde-spanned-0.6.9
                            rust-serde-spanned-1.0.0
                            rust-serde-test-1.0.177
                            rust-serde-wasm-bindgen-0.6.5
                            rust-serde-with-3.14.0
                            rust-serde-with-macros-3.14.0
                            rust-sha2-0.10.9
                            rust-sharded-slab-0.1.7
                            rust-shellexpand-3.1.1
                            rust-shlex-1.3.0
                            rust-similar-2.7.0
                            rust-siphasher-1.0.1
                            rust-smallvec-1.15.1
                            rust-snapbox-0.6.21
                            rust-snapbox-macros-0.3.10
                            rust-stable-deref-trait-1.2.0
                            rust-static-assertions-1.1.0
                            rust-strip-ansi-escapes-0.2.1
                            rust-strsim-0.11.1
                            rust-strum-0.26.3
                            rust-strum-macros-0.26.4
                            rust-syn-2.0.106
                            rust-synstructure-0.13.2
                            rust-target-triple-0.1.4
                            rust-tempfile-3.21.0
                            rust-termcolor-1.4.1
                            rust-terminal-size-0.4.3
                            rust-terminfo-0.9.0
                            rust-termtree-0.5.1
                            rust-test-case-3.3.1
                            rust-test-case-core-3.3.1
                            rust-test-case-macros-3.3.1
                            rust-test-log-0.2.18
                            rust-test-log-macros-0.2.18
                            rust-thiserror-1.0.69
                            rust-thiserror-2.0.16
                            rust-thiserror-impl-1.0.69
                            rust-thiserror-impl-2.0.16
                            rust-thread-local-1.1.9
                            rust-threadpool-1.8.1
                            rust-tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
                            rust-tikv-jemallocator-0.6.0
                            rust-tinystr-0.8.1
                            rust-tinytemplate-1.2.1
                            rust-tinyvec-1.10.0
                            rust-tinyvec-macros-0.1.1
                            rust-toml-0.8.23
                            rust-toml-0.9.5
                            rust-toml-datetime-0.6.11
                            rust-toml-datetime-0.7.0
                            rust-toml-edit-0.22.27
                            rust-toml-parser-1.0.2
                            rust-toml-write-0.1.2
                            rust-toml-writer-1.0.2
                            rust-tracing-0.1.41
                            rust-tracing-attributes-0.1.30
                            rust-tracing-core-0.1.34
                            rust-tracing-flame-0.2.0
                            rust-tracing-indicatif-0.3.6
                            rust-tracing-log-0.2.0
                            rust-tracing-subscriber-0.3.19
                            rust-tracing-tree-0.4.0
                            rust-trybuild-1.0.110
                            rust-tryfn-0.2.3
                            rust-typed-arena-2.0.2
                            rust-typenum-1.18.0
                            rust-ucd-trie-0.1.7
                            rust-unic-char-property-0.9.0
                            rust-unic-char-range-0.9.0
                            rust-unic-common-0.9.0
                            rust-unic-ucd-category-0.9.0
                            rust-unic-ucd-version-0.9.0
                            rust-unicode-ident-1.0.18
                            rust-unicode-names2-1.3.0
                            rust-unicode-names2-generator-1.3.0
                            rust-unicode-normalization-0.1.24
                            rust-unicode-width-0.1.14
                            rust-unicode-width-0.2.1
                            rust-unit-prefix-0.5.1
                            rust-unscanny-0.1.0
                            rust-url-2.5.7
                            rust-urlencoding-2.1.3
                            rust-utf8-iter-1.0.4
                            rust-utf8-width-0.1.7
                            rust-utf8parse-0.2.2
                            rust-uuid-1.18.0
                            rust-uuid-macro-internal-1.18.0
                            rust-valuable-0.1.1
                            rust-version-check-0.9.5
                            rust-version-ranges-0.1.1
                            rust-vt100-0.15.2
                            rust-vte-0.11.1
                            rust-vte-0.14.1
                            rust-vte-generate-state-changes-0.1.2
                            rust-wait-timeout-0.2.1
                            rust-walkdir-2.5.0
                            rust-wasi-0.11.1+wasi-snapshot-preview1
                            rust-wasi-0.14.2+wasi-0.2.4
                            rust-wasm-bindgen-0.2.100
                            rust-wasm-bindgen-backend-0.2.100
                            rust-wasm-bindgen-futures-0.4.50
                            rust-wasm-bindgen-macro-0.2.100
                            rust-wasm-bindgen-macro-support-0.2.100
                            rust-wasm-bindgen-shared-0.2.100
                            rust-wasm-bindgen-test-0.3.50
                            rust-wasm-bindgen-test-macro-0.3.50
                            rust-web-sys-0.3.77
                            rust-web-time-1.1.0
                            rust-which-8.0.0
                            rust-wild-2.2.1
                            rust-winapi-0.3.9
                            rust-winapi-i686-pc-windows-gnu-0.4.0
                            rust-winapi-util-0.1.10
                            rust-winapi-x86-64-pc-windows-gnu-0.4.0
                            rust-windows-aarch64-gnullvm-0.48.5
                            rust-windows-aarch64-gnullvm-0.52.6
                            rust-windows-aarch64-gnullvm-0.53.0
                            rust-windows-aarch64-msvc-0.48.5
                            rust-windows-aarch64-msvc-0.52.6
                            rust-windows-aarch64-msvc-0.53.0
                            rust-windows-core-0.61.2
                            rust-windows-i686-gnu-0.48.5
                            rust-windows-i686-gnu-0.52.6
                            rust-windows-i686-gnu-0.53.0
                            rust-windows-i686-gnullvm-0.52.6
                            rust-windows-i686-gnullvm-0.53.0
                            rust-windows-i686-msvc-0.48.5
                            rust-windows-i686-msvc-0.52.6
                            rust-windows-i686-msvc-0.53.0
                            rust-windows-implement-0.60.0
                            rust-windows-interface-0.59.1
                            rust-windows-link-0.1.3
                            rust-windows-result-0.3.4
                            rust-windows-strings-0.4.2
                            rust-windows-sys-0.48.0
                            rust-windows-sys-0.52.0
                            rust-windows-sys-0.59.0
                            rust-windows-sys-0.60.2
                            rust-windows-targets-0.48.5
                            rust-windows-targets-0.52.6
                            rust-windows-targets-0.53.3
                            rust-windows-x86-64-gnu-0.48.5
                            rust-windows-x86-64-gnu-0.52.6
                            rust-windows-x86-64-gnu-0.53.0
                            rust-windows-x86-64-gnullvm-0.48.5
                            rust-windows-x86-64-gnullvm-0.52.6
                            rust-windows-x86-64-gnullvm-0.53.0
                            rust-windows-x86-64-msvc-0.48.5
                            rust-windows-x86-64-msvc-0.52.6
                            rust-windows-x86-64-msvc-0.53.0
                            rust-winnow-0.7.13
                            rust-winsafe-0.0.19
                            rust-wit-bindgen-rt-0.39.0
                            rust-writeable-0.6.1
                            rust-yansi-1.0.1
                            rust-yoke-0.8.0
                            rust-yoke-derive-0.8.0
                            rust-zerocopy-0.8.26
                            rust-zerocopy-derive-0.8.26
                            rust-zerofrom-0.1.6
                            rust-zerofrom-derive-0.1.6
                            rust-zerotrie-0.2.2
                            rust-zerovec-0.11.4
                            rust-zerovec-derive-0.11.1
                            rust-zip-0.6.6
                            rust-zstd-0.11.2+zstd.1.5.2
                            rust-zstd-safe-5.0.2+zstd.1.5.2
                            rust-zstd-sys-2.0.15+zstd.1.5.7))
                     (salsa =>
                            (list rust-ahash-0.8.12
                             rust-aho-corasick-1.1.3
                             rust-anes-0.1.6
                             rust-annotate-snippets-0.11.5
                             rust-anstream-0.6.20
                             rust-anstyle-1.0.11
                             rust-anstyle-parse-0.2.7
                             rust-anstyle-query-1.1.4
                             rust-anstyle-wincon-3.0.10
                             rust-append-only-vec-0.1.7
                             rust-arc-swap-1.7.1
                             rust-autocfg-1.5.0
                             rust-bitflags-1.3.2
                             rust-bitflags-2.9.3
                             rust-bumpalo-3.19.0
                             rust-cast-0.3.0
                             rust-cfg-if-1.0.3
                             rust-ciborium-0.2.2
                             rust-ciborium-io-0.2.2
                             rust-ciborium-ll-0.2.2
                             rust-clap-4.5.45
                             rust-clap-builder-4.5.44
                             rust-clap-lex-0.7.5
                             rust-codspeed-2.10.1
                             rust-codspeed-criterion-compat-2.10.1
                             rust-codspeed-criterion-compat-walltime-2.10.1
                             rust-colorchoice-1.0.4
                             rust-colored-2.2.0
                             rust-criterion-plot-0.5.0
                             rust-crossbeam-0.8.4
                             rust-crossbeam-channel-0.5.15
                             rust-crossbeam-deque-0.8.6
                             rust-crossbeam-epoch-0.9.18
                             rust-crossbeam-queue-0.3.12
                             rust-crossbeam-utils-0.8.21
                             rust-crunchy-0.2.4
                             rust-dashmap-6.1.0
                             rust-derive-new-0.6.0
                             rust-dissimilar-1.0.10
                             rust-either-1.15.0
                             rust-env-filter-0.1.3
                             rust-env-logger-0.11.8
                             rust-equivalent-1.0.2
                             rust-expect-test-1.5.1
                             rust-eyre-0.6.12
                             rust-filetime-0.2.26
                             rust-fsevent-sys-4.1.0
                             rust-getrandom-0.3.3
                             rust-glob-0.3.3
                             rust-half-2.6.0
                             rust-hashbrown-0.14.5
                             rust-hashbrown-0.15.5
                             rust-hashlink-0.9.1
                             rust-heck-0.5.0
                             rust-hermit-abi-0.5.2
                             rust-indenter-0.3.4
                             rust-indexmap-2.11.0
                             rust-inotify-0.9.6
                             rust-inotify-sys-0.1.5
                             rust-is-terminal-0.4.16
                             rust-is-terminal-polyfill-1.70.1
                             rust-itertools-0.10.5
                             rust-itoa-1.0.15
                             rust-js-sys-0.3.77
                             rust-kqueue-1.1.1
                             rust-kqueue-sys-1.0.4
                             rust-lazy-static-1.5.0
                             rust-libc-0.2.175
                             rust-libredox-0.1.9
                             rust-lock-api-0.4.13
                             rust-log-0.4.27
                             rust-matchers-0.1.0
                             rust-memchr-2.7.5
                             rust-mio-0.8.11
                             rust-notify-6.1.1
                             rust-notify-debouncer-mini-0.4.1
                             rust-nu-ansi-term-0.46.0
                             rust-num-traits-0.2.19
                             rust-once-cell-1.21.3
                             rust-once-cell-polyfill-1.70.1
                             rust-oorandom-11.1.5
                             rust-ordered-float-4.6.0
                             rust-overload-0.1.1
                             rust-parking-lot-0.12.4
                             rust-parking-lot-core-0.9.11
                             rust-pin-project-lite-0.2.16
                             rust-proc-macro2-1.0.101
                             rust-quote-1.0.40
                             rust-r-efi-5.3.0
                             rust-rayon-1.11.0
                             rust-rayon-core-1.13.0
                             rust-redox-syscall-0.5.17
                             rust-regex-1.11.1
                             rust-regex-automata-0.1.10
                             rust-regex-automata-0.4.9
                             rust-regex-syntax-0.6.29
                             rust-regex-syntax-0.8.5
                             rust-rustc-hash-2.1.1
                             rust-rustversion-1.0.22
                             rust-ryu-1.0.20
                             rust-same-file-1.0.6
                             rust-scopeguard-1.2.0
                             rust-serde-1.0.219
                             rust-serde-derive-1.0.219
                             rust-serde-json-1.0.143
                             rust-serde-spanned-1.0.0
                             rust-sharded-slab-0.1.7
                             rust-smallvec-1.15.1
                             rust-syn-2.0.106
                             rust-synstructure-0.13.2
                             rust-target-triple-0.1.4
                             rust-termcolor-1.4.1
                             rust-test-log-0.2.18
                             rust-test-log-macros-0.2.18
                             rust-thread-local-1.1.9
                             rust-tinytemplate-1.2.1
                             rust-toml-0.9.5
                             rust-toml-datetime-0.7.0
                             rust-toml-parser-1.0.2
                             rust-toml-writer-1.0.2
                             rust-tracing-0.1.41
                             rust-tracing-attributes-0.1.30
                             rust-tracing-core-0.1.34
                             rust-tracing-log-0.2.0
                             rust-tracing-subscriber-0.3.19
                             rust-trybuild-1.0.110
                             rust-unicode-ident-1.0.18
                             rust-unicode-width-0.2.1
                             rust-utf8parse-0.2.2
                             rust-uuid-1.18.0
                             rust-valuable-0.1.1
                             rust-version-check-0.9.5
                             rust-walkdir-2.5.0
                             rust-wasi-0.11.1+wasi-snapshot-preview1
                             rust-wasi-0.14.2+wasi-0.2.4
                             rust-wasm-bindgen-0.2.100
                             rust-wasm-bindgen-backend-0.2.100
                             rust-wasm-bindgen-macro-0.2.100
                             rust-wasm-bindgen-macro-support-0.2.100
                             rust-wasm-bindgen-shared-0.2.100
                             rust-winapi-0.3.9
                             rust-winapi-i686-pc-windows-gnu-0.4.0
                             rust-winapi-util-0.1.10
                             rust-winapi-x86-64-pc-windows-gnu-0.4.0
                             rust-windows-link-0.1.3
                             rust-windows-sys-0.48.0
                             rust-windows-sys-0.59.0
                             rust-windows-sys-0.60.2
                             rust-windows-targets-0.48.5
                             rust-windows-targets-0.52.6
                             rust-windows-targets-0.53.3
                             rust-windows-aarch64-gnullvm-0.48.5
                             rust-windows-aarch64-gnullvm-0.52.6
                             rust-windows-aarch64-gnullvm-0.53.0
                             rust-windows-aarch64-msvc-0.48.5
                             rust-windows-aarch64-msvc-0.52.6
                             rust-windows-aarch64-msvc-0.53.0
                             rust-windows-i686-gnu-0.48.5
                             rust-windows-i686-gnu-0.52.6
                             rust-windows-i686-gnu-0.53.0
                             rust-windows-i686-gnullvm-0.52.6
                             rust-windows-i686-gnullvm-0.53.0
                             rust-windows-i686-msvc-0.48.5
                             rust-windows-i686-msvc-0.52.6
                             rust-windows-i686-msvc-0.53.0
                             rust-windows-x86-64-gnu-0.48.5
                             rust-windows-x86-64-gnu-0.52.6
                             rust-windows-x86-64-gnu-0.53.0
                             rust-windows-x86-64-gnullvm-0.48.5
                             rust-windows-x86-64-gnullvm-0.52.6
                             rust-windows-x86-64-gnullvm-0.53.0
                             rust-windows-x86-64-msvc-0.48.5
                             rust-windows-x86-64-msvc-0.52.6
                             rust-windows-x86-64-msvc-0.53.0
                             rust-winnow-0.7.13
                             rust-wit-bindgen-rt-0.39.0
                             rust-zerocopy-0.8.26
                             rust-zerocopy-derive-0.8.26))
                     (jiter =>
                            (list rust-ahash-0.8.12
                                  rust-arbitrary-1.4.2
                                  rust-autocfg-1.5.0
                                  rust-bencher-0.1.5
                                  rust-bitflags-2.9.3
                                  rust-bitvec-1.0.1
                                  rust-bumpalo-3.19.0
                                  rust-cc-1.2.34
                                  rust-cfg-if-1.0.3
                                  rust-codspeed-2.10.1
                                  rust-codspeed-bencher-compat-2.10.1
                                  rust-colored-2.2.0
                                  rust-equivalent-1.0.2
                                  rust-funty-2.0.0
                                  rust-getrandom-0.3.3
                                  rust-hashbrown-0.15.5
                                  rust-heck-0.5.0
                                  rust-indexmap-2.11.0
                                  rust-indoc-2.0.6
                                  rust-itoa-1.0.15
                                  rust-jobserver-0.1.34
                                  rust-js-sys-0.3.77
                                  rust-lazy-static-1.5.0
                                  rust-lexical-parse-float-0.8.5
                                  rust-lexical-parse-integer-0.8.6
                                  rust-lexical-util-0.8.5
                                  rust-libc-0.2.175
                                  rust-libfuzzer-sys-0.4.10
                                  rust-log-0.4.27
                                  rust-memchr-2.7.5
                                  rust-memoffset-0.9.1
                                  rust-num-bigint-0.4.6
                                  rust-num-integer-0.1.46
                                  rust-num-traits-0.2.19
                                  rust-once-cell-1.21.3
                                  rust-paste-1.0.15
                                  rust-portable-atomic-1.11.1
                                  rust-proc-macro2-1.0.101
                                  rust-pyo3-0.23.5
                                  rust-pyo3-build-config-0.23.5
                                  rust-pyo3-ffi-0.23.5
                                  rust-pyo3-macros-0.23.5
                                  rust-pyo3-macros-backend-0.23.5
                                  rust-python3-dll-a-0.2.14
                                  rust-quote-1.0.40
                                  rust-r-efi-5.3.0
                                  rust-radium-0.7.0
                                  rust-rustversion-1.0.22
                                  rust-ryu-1.0.20
                                  rust-serde-1.0.219
                                  rust-serde-derive-1.0.219
                                  rust-serde-json-1.0.143
                                  rust-shlex-1.3.0
                                  rust-smallvec-1.15.1
                                  rust-static-assertions-1.1.0
                                  rust-syn-2.0.106
                                  rust-tap-1.0.1
                                  rust-target-lexicon-0.12.16
                                  rust-unicode-ident-1.0.18
                                  rust-unindent-0.2.4
                                  rust-uuid-1.18.0
                                  rust-version-check-0.9.5
                                  rust-wasi-0.14.2+wasi-0.2.4
                                  rust-wasm-bindgen-0.2.100
                                  rust-wasm-bindgen-backend-0.2.100
                                  rust-wasm-bindgen-macro-0.2.100
                                  rust-wasm-bindgen-macro-support-0.2.100
                                  rust-wasm-bindgen-shared-0.2.100
                                  rust-windows-sys-0.59.0
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-wit-bindgen-rt-0.39.0
                                  rust-wyz-0.5.1
                                  rust-zerocopy-0.8.26
                                  rust-zerocopy-derive-0.8.26))
                     (outlines-core =>
                                    (list rust-adler2-2.0.1
                                     rust-aho-corasick-1.1.3
                                     rust-atomic-waker-1.1.2
                                     rust-autocfg-1.5.0
                                     rust-aws-lc-rs-1.14.1
                                     rust-aws-lc-sys-0.32.3
                                     rust-base64-0.13.1
                                     rust-base64-0.22.1
                                     rust-bincode-2.0.1
                                     rust-bincode-derive-2.0.1
                                     rust-bindgen-0.72.1
                                     rust-bitflags-2.10.0
                                     rust-bumpalo-3.19.0
                                     rust-byteorder-1.5.0
                                     rust-bytes-1.10.1
                                     rust-cc-1.2.43
                                     rust-cexpr-0.6.0
                                     rust-cfg-if-1.0.4
                                     rust-cfg-aliases-0.2.1
                                     rust-clang-sys-1.8.1
                                     rust-cmake-0.1.54
                                     rust-console-0.15.11
                                     rust-crc32fast-1.5.0
                                     rust-crossbeam-deque-0.8.6
                                     rust-crossbeam-epoch-0.9.18
                                     rust-crossbeam-utils-0.8.21
                                     rust-darling-0.20.11
                                     rust-darling-core-0.20.11
                                     rust-darling-macro-0.20.11
                                     rust-derive-builder-0.20.2
                                     rust-derive-builder-core-0.20.2
                                     rust-derive-builder-macro-0.20.2
                                     rust-dirs-5.0.1
                                     rust-dirs-sys-0.4.1
                                     rust-displaydoc-0.2.5
                                     rust-dunce-1.0.5
                                     rust-either-1.15.0
                                     rust-encode-unicode-1.0.0
                                     rust-equivalent-1.0.2
                                     rust-esaxx-rs-0.1.10
                                     rust-find-msvc-tools-0.1.4
                                     rust-flate2-1.1.5
                                     rust-fnv-1.0.7
                                     rust-form-urlencoded-1.2.2
                                     rust-fs-extra-1.3.0
                                     rust-futures-channel-0.3.31
                                     rust-futures-core-0.3.31
                                     rust-futures-io-0.3.31
                                     rust-futures-macro-0.3.31
                                     rust-futures-sink-0.3.31
                                     rust-futures-task-0.3.31
                                     rust-futures-util-0.3.31
                                     rust-getrandom-0.2.16
                                     rust-getrandom-0.3.4
                                     rust-glob-0.3.3
                                     rust-hashbrown-0.16.0
                                     rust-heck-0.5.0
                                     rust-hf-hub-0.4.1
                                     rust-http-1.3.1
                                     rust-http-body-1.0.1
                                     rust-http-body-util-0.1.3
                                     rust-httparse-1.10.1
                                     rust-hyper-1.7.0
                                     rust-hyper-rustls-0.27.7
                                     rust-hyper-util-0.1.17
                                     rust-icu-collections-2.0.0
                                     rust-icu-locale-core-2.0.0
                                     rust-icu-normalizer-2.0.0
                                     rust-icu-normalizer-data-2.0.0
                                     rust-icu-properties-2.0.1
                                     rust-icu-properties-data-2.0.1
                                     rust-icu-provider-2.0.0
                                     rust-ident-case-1.0.1
                                     rust-idna-1.1.0
                                     rust-idna-adapter-1.2.1
                                     rust-indexmap-2.12.0
                                     rust-indicatif-0.17.11
                                     rust-indoc-2.0.7
                                     rust-ipnet-2.11.0
                                     rust-iri-string-0.7.8
                                     rust-itertools-0.11.0
                                     rust-itertools-0.13.0
                                     rust-itoa-1.0.15
                                     rust-jobserver-0.1.34
                                     rust-js-sys-0.3.81
                                     rust-lazy-static-1.5.0
                                     rust-libc-0.2.177
                                     rust-libloading-0.8.9
                                     rust-libredox-0.1.10
                                     rust-litemap-0.8.0
                                     rust-log-0.4.28
                                     rust-lru-slab-0.1.2
                                     rust-macro-rules-attribute-0.2.2
                                     rust-macro-rules-attribute-proc-macro-0.2.2
                                     rust-memchr-2.7.6
                                     rust-memoffset-0.9.1
                                     rust-minimal-lexical-0.2.1
                                     rust-miniz-oxide-0.8.9
                                     rust-mio-1.1.0
                                     rust-monostate-0.1.18
                                     rust-monostate-impl-0.1.18
                                     rust-nom-7.1.3
                                     rust-number-prefix-0.4.0
                                     rust-once-cell-1.21.3
                                     rust-onig-6.5.1
                                     rust-onig-sys-69.9.1
                                     rust-option-ext-0.2.0
                                     rust-paste-1.0.15
                                     rust-percent-encoding-2.3.2
                                     rust-pin-project-lite-0.2.16
                                     rust-pin-utils-0.1.0
                                     rust-pkg-config-0.3.32
                                     rust-portable-atomic-1.11.1
                                     rust-potential-utf-0.1.3
                                     rust-ppv-lite86-0.2.21
                                     rust-prettyplease-0.2.37
                                     rust-proc-macro2-1.0.103
                                     rust-pyo3-0.24.2
                                     rust-pyo3-build-config-0.24.2
                                     rust-pyo3-ffi-0.24.2
                                     rust-pyo3-macros-0.24.2
                                     rust-pyo3-macros-backend-0.24.2
                                     rust-python3-dll-a-0.2.14
                                     rust-quinn-0.11.9
                                     rust-quinn-proto-0.11.13
                                     rust-quinn-udp-0.5.14
                                     rust-quote-1.0.41
                                     rust-r-efi-5.3.0
                                     rust-rand-0.8.5
                                     rust-rand-0.9.2
                                     rust-rand-chacha-0.3.1
                                     rust-rand-chacha-0.9.0
                                     rust-rand-core-0.6.4
                                     rust-rand-core-0.9.3
                                     rust-rayon-1.11.0
                                     rust-rayon-cond-0.3.0
                                     rust-rayon-core-1.13.0
                                     rust-redox-users-0.4.6
                                     rust-regex-1.12.2
                                     rust-regex-automata-0.4.13
                                     rust-regex-syntax-0.8.8
                                     rust-reqwest-0.12.24
                                     rust-ring-0.17.14
                                     rust-rustc-hash-2.1.1
                                     rust-rustls-0.23.34
                                     rust-rustls-pki-types-1.12.0
                                     rust-rustls-webpki-0.103.7
                                     rust-rustversion-1.0.22
                                     rust-ryu-1.0.20
                                     rust-serde-1.0.228
                                     rust-serde-pyobject-0.6.2
                                     rust-serde-core-1.0.228
                                     rust-serde-derive-1.0.228
                                     rust-serde-json-1.0.145
                                     rust-serde-urlencoded-0.7.1
                                     rust-shlex-1.3.0
                                     rust-simd-adler32-0.3.7
                                     rust-slab-0.4.11
                                     rust-smallvec-1.15.1
                                     rust-socket2-0.6.1
                                     rust-socks-0.3.4
                                     rust-spm-precompiled-0.1.4
                                     rust-stable-deref-trait-1.2.1
                                     rust-strsim-0.11.1
                                     rust-subtle-2.6.1
                                     rust-syn-2.0.108
                                     rust-sync-wrapper-1.0.2
                                     rust-synstructure-0.13.2
                                     rust-target-lexicon-0.13.3
                                     rust-thiserror-1.0.69
                                     rust-thiserror-2.0.17
                                     rust-thiserror-impl-1.0.69
                                     rust-thiserror-impl-2.0.17
                                     rust-tinystr-0.8.1
                                     rust-tinyvec-1.10.0
                                     rust-tinyvec-macros-0.1.1
                                     rust-tokenizers-0.21.1
                                     rust-tokio-1.48.0
                                     rust-tokio-rustls-0.26.4
                                     rust-tokio-util-0.7.16
                                     rust-tower-0.5.2
                                     rust-tower-http-0.6.6
                                     rust-tower-layer-0.3.3
                                     rust-tower-service-0.3.3
                                     rust-tracing-0.1.41
                                     rust-tracing-core-0.1.34
                                     rust-try-lock-0.2.5
                                     rust-unicode-ident-1.0.20
                                     rust-unicode-normalization-alignments-0.1.12
                                     rust-unicode-segmentation-1.12.0
                                     rust-unicode-width-0.2.2
                                     rust-unicode-categories-0.1.1
                                     rust-unindent-0.2.4
                                     rust-untrusted-0.9.0
                                     rust-unty-0.0.4
                                     rust-ureq-2.12.1
                                     rust-url-2.5.7
                                     rust-utf8-iter-1.0.4
                                     rust-virtue-0.0.18
                                     rust-want-0.3.1
                                     rust-wasi-0.11.1+wasi-snapshot-preview1
                                     rust-wasip2-1.0.1+wasi-0.2.4
                                     rust-wasm-bindgen-0.2.104
                                     rust-wasm-bindgen-backend-0.2.104
                                     rust-wasm-bindgen-futures-0.4.54
                                     rust-wasm-bindgen-macro-0.2.104
                                     rust-wasm-bindgen-macro-support-0.2.104
                                     rust-wasm-bindgen-shared-0.2.104
                                     rust-wasm-streams-0.4.2
                                     rust-web-sys-0.3.81
                                     rust-web-time-1.1.0
                                     rust-webpki-roots-0.26.11
                                     rust-webpki-roots-1.0.3
                                     rust-winapi-0.3.9
                                     rust-winapi-i686-pc-windows-gnu-0.4.0
                                     rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                     rust-windows-link-0.2.1
                                     rust-windows-sys-0.48.0
                                     rust-windows-sys-0.52.0
                                     rust-windows-sys-0.59.0
                                     rust-windows-sys-0.60.2
                                     rust-windows-sys-0.61.2
                                     rust-windows-targets-0.48.5
                                     rust-windows-targets-0.52.6
                                     rust-windows-targets-0.53.5
                                     rust-windows-aarch64-gnullvm-0.48.5
                                     rust-windows-aarch64-gnullvm-0.52.6
                                     rust-windows-aarch64-gnullvm-0.53.1
                                     rust-windows-aarch64-msvc-0.48.5
                                     rust-windows-aarch64-msvc-0.52.6
                                     rust-windows-aarch64-msvc-0.53.1
                                     rust-windows-i686-gnu-0.48.5
                                     rust-windows-i686-gnu-0.52.6
                                     rust-windows-i686-gnu-0.53.1
                                     rust-windows-i686-gnullvm-0.52.6
                                     rust-windows-i686-gnullvm-0.53.1
                                     rust-windows-i686-msvc-0.48.5
                                     rust-windows-i686-msvc-0.52.6
                                     rust-windows-i686-msvc-0.53.1
                                     rust-windows-x86-64-gnu-0.48.5
                                     rust-windows-x86-64-gnu-0.52.6
                                     rust-windows-x86-64-gnu-0.53.1
                                     rust-windows-x86-64-gnullvm-0.48.5
                                     rust-windows-x86-64-gnullvm-0.52.6
                                     rust-windows-x86-64-gnullvm-0.53.1
                                     rust-windows-x86-64-msvc-0.48.5
                                     rust-windows-x86-64-msvc-0.52.6
                                     rust-windows-x86-64-msvc-0.53.1
                                     rust-wit-bindgen-0.46.0
                                     rust-writeable-0.6.1
                                     rust-yoke-0.8.0
                                     rust-yoke-derive-0.8.0
                                     rust-zerocopy-0.8.27
                                     rust-zerocopy-derive-0.8.27
                                     rust-zerofrom-0.1.6
                                     rust-zerofrom-derive-0.1.6
                                     rust-zeroize-1.8.2
                                     rust-zerotrie-0.2.2
                                     rust-zerovec-0.11.4
                                     rust-zerovec-derive-0.11.1)))
