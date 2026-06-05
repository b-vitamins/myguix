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

(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-aligned-vec-0.5.0
  (crate-source "aligned-vec" "0.5.0"
                "1lb8qjqfap028ylf8zap6rkwrnrqimc3v6h3cixycjrdx1y0vaaa"))

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

(define rust-anstream-0.6.18
  (crate-source "anstream" "0.6.18"
                "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstyle-1.0.10
  (crate-source "anstyle" "1.0.10"
                "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))

(define rust-anstyle-1.0.11
  (crate-source "anstyle" "1.0.11"
                "1gbbzi0zbgff405q14v8hhpi1kz2drzl9a75r3qhks47lindjbl6"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-lossy-1.1.4
  (crate-source "anstyle-lossy" "1.1.4"
                "07x0kqkklc0124cbn49fc21d9wzp9w2vhaw827md113ghbfablq4"))

(define rust-anstyle-parse-0.2.6
  (crate-source "anstyle-parse" "0.2.6"
                "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.2
  (crate-source "anstyle-query" "1.1.2"
                "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-svg-0.1.10
  (crate-source "anstyle-svg" "0.1.10"
                "1abg68rnik8qbzfihhmp2k7j6r8fmhh62iqfdhwy2vshxxqaf0yw"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anstyle-wincon-3.0.7
  (crate-source "anstyle-wincon" "3.0.7"
                "0kmf0fq4c8yribdpdpylzz1zccpy84hizmcsac3wrac1f7kk8dfa"))

(define rust-anyhow-1.0.100
  (crate-source "anyhow" "1.0.100"
                "0qbfmw4hhv2ampza1csyvf1jqjs2dgrj29cv3h3sh623c6qvcgm2"))

(define rust-anyhow-1.0.102
  (crate-source "anyhow" "1.0.102"
                "0b447dra1v12z474c6z4jmicdmc5yxz5bakympdnij44ckw2s83z"))

(define rust-anyhow-1.0.98
  (crate-source "anyhow" "1.0.98"
                "11ylvjdrcjs0q9jgk1af4r5cx1qppj63plxqkq595vmc24rjsvg1"))

(define rust-anyhow-1.0.99
  (crate-source "anyhow" "1.0.99"
                "001icqvkfl28rxxmk99rm4gvdzxqngj5v50yg2bh3dzcvqfllrxh"))

(define rust-append-only-vec-0.1.7
  (crate-source "append-only-vec" "0.1.7"
                "0wg596rw1dhw8wjgd5dvd4cx7sx2jpabycfxj9lykkrmq1g0i4kr"))

(define rust-arbitrary-1.4.1
  (crate-source "arbitrary" "1.4.1"
                "08zj2yanll5s5gsbmvgwvbq39iqzy3nia3yx3db3zwba08yhpqnx"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-atty-0.2.14
  (crate-source "atty" "0.2.14"
                "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))

(define rust-arg-enum-proc-macro-0.3.4
  (crate-source "arg_enum_proc_macro" "0.3.4"
                "1sjdfd5a8j6r99cf0bpqrd6b160x9vz97y5rysycsjda358jms8a"))

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

(define rust-autocfg-1.4.0
  (crate-source "autocfg" "1.4.0"
                "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-av1-grain-0.2.3
  (crate-source "av1-grain" "0.2.3"
                "1gvqdh21bm1cfqiwyiinbqi0mg7x2lg2fwgmphma8ijxijfr0y36"))

(define rust-avif-serialize-0.8.3
  (crate-source "avif-serialize" "0.8.3"
                "13k0sy5qd6pyvfqzbd06zadz5cavq36fxn391j10ijzv9im2v4lq"))

(define rust-aws-lc-rs-1.14.1
  (crate-source "aws-lc-rs" "1.14.1"
                "03a4f2h4h724pjwbbjwii73ra5zxiysyc2nwli5l1srdb64nr6w7"))

(define rust-aws-lc-sys-0.32.3
  ;; TODO: Check bundled sources.
  (crate-source "aws-lc-sys" "0.32.3"
                "134gmaf47gaa0pnxbfm8z63skwi51vp8vfw49vh676dbkjflwyhh"))

(define rust-backtrace-0.3.75
  (crate-source "backtrace" "0.3.75"
                "00hhizz29mvd7cdqyz5wrj98vqkihgcxmv2vl7z0d0f53qrac1k8"))

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

(define rust-bit-field-0.10.2
  (crate-source "bit_field" "0.10.2"
                "0qav5rpm4hqc33vmf4vc4r0mh51yjx5vmd9zhih26n9yjs3730nw"))

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

(define rust-bitflags-2.11.0
  (crate-source "bitflags" "2.11.0"
                "1bwjibwry5nfwsfm9kjg2dqx5n5nja9xymwbfl6svnn8jsz6ff44"))

(define rust-bytemuck-1.24.0
  (crate-source "bytemuck" "1.24.0"
                "1x65wc9kwf0dfnmglkl8r46d29pfl7yilll5wh9bcf0g6a0gbg8z"))

(define rust-bytemuck-derive-1.10.2
  (crate-source "bytemuck_derive" "1.10.2"
                "1zvmjmw1sdmx9znzm4dpbb2yvz9vyim8w6gp4z256l46qqdvvazr"))

(define rust-bitflags-2.9.0
  (crate-source "bitflags" "2.9.0"
                "1gb5w7pxnmx8l2bjz1i6rkbwbm2167k294rhy6cl1y3vbc8i90jw"))

(define rust-bitflags-2.9.3
  (crate-source "bitflags" "2.9.3"
                "0pgjwsd9qgdmsmwpvg47p9ccrsc26yfjqawbhsi9qds5sg6brvrl"))

(define rust-bitstream-io-2.6.0
  (crate-source "bitstream-io" "2.6.0"
                "1cli390l1dhp9skygyjjnqvczp36b7f31mkx9ry3dg26330cv6b0"))

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

(define rust-built-0.7.7
  (crate-source "built" "0.7.7"
                "0ywn0m11xm80pg6zrzq3sdj3vmzg3qs6baqnvfmkd377ly8n3van"))

(define rust-bumpalo-3.17.0
  (crate-source "bumpalo" "3.17.0"
                "1gxxsn2fsjmv03g8p3m749mczv2k4m8xspifs5l7bcx0vx3gna0n"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-bumpalo-3.20.2
  (crate-source "bumpalo" "3.20.2"
                "1jrgxlff76k9glam0akhwpil2fr1w32gbjdf5hpipc7ld2c7h82x"))

(define rust-bytemuck-1.22.0
  (crate-source "bytemuck" "1.22.0"
                "0h6m8wh7iw98cn69k53plbyqff78c2yrs32l0fy4wqdcvc8grcdn"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

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

(define rust-cc-1.2.18
  (crate-source "cc" "1.2.18"
                "0p6d2pfyrjgqpf2w399wzj4hmyffj6g0gyzg3pdy6xl3gmhlcl2j"))

(define rust-cc-1.2.34
  (crate-source "cc" "1.2.34"
                "1p5ycww65h7xca03lwdp264qalw8v357rg5h17s7naq3h3m4mg22"))

(define rust-cc-1.2.43
  (crate-source "cc" "1.2.43"
                "1hpg1f1srgd5bfivvln1s3kcajdxpqvjsvd8m4y4nmap8pwv17kk"))

(define rust-cc-1.2.56
  (crate-source "cc" "1.2.56"
                "1chvh9g2izhqad7vzy4cc7xpdljdvqpsr6x6hv1hmyqv3mlkbgxf"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-chrono-0.4.40
  (crate-source "chrono" "0.4.40"
                "0z334kqnvq5zx6xsq1k6zk8g9z14fgk2w3vkn4n13pvi3mhn8y8s"))

(define rust-chrono-0.4.41
  (crate-source "chrono" "0.4.41"
                "0k8wy2mph0mgipq28vv3wirivhb31pqs7jyid0dzjivz0i9djsf4"))

(define rust-chrono-0.4.44
  (crate-source "chrono" "0.4.44"
                "1c64mk9a235271j5g3v4zrzqqmd43vp9vki7vqfllpqf5rd0fwy6"))

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

(define rust-clap-3.2.25
  (crate-source "clap" "3.2.25"
                "08vi402vfqmfj9f07c4gl6082qxgf4c9x98pbndcnwbgaszq38af"))

(define rust-clap-4.5.38
  (crate-source "clap" "4.5.38"
                "0060d8gx692via31il45pdnc6pix8l2hynf28bgk1acbby0bk4zd"))

(define rust-clap-4.5.45
  (crate-source "clap" "4.5.45"
                "0663m85dd0aq1g3mkwz5b8pkjv4f5k2smlz7bagib4iqf15fgh0z"))

(define rust-clap-4.5.60
  (crate-source "clap" "4.5.60"
                "02h3nzznssjgp815nnbzk0r62y2iw03kdli75c233kirld6z75r7"))

(define rust-clap-builder-4.5.38
  (crate-source "clap_builder" "4.5.38"
                "0821n0ri2nf1xqj11q1fn78i2hhw6qs96qpan08zdb1z53zjd41p"))

(define rust-clap-builder-4.5.44
  (crate-source "clap_builder" "4.5.44"
                "1a48x3c9q1l7r6wbgy71mq6kfsihpqzxsnbaaamcgwvp88hz9rxk"))

(define rust-clap-builder-4.5.60
  (crate-source "clap_builder" "4.5.60"
                "0xk8mdizvmmn6w5ij5cwhy5pbgyac4w9pfvl6nqmjl7a5hql38i4"))

(define rust-clap-complete-4.5.57
  (crate-source "clap_complete" "4.5.57"
                "1bbixanlxdsb47qhk9fp1jl31vbk218rmnh1xsxzf2az7yyh35ad"))

(define rust-clap-complete-command-0.6.1
  (crate-source "clap_complete_command" "0.6.1"
                "0qhv99j7msqyw7j17hswqwpqbdvqawy8l7ip6rnnh5930n61k3ns"))

(define rust-clap-complete-nushell-4.5.8
  (crate-source "clap_complete_nushell" "4.5.8"
                "1kixnzc8rjqjhk189s1jjvg24v21d1ymj7a2knzna7k9jhb9a30a"))

(define rust-clap-derive-4.5.32
  (crate-source "clap_derive" "4.5.32"
                "1mqcag8qapb5yhygg2hi153kzmbf7w5hqp3nl3fvl5cn4yp6l5q9"))

(define rust-clap-derive-4.5.45
  (crate-source "clap_derive" "4.5.45"
                "1xir8wn5d10wpmnzmzjf2k1ib7j5mmzsm6v3yap6qlvx1axk3jql"))

(define rust-clap-derive-4.5.55
  (crate-source "clap_derive" "4.5.55"
                "1r949xis3jmhzh387smd70vc8a3b9734ck3g5ahg59a63bd969x9"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-clap-lex-0.7.5
  (crate-source "clap_lex" "0.7.5"
                "0xb6pjza43irrl99axbhs12pxq4sr8x7xd36p703j57f5i3n2kxr"))

(define rust-clap-lex-0.2.4
  (crate-source "clap_lex" "0.2.4"
                "1ib1a9v55ybnaws11l63az0jgz5xiy24jkdgsmyl7grcm3sz4l18"))

(define rust-clap-lex-1.0.0
  (crate-source "clap_lex" "1.0.0"
                "0c8888qi1l9sayqlv666h8s0yxn2qc6jr88v1zagk43mpjjjx0is"))

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

(define rust-color-quant-1.1.0
  (crate-source "color_quant" "1.1.0"
                "12q1n427h2bbmmm1mnglr57jaz2dj9apk0plcxw7nwqiai7qjyrx"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

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

(define rust-crc32fast-1.4.2
  (crate-source "crc32fast" "1.4.2"
                "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))

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

(define rust-crunchy-0.2.3
  (crate-source "crunchy" "0.2.3"
                "0aa9k4izp962qlsn5ndgw2zq62mizcpnkns8bxscgz3gqr35knj3"))

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

(define rust-deranged-0.4.0
  (crate-source "deranged" "0.4.0"
                "13h6skwk411wzhf1l9l7d3yz5y6vg9d7s3dwhhb4a942r88nm7lw"))

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

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

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

(define rust-exr-1.73.0
  (crate-source "exr" "1.73.0"
                "1q47yq78q9k210r6jy1wwrilxwwxqavik9l3l426rd17k7srfcgq"))

(define rust-eyre-0.6.12
  (crate-source "eyre" "0.6.12"
                "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))

(define rust-fallible-iterator-0.3.0
  (crate-source "fallible-iterator" "0.3.0"
                "0ja6l56yka5vn4y4pk6hn88z0bpny7a8k1919aqjzp0j1yhy9k1a"))

(define rust-fallible-streaming-iterator-0.1.9
  (crate-source "fallible-streaming-iterator" "0.1.9"
                "0nj6j26p71bjy8h42x6jahx1hn0ng6mc2miwpgwnp8vnwqf4jq3k"))

(define rust-fancy-regex-0.13.0
  (crate-source "fancy-regex" "0.13.0"
                "1wjbqjsdj8fkq6z2i9llq25iaqzd9f208vxnwg8mdbr2ba1lc7jk"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"))

(define rust-fern-0.7.1
  (crate-source "fern" "0.7.1"
                "0a9v59vcq2fgd6bwgbfl7q6b0zzgxn85y6g384z728wvf1gih5j3"))

(define rust-filetime-0.2.26
  (crate-source "filetime" "0.2.26"
                "1vb3vz83saxr084wjf2032hspx7wfc5ggggnhc15i9kg3g6ha1dw"))

(define rust-find-msvc-tools-0.1.4
  (crate-source "find-msvc-tools" "0.1.4"
                "09x1sfinrz86bkm6i2d85lpsfnxn0w797g5zisv1nwhaz1w1h1aj"))

(define rust-find-msvc-tools-0.1.9
  (crate-source "find-msvc-tools" "0.1.9"
                "10nmi0qdskq6l7zwxw5g56xny7hb624iki1c39d907qmfh3vrbjv"))

(define rust-flate2-1.1.1
  (crate-source "flate2" "1.1.1"
                "1kpycx57dqpkr3vp53b4nq75p9mflh0smxy8hkys4v4ndvkr5vbw"))

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

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

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

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

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

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.3.2
  (crate-source "getrandom" "0.3.2"
                "1w2mlixa1989v7czr68iji7h67yra2pbg3s480wsqjza1r2sizkk"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.2
  (crate-source "getrandom" "0.4.2"
                "0mb5833hf9pvn9dhvxjgfg5dx0m77g8wavvjdpvpnkp9fil1xr8d"))

(define rust-gif-0.13.1
  (crate-source "gif" "0.13.1"
                "1whrkvdg26gp1r7f95c6800y6ijqw5y0z8rgj6xihpi136dxdciz"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-glob-0.3.3
  (crate-source "glob" "0.3.3"
                "106jpd3syfzjfj2k70mwm0v436qbx96wig98m4q8x071yrq35hhc"))

(define rust-globset-0.4.16
  (crate-source "globset" "0.4.16"
                "1xa9ivqs74imf1q288spxh49g6iw2mn3x9snibdgapazzj6h58al"))

(define rust-globwalk-0.9.1
  (crate-source "globwalk" "0.9.1"
                "0mz7bsa66p2rrgnz3l94ac4kbklh7mq8j30iizyxjy4qyvmn1xqb"))

(define rust-half-2.5.0
  (crate-source "half" "2.5.0"
                "1ldv2i761fjqxl4rn033nasjrdnw5ysnc1xalsfkfl5skc9zzckx"))

(define rust-half-2.6.0
  (crate-source "half" "2.6.0"
                "1j83v0xaqvrw50ppn0g33zig0zsbdi7xiqbzgn7sd5al57nrd4a5"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-hashbrown-0.16.1
  (crate-source "hashbrown" "0.16.1"
                "004i3njw38ji3bzdp9z178ba9x3k0c1pgy8x69pj7yfppv4iq7c4"))

(define rust-hashlink-0.10.0
  (crate-source "hashlink" "0.10.0"
                "1h8lzvnl9qxi3zyagivzz2p1hp6shgddfmccyf6jv7s1cdicz0kk"))

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

(define rust-hermit-abi-0.1.19
  (crate-source "hermit-abi" "0.1.19"
                "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

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

(define rust-hyper-1.6.0
  (crate-source "hyper" "1.6.0"
                "103ggny2k31z0iq2gzwk2vbx601wx6xkpjpxn40hr3p3b0b5fayc"))

(define rust-hyper-1.7.0
  (crate-source "hyper" "1.7.0"
                "07n59pxzlq621z611cbpvh7p4h9h15v0r7m5wgxygpx02d5aafpb"))

(define rust-hyper-rustls-0.27.5
  (crate-source "hyper-rustls" "0.27.5"
                "1cjr3yf3x5mr3194llsfibacl6j7n2dknii2dwjha4ysyf1ia69d"))

(define rust-hyper-rustls-0.27.7
  (crate-source "hyper-rustls" "0.27.7"
                "0n6g8998szbzhnvcs1b7ibn745grxiqmlpg53xz206v826v3xjg3"))

(define rust-hyper-util-0.1.11
  (crate-source "hyper-util" "0.1.11"
                "1wj3svb1r6yv6kgk5fsz6wwajmngc4zxcw4wxpwlmpbgl8rvqys9"))

(define rust-hyper-util-0.1.17
  (crate-source "hyper-util" "0.1.17"
                "1a5fcnz0alrg4lx9xf6ja66ihaab58jnm5msnky804wg39cras9w"))

(define rust-iana-time-zone-0.1.63
  (crate-source "iana-time-zone" "0.1.63"
                "1n171f5lbc7bryzmp1h30zw86zbvl5480aq02z92lcdwvvjikjdh"))

(define rust-iana-time-zone-0.1.65
  (crate-source "iana-time-zone" "0.1.65"
                "0w64khw5p8s4nzwcf36bwnsmqzf61vpwk9ca1920x82bk6nwj6z3"))

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

(define rust-icu-properties-2.0.0
  (crate-source "icu_properties" "2.0.0"
                "16hk94ng18d1r8989yr4inswacfr2psad8hbr1cjzj21fa6clj95"))

(define rust-icu-properties-2.0.1
  (crate-source "icu_properties" "2.0.1"
                "0az349pjg8f18lrjbdmxcpg676a7iz2ibc09d2wfz57b3sf62v01"))

(define rust-icu-properties-data-2.0.0
  (crate-source "icu_properties_data" "2.0.0"
                "016yaw60mm4m21zr7chq9c00dv1vj1rf2jajv7vzhs3vwikfi5w1"))

(define rust-icu-properties-data-2.0.1
  (crate-source "icu_properties_data" "2.0.1"
                "0cnn3fkq6k88w7p86w7hsd1254s4sl783rpz4p6hlccq74a5k119"))

(define rust-icu-provider-2.0.0
  (crate-source "icu_provider" "2.0.0"
                "1bz5v02gxv1i06yhdhs2kbwxkw3ny9r2vvj9j288fhazgfi0vj03"))

(define rust-id-arena-2.3.0
  (crate-source "id-arena" "2.3.0"
                "0m6rs0jcaj4mg33gkv98d71w3hridghp5c4yr928hplpkgbnfc1x"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.0.3
  (crate-source "idna" "1.0.3"
                "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-ignore-0.4.23
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-image-0.25.6
  (crate-source "image" "0.25.6"
                "06i522bq4qlwylwnlmcn0sgqg72swwan544aldbhi0drwr66cdfv"))

(define rust-image-webp-0.2.1
  (crate-source "image-webp" "0.2.1"
                "0zwg4gpnp69dpn8pdhgjy14mawwi3md02mp1162al6s64bl02zdp"))

(define rust-imara-diff-0.1.8
  (crate-source "imara-diff" "0.1.8"
                "1lmk5dpha2fhahrnsrgavxn1qz6ydp1w8jz8fpvlb28p89ylplqp"))

(define rust-imgref-1.11.0
  (crate-source "imgref" "1.11.0"
                "0254wzkakm31fdix6diqng0fkggknibh0b1iv570ap0djwykl9nh"))

(define rust-imperative-1.0.6
  (crate-source "imperative" "1.0.6"
                "1aclnvya7k1adh2q12yhk4rpz97wxfw7mvffr6pgj8gpd99gd899"))

(define rust-indenter-0.3.4
  (crate-source "indenter" "0.3.4"
                "1maq7yl2px9y40f68c2g2gjsq93rabphzp5shinj8nsldplfckcn"))

(define rust-indexmap-1.9.3
  (crate-source "indexmap" "1.9.3"
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))

(define rust-indexmap-2.11.0
  (crate-source "indexmap" "2.11.0"
                "1sb3nmhisf9pdwfcxzqlvx97xifcvlh5g0rqj9j7i7qg8f01jj7j"))

(define rust-indexmap-2.12.0
  (crate-source "indexmap" "2.12.0"
                "17xs7cqf9nzv8iw8yzpvpjh43lcf9492i8a3xfia2ad9lp9ah5v7"))

(define rust-indexmap-2.13.0
  (crate-source "indexmap" "2.13.0"
                "05qh5c4h2hrnyypphxpwflk45syqbzvqsvvyxg43mp576w2ff53p"))

(define rust-indexmap-2.9.0
  (crate-source "indexmap" "2.9.0"
                "07m15a571yywmvqyb7ms717q9n42b46badbpsmx215jrg7dhv9yf"))

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

(define rust-interpolate-name-0.2.4
  (crate-source "interpolate_name" "0.2.4"
                "0q7s5mrfkx4p56dl8q9zq71y1ysdj4shh6f28qf9gly35l21jj63"))

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

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

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

(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))

(define rust-jiff-0.2.15
  (crate-source "jiff" "0.2.15"
                "0jby6kbs2ra33ji0rx4swcp66jzmcvgszc5v4izwfsgbn6w967xy"))

(define rust-jiff-static-0.2.15
  (crate-source "jiff-static" "0.2.15"
                "1d4l4pvlhz3w487gyhnzvagpbparspv4c8f35qk6g5w9zx8k8d03"))

(define rust-jobserver-0.1.33
  (crate-source "jobserver" "0.1.33"
                "12jkn3cxvfs7jsb6knmh9y2b41lwmrk3vdqywkmssx61jzq65wiq"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-jod-thread-0.1.2
  (crate-source "jod-thread" "0.1.2"
                "1bj7g6l59ybcf33znf80ccqbxvs1cmd8ynd4m8h7ywdqk473c8wb"))

(define rust-jpeg-decoder-0.3.1
  (crate-source "jpeg-decoder" "0.3.1"
                "1c1k53svpdyfhibkmm0ir5w0v3qmcmca8xr8vnnmizwf6pdagm7m"))

(define rust-js-sys-0.3.77
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-js-sys-0.3.81
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.81"
                "01ckbf16iwh7qj92fax9zh8vf2y9sk60cli6999cn7a1jxx96j7c"))

(define rust-js-sys-0.3.91
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.91"
                "171rzgq33wc1nxkgnvhlqqwwnrifs13mg3jjpjj5nf1z0yvib5xl"))

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

(define rust-leb128fmt-0.1.0
  (crate-source "leb128fmt" "0.1.0"
                "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))

(define rust-lebe-0.5.2
  (crate-source "lebe" "0.5.2"
                "1j2l6chx19qpa5gqcw434j83gyskq3g2cnffrbl3842ymlmpq203"))

(define rust-lexical-parse-float-0.8.5
  (crate-source "lexical-parse-float" "0.8.5"
                "0py0gp8hlzcrlvjqmqlpl2v1as65iiqxq2xsabxvhc01pmg3lfv8"))

(define rust-lexical-parse-integer-0.8.6
  (crate-source "lexical-parse-integer" "0.8.6"
                "1sayji3mpvb2xsjq56qcq3whfz8px9a6fxk5v7v15hyhbr4982bd"))

(define rust-lexical-util-0.8.5
  (crate-source "lexical-util" "0.8.5"
                "1z73qkv7yxhsbc4aiginn1dqmsj8jarkrdlyxc88g2gz2vzvjmaj"))

(define rust-libc-0.2.171
  (crate-source "libc" "0.2.171"
                "1mipla3dy3l59pfa9xy4iw2vdgn8n30dzf4vdnasjflxdqhkg6f1"))

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-libc-0.2.177
  (crate-source "libc" "0.2.177"
                "0xjrn69cywaii1iq2lib201bhlvan7czmrm604h5qcm28yps4x18"))

(define rust-libc-0.2.182
  (crate-source "libc" "0.2.182"
                "04k1w1mq9f4cxv520dbr5xw1i7xkbc9fcrvaggyjy25jdkdvl038"))

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

(define rust-libfuzzer-sys-0.4.9
  ;; TODO: Check bundled sources.
  (crate-source "libfuzzer-sys" "0.4.9"
                "0xfwg8shqvysl2bma2lyfcswbbdljajphflp795diwhc80nzay6g"))

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

(define rust-libsqlite3-sys-0.35.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libsqlite3-sys" "0.35.0"
                "0gy1m6j1l94fxsirzp4h4rkrksf78rz7jy3px57qd1rcd8m1hg0k"))

(define rust-libtest-mimic-0.7.3
  (crate-source "libtest-mimic" "0.7.3"
                "0n4vdf4wz4zglammhdzgwxqal9v1a8gbj6rc4q22jfjvxm2xl2yc"))

(define rust-linux-raw-sys-0.12.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.12.1"
                "0lwasljrqxjjfk9l2j8lyib1babh2qjlnhylqzl01nihw14nk9ij"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

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

(define rust-log-0.4.29
  (crate-source "log" "0.4.29"
                "15q8j9c8g5zpkcw0hnd6cf2z7fxqnvsjh3rw5mv5q10r83i34l2y"))

(define rust-loop9-0.1.5
  (crate-source "loop9" "0.1.5"
                "0qphc1c0cbbx43pwm6isnwzwbg6nsxjh7jah04n1sg5h4p0qgbhg"))

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

(define rust-maybe-rayon-0.1.1
  (crate-source "maybe-rayon" "0.1.1"
                "06cmvhj4n36459g327ng5dnj8d58qs472pv5ahlhm7ynxl6g78cf"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memchr-2.8.0
  (crate-source "memchr" "2.8.0"
                "0y9zzxcqxvdqg6wyag7vc3h0blhdn7hkq164bxyx2vph8zs5ijpq"))

(define rust-memmap2-0.9.8
  (crate-source "memmap2" "0.9.8"
                "1dqxjs89krh8cin0k7ksqc9myw9yni9kn8d8cllwq4fn1isrhfl4"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-mimalloc-0.1.47
  (crate-source "mimalloc" "0.1.47"
                "0h5wyqdywhgrpbbgknv9iwazf885fvv20vzhcibsz58y22z1qydi"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-mime-guess-2.0.5
  (crate-source "mime_guess" "2.0.5"
                "03jmg3yx6j39mg0kayf7w4a886dl3j15y8zs119zw01ccy74zi7p"))

(define rust-minicov-0.3.7
  (crate-source "minicov" "0.3.7"
                "0jsvi62lklfyvdmsiizipkqcfpsc7h4c4illgxlf28iwrkqyjzzj"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.8.7
  (crate-source "miniz_oxide" "0.8.7"
                "0c4lj692adnzw0h9j8l24d7imds3icpgdkk3b03zlhxf90zcww7z"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-0.8.11
  (crate-source "mio" "0.8.11"
                "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))

(define rust-mio-1.0.3
  (crate-source "mio" "1.0.3"
                "1gah0h4ia3avxbwym0b6bi6lr6rpysmj9zvw6zis5yq0z0xq91i8"))

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

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

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

(define rust-noop-proc-macro-0.3.0
  (crate-source "noop_proc_macro" "0.3.0"
                "1j2v1c6ric4w9v12h34jghzmngcwmn0hll1ywly4h6lcm4rbnxh6"))

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

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-cpus-1.17.0
  (crate-source "num_cpus" "1.17.0"
                "0fxjazlng4z8cgbmsvbzv411wrg7x3hyxdq8nxixgzjswyylppwi"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-rational-0.4.2
  (crate-source "num-rational" "0.4.2"
                "093qndy02817vpgcqjnj139im3jl7vkq4h68kykdqqh577d18ggq"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-number-prefix-0.4.0
  (crate-source "number_prefix" "0.4.0"
                "1wvh13wvlajqxkb1filsfzbrnq0vrmrw298v2j3sy82z1rm282w3"))

(define rust-numpy-0.20.0
  (crate-source "numpy" "0.20.0"
                "0cfkj99lqjc9i1bxl2r43jrkkbznrq6f6naja8q3pa3y86xirx5y"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-1.20.3
  (crate-source "once_cell" "1.20.3"
                "0bp6rgrsri1vfdcahsimk08zdiilv14ppgcnpbiw8hqyp2j64m4l"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

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

(define rust-open-enum-0.3.0
  (crate-source "open-enum" "0.3.0"
                "07lirnywmcrhdh3cgff6c93qw5x5kbpfay8sh36cfkpqkhcz21wq"))

(define rust-open-enum-derive-0.3.0
  (crate-source "open-enum-derive" "0.3.0"
                "125vh04g7mav3b35729w3fkky6a47x3n7fdk78bzgv4zwm1y8jl9"))

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

(define rust-os-str-bytes-6.6.1
  (crate-source "os_str_bytes" "6.6.1"
                "1885z1x4sm86v5p41ggrl49m58rbzzhd1kj72x46yy53p62msdg2"))

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

(define rust-path-clean-1.0.1
  (crate-source "path-clean" "1.0.1"
                "1vzwcrlz39rd94l89rppvkbsn7dvng449f1bnkyk3ayp43y9ld8p"))

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

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

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

(define rust-png-0.17.16
  (crate-source "png" "0.17.16"
                "09kmkms9fmkbkarw0lnf0scqvjwwg3r7riddag0i3q39r0pil5c2"))

(define rust-portable-atomic-1.11.0
  (crate-source "portable-atomic" "1.11.0"
                "0glb2wngflvfmg789qbf6dbnwcf6ai212fs7n0lf1c66rd49n3im"))

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

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

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

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

(define rust-proc-macro2-1.0.94
  (crate-source "proc-macro2" "1.0.94"
                "114wxb56gdj9vy44q0ll3l2x9niqzcbyqikydmlb5f3h5rsp26d3"))

(define rust-profiling-1.0.16
  (crate-source "profiling" "1.0.16"
                "0kcz2xzg4qx01r5az8cf9ffjasi2srj56sna32igddh0vi7cggdg"))

(define rust-profiling-procmacros-1.0.16
  (crate-source "profiling-procmacros" "1.0.16"
                "0c7y2k4mz5dp2ksj1h4zbxsxq4plmjzccscdaml3h1pizdh2wpx6"))

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

(define rust-pyo3-0.25.0
  (crate-source "pyo3" "0.25.0"
                "19277ka0xfam1sljmm5129iars41nbqpflpqzqxfgkiv6rbdcfgj"))

(define rust-pyo3-0.26.0
  (crate-source "pyo3" "0.26.0"
                "10vkw1a27ymxbi5rrcp71k9q645ybbjdli20akk1w40j89zi383v"))

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

(define rust-pyo3-build-config-0.25.0
  (crate-source "pyo3-build-config" "0.25.0"
                "0v8viwyp3whd39wywfh0lv53kjhrykvalisj2vx48h63l5qscpkm"))

(define rust-pyo3-build-config-0.26.0
  (crate-source "pyo3-build-config" "0.26.0"
                "0pyzhzxsn7lhhbhjcm1nyjw53f5i3x1nbb1imali4zcl4jpxvijg"))

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

(define rust-pyo3-ffi-0.25.0
  (crate-source "pyo3-ffi" "0.25.0"
                "0vlm6m6q3060g2gzvfpijkrr6lmy8kvhyc7asj8lgr4if3ka55gw"))

(define rust-pyo3-ffi-0.26.0
  (crate-source "pyo3-ffi" "0.26.0"
                "01a137mrhpg442g1k5km3j80qh2alx24fvf3iaryyf47jb9p8m02"))

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

(define rust-pyo3-macros-0.25.0
  (crate-source "pyo3-macros" "0.25.0"
                "0502a6860s62sw7vlw92vg4bd4fp1ryfh59glqlhi4lk3cfn8yd1"))

(define rust-pyo3-macros-0.26.0
  (crate-source "pyo3-macros" "0.26.0"
                "1vgx5z2csmznj371rj1g13rijz0yqi6c8xqvj6airzi2kx4fnr1f"))

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

(define rust-pyo3-macros-backend-0.25.0
  (crate-source "pyo3-macros-backend" "0.25.0"
                "17p8l74mxkrzwd3zcx2yky5fmv33d45f8yiz1qdl935brbmqbzwx"))

(define rust-pyo3-macros-backend-0.26.0
  (crate-source "pyo3-macros-backend" "0.26.0"
                "1kqg5q8563i754fq8g4syad5ci1k46lmb10v6isv807lxk04c0hh"))

(define rust-pyproject-toml-0.13.5
  (crate-source "pyproject-toml" "0.13.5"
                "0qs66c4lw8194fr01wlx1g6zd5kz3nnmiffrc298naa8vih623vv"))

(define rust-python3-dll-a-0.2.14
  (crate-source "python3-dll-a" "0.2.14"
                "1n40cyv71pri995yrbbkiz95ran6fgklv2jzz6jls2z778qyz0fk"))

(define rust-qoi-0.4.1
  (crate-source "qoi" "0.4.1"
                "00c0wkb112annn2wl72ixyd78mf56p4lxkhlmsggx65l3v3n8vbz"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

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

(define rust-quinn-0.11.6
  (crate-source "quinn" "0.11.6"
                "1vq55p4kfc4zjxj58xrpf3kcjjqi4mn0wf52a5rzkiky4w46isb2"))

(define rust-quinn-0.11.9
  (crate-source "quinn" "0.11.9"
                "086gzj666dr3slmlynkvxlndy28hahgl361d6bf93hk3i6ahmqmr"))

(define rust-quinn-proto-0.11.13
  (crate-source "quinn-proto" "0.11.13"
                "0cca3mgja9p4w66f6sl1kfhj8rdf4mwsg1jxzssh9g63n14np47i"))

(define rust-quinn-proto-0.11.9
  (crate-source "quinn-proto" "0.11.9"
                "0p8k3iqd0rcxc7b6m2yyijhw4bpfwa61lyzigwvjwzax97rmxzm2"))

(define rust-quinn-udp-0.5.10
  (crate-source "quinn-udp" "0.5.10"
                "0i2rkq8lrkr89csw00mhnhp8zjh2prv4n5n65fwzd1b7hrak0vz4"))

(define rust-quinn-udp-0.5.14
  (crate-source "quinn-udp" "0.5.14"
                "1gacawr17a2zkyri0r3m0lc9spzmxbq1by3ilyb8v2mdvjhcdpmd"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-quote-1.0.41
  (crate-source "quote" "1.0.41"
                "1lg108nb57lwbqlnpsii89cchk6i8pkcvrv88xh1p7a9gdz7c9ff"))

(define rust-quote-1.0.45
  (crate-source "quote" "1.0.45"
                "095rb5rg7pbnwdp6v8w5jw93wndwyijgci1b5lw8j1h5cscn3wj1"))

(define rust-r-efi-5.2.0
  (crate-source "r-efi" "5.2.0"
                "1ig93jvpqyi87nc5kb6dri49p56q7r7qxrn8kfizmqkfj5nmyxkl"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-r-efi-6.0.0
  (crate-source "r-efi" "6.0.0"
                "1gyrl2k5fyzj9k7kchg2n296z5881lg7070msabid09asp3wkp7q"))

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

(define rust-rav1e-0.7.1
  (crate-source "rav1e" "0.7.1"
                "1sawva6nmj2fvynydbcirr3nb7wjyg0id2hz2771qnv6ly0cx1yd"))

(define rust-ravif-0.11.11
  (crate-source "ravif" "0.11.11"
                "1ij51acd3pkl3rr2ha3r3nc7pvg649m49bvyngpcv98fpnbgs4r4"))

(define rust-rawpointer-0.2.1
  (crate-source "rawpointer" "0.2.1"
                "1qy1qvj17yh957vhffnq6agq0brvylw27xgks171qrah75wmg8v0"))

(define rust-rayon-1.10.0
  (crate-source "rayon" "1.10.0"
                "1ylgnzwgllajalr4v00y4kj22klq2jbwllm70aha232iah0sc65l"))

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-cond-0.3.0
  (crate-source "rayon-cond" "0.3.0"
                "1ybxppq84p3q60h9rng9j3dm79f6970hn4wljyf31lpgan5m77q5"))

(define rust-rayon-core-1.12.1
  (crate-source "rayon-core" "1.12.1"
                "1qpwim68ai5h0j7axa8ai8z0payaawv3id0lrgkqmapx7lx8fr8l"))

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

(define rust-regex-1.12.3
  (crate-source "regex" "1.12.3"
                "0xp2q0x7ybmpa5zlgaz00p8zswcirj9h8nry3rxxsdwi9fhm81z1"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.13
  (crate-source "regex-automata" "0.4.13"
                "070z0j23pjfidqz0z89id1fca4p572wxpcr20a0qsv68bbrclxjj"))

(define rust-regex-automata-0.4.14
  (crate-source "regex-automata" "0.4.14"
                "13xf7hhn4qmgfh784llcp2kzrvljd13lb2b1ca0mwnf15w9d87bf"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.10
  (crate-source "regex-syntax" "0.8.10"
                "02jx311ka0daxxc7v45ikzhcl3iydjbbb0mdrpc1xgg8v7c7v2fw"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-regex-syntax-0.8.8
  (crate-source "regex-syntax" "0.8.8"
                "0n7ggnpk0r32rzgnycy5xrc1yp2kq19m6pz98ch3c6dkaxw9hbbs"))

(define rust-reqwest-0.12.15
  (crate-source "reqwest" "0.12.15"
                "1fvvrl3jmsnlm99ldl0ariklrlsmrky06qabp7dc92ylznk4d76i"))

(define rust-reqwest-0.12.24
  (crate-source "reqwest" "0.12.24"
                "0vx3f2n6hfnv81y66v5wayrqh6jlzz4gakky88m0hywz1d0lc2cx"))

(define rust-rgb-0.8.50
  (crate-source "rgb" "0.8.50"
                "02ii3nsciska0sj23ggxaz8gj64ksw8nbpfjcwxlh037chb7sfap"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-ron-0.7.1
  (crate-source "ron" "0.7.1"
                "06iz51r6pyi197jjpfddq8h8884y85myaswfan07cnqylqwkj1w8"))

(define rust-rusqlite-0.37.0
  (crate-source "rusqlite" "0.37.0"
                "0gqzwykyfaaddq5rg1jk0940wby6ifarnwp3fcakbq90ggjscp0n"))

(define rust-rust-stemmers-1.2.0
  (crate-source "rust-stemmers" "1.2.0"
                "0m6acgdflrrcm17dj7lp7x4sfqqhga24qynv660qinwz04v20sp4"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustix-1.0.8
  (crate-source "rustix" "1.0.8"
                "1j6ajqi61agdnh1avr4bplrsgydjw1n4mycdxw3v8g94pyx1y60i"))

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

(define rust-rustix-1.1.4
  (crate-source "rustix" "1.1.4"
                "14511f9yjqh0ix07xjrjpllah3325774gfwi9zpq72sip5jlbzmn"))

(define rust-rustls-0.23.27
  (crate-source "rustls" "0.23.27"
                "08d3nipyhmy4apksjyrb98s9k91wjyg1k7y0flx2671w135482bk"))

(define rust-rustls-0.23.34
  (crate-source "rustls" "0.23.34"
                "19vzmdybp5rlgr0bjb4fykp28w2d6fkqq150aamqykrbxvlqd5ba"))

(define rust-rustls-pemfile-2.2.0
  (crate-source "rustls-pemfile" "2.2.0"
                "0l3f3mrfkgdjrava7ibwzgwc4h3dljw3pdkbsi9rkwz3zvji9qyw"))

(define rust-rustls-pki-types-1.12.0
  (crate-source "rustls-pki-types" "1.12.0"
                "0yawbdpix8jif6s8zj1p2hbyb7y3bj66fhx0y7hyf4qh4964m6i2"))

(define rust-rustls-webpki-0.103.3
  (crate-source "rustls-webpki" "0.103.3"
                "0ddl9qxx94iyichk05r7l30d9dxfd35ybffhsxpsr9pppki2z9z4"))

(define rust-rustls-webpki-0.103.7
  (crate-source "rustls-webpki" "0.103.7"
                "1gqlsd0yqiqch97g0wbsnbmyrp75j6nbzfpf8dmhxa78j50ky2z1"))

(define rust-rustversion-1.0.20
  (crate-source "rustversion" "1.0.20"
                "1lhwjb16dsm8brd18bn2bh0ryzc7qi29bi2jjsc6ny2zbwn3ivgd"))

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

(define rust-semver-1.0.27
  (crate-source "semver" "1.0.27"
                "1qmi3akfrnqc2hfkdgcxhld5bv961wbk8my3ascv5068mc5fnryp"))

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

(define rust-serde-json-1.0.140
  (crate-source "serde_json" "1.0.140"
                "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))

(define rust-serde-json-1.0.143
  (crate-source "serde_json" "1.0.143"
                "0njabwzldvj13ykrf1aaf4gh5cgl25kf9hzbpafbv3qh3ppsn0fl"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-pyobject-0.6.2
  (crate-source "serde-pyobject" "0.6.2"
                "1mxpaca0f11mbs7ripdb9xhkk2jlqz28x09fnwnmy6jylr9mhj2c"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-0.6.8
  (crate-source "serde_spanned" "0.6.8"
                "1q89g70azwi4ybilz5jb8prfpa575165lmrffd49vmcf76qpqq47"))

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

(define rust-serde-with-3.12.0
  (crate-source "serde_with" "3.12.0"
                "1ai9c3cbdgrsvmlc4qpg9z73y80yplk3k7zp45wp97xnzkrggdnn"))

(define rust-serde-with-3.14.0
  (crate-source "serde_with" "3.14.0"
                "1manlm83865xwlvgv8frc472x19b75pd89a54mpxpagg3zb5ri7j"))

(define rust-serde-with-macros-3.12.0
  (crate-source "serde_with_macros" "3.12.0"
                "13hznly0qq1rngsdh8gpnajab2knkrmvwwrbmii86g1s36jwl04d"))

(define rust-serde-with-macros-3.14.0
  (crate-source "serde_with_macros" "3.14.0"
                "03xk9ghj2s6n331r565mgh22w0749vnq50094nd0vkk5cmg9946y"))

(define rust-sha1-0.10.6
  (crate-source "sha1" "0.10.6"
                "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"))

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

(define rust-simd-helpers-0.1.0
  (crate-source "simd_helpers" "0.1.0"
                "19idqicn9k4vhd04ifh2ff41wvna79zphdf2c81rlmpc7f3hz2cm"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-smallvec-1.15.0
  (crate-source "smallvec" "1.15.0"
                "1sgfw8z729nlxk8k13dhs0a762wnaxmlx70a7xlf3wz989bjh5w9"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-snapbox-0.6.21
  (crate-source "snapbox" "0.6.21"
                "0ss3nd9ky0fkq7idj7jzr22kvkhxz3ylrq9fmiq5sdg3h52zrp4n"))

(define rust-snapbox-macros-0.3.10
  (crate-source "snapbox-macros" "0.3.10"
                "1bv4lq1kw1vrd9lk7yk79a0z8q8nma2502ifysv1p913r99rymhn"))

(define rust-socket2-0.5.9
  (crate-source "socket2" "0.5.9"
                "1vzds1wwwi0a51fn10r98j7cx3ir4shvhykpbk7md2h5h1ydapsg"))

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

(define rust-syn-2.0.100
  (crate-source "syn" "2.0.100"
                "18623wdkns03blpv65xsjn8fipl9p9hj98vlrnhin7nqran496mh"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-syn-2.0.108
  (crate-source "syn" "2.0.108"
                "05z908svb0yw5wzrlv27l2i8j1d8l16hd5r8bjh809146myr2n6s"))

(define rust-syn-2.0.117
  (crate-source "syn" "2.0.117"
                "16cv7c0wbn8amxc54n4w15kxlx5ypdmla8s0gxr2l7bv7s0bhrg6"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"))

(define rust-tap-1.0.1
  (crate-source "tap" "1.0.1"
                "0sc3gl4nldqpvyhqi3bbd0l9k7fngrcl4zs47n314nqqk4bpx4sm"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-target-lexicon-0.13.2
  (crate-source "target-lexicon" "0.13.2"
                "16m6smfz533im9dyxfhnzmpi4af75g2iii36ylc4gfmqvf6gf0p5"))

(define rust-target-lexicon-0.13.3
  (crate-source "target-lexicon" "0.13.3"
                "0355pbycq0cj29h1rp176l57qnfwmygv7hwzchs7iq15gibn4zyz"))

(define rust-target-triple-0.1.4
  (crate-source "target-triple" "0.1.4"
                "140p6rjx7ychv0sryndziia1w14cfjflmhh7ccjj57ar3wvsmj8s"))

(define rust-tempfile-3.21.0
  (crate-source "tempfile" "3.21.0"
                "07kx58ibjk3ydq1gcb7q637fs5zkxaa550lxckhgg9p3427izdhm"))

(define rust-tempfile-3.23.0
  (crate-source "tempfile" "3.23.0"
                "05igl2gml6z6i2va1bv49f9f1wb3f752c2i63lvlb9s2vxxwfc9d"))

(define rust-tempfile-3.26.0
  (crate-source "tempfile" "3.26.0"
                "182lfcv9d5w9349i0rjlgn4431k2m3yqfn9ls84p9d3ifxv2r9w2"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-textwrap-0.16.1
  (crate-source "textwrap" "0.16.1"
                "1fgqn3mg9gdbjxwfxl76fg0qiq53w3mk4hdh1x40jylnz39k9m13"))

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

(define rust-thiserror-2.0.12
  (crate-source "thiserror" "2.0.12"
                "024791nsc0np63g2pq30cjf9acj38z3jwx9apvvi8qsqmqnqlysn"))

(define rust-thiserror-2.0.16
  (crate-source "thiserror" "2.0.16"
                "1h30bqyjn5s9ypm668yd9849371rzwk185klwgjg503k2hadcrrl"))

(define rust-thiserror-2.0.17
  (crate-source "thiserror" "2.0.17"
                "1j2gixhm2c3s6g96vd0b01v0i0qz1101vfmw0032mdqj1z58fdgn"))

(define rust-thiserror-2.0.18
  (crate-source "thiserror" "2.0.18"
                "1i7vcmw9900bvsmay7mww04ahahab7wmr8s925xc083rpjybb222"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.12
  (crate-source "thiserror-impl" "2.0.12"
                "07bsn7shydaidvyyrm7jz29vp78vrxr9cr9044rfmn078lmz8z3z"))

(define rust-thiserror-impl-2.0.16
  (crate-source "thiserror-impl" "2.0.16"
                "0q3r1ipr1rhff6cgrcvc0njffw17rpcqz9hdc7p754cbqkhinpkc"))

(define rust-thiserror-impl-2.0.17
  (crate-source "thiserror-impl" "2.0.17"
                "04y92yjwg1a4piwk9nayzjfs07sps8c4vq9jnsfq9qvxrn75rw9z"))

(define rust-thiserror-impl-2.0.18
  (crate-source "thiserror-impl" "2.0.18"
                "1mf1vrbbimj1g6dvhdgzjmn6q09yflz2b92zs1j9n3k7cxzyxi7b"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-threadpool-1.8.1
  (crate-source "threadpool" "1.8.1"
                "1amgfyzvynbm8pacniivzq9r0fh3chhs7kijic81j76l6c5ycl6h"))

(define rust-tiff-0.9.1
  (crate-source "tiff" "0.9.1"
                "0ghyxlz566dzc3scvgmzys11dhq2ri77kb8sznjakijlxby104xs"))

(define rust-tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
  ;; TODO: Check bundled sources.
  (crate-source "tikv-jemalloc-sys"
                "0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7"
                "0baf5vjpg9ipa388md4yxim77rdblnk8r95mnp1akbqjcj860g6d"))

(define rust-tikv-jemallocator-0.6.0
  (crate-source "tikv-jemallocator" "0.6.0"
                "0r985npb7d9hrbs3mb0bkfbv0nvzjpgvzsbpyj21bn0qhpqmzv2c"))

(define rust-time-0.3.41
  (crate-source "time" "0.3.41"
                "0h0cpiyya8cjlrh00d2r72bmgg4lsdcncs76qpwy0rn2kghijxla"))

(define rust-time-core-0.1.4
  (crate-source "time-core" "0.1.4"
                "0z5h9fknvdvbs2k2s1chpi3ab3jvgkfhdnqwrvixjngm263s7sf9"))

(define rust-time-macros-0.2.22
  (crate-source "time-macros" "0.2.22"
                "0jcaxpw220han2bzbrdlpqhy1s5k9i8ri3lw6n5zv4zcja9p69im"))

(define rust-tinystr-0.8.1
  (crate-source "tinystr" "0.8.1"
                "12sc6h3hnn6x78iycm5v6wrs2xhxph0ydm43yyn7gdfw8l8nsksx"))

(define rust-tinytemplate-1.2.1
  (crate-source "tinytemplate" "1.2.1"
                "1g5n77cqkdh9hy75zdb01adxn45mkh9y40wdr7l68xpz35gnnkdy"))

(define rust-tinyvec-1.10.0
  (crate-source "tinyvec" "1.10.0"
                "1yhk0qdqyiaa4v2j9h8pzax5gxgwpz4da0lcphfil6g6pk1zv9dz"))

(define rust-tinyvec-1.11.0
  (crate-source "tinyvec" "1.11.0"
                "1wvycrghzmaysnw34kzwnf0mfx6r75045s24r214wnnjadqfcq9y"))

(define rust-tinyvec-1.8.1
  (crate-source "tinyvec" "1.8.1"
                "1s41rv7n39sjsxz3kd3d4adw45ndkxz1d18rfbz2wd7s9n8bhb82"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokenizers-0.15.2
  (crate-source "tokenizers" "0.15.2"
                "0kb5sfrgrdd8yaxn4080fhagsdniahbvz3si6gyyfdmsn1i7km1x"))

(define rust-tokenizers-0.21.1
  (crate-source "tokenizers" "0.21.1"
                "1mqbli35mfkz4rsj7wlf90c95m1mldw7kvnajp49cm4jbwcv6s9i"))

(define rust-tokio-1.45.0
  (crate-source "tokio" "1.45.0"
                "0rg1i83awynp1xnhz4y1klmi1jq787pa8wgy4gxy1vgr9rlwl4r5"))

(define rust-tokio-1.48.0
  (crate-source "tokio" "1.48.0"
                "0244qva5pksy8gam6llf7bd6wbk2vkab9lx26yyf08dix810wdpz"))

(define rust-tokio-rustls-0.26.2
  (crate-source "tokio-rustls" "0.26.2"
                "16wf007q3584j46wc4s0zc4szj6280g23hka6x6bgs50l4v7nwlf"))

(define rust-tokio-rustls-0.26.4
  (crate-source "tokio-rustls" "0.26.4"
                "0qggwknz9w4bbsv1z158hlnpkm97j3w8v31586jipn99byaala8p"))

(define rust-tokio-util-0.7.15
  (crate-source "tokio-util" "0.7.15"
                "1pypd9lm1fdnpw0779pqvc16qqrxjy63dgfm20ajhpbdmnlkk9b6"))

(define rust-tokio-util-0.7.16
  (crate-source "tokio-util" "0.7.16"
                "1r9wdrg1k5hna3m0kc8kcb8jdb6n52g7vnw93kw2xxw4cyc7qc0l"))

(define rust-toml-0.8.20
  (crate-source "toml" "0.8.20"
                "0j012b37iz1mihksr6a928s6dzszxvblzg3l5wxp7azzsv6sb1yd"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-0.9.5
  (crate-source "toml" "0.9.5"
                "1s7n4l40hvpf46jmgidfknnzpyblz4hip7gfkymgn2q0qlfrw4km"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.6.8
  (crate-source "toml_datetime" "0.6.8"
                "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd"))

(define rust-toml-datetime-0.7.0
  (crate-source "toml_datetime" "0.7.0"
                "1qwivxqkjxxwcqsvfhxnphpwphci0grdfk197wyxfn1gj0z1rpms"))

(define rust-toml-edit-0.22.24
  (crate-source "toml_edit" "0.22.24"
                "0x0lgp70x5cl9nla03xqs5vwwwlrwmd0djkdrp3h3lpdymgpkd0p"))

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

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

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

(define rust-unicase-2.8.1
  (crate-source "unicase" "2.8.1"
                "0fd5ddbhpva7wrln2iah054ar2pc1drqjcll0f493vj3fv8l9f3m"))

(define rust-unicode-categories-0.1.1
  (crate-source "unicode_categories" "0.1.1"
                "0kp1d7fryxxm7hqywbk88yb9d1avsam9sg76xh36k5qx2arj9v1r"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"))

(define rust-unicode-ident-1.0.20
  (crate-source "unicode-ident" "1.0.20"
                "01lafj17xwizrlvn006zz8ip99hqisf77kjk0a8flfmpmrsynbj6"))

(define rust-unicode-ident-1.0.24
  (crate-source "unicode-ident" "1.0.24"
                "0xfs8y1g7syl2iykji8zk5hgfi5jw819f5zsrbaxmlzwsly33r76"))

(define rust-unicode-names2-1.3.0
  (crate-source "unicode_names2" "1.3.0"
                "1pdj8zspi52axhq2kmd0icf63cygkl8f90hvz3jlvj42jz53wryi"))

(define rust-unicode-names2-generator-1.3.0
  (crate-source "unicode_names2_generator" "1.3.0"
                "0vn5f32qcjpanfgkrmsc1174yxvnxy9xrmzgjw0i45hhc625n7mr"))

(define rust-unicode-normalization-0.1.24
  (crate-source "unicode-normalization" "0.1.24"
                "0mnrk809z3ix1wspcqy97ld5wxdb31f3xz6nsvg5qcv289ycjcsh"))

(define rust-unicode-normalization-0.1.25
  (crate-source "unicode-normalization" "0.1.25"
                "1s76dcrxw7vs32yhpi0p074apdc3s7lak7809f3qvclwij3zdm2z"))

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

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

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

(define rust-url-2.5.4
  (crate-source "url" "2.5.4"
                "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j"))

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

(define rust-uuid-1.22.0
  (crate-source "uuid" "1.22.0"
                "0dvsfn44sddhyhlhk7m3i559wyb125h86799fm5abky0067kr3d6"))

(define rust-uuid-macro-internal-1.18.0
  (crate-source "uuid-macro-internal" "1.18.0"
                "0zmrvbnbvvxx3ksy8xnvc26ip2d7v9wblva3x9gxnxl20q0avdr2"))

(define rust-v-frame-0.3.8
  (crate-source "v_frame" "0.3.8"
                "0az9nd6qi1gyikh9yb3lhm453kf7d5isd6xai3j13kds4jm2mwyn"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

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

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasi-0.14.2+wasi-0.2.4
  (crate-source "wasi" "0.14.2+wasi-0.2.4"
                "1cwcqjr3dgdq8j325awgk8a715h0hg0f7jqzsb077n4qm6jzk0wn"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

(define rust-wasm-bindgen-0.2.100
  (crate-source "wasm-bindgen" "0.2.100"
                "1x8ymcm6yi3i1rwj78myl1agqv2m86i648myy3lc97s9swlqkp0y"))

(define rust-wasm-bindgen-0.2.104
  (crate-source "wasm-bindgen" "0.2.104"
                "0b8f4l6pqm0bz0lj5xgwmchb6977n71vmh7srd0axwg93b011nn1"))

(define rust-wasm-bindgen-0.2.114
  (crate-source "wasm-bindgen" "0.2.114"
                "13nkhw552hpllrrmkd2x9y4bmcxr82kdpky2n667kqzcq6jzjck5"))

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

(define rust-wasm-bindgen-macro-0.2.114
  (crate-source "wasm-bindgen-macro" "0.2.114"
                "1rhq9kkl7n0zjrag9p25xsi4aabpgfkyf02zn4xv6pqhrw7xb8hq"))

(define rust-wasm-bindgen-macro-support-0.2.100
  (crate-source "wasm-bindgen-macro-support" "0.2.100"
                "1plm8dh20jg2id0320pbmrlsv6cazfv6b6907z19ys4z1jj7xs4a"))

(define rust-wasm-bindgen-macro-support-0.2.104
  (crate-source "wasm-bindgen-macro-support" "0.2.104"
                "1mr18kx7ima1pmsqlkk982q4a0vf3r8s1x6901jb59sd1prd41wz"))

(define rust-wasm-bindgen-macro-support-0.2.114
  (crate-source "wasm-bindgen-macro-support" "0.2.114"
                "1qriqqjpn922kv5c7f7627fj823k5aifv06j2gvwsiy5map4rkh3"))

(define rust-wasm-bindgen-shared-0.2.100
  (crate-source "wasm-bindgen-shared" "0.2.100"
                "0gffxvqgbh9r9xl36gprkfnh3w9gl8wgia6xrin7v11sjcxxf18s"))

(define rust-wasm-bindgen-shared-0.2.104
  (crate-source "wasm-bindgen-shared" "0.2.104"
                "1la1xj9v3gmawnlyi7lc3mb3xi447r6frb98hi2fb9m1nb47vmms"))

(define rust-wasm-bindgen-shared-0.2.114
  (crate-source "wasm-bindgen-shared" "0.2.114"
                "05lc6w64jxlk4wk8rjci4z61lhx2ams90la27a41gvi3qaw2d8vm"))

(define rust-wasm-bindgen-test-0.3.50
  (crate-source "wasm-bindgen-test" "0.3.50"
                "1hsjc60wynlhgw02p32pgb93303pqmsdfxj67gxdkdm37kixbj36"))

(define rust-wasm-bindgen-test-macro-0.3.50
  (crate-source "wasm-bindgen-test-macro" "0.3.50"
                "16znd6wz79v2i3b2sf5n4ld2kcci8br3wcx7z5c9c07sqln09m8p"))

(define rust-wasm-encoder-0.244.0
  (crate-source "wasm-encoder" "0.244.0"
                "06c35kv4h42vk3k51xjz1x6hn3mqwfswycmr6ziky033zvr6a04r"))

(define rust-wasm-metadata-0.244.0
  (crate-source "wasm-metadata" "0.244.0"
                "02f9dhlnryd2l7zf03whlxai5sv26x4spfibjdvc3g9gd8z3a3mv"))

(define rust-wasm-streams-0.4.2
  (crate-source "wasm-streams" "0.4.2"
                "0rddn007hp6k2cm91mm9y33n79b0jxv0c3znzszcvv67hn6ks18m"))

(define rust-wasmparser-0.244.0
  (crate-source "wasmparser" "0.244.0"
                "1zi821hrlsxfhn39nqpmgzc0wk7ax3dv6vrs5cw6kb0v5v3hgf27"))

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

(define rust-webpki-roots-0.26.8
  (crate-source "webpki-roots" "0.26.8"
                "1jf54brni9lk4ak5pkma2pn18hli22gr7i7wp9zn2lzayy8v4412"))

(define rust-webpki-roots-1.0.3
  (crate-source "webpki-roots" "1.0.3"
                "1f49w0s7f3fgczvjri179wh2a9g8jpkmdi5bi5l8p7ylsb031c9j"))

(define rust-weezl-0.1.8
  (crate-source "weezl" "0.1.8"
                "10lhndjgs6y5djpg3b420xngcr6jkmv70q8rb1qcicbily35pa2k"))

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

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

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

(define rust-windows-core-0.61.0
  (crate-source "windows-core" "0.61.0"
                "104915nsby2cgp322pqqkmj2r82v5sg4hil0hxddg1hc67gc2qs7"))

(define rust-windows-core-0.61.2
  (crate-source "windows-core" "0.61.2"
                "1qsa3iw14wk4ngfl7ipcvdf9xyq456ms7cx2i9iwf406p7fx7zf0"))

(define rust-windows-core-0.62.2
  (crate-source "windows-core" "0.62.2"
                "1swxpv1a8qvn3bkxv8cn663238h2jccq35ff3nsj61jdsca3ms5q"))

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

(define rust-windows-implement-0.60.2
  (crate-source "windows-implement" "0.60.2"
                "1psxhmklzcf3wjs4b8qb42qb6znvc142cb5pa74rsyxm1822wgh5"))

(define rust-windows-interface-0.59.1
  (crate-source "windows-interface" "0.59.1"
                "1a4zr8740gyzzhq02xgl6vx8l669jwfby57xgf0zmkcdkyv134mx"))

(define rust-windows-interface-0.59.3
  (crate-source "windows-interface" "0.59.3"
                "0n73cwrn4247d0axrk7gjp08p34x1723483jxjxjdfkh4m56qc9z"))

(define rust-windows-link-0.1.1
  (crate-source "windows-link" "0.1.1"
                "0f2cq7imbrppsmmnz8899hfhg07cp5gq6rh0bjhb1qb6nwshk13n"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-registry-0.4.0
  (crate-source "windows-registry" "0.4.0"
                "18wbgr6z6765qdnasi8mmvxhvp82xd1zlvd6s7pp2l5lvn8av1j2"))

(define rust-windows-result-0.3.2
  (crate-source "windows-result" "0.3.2"
                "0li2f76anf0rg7i966d9qs5iprsg555g9rgyzj7gcpfr9wdd2ky6"))

(define rust-windows-result-0.3.4
  (crate-source "windows-result" "0.3.4"
                "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-strings-0.3.1
  (crate-source "windows-strings" "0.3.1"
                "06bkhkyclbfchcsv5bnhz77r290k20m15glj2xq60ra0bp64iyl7"))

(define rust-windows-strings-0.4.0
  (crate-source "windows-strings" "0.4.0"
                "15rg6a0ha1d231wwps2qlgyqrgkyj1r8v9vsb8nlbvih4ijajavs"))

(define rust-windows-strings-0.4.2
  (crate-source "windows-strings" "0.4.2"
                "0mrv3plibkla4v5kaakc2rfksdd0b14plcmidhbkcfqc78zwkrjn"))

(define rust-windows-strings-0.5.1
  (crate-source "windows-strings" "0.5.1"
                "14bhng9jqv4fyl7lqjz3az7vzh8pw0w4am49fsqgcz67d67x0dvq"))

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

(define rust-windows-targets-0.53.0
  (crate-source "windows-targets" "0.53.0"
                "12yakpjizhfpppz1i3zgcwxlbar8axrp9j87fmywpydarvlcgr5i"))

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

(define rust-winnow-0.7.6
  (crate-source "winnow" "0.7.6"
                "047abhm7qqgc32pf9a2arini5wsrx7p9wsbx3s106jx4pgczrlv3"))

(define rust-winsafe-0.0.19
  (crate-source "winsafe" "0.0.19"
                "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-wit-bindgen-0.51.0
  (crate-source "wit-bindgen" "0.51.0"
                "19fazgch8sq5cvjv3ynhhfh5d5x08jq2pkw8jfb05vbcyqcr496p"))

(define rust-wit-bindgen-core-0.51.0
  (crate-source "wit-bindgen-core" "0.51.0"
                "1p2jszqsqbx8k7y8nwvxg65wqzxjm048ba5phaq8r9iy9ildwqga"))

(define rust-wit-bindgen-rt-0.39.0
  (crate-source "wit-bindgen-rt" "0.39.0"
                "1hd65pa5hp0nl664m94bg554h4zlhrzmkjsf6lsgsb7yc4734hkg"))

(define rust-wit-bindgen-rust-0.51.0
  (crate-source "wit-bindgen-rust" "0.51.0"
                "08bzn5fsvkb9x9wyvyx98qglknj2075xk1n7c5jxv15jykh6didp"))

(define rust-wit-bindgen-rust-macro-0.51.0
  (crate-source "wit-bindgen-rust-macro" "0.51.0"
                "0ymizapzv2id89igxsz2n587y2hlfypf6n8kyp68x976fzyrn3qc"))

(define rust-wit-component-0.244.0
  (crate-source "wit-component" "0.244.0"
                "1clwxgsgdns3zj2fqnrjcp8y5gazwfa1k0sy5cbk0fsmx4hflrlx"))

(define rust-wit-parser-0.244.0
  (crate-source "wit-parser" "0.244.0"
                "0dm7avvdxryxd5b02l0g5h6933z1cw5z0d4wynvq2cywq55srj7c"))

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

(define rust-zerocopy-0.8.24
  (crate-source "zerocopy" "0.8.24"
                "0yb8hyzfnwzr2wg4p7cnqmjps8fsw8xqnprafgpmfs8qisigx1i5"))

(define rust-zerocopy-0.8.26
  (crate-source "zerocopy" "0.8.26"
                "0bvsj0qzq26zc6nlrm3z10ihvjspyngs7n0jw1fz031i7h6xsf8h"))

(define rust-zerocopy-0.8.27
  (crate-source "zerocopy" "0.8.27"
                "0b1870gf2zzlckca69v2k4mqwmf8yh2li37qldnzvvd3by58g508"))

(define rust-zerocopy-derive-0.8.24
  (crate-source "zerocopy-derive" "0.8.24"
                "1gk9047pbq1yjj2jyiv0s37nqc53maqbmhcsjp6lhi2w7kvai5m9"))

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

(define rust-zeroize-1.8.1
  (crate-source "zeroize" "1.8.1"
                "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))

(define rust-zeroize-1.8.2
  (crate-source "zeroize" "1.8.2"
                "1l48zxgcv34d7kjskr610zqsm6j2b4fcr2vfh9jm9j1jgvk58wdr"))

(define rust-zerotrie-0.2.2
  (crate-source "zerotrie" "0.2.2"
                "15gmka7vw5k0d24s0vxgymr2j6zn2iwl12wpmpnpjgsqg3abpw1n"))

(define rust-zerovec-0.11.2
  (crate-source "zerovec" "0.11.2"
                "0a2457fmz39k9vrrj3rm82q5ykdhgxgbwfz2r6fa6nq11q4fn1aa"))

(define rust-zerovec-0.11.4
  (crate-source "zerovec" "0.11.4"
                "0fz7j1ns8d86m2fqg2a4bzi5gnh5892bxv4kcr9apwc6a3ajpap7"))

(define rust-zerovec-derive-0.11.1
  (crate-source "zerovec-derive" "0.11.1"
                "13zms8hj7vzpfswypwggyfr4ckmyc7v3di49pmj8r1qcz9z275jv"))

(define rust-zip-0.6.6
  (crate-source "zip" "0.6.6"
                "0qcjbqfvbwxi5g9wbymf2r05cvziic2qqj4xy64q3hp48vi980vn"))

(define rust-zmij-1.0.21
  (crate-source "zmij" "1.0.21"
                "1amb5i6gz7yjb0dnmz5y669674pqmwbj44p4yfxfv2ncgvk8x15q"))

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

(define rust-zune-core-0.4.12
  (crate-source "zune-core" "0.4.12"
                "0jj1ra86klzlcj9aha9als9d1dzs7pqv3azs1j3n96822wn3lhiz"))

(define rust-zune-inflate-0.2.54
  (crate-source "zune-inflate" "0.2.54"
                "00kg24jh3zqa3i6rg6yksnb71bch9yi1casqydl00s7nw8pk7avk"))

(define rust-zune-jpeg-0.4.14
  (crate-source "zune-jpeg" "0.4.14"
                "0a70sbnxxkgfm777i1xjkhyn8mx07swg5cabbi083pyysywbm9cr"))

(define rust-libc-0.2.172
  (crate-source "libc" "0.2.172"
                "1ykz4skj7gac14znljm5clbnrhini38jkq3d60jggx3y5w2ayl6p"))

(define rust-bitflags-2.9.1
  (crate-source "bitflags" "2.9.1"
                "0rz9rpp5wywwqb3mxfkywh4drmzci2fch780q7lifbf6bsc5d3hv"))

(define rust-cc-1.2.62
  (crate-source "cc" "1.2.62"
                "164zsxcy2zzvbbh1qpbrsssz8kmria41j4agih47sal3y1cyip51"))

(define rust-libmimalloc-sys-0.1.47
  (crate-source "libmimalloc-sys" "0.1.47"
                "1xkx0r1pwgsdpskvx206dn3gj04zcsjnn4rwhxgc4gn367xaq7id"))

(define rust-mimalloc-0.1.50
  (crate-source "mimalloc" "0.1.50"
                "0h06df0h7ia6yqz4qgbjvqzcjn8xxima9fnac296ny6zf917qqmk"))

(define rust-phf-macros-0.11.3
  (crate-source "phf_macros" "0.11.3"
                "05kjfbyb439344rhmlzzw0f9bwk9fp95mmw56zs7yfn1552c0jpq"))

(define rust-portable-atomic-1.13.1
  (crate-source "portable-atomic" "1.13.1"
                "0j8vlar3n5acyigq8q6f4wjx3k3s5yz0rlpqrv76j73gi5qr8fn3"))

(define rust-proc-macro2-1.0.95
  (crate-source "proc-macro2" "1.0.95"
                "0y7pwxv6sh4fgg6s715ygk1i7g3w02c0ljgcsfm046isibkfbcq2"))

(define rust-pyo3-0.27.2
  (crate-source "pyo3" "0.27.2"
                "0zfqwq1nnszqfcxv0374dd9fjsdysq2lzs0ghald58fizi3w0lxb"))

(define rust-pyo3-build-config-0.27.2
  (crate-source "pyo3-build-config" "0.27.2"
                "19hy4vlkpfxkl0a4520lc7n9v29d5j8nvlky92s451ny0wqr6mdl"))

(define rust-pyo3-ffi-0.27.2
  (crate-source "pyo3-ffi" "0.27.2"
                "12d0faw2kmgazv8i2k9wyv7ybsapxnd2150m4aqm3xnxzb5wk18w"))

(define rust-pyo3-macros-0.27.2
  (crate-source "pyo3-macros" "0.27.2"
                "00iv182px80k6ghm4nmbqyadzj155p5d5d3zj5fi524qpz4i0nqa"))

(define rust-pyo3-macros-backend-0.27.2
  (crate-source "pyo3-macros-backend" "0.27.2"
                "1ya05hs8cylhf7612jicrhvzpd6gq3a72n3z699nx0qlsch1gd83"))

(define rust-syn-2.0.101
  (crate-source "syn" "2.0.101"
                "1brwsh7fn3bnbj50d2lpwy9akimzb3lghz0ai89j8fhvjkybgqlc"))

(define rust-rustversion-1.0.21
  (crate-source "rustversion" "1.0.21"
                "07bb1xx05hhwpnl43sqrhsmxyk5sd5m5baadp19nxp69s9xij3ca"))

(define rust-target-lexicon-0.13.5
  (crate-source "target-lexicon" "0.13.5"
                "1jm6lmf9hsn7ri2d6v9gg6fy24lylhskh6pbxh71f82wdxd97dmd"))

(define rust-winnow-0.5.40
  (crate-source "winnow" "0.5.40"
                "0xk8maai7gyxda673mmw3pj1hdizy5fpi7287vaywykkk19sk4zm"))

(define rust-zerocopy-0.8.25
  (crate-source "zerocopy" "0.8.25"
                "1jx07cd3b3456c9al9zjqqdzpf1abb0vf6z0fj8xnb93hfajsw51"))

(define rust-zerocopy-derive-0.8.25
  (crate-source "zerocopy-derive" "0.8.25"
                "1vsmpq0hp61xpqj9yk8b5jihkqkff05q1wv3l2568mhifl6y59i8"))

(define rust-biblatex-0.11.0
  (crate-source "biblatex" "0.11.0"
                "0rlzfa1m33mh89ygwd4igfblphz71kq1qz1a0icrl6xszrsc7l2k"))


(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))


(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))


(define rust-bytecount-0.6.9
  (crate-source "bytecount" "0.6.9"
                "0pinq0n8zza8qr2lyc3yf17k963129kdbf0bwnmvdk1bpvh14n0p"))


(define rust-clap-4.5.39
  (crate-source "clap" "4.5.39"
                "17raqwxkhhhm80iyblp1v83fvpddkg7rgqr2cjsmz3p6kczfcq7x"))


(define rust-clap-builder-4.5.39
  (crate-source "clap_builder" "4.5.39"
                "0lggb5vscs21jliisvjjphcazzb1iw8347yp42wbwazpl6967k49"))


(define rust-errno-0.3.12
  (crate-source "errno" "0.3.12"
                "066ss2qln9z5q4816d3wcvq2bzasn7dajfkhcfqflfsy6pwlx8ff"))


(define rust-hermit-abi-0.5.1
  (crate-source "hermit-abi" "0.5.1"
                "026bh0y8gpfd62gjm7gx6nyf6bgdyxdn0jc67i1ysl37hm3cwm7i"))


(define rust-nom-8.0.0
  (crate-source "nom" "8.0.0"
                "01cl5xng9d0gxf26h39m0l8lprgpa00fcc75ps1yzgbib1vn35yz"))


(define rust-nom-bibtex-0.6.0
  (crate-source "nom-bibtex" "0.6.0"
                "109zzg2ypmnpvvs6jcp0b39ap6dyfdqxaxgbyl8clbrarc5bmv4r"))


(define rust-nom-language-0.1.0
  (crate-source "nom-language" "0.1.0"
                "0abbzawam1nh75igvyn1vh5pgxgzm0wqj2y9jbpxmzhv8mdvrqid"))


(define rust-nom-tracable-0.9.1
  (crate-source "nom-tracable" "0.9.1"
                "1552z4bvp16r2z9r6cpb6rajgh4590g6pga0lmn83jav9vnd6fba"))


(define rust-nom-tracable-macros-0.9.1
  (crate-source "nom-tracable-macros" "0.9.1"
                "0jnzbfg6nyghi5g5irkihjjy36pirbv87q1w7ccswk952r9qzin9"))


(define rust-nom-locate-4.2.0
  (crate-source "nom_locate" "4.2.0"
                "1wx87c2pm84h63rb4rsjrqzgx574x1zy93av1jk3swdhag086g0y"))


(define rust-nom-locate-5.0.0
  (crate-source "nom_locate" "5.0.0"
                "13a3d1f5gcqhjwchp1j2633wfk0wmpx2xdd2rd04fz42d4npwmqb"))


(define rust-plotters-0.3.7
  (crate-source "plotters" "0.3.7"
                "0ixpy9svpmr2rkzkxvvdpysjjky4gw104d73n7pi2jbs7m06zsss"))


(define rust-plotters-backend-0.3.7
  (crate-source "plotters-backend" "0.3.7"
                "0ahpliim4hrrf7d4ispc2hwr7rzkn6d6nf7lyyrid2lm28yf2hnz"))


(define rust-plotters-svg-0.3.7
  (crate-source "plotters-svg" "0.3.7"
                "0w56sxaa2crpasa1zj0bhxzihlapqfkncggavyngg0w86anf5fji"))


(define rust-proptest-1.6.0
  (crate-source "proptest" "1.6.0"
                "0l4y4bb8hffv7cys7d59qwqdmvmqjfzz0x9vblc08209clqfkjhl"))


(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))


(define rust-rand-xorshift-0.3.0
  (crate-source "rand_xorshift" "0.3.0"
                "13vcag7gmqspzyabfl1gr9ykvxd2142q2agrj8dkyjmfqmgg4nyj"))


(define rust-roman-numerals-rs-3.1.0
  (crate-source "roman-numerals-rs" "3.1.0"
                "0ppq9wz9z892y074c1p6jk7skiklw6c68ygy4ha0nld46dxd8p68"))


(define rust-rustix-1.0.7
  (crate-source "rustix" "1.0.7"
                "0rhjh16bnxi86nrn9qwcnw5632mvd5m1vdy61s4n9zz7mzb867n7"))


(define rust-rusty-fork-0.3.0
  (crate-source "rusty-fork" "0.3.0"
                "0kxwq5c480gg6q0j3bg4zzyfh2kwmc3v2ba94jw8ncjc8mpcqgfb"))


(define rust-serde-bibtex-0.7.1
  (crate-source "serde_bibtex" "0.7.1"
                "1f1bxcz55489ys35y1cnbr7n7893g1dan85hgqd2l0isg7ipmjka"))


(define rust-strum-0.27.2
  (crate-source "strum" "0.27.2"
                "1ksb9jssw4bg9kmv9nlgp2jqa4vnsa3y4q9zkppvl952q7vdc8xg"))


(define rust-strum-macros-0.27.2
  (crate-source "strum_macros" "0.27.2"
                "19xwikxma0yi70fxkcy1yxcv0ica8gf3jnh5gj936jza8lwcx5bn"))


(define rust-tempfile-3.20.0
  (crate-source "tempfile" "3.20.0"
                "18fnp7mjckd9c9ldlb2zhp1hd4467y2hpvx9l50j97rlhlwlx9p8"))


(define rust-tinyvec-1.9.0
  (crate-source "tinyvec" "1.9.0"
                "0w9w8qcifns9lzvlbfwa01y0skhr542anwa3rpn28rg82wgndcq9"))


(define rust-unarray-0.1.4
  (crate-source "unarray" "0.1.4"
                "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa"))


(define rust-winapi-util-0.1.9
  (crate-source "winapi-util" "0.1.9"
                "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))


(define rust-anstream-1.0.0
  (crate-source "anstream" "1.0.0"
                "13d2bj0xfg012s4rmq44zc8zgy1q8k9yp7yhvfnarscnmwpj2jl2"))

(define rust-anstyle-1.0.14
  (crate-source "anstyle" "1.0.14"
                "0030szmgj51fxkic1hpakxxgappxzwm6m154a3gfml83lq63l2wl"))

(define rust-anstyle-parse-1.0.0
  (crate-source "anstyle-parse" "1.0.0"
                "03hkv2690s0crssbnmfkr76kw1k7ah2i6s5amdy9yca2n8w7zkjj"))

(define rust-bibtex-parser-0.3.1
  (crate-source "bibtex-parser" "0.3.1"
                "0kxkw3m2jgs8ch4gsykmnvpwx883qsf6al9hg89hhm25qws6vr70"))

(define rust-bibtex-parser-0.4.0
  (crate-source "bibtex-parser" "0.4.0"
                "1p8wlcbs9a8qc1dypmjfrjs94lqzkhsjjy5915phk0ga6ivqlh7l"))

(define rust-bitflags-2.11.1
  (crate-source "bitflags" "2.11.1"
                "1cvqijg3rvwgis20a66vfdxannjsxfy5fgjqkaq3l13gyfcj4lf4"))

(define rust-block-buffer-0.12.0
  (crate-source "block-buffer" "0.12.0"
                "1glh8w49a7cj0wlkalyn9j605jzf2ss0lg8dqq5xh8cr2q451lyd"))

(define rust-bstr-1.12.1
  (crate-source "bstr" "1.12.1"
                "1arc1v7h5l86vd6z76z3xykjzldqd5icldn7j9d3p7z6x0d4w133"))

(define rust-clap-4.6.1
  (crate-source "clap" "4.6.1"
                "0lcf88l7vlg796rrqr7wipbbmfa5sgsgx4211b7xmxxv8dz13nqx"))

(define rust-clap-builder-4.6.0
  (crate-source "clap_builder" "4.6.0"
                "17q6np22yxhh5y5v53y4l31ps3hlaz45mvz2n2nicr7n3c056jki"))

(define rust-clap-derive-4.6.1
  (crate-source "clap_derive" "4.6.1"
                "1acpz49hi00iv9jkapixjzcv7s51x8qkfaqscjm36rqgf428dkpj"))

(define rust-clap-lex-1.1.0
  (crate-source "clap_lex" "1.1.0"
                "1ycqkpygnlqnndghhcxjb44lzl0nmgsia64x9581030yifxs7m68"))

(define rust-colorchoice-1.0.5
  (crate-source "colorchoice" "1.0.5"
                "0w75k89hw39p0mnnhlrwr23q50rza1yjki44qvh2mgrnj065a1qx"))

(define rust-const-oid-0.10.2
  (crate-source "const-oid" "0.10.2"
                "0p7m286mp8aai4sa72g7ji6qm0d4ns8wg4i4b2hj9p9615zm3vx6"))

(define rust-cpufeatures-0.3.0
  (crate-source "cpufeatures" "0.3.0"
                "00fjhygsqmh4kbxxlb99mcsbspxcai6hjydv4c46pwb67wwl2alb"))

(define rust-crypto-common-0.2.1
  (crate-source "crypto-common" "0.2.1"
                "041p8bs680hrg6rhicfifn19cfvybq9aya5i4i0k08d9byqpnwkp"))

(define rust-digest-0.11.3
  (crate-source "digest" "0.11.3"
                "1hnmhd4rkybr11292w42pz9ppzx1h49glrhqg107k4s1b2xnvpgi"))

(define rust-foldhash-0.2.0
  (crate-source "foldhash" "0.2.0"
                "1nvgylb099s11xpfm1kn2wcsql080nqmnhj1l25bp3r2b35j9kkp"))

(define rust-globset-0.4.18
  (crate-source "globset" "0.4.18"
                "1qsp3wg0mgxzmshcgymdlpivqlc1bihm6133pl6dx2x4af8w3psj"))

(define rust-hashlink-0.11.0
  (crate-source "hashlink" "0.11.0"
                "0c6jpsyb9f3j5yrlbw8rnr5kpkar08z02b1h3b5sf14w39b242za"))

(define rust-hybrid-array-0.4.12
  (crate-source "hybrid-array" "0.4.12"
                "1njpm3mmsb6lgr9nn97ld5aavwjzrvijjb4nav0anhnimf1aamci"))

(define rust-itoa-1.0.18
  (crate-source "itoa" "1.0.18"
                "10jnd1vpfkb8kj38rlkn2a6k02afvj3qmw054dfpzagrpl6achlg"))

(define rust-js-sys-0.3.98
  (crate-source "js-sys" "0.3.98"
                "024zjwpxp6fri4j79bh1686q1x4nw4a06fh1a28zv2rzc4973pv7"))

(define rust-libc-0.2.186
  (crate-source "libc" "0.2.186"
                "0rnyhzjyqq9x56skkllbjzzzwym3r61lq3l4hqj64v71gw0r3av8"))

(define rust-libsqlite3-sys-0.37.0
  (crate-source "libsqlite3-sys" "0.37.0"
                "1cdrrwqarq4rq873ni5645r9cqllc73l8knkkjj62z0yqk413wdi"))

(define rust-once-cell-1.21.4
  (crate-source "once_cell" "1.21.4"
                "0l1v676wf71kjg2khch4dphwh1jp3291ffiymr2mvy1kxd5kwz4z"))

(define rust-pkg-config-0.3.33
  (crate-source "pkg-config" "0.3.33"
                "17jnqmcbxsnwhg9gjf0nh6dj5k0x3hgwi3mb9krjnmfa9v435w8r"))

(define rust-rsqlite-vfs-0.1.0
  (crate-source "rsqlite-vfs" "0.1.0"
                "0kap86yzwl355byfs891185h723nxvl746fdp8gnpvrna0qz58d8"))

(define rust-rusqlite-0.39.0
  (crate-source "rusqlite" "0.39.0"
                "0knd7cqpz58v2lac6gzbdrals1jm5axw01xiggv1nrnrdlab1lm0"))

(define rust-sha2-0.11.0
  (crate-source "sha2" "0.11.0"
                "1x15x22c5yf54ac0np5bfqnq5x0hdw4wqzpi48zwn94ma0bsfss4"))

(define rust-sqlite-wasm-rs-0.5.3
  (crate-source "sqlite-wasm-rs" "0.5.3"
                "0disglizpgix60hm9iq8f4dnd5f7537qllgfxbfhf11h0w37cb0v"))

(define rust-typenum-1.20.0
  (crate-source "typenum" "1.20.0"
                "1pj35y6q11d3y55gdl6g1h2dfhmybjming0jdi9bh0bpnqm11kj0"))

(define rust-wasip2-1.0.3+wasi-0.2.9
  (crate-source "wasip2" "1.0.3+wasi-0.2.9"
                "1mi3w855dz99xzjqc4aa8c9q5b6z1y5c963pkk4cvmr6vdr4c1i0"))

(define rust-wasm-bindgen-0.2.121
  (crate-source "wasm-bindgen" "0.2.121"
                "14375vc40l67lk9rxp59my4r6s64h2an3vjfh9j0hnqngk8f3b29"))

(define rust-wasm-bindgen-macro-0.2.121
  (crate-source "wasm-bindgen-macro" "0.2.121"
                "0y45ghbkvs5rmxvdyhqrx8nzyy45rdx6619c01iaarykmzsfcs4f"))

(define rust-wasm-bindgen-macro-support-0.2.121
  (crate-source "wasm-bindgen-macro-support" "0.2.121"
                "1wjr69qa8rwmk4v7243dr100k393qi0avznk6p5sgck4bk1rwnnr"))

(define rust-wasm-bindgen-shared-0.2.121
  (crate-source "wasm-bindgen-shared" "0.2.121"
                "0h9la4176j5bvgbr64cqkmirif8z59vrcax9i4qx1w79045i1q64"))

(define rust-wit-bindgen-0.57.1
  (crate-source "wit-bindgen" "0.57.1"
                "0vjk2jb593ri9k1aq4iqs2si9mrw5q46wxnn78im7hm7hx799gqy"))

(define rust-zerocopy-0.8.48
  (crate-source "zerocopy" "0.8.48"
                "1sb8plax8jbrsng1jdval7bdhk7hhrx40dz3hwh074k6knzkgm7f"))

(define rust-zerocopy-derive-0.8.48
  (crate-source "zerocopy-derive" "0.8.48"
                "1m5s0g92cxggqc74j83k1priz24k3z93sj5gadppd20p9c4cvqvh"))

(define rust-abi-stable-0.11.3
  (crate-source "abi_stable" "0.11.3"
                "0if428pq8ly97zi6q1842nak977rwxnj17650i8gwpxh7qnm3mk9"))

(define rust-abi-stable-derive-0.11.3
  (crate-source "abi_stable_derive" "0.11.3"
                "16780mmr2hwx8ajcq59nhvq3krv5i8r7mg41x08fx907nil885yp"))

(define rust-abi-stable-shared-0.11.0
  (crate-source "abi_stable_shared" "0.11.0"
                "0qrbmlypvxx3zij1c6w6yykpp5pjcfx9qr2d9lzyc8y1i1vdzddj"))

(define rust-aes-0.8.4
  (crate-source "aes" "0.8.4"
                "1853796anlwp4kqim0s6wm1srl4ib621nm0cl2h3c8klsjkgfsdi"))

(define rust-alloc-no-stdlib-2.0.4
  (crate-source "alloc-no-stdlib" "2.0.4"
                "1cy6r2sfv5y5cigv86vms7n5nlwhx1rbyxwcraqnmm1rxiib2yyc"))

(define rust-alloc-stdlib-0.2.2
  (crate-source "alloc-stdlib" "0.2.2"
                "1kkfbld20ab4165p29v172h8g0wvq8i06z8vnng14whw0isq5ywl"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-approx-0.5.1
  (crate-source "approx" "0.5.1"
                "1ilpv3dgd58rasslss0labarq7jawxmivk17wsh8wmkdm3q15cfa"))

(define rust-ar-archive-writer-0.5.1
  (crate-source "ar_archive_writer" "0.5.1"
                "02rlgsw6k2dh3dk616qyrsl939fwznns1cvf9x0jghmrcfxkpfby"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrow-58.3.0
  (crate-source "arrow" "58.3.0"
                "0wdlcqhh51w43q235hyhfrppwwcr0wqlbcsfq6iysyflbkjk119p"))

(define rust-arrow-arith-58.3.0
  (crate-source "arrow-arith" "58.3.0"
                "0f5ps1fd9f0yvkdgkzw0r6zb1jxvivbi4lhwyl1fi1hq5hnj3ax0"))

(define rust-arrow-array-58.3.0
  (crate-source "arrow-array" "58.3.0"
                "17lr9qmwaz09gcq6ns6gw0mmcafkk7g44ny7k10481zjj8z3vlyg"))

(define rust-arrow-buffer-58.3.0
  (crate-source "arrow-buffer" "58.3.0"
                "1h0n0hx1ml8s97gycb3qwhnxsdyisk4kv10d2nycsfv9q8jd8v0c"))

(define rust-arrow-cast-58.3.0
  (crate-source "arrow-cast" "58.3.0"
                "0zzdrphhd46hqsh2pzzwy21qjpxqb0828ih7ngifj0icdasyynjc"))

(define rust-arrow-csv-58.3.0
  (crate-source "arrow-csv" "58.3.0"
                "1plpmc99ndd3b0jdfma6m1icmi1qrwx2d88yp597lr8pwpvqqkp9"))

(define rust-arrow-data-58.3.0
  (crate-source "arrow-data" "58.3.0"
                "1q4fl2xf23lrlwlzl5b5pnyc73q2lc4n6dmgjqcfxgx24c02321w"))

(define rust-arrow-ipc-58.3.0
  (crate-source "arrow-ipc" "58.3.0"
                "0ky6wfb2jv1kq0s1phzb1liv4f3ibbzbcvc9i0yp10s4hgq3i113"))

(define rust-arrow-json-58.3.0
  (crate-source "arrow-json" "58.3.0"
                "0sq301w9r9kycxvwyj6rbvckhxq2izk30vrw2df9srvdkq8s4p10"))

(define rust-arrow-ord-58.3.0
  (crate-source "arrow-ord" "58.3.0"
                "1l7i357wxgrbnjy41680g550krbkk0arij5scdfnla3r4pyxizqv"))

(define rust-arrow-pyarrow-58.3.0
  (crate-source "arrow-pyarrow" "58.3.0"
                "1kj9kqm5ylrji7qykl5f956rc64rgx2n29pxazmil758fbvbv6nj"))

(define rust-arrow-row-58.3.0
  (crate-source "arrow-row" "58.3.0"
                "032y24jhd0qzdx3v0sscfy00gy2hdihng4m67g3h6ii0653rkdds"))

(define rust-arrow-schema-58.3.0
  (crate-source "arrow-schema" "58.3.0"
                "1vd1g3h4awivk49zcy6wnzxp3sqn91llrqzr3gd9l0wwygyxnczn"))

(define rust-arrow-select-58.3.0
  (crate-source "arrow-select" "58.3.0"
                "08i2jic045ycyksgsxmsxlkdqgqdgq8d9y7jydy7ib3j872nbl4c"))

(define rust-arrow-string-58.3.0
  (crate-source "arrow-string" "58.3.0"
                "0hkbq2vvmsb4vwnj7n5n5fqigk30sx2c9b74l91jysdr7bd7rp99"))

(define rust-as-derive-utils-0.11.0
  (crate-source "as_derive_utils" "0.11.0"
                "1i2kwzxdhydicj9bqscz5w73nmx612yi3ha137qlr900b5j9cg7z"))

(define rust-async-channel-2.5.0
  (crate-source "async-channel" "2.5.0"
                "1ljq24ig8lgs2555myrrjighycpx2mbjgrm3q7lpa6rdsmnxjklj"))

(define rust-async-compression-0.4.42
  (crate-source "async-compression" "0.4.42"
                "1b59jb3y26pmxdshyjb7slxrp184ydlzq80ryfc2ik6cg653z6z7"))

(define rust-async-ffi-0.5.0
  (crate-source "async-ffi" "0.5.0"
                "0l0s134bsiwwr5f7ifh0ygvh219zjmy7dbsidramlzpgzv023ppl"))

(define rust-async-lock-3.4.2
  (crate-source "async-lock" "3.4.2"
                "04c3xrrdrfrvh9v0ajxrangpy38qi76qq268zslphnxxjqjpy3r9"))

(define rust-async-recursion-1.1.1
  (crate-source "async-recursion" "1.1.1"
                "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))

(define rust-async-trait-0.1.89
  (crate-source "async-trait" "0.1.89"
                "1fsxxmz3rzx1prn1h3rs7kyjhkap60i7xvi0ldapkvbb14nssdch"))

(define rust-async-cell-0.2.3
  (crate-source "async_cell" "0.2.3"
                "1hp6i3igi3ajx5vfr5hp1gwynaam9sj041qjid0gaidkzf5b4yj4"))

(define rust-atoi-2.0.0
  (crate-source "atoi" "2.0.0"
                "0a05h42fggmy7h0ajjv6m7z72l924i7igbx13hk9d8pyign9k3gj"))

(define rust-aws-config-1.8.14
  (crate-source "aws-config" "1.8.14"
                "1clrn2xwqvfpsrgba712095dpkjm69g40c7j2x8fhvrxsmvc33wa"))

(define rust-aws-credential-types-1.2.13
  (crate-source "aws-credential-types" "1.2.13"
                "1l222aigx4km17lnnsrx48q0jb3w3n3x0p2zcslcnvb2y85kn83d"))

(define rust-aws-lc-rs-1.16.3
  (crate-source "aws-lc-rs" "1.16.3"
                "13q3i8j2nfykpw0yln0n21m70nfgls5b9gz1lxgsf94hwqzzpihf"))

(define rust-aws-lc-sys-0.40.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "aws-lc-sys" "0.40.0"
                "1rrghm82i50zz40ykizqnrn65lavf856hqfiz6wffh8ybvp3f07m"))

(define rust-aws-runtime-1.7.1
  (crate-source "aws-runtime" "1.7.1"
                "0xby548lmqgg5dxb792vnr4xr9kpcs6w4n1krv3arj76jg2xvqpd"))

(define rust-aws-sdk-dynamodb-1.107.0
  (crate-source "aws-sdk-dynamodb" "1.107.0"
                "0nn85j85vz2d3y50vfxbhij1l1glyc9pnlbbhz35j9wahmpgh6sn"))

(define rust-aws-sdk-sso-1.95.0
  (crate-source "aws-sdk-sso" "1.95.0"
                "0j749kz1qlizff3v35b4z6ygdzbncrrjwvp2wsavsb5sqqkzzi80"))

(define rust-aws-sdk-ssooidc-1.97.0
  (crate-source "aws-sdk-ssooidc" "1.97.0"
                "1y7y5w3wm3cnpbb6zq84s62f1inc2lqhnr50wn4a351nb8g6y62d"))

(define rust-aws-sdk-sts-1.99.0
  (crate-source "aws-sdk-sts" "1.99.0"
                "0jalklraxg9g6zi3dks9amdrm2ydma63m2lrz8428kix5z3agjws"))

(define rust-aws-sigv4-1.4.1
  (crate-source "aws-sigv4" "1.4.1"
                "1abaj80csqkjg04xinsmsljdnfb4izqyg34ml0y0rsjb1y71yh9p"))

(define rust-aws-smithy-async-1.2.13
  (crate-source "aws-smithy-async" "1.2.13"
                "04xf125aiw0rvl79wsaxqgl7smy8pdxbsfi2hi5ph577cc7hviaw"))

(define rust-aws-smithy-http-0.63.5
  (crate-source "aws-smithy-http" "0.63.5"
                "1rv84lzzc30z0dx6j8793q6smbs6d09bq0a8k5k0kmqa94ykf6fn"))

(define rust-aws-smithy-http-client-1.1.11
  (crate-source "aws-smithy-http-client" "1.1.11"
                "15fxpgqmvg3k5ah1bqkwfvqvbsp25vj8i09r5y8wysqgq44bpk00"))

(define rust-aws-smithy-json-0.62.4
  (crate-source "aws-smithy-json" "0.62.4"
                "1fz9nv0fgjiblmb96nscih0ra78y4v28vq5sigccl61y15wsgcr7"))

(define rust-aws-smithy-observability-0.2.5
  (crate-source "aws-smithy-observability" "0.2.5"
                "0fqqpawz772dcm03m64a4jjjfnbdy5bla52r3m3ay6l7pgakjgsd"))

(define rust-aws-smithy-query-0.60.14
  (crate-source "aws-smithy-query" "0.60.14"
                "1c3ig880w2i7gzxx2k7sq9jsz8i580hn71ylwmhqk3rx1rc6mxq5"))

(define rust-aws-smithy-runtime-1.10.2
  (crate-source "aws-smithy-runtime" "1.10.2"
                "1gxn9m2vs7p7svw4np36271mjmhqdin80x4vrvwdrcm8xgvggk12"))

(define rust-aws-smithy-runtime-api-1.11.5
  (crate-source "aws-smithy-runtime-api" "1.11.5"
                "05km6xfxs9533dha8pgwhrwnygb0lidcbsmvg5j89gi8xxfnxbxl"))

(define rust-aws-smithy-types-1.4.5
  (crate-source "aws-smithy-types" "1.4.5"
                "1lxyl9h5icr53p79h5b1lhs8nzafv02kcc9p7cs4afli2r6778lc"))

(define rust-aws-smithy-xml-0.60.14
  (crate-source "aws-smithy-xml" "0.60.14"
                "1apwabx6d8vpxrbz1bb7idhv74bjijlh9xs42q2kzm3fp2s46ddm"))

(define rust-aws-types-1.3.13
  (crate-source "aws-types" "1.3.13"
                "15iy6921wj341qp9j9hnm7bfy9lxf6l11pvb6j3f5ijpfq2cqw04"))

(define rust-axum-0.7.9
  (crate-source "axum" "0.7.9"
                "07z7wqczi9i8xb4460rvn39p4wjqwr32hx907crd1vwb2fy8ijpd"))

(define rust-axum-core-0.4.5
  (crate-source "axum-core" "0.4.5"
                "16b1496c4gm387q20hkv5ic3k5bd6xmnvk50kwsy6ymr8rhvvwh9"))

(define rust-backon-1.6.0
  (crate-source "backon" "1.6.0"
                "1vzphngmym91xh29x7px6vw1xgcv5vjzw86b9zy6ddkm329hxyyg"))

(define rust-base64-simd-0.8.0
  (crate-source "base64-simd" "0.8.0"
                "15cihnjqpxy0h7llpk816czyp5z613yrvsivw9i8f5vkivkvp6ik"))

(define rust-base64ct-1.8.3
  (crate-source "base64ct" "1.8.3"
                "01nyyyx84bhwrcc168hn47d8gvz2pzpv3y3lmck7mq4hw5vh3x9a"))

(define rust-bigdecimal-0.4.10
  (crate-source "bigdecimal" "0.4.10"
                "159nc0bs6bbzxrpfxbnn83ccyzq8bc2ia40zd22ssfjvavqnfs2d"))

(define rust-bitpacking-0.9.3
  (crate-source "bitpacking" "0.9.3"
                "06dh7qyax30q7xbg8cif2xv9bp7kkhw0m4kgrpwfp71xpnd179wn"))

(define rust-blake2-0.10.6
  (crate-source "blake2" "0.10.6"
                "1zlf7w7gql12v61d9jcbbswa3dw8qxsjglylsiljp9f9b3a2ll26"))

(define rust-blake3-1.8.5
  (crate-source "blake3" "1.8.5"
                "1khz6wq61fnr0gl1kmy4bxadc7gbcv4gbq05z4jdjhr8wqs3ra0a"))

(define rust-block-padding-0.3.3
  (crate-source "block-padding" "0.3.3"
                "14wdad0r1qk5gmszxqd8cky6vx8qg7c153jv981mixzrpzmlz2d8"))

(define rust-brotli-8.0.2
  (crate-source "brotli" "8.0.2"
                "0q25r00z3gm5wzvv4vfxvlx5zjb8i4jwyznrvdcp7abs7ihbkn2b"))

(define rust-brotli-decompressor-5.0.0
  (crate-source "brotli-decompressor" "5.0.0"
                "00yyswj1rj20ma4wr4wcci4r9ywlgvxa87nqsv5rik5y588vhjw7"))

(define rust-bs58-0.5.1
  (crate-source "bs58" "0.5.1"
                "1x3v51n5n2s3l0rgrsn142akdf331n2qsa75pscw71fi848vm25z"))

(define rust-bytemuck-1.25.0
  (crate-source "bytemuck" "1.25.0"
                "1v1z32igg9zq49phb3fra0ax5r2inf3aw473vldnm886sx5vdvy8"))

(define rust-bytes-1.11.1
  (crate-source "bytes" "1.11.1"
                "0czwlhbq8z29wq0ia87yass2mzy1y0jcasjb8ghriiybnwrqfx0y"))

(define rust-bytes-utils-0.1.4
  (crate-source "bytes-utils" "0.1.4"
                "0dcd0lxfpj367j9nwm7izj4mkib3slg61rg4wqmpw0kvfnlf7bvx"))

(define rust-bzip2-0.6.1
  (crate-source "bzip2" "0.6.1"
                "0v1lgjxy944fdvsl97wmqs7f288crv7xddalk6y82jpk4jn3z9gk"))

(define rust-cbc-0.1.2
  (crate-source "cbc" "0.1.2"
                "19l9y9ccv1ffg6876hshd123f2f8v7zbkc4nkckqycxf8fajmd96"))

(define rust-cedarwood-0.4.6
  (crate-source "cedarwood" "0.4.6"
                "142fw2aj0c34v56bsml95v9cjlq88r3x5gnhccr7691csvnhp4bd"))

(define rust-chrono-tz-0.10.4
  (crate-source "chrono-tz" "0.10.4"
                "1hr6rmdvqwgk748g2f69mnk97fzhdkfzaczvdn0wz4pdjy2rl4x6"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-cmake-0.1.58
  (crate-source "cmake" "0.1.58"
                "0y06zxw5sv1p5vvpp5rz1qwbrq7ccawrl09nqy5ahx1a5418mxy0"))

(define rust-comfy-table-7.2.2
  (crate-source "comfy-table" "7.2.2"
                "0ixdw77rly84i5z1mxyw6v8lp1isaawnmgxv5d64n88zrxp5v34m"))

(define rust-compression-codecs-0.4.38
  (crate-source "compression-codecs" "0.4.38"
                "1kqq2b8hpv7y3jnakkp66cdlrzl6my02dapn3g12j6cw3qwlh9ff"))

(define rust-compression-core-0.4.32
  (crate-source "compression-core" "0.4.32"
                "12bp209x76flr67jm5fql4hq8d14nkjzkk24g9gi0yh2rxjza56c"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-const-oid-0.9.6
  (crate-source "const-oid" "0.9.6"
                "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2"))

(define rust-const-random-0.1.18
  (crate-source "const-random" "0.1.18"
                "0n8kqz3y82ks8znvz1mxn3a9hadca3amzf33gmi6dc3lzs103q47"))

(define rust-const-random-macro-0.1.16
  (crate-source "const-random-macro" "0.1.16"
                "03iram4ijjjq9j5a7hbnmdngj8935wbsd0f5bm8yw2hblbr3kn7r"))

(define rust-const-panic-0.2.15
  (crate-source "const_panic" "0.2.15"
                "0lp6i96dnbpal6k6zdmlpmwa2zgbrpwnjff46jpf7514qjmcsqp2"))

(define rust-constant-time-eq-0.4.2
  (crate-source "constant_time_eq" "0.4.2"
                "16zamq60dq80k3rqlzh9j9cpjhishmh924lnwbplgrnmkkvfylix"))

(define rust-core-foundation-0.10.1
  (crate-source "core-foundation" "0.10.1"
                "1xjns6dqf36rni2x9f47b65grxwdm20kwdg9lhmzdrrkwadcv9mj"))

(define rust-core-extensions-1.5.4
  (crate-source "core_extensions" "1.5.4"
                "00vhspf51swhiq084xfflwkirn0nkkrdmkm6krrlzzb909fmxfs2"))

(define rust-core-extensions-proc-macros-1.5.4
  (crate-source "core_extensions_proc_macros" "1.5.4"
                "1sjr8bfdhxis6xkamrkr52kyk6gb9m8f864fzc47d6vhsbn3hgak"))

(define rust-crc32c-0.6.8
  (crate-source "crc32c" "0.6.8"
                "0iwyr3jivcnhylczqgk1rkpp9b46r25vi5dj1y7il29dc8hsyirs"))

(define rust-crossbeam-skiplist-0.1.3
  (crate-source "crossbeam-skiplist" "0.1.3"
                "06qmzagqmrv4zwmrvppv6lja6lbm6hi3vv47wp32rjjq1i2dwafz"))

(define rust-crypto-common-0.1.7
  (crate-source "crypto-common" "0.1.7"
                "02nn2rhfy7kvdkdjl457q2z0mklcvj9h662xrq6dzhfialh2kj3q"))

(define rust-csv-1.4.0
  (crate-source "csv" "1.4.0"
                "0f7r2ip0rbi7k377c3xmsh9xd69sillffhpfmbgnvz3yrxl9vkaj"))

(define rust-csv-core-0.1.13
  (crate-source "csv-core" "0.1.13"
                "10lppd3fdb1i5npgx9xqjs5mjmy2qbdi8n16i48lg03ak4k3qjkh"))

(define rust-darling-0.23.0
  (crate-source "darling" "0.23.0"
                "179fj6p6ajw4dnkrik51wjhifxwy02x5zhligyymcb905zd17bi5"))

(define rust-darling-core-0.23.0
  (crate-source "darling_core" "0.23.0"
                "1c033vrks38vpw8kwgd5w088dsr511kfz55n9db56prkgh7sarcq"))

(define rust-darling-macro-0.23.0
  (crate-source "darling_macro" "0.23.0"
                "13fvzji9xyp304mgq720z5l0xgm54qj68jibwscagkynggn88fdc"))

(define rust-datafusion-53.1.0
  (crate-source "datafusion" "53.1.0"
                "06qldq0p021fd642qwv34ccn884jm3lgfmyp5izjyqa071i0xnwk"))

(define rust-datafusion-catalog-53.1.0
  (crate-source "datafusion-catalog" "53.1.0"
                "0rpvvxdxwyz6mww753an5ggr2brj6clgzlp9c7zz99r61gkgvkip"))

(define rust-datafusion-catalog-listing-53.1.0
  (crate-source "datafusion-catalog-listing" "53.1.0"
                "1yriyk7s821pkqqiqlc3f4ivqm7z60ilq6hi66isgmhmfwq15q8p"))

(define rust-datafusion-common-53.1.0
  (crate-source "datafusion-common" "53.1.0"
                "1hjwmwvbcawk7qfplinbga56wj8pqy0bnfhdhy0i0pm98k512anp"))

(define rust-datafusion-common-runtime-53.1.0
  (crate-source "datafusion-common-runtime" "53.1.0"
                "1vrds5kjrckras61pknir55kzzj9qynl6dh5sr7yqw4nsapazx49"))

(define rust-datafusion-datasource-53.1.0
  (crate-source "datafusion-datasource" "53.1.0"
                "1dnbhnsbc04zm5p4m8nl8ixr9dr20259f6b4m5am6dci2rp3iyz9"))

(define rust-datafusion-datasource-arrow-53.1.0
  (crate-source "datafusion-datasource-arrow" "53.1.0"
                "15n0bwz3fp5h9ya6qs5z9191fn7mfl0iqkbmjdglqwyhzqncb9pz"))

(define rust-datafusion-datasource-csv-53.1.0
  (crate-source "datafusion-datasource-csv" "53.1.0"
                "1jndp2gvfyxngrypg0n2h0gxl04kgpsmyrlday4w27rcb3h2jgsh"))

(define rust-datafusion-datasource-json-53.1.0
  (crate-source "datafusion-datasource-json" "53.1.0"
                "1lmkwbmlv9l53gp8cq3g9i90w1xq7i42hcj7ny60r3dwk9s08f73"))

(define rust-datafusion-datasource-parquet-53.1.0
  (crate-source "datafusion-datasource-parquet" "53.1.0"
                "15s9bf2krj0z1flqn084lfhnb46gmfmz04nrjkzyh20fbqvf1a1j"))

(define rust-datafusion-doc-53.1.0
  (crate-source "datafusion-doc" "53.1.0"
                "1vl799vgmj106pav36gifb3wggicndw7k61wmm492av6y46srrld"))

(define rust-datafusion-execution-53.1.0
  (crate-source "datafusion-execution" "53.1.0"
                "02g7mgpncr2kyh5vp6b58bxknxi54gyaa9g4zzv4xjpwmsypyg60"))

(define rust-datafusion-expr-53.1.0
  (crate-source "datafusion-expr" "53.1.0"
                "1pfhpaa6v0sjy5q5ip6r19j323zrrbji5zyb26kd5nzyfxlrnjsp"))

(define rust-datafusion-expr-common-53.1.0
  (crate-source "datafusion-expr-common" "53.1.0"
                "0zmq4jxdvis3m098wz4g79ck9dz8r1cvc35r5vln3gxq7pgklz3x"))

(define rust-datafusion-ffi-53.1.0
  (crate-source "datafusion-ffi" "53.1.0"
                "0g4s3dqpxh4nvcssd4k3rwa48vhsim7z96wlbisn5fh49ls76ldr"))

(define rust-datafusion-functions-53.1.0
  (crate-source "datafusion-functions" "53.1.0"
                "1djiphyhlsskskcrac35m8qppvwk9p52mrqh9dvq5rw40ghs92pj"))

(define rust-datafusion-functions-aggregate-53.1.0
  (crate-source "datafusion-functions-aggregate" "53.1.0"
                "1bfh0vwfzif8qq91a0lqlg6882igwmpiff53w22bm630wlbn5ah0"))

(define rust-datafusion-functions-aggregate-common-53.1.0
  (crate-source "datafusion-functions-aggregate-common" "53.1.0"
                "0iqdnml49h52kjrkdlnz42db2msm7zk2varj732bfza0941ja4dm"))

(define rust-datafusion-functions-nested-53.1.0
  (crate-source "datafusion-functions-nested" "53.1.0"
                "0ai1y0nrklpxlk6bq5bm4izmkrxlhanfg5jypflz0l0dw9cah4zg"))

(define rust-datafusion-functions-table-53.1.0
  (crate-source "datafusion-functions-table" "53.1.0"
                "0zhyym7k77m77d6w59sqjyv7fp2rm62r9kmirkwhafdvbczhvd3j"))

(define rust-datafusion-functions-window-53.1.0
  (crate-source "datafusion-functions-window" "53.1.0"
                "1dmzbn9dkr885awfvp0d8c82dmv2pvkzx2zm09fni6vwsp4qxs6l"))

(define rust-datafusion-functions-window-common-53.1.0
  (crate-source "datafusion-functions-window-common" "53.1.0"
                "0p6y1r3wavqsx640v1qd41r3zvpyz86178936y8r26lva69vn1w3"))

(define rust-datafusion-macros-53.1.0
  (crate-source "datafusion-macros" "53.1.0"
                "1g9649kdl5h0x036jb0p3wfqpcsr438qabwvsayhw785f5m7wdif"))

(define rust-datafusion-optimizer-53.1.0
  (crate-source "datafusion-optimizer" "53.1.0"
                "1klai1v7rnfph4rcbkgnvrgl9i207azv49xpv3cpfzx6a5a02ag9"))

(define rust-datafusion-physical-expr-53.1.0
  (crate-source "datafusion-physical-expr" "53.1.0"
                "0nfb8h10hlnax6pn80a1lycdi6lqdwymmwpx3i051cx4lymnh7jb"))

(define rust-datafusion-physical-expr-adapter-53.1.0
  (crate-source "datafusion-physical-expr-adapter" "53.1.0"
                "1d4xkzpakipyh9gccrfmk0wxjvjz50pc93jf23f0lbng6dgk28pa"))

(define rust-datafusion-physical-expr-common-53.1.0
  (crate-source "datafusion-physical-expr-common" "53.1.0"
                "0qj3ryg6843cl7nh79n756b1qn867q2pcapay9w2vb6kibm4ajxh"))

(define rust-datafusion-physical-optimizer-53.1.0
  (crate-source "datafusion-physical-optimizer" "53.1.0"
                "15cb4f3l2h5lj6qll01hwfiia81k7xjyipq8yqc5jhlsh2bk7cbw"))

(define rust-datafusion-physical-plan-53.1.0
  (crate-source "datafusion-physical-plan" "53.1.0"
                "0yfcdnbzdvx11vcmp7ng9nyx6yszpx5c5i2czgazj6krflv05p2y"))

(define rust-datafusion-proto-53.1.0
  (crate-source "datafusion-proto" "53.1.0"
                "0f2p95bljwpr2h903vrs314p9v2hz0dbv0dwdaxidp29z6p7lf3a"))

(define rust-datafusion-proto-common-53.1.0
  (crate-source "datafusion-proto-common" "53.1.0"
                "1nb35jfkyqc025gq648361bjwjdv208842w5d963171sqp3i9rhn"))

(define rust-datafusion-pruning-53.1.0
  (crate-source "datafusion-pruning" "53.1.0"
                "0ningrbcs2w6212kgzq10ylgf6pcijbbbjl1a1aicmim1s37d35c"))

(define rust-datafusion-session-53.1.0
  (crate-source "datafusion-session" "53.1.0"
                "07j9vhcrzpjyrqjms52hnk6f9dx6yy9f24k1jax2894flhd124jl"))

(define rust-datafusion-sql-53.1.0
  (crate-source "datafusion-sql" "53.1.0"
                "1hayqkxbpgls737vr9p9ky3pngkqgwah1jfa5a3kp6wbvwyi63gs"))

(define rust-datafusion-substrait-53.1.0
  (crate-source "datafusion-substrait" "53.1.0"
                "1rfjkp9gskn3dz1989klbkp73jzqy32pnv6q8b67k2a6llwlajcq"))

(define rust-deepsize-0.2.0
  (crate-source "deepsize" "0.2.0"
                "0v5rn98i6j4jfpcm93mg8by3ddwhanvjiyd3pszzfsvgqdz9inqw"))

(define rust-deepsize-derive-0.1.2
  (crate-source "deepsize_derive" "0.1.2"
                "15i7qybxhdp2y2h3xksyiqrwqki8xrvl60j1asjc3j1v3za020cr"))

(define rust-der-0.7.10
  (crate-source "der" "0.7.10"
                "1jyxacyxdx6mxbkfw99jz59dzvcd9k17rq01a7xvn1dr6wl87hg7"))

(define rust-deranged-0.5.8
  (crate-source "deranged" "0.5.8"
                "0711df3w16vx80k55ivkwzwswziinj4dz05xci3rvmn15g615n3w"))

(define rust-dlv-list-0.5.2
  (crate-source "dlv-list" "0.5.2"
                "0pqvrinxzdz7bpy4a3p450h8krns3bd0mc3w0qqvm03l2kskj824"))

(define rust-earcutr-0.4.3
  (crate-source "earcutr" "0.4.3"
                "00dddrgzsrkbv8ifsmakcxwxrdzzgia7i6cy81y6imw5kbapw4kr"))

(define rust-encoding-0.2.33
  (crate-source "encoding" "0.2.33"
                "1v1ndmkarh9z3n5hk53da4z56hgk9wa5kcsm7cnx345raqw983bb"))

(define rust-encoding-index-japanese-1.20141219.5
  (crate-source "encoding-index-japanese" "1.20141219.5"
                "148c1lmd640p1d7fzk0nv7892mbyavvwddgqvcsm78798bzv5s04"))

(define rust-encoding-index-korean-1.20141219.5
  (crate-source "encoding-index-korean" "1.20141219.5"
                "10cxabp5ppygbq4y6y680856zl9zjvq7ahpiw8zj3fmwwsw3zhsd"))

(define rust-encoding-index-simpchinese-1.20141219.5
  (crate-source "encoding-index-simpchinese" "1.20141219.5"
                "1xria2i7mc5dqdrpqxasdbxv1qx46jjbm53if3y1i4cvj2a72ynq"))

(define rust-encoding-index-singlebyte-1.20141219.5
  (crate-source "encoding-index-singlebyte" "1.20141219.5"
                "0jp85bz2pprzvg9m95w4q0vibh67b6w3bx35lafay95jzyndal9k"))

(define rust-encoding-index-tradchinese-1.20141219.5
  (crate-source "encoding-index-tradchinese" "1.20141219.5"
                "060ci4iz6xfvzk38syfbjvs7pix5hch3mvxkksswmqwcd3aj03px"))

(define rust-encoding-index-tests-0.1.4
  (crate-source "encoding_index_tests" "0.1.4"
                "0s85y091gl17ixass49bzaivng7w8p82p6nyvz2r3my9w4mxhim2"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-encoding-rs-io-0.1.7
  (crate-source "encoding_rs_io" "0.1.7"
                "10ra4l688cdadd8h1lsbahld1zbywnnqv68366mbhamn3xjwbhqw"))

(define rust-env-filter-1.0.1
  (crate-source "env_filter" "1.0.1"
                "1vvf9xhaxm0m78bp23b8j3cbv1vm5vffn3gaas27mc64rhm0rs9j"))

(define rust-env-logger-0.11.10
  (crate-source "env_logger" "0.11.10"
                "0smmk1hqzk7z91rg7fdq98d03gh9kidkd0ymim43zb4n457w0886"))

(define rust-ethnum-1.5.3
  (crate-source "ethnum" "1.5.3"
                "0pw35s7spvgkn3bvmyv9pgyhkhqplzvdsrp8dzdc87jibwzlqh20"))

(define rust-event-listener-5.4.1
  (crate-source "event-listener" "5.4.1"
                "1asnp3agbr8shcl001yd935m167ammyi8hnvl0q1ycajryn6cfz1"))

(define rust-event-listener-strategy-0.5.4
  (crate-source "event-listener-strategy" "0.5.4"
                "14rv18av8s7n8yixg38bxp5vg2qs394rl1w052by5npzmbgz7scb"))

(define rust-fast-float2-0.2.3
  (crate-source "fast-float2" "0.2.3"
                "0mbadcgq221clfpihsfiahizfsgfwk8n3dbgi1fd48vlbi65dszq"))

(define rust-fastrand-2.4.1
  (crate-source "fastrand" "2.4.1"
                "1mnqxxnxvd69ma9mczabpbbsgwlhd6l78yv3vd681453a9s247wz"))

(define rust-filetime-0.2.28
  (crate-source "filetime" "0.2.28"
                "1xmfmyk7l1my44lhlw6919ln19qibg70jmg5fzw6kgxgdzpjwnrd"))

(define rust-fixedbitset-0.5.7
  (crate-source "fixedbitset" "0.5.7"
                "16fd3v9d2cms2vddf9xhlm56sz4j0zgrk3d2h6v1l7hx760lwrqx"))

(define rust-flatbuffers-25.12.19
  (crate-source "flatbuffers" "25.12.19"
                "1wvfm49ybn098zknzlim8xpxrfn2y0sazzqyaggav61vgffq7xim"))

(define rust-flate2-1.1.9
  (crate-source "flate2" "1.1.9"
                "0g2pb7cxnzcbzrj8bw4v6gpqqp21aycmf6d84rzb6j748qkvlgw4"))

(define rust-float-next-after-1.0.0
  (crate-source "float_next_after" "1.0.0"
                "1s7ikn69b394frihag05b0qcw9i9y04qanlhp5c8sjrw70bcrxwb"))

(define rust-fst-0.4.7
  (crate-source "fst" "0.4.7"
                "06mnksicgv9rp8b7w0ykkshf355l05zym3ygm74qr5z30ndmpf3s"))

(define rust-futures-0.3.32
  (crate-source "futures" "0.3.32"
                "0b9q86r5ar18v5xjiyqn7sb8sa32xv98qqnfz779gl7ns7lpw54b"))

(define rust-futures-channel-0.3.32
  (crate-source "futures-channel" "0.3.32"
                "07fcyzrmbmh7fh4ainilf1s7gnwvnk07phdq77jkb9fpa2ffifq7"))

(define rust-futures-core-0.3.32
  (crate-source "futures-core" "0.3.32"
                "07bbvwjbm5g2i330nyr1kcvjapkmdqzl4r6mqv75ivvjaa0m0d3y"))

(define rust-futures-executor-0.3.32
  (crate-source "futures-executor" "0.3.32"
                "17aplz3ns74qn7a04qg7qlgsdx5iwwwkd4jvdfra6hl3h4w9rwms"))

(define rust-futures-io-0.3.32
  (crate-source "futures-io" "0.3.32"
                "063pf5m6vfmyxj74447x8kx9q8zj6m9daamj4hvf49yrg9fs7jyf"))

(define rust-futures-macro-0.3.32
  (crate-source "futures-macro" "0.3.32"
                "0ys4b1lk7s0bsj29pv42bxsaavalch35rprp64s964p40c1bfdg8"))

(define rust-futures-sink-0.3.32
  (crate-source "futures-sink" "0.3.32"
                "14q8ml7hn5a6gyy9ri236j28kh0svqmrk4gcg0wh26rkazhm95y3"))

(define rust-futures-task-0.3.32
  (crate-source "futures-task" "0.3.32"
                "14s3vqf8llz3kjza33vn4ixg6kwxp61xrysn716h0cwwsnri2xq3"))

(define rust-futures-util-0.3.32
  (crate-source "futures-util" "0.3.32"
                "1mn60lw5kh32hz9isinjlpw34zx708fk5q1x0m40n6g6jq9a971q"))

(define rust-generational-arena-0.2.9
  (crate-source "generational-arena" "0.2.9"
                "1rwnfyprjwqafkwdz2irkds5a41jcjb3bsma3djknx4fy2pr8zl7"))

(define rust-generator-0.8.8
  (crate-source "generator" "0.8.8"
                "1ybcxxz9vdh7nyh9q5654zv5q790b63a83w0zrv0r8id2pj4mw2j"))

(define rust-geo-0.31.0
  (crate-source "geo" "0.31.0"
                "0jjvcx3jgi6q1y5i28d1isp39y77p11wvdna9fdzrgjlirks3h9g"))

(define rust-geo-traits-0.3.0
  (crate-source "geo-traits" "0.3.0"
                "01h24v6yv43q9xzx72dzcnv1ijz6gwdgp2xsn7xcq15728ykaz1f"))

(define rust-geo-types-0.7.19
  (crate-source "geo-types" "0.7.19"
                "1xg0ixgnk9nm9ppcryxbr4b56qnm58y13xisl4q0v5azqhr60xwl"))

(define rust-geoarrow-array-0.8.0
  (crate-source "geoarrow-array" "0.8.0"
                "03ib7hznjcx1z3q1k72z0rjzlmd99i1sdmlz16vsicgswdyppzns"))

(define rust-geoarrow-expr-geo-0.8.0
  (crate-source "geoarrow-expr-geo" "0.8.0"
                "0d64ffrwcril9mvvj6c1bbdrdvmkji2misl1xk32fs6836n64jlf"))

(define rust-geoarrow-schema-0.8.0
  (crate-source "geoarrow-schema" "0.8.0"
                "1l1wbs565kqq1lnd8wb2hda06jhqr2lk4lw0jd5051qx5bdpwjjd"))

(define rust-geodatafusion-0.4.0
  (crate-source "geodatafusion" "0.4.0"
                "12rv4f4y4daklz0v6xw97h9gv9hf86nj9n2kf34rpxd1y4qd8z5g"))

(define rust-geographiclib-rs-0.2.7
  (crate-source "geographiclib-rs" "0.2.7"
                "1iw90j5zx33m97pywa1k0wz0jig6qpknhxfsdrx7767x224z19y5"))

(define rust-geohash-0.13.1
  (crate-source "geohash" "0.13.1"
                "1rmibl9jrc9qwnz43y3lcv114j1nm90912lm4aznq7a0cld4pf8g"))

(define rust-getrandom-0.2.17
  (crate-source "getrandom" "0.2.17"
                "1l2ac6jfj9xhpjjgmcx6s1x89bbnw9x6j9258yy6xjkzpq0bqapz"))

(define rust-gloo-timers-0.3.0
  (crate-source "gloo-timers" "0.3.0"
                "1519157n7xppkk6pdw5w52vy1llzn5iljkqd7q1h5609jv7l7cdv"))

(define rust-h2-0.4.14
  (crate-source "h2" "0.4.14"
                "0cw7jk7kn2vn6f8w8ssh6gis1mljnfjxd606gvi4sjpyjayfy7qp"))

(define rust-half-2.7.1
  (crate-source "half" "2.7.1"
                "0jyq42xfa6sghc397mx84av7fayd4xfxr4jahsqv90lmjr5xi8kf"))

(define rust-hash32-0.3.1
  (crate-source "hash32" "0.3.1"
                "01h68z8qi5gl9lnr17nz10lay8wjiidyjdyd60kqx8ibj090pmj7"))

(define rust-hashbrown-0.17.1
  (crate-source "hashbrown" "0.17.1"
                "0jmqz7i4yl6cm7rbn0i2ffkfrmwi6xkmzkaldr2v8bcsx2v0jngd"))

(define rust-heapless-0.8.0
  (crate-source "heapless" "0.8.0"
                "1b9zpdjv4qkl2511s2c80fz16fx9in4m9qkhbaa8j73032v9xyqb"))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))

(define rust-home-0.5.12
  (crate-source "home" "0.5.12"
                "13bjyzgx6q9srnfvl43dvmhn93qc8mh5w7cylk2g13sj3i3pyqnc"))

(define rust-http-0.2.12
  (crate-source "http" "0.2.12"
                "1w81s4bcbmcj9bjp7mllm8jlz6b31wzvirz8bgpzbqkpwmbvn730"))

(define rust-http-1.4.0
  (crate-source "http" "1.4.0"
                "06iind4cwsj1d6q8c2xgq8i2wka4ps74kmws24gsi1bzdlw2mfp3"))

(define rust-http-body-0.4.6
  (crate-source "http-body" "0.4.6"
                "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw"))

(define rust-httpdate-1.0.3
  (crate-source "httpdate" "1.0.3"
                "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))

(define rust-humantime-2.3.0
  (crate-source "humantime" "2.3.0"
                "092lpipp32ayz4kyyn4k3vz59j9blng36wprm5by0g2ykqr14nqk"))

(define rust-hyper-1.9.0
  (crate-source "hyper" "1.9.0"
                "1jmwbwqcaficskg76kq402gbymbnh2z4v99xwq3l5aa6n8bg16b2"))

(define rust-hyper-rustls-0.27.9
  (crate-source "hyper-rustls" "0.27.9"
                "03vfnsm873wsp1dk0q85nxvk7w6syp8c2m5bcdjcyfgg4786ijik"))

(define rust-hyper-util-0.1.20
  (crate-source "hyper-util" "0.1.20"
                "186zdc58hmm663csmjvrzgkr6jdh93sfmi3q2pxi57gcaqjpqm4n"))

(define rust-hyperloglogplus-0.4.1
  (crate-source "hyperloglogplus" "0.4.1"
                "1qzvq6b4c0n313dvjd58c81rrsnm6inxgzbm0kjk7b6wjkgyn7b2"))

(define rust-i-float-1.15.0
  (crate-source "i_float" "1.15.0"
                "0ns0mr4nrcfwkbm0kiq6dzx4k95ghi8vpf6h88pdij1jqp12a001"))

(define rust-i-key-sort-0.6.0
  (crate-source "i_key_sort" "0.6.0"
                "09zw716r244bnn7y5g6vbhmh0c0kignjlfr2vn5aqf6a0rkzi44i"))

(define rust-i-overlay-4.0.7
  (crate-source "i_overlay" "4.0.7"
                "1cyipq6b76hbax23lifm31lnwm413dk1y2kxiphqj0kfiq386ca1"))

(define rust-i-shape-1.14.0
  (crate-source "i_shape" "1.14.0"
                "10hhsr29vxprxfkhblzdzkpbbivbv3mdbz4p52p3vm7p8avm988y"))

(define rust-i-tree-0.16.0
  (crate-source "i_tree" "0.16.0"
                "0589wbh5052ln3r7pn57c6hn4y4jg8g7g70xqlmviiylwrcdbrim"))

(define rust-icu-collections-2.2.0
  (crate-source "icu_collections" "2.2.0"
                "070r7xd0pynm0hnc1v2jzlbxka6wf50f81wybf9xg0y82v6x3119"))

(define rust-icu-locale-core-2.2.0
  (crate-source "icu_locale_core" "2.2.0"
                "0a9cmin5w1x3bg941dlmgszn33qgq428k7qiqn5did72ndi9n8cj"))

(define rust-icu-normalizer-2.2.0
  (crate-source "icu_normalizer" "2.2.0"
                "1d7krxr0xpc4x9635k1100a24nh0nrc59n65j6yk6gbfkplmwvn5"))

(define rust-icu-normalizer-data-2.2.0
  (crate-source "icu_normalizer_data" "2.2.0"
                "0f5d5d5fhhr9937m2z6z38fzh6agf14z24kwlr6lyczafypf0fys"))

(define rust-icu-properties-2.2.0
  (crate-source "icu_properties" "2.2.0"
                "1pkh3s837808cbwxvfagwc28cvwrz2d9h5rl02jwrhm51ryvdqxy"))

(define rust-icu-properties-data-2.2.0
  (crate-source "icu_properties_data" "2.2.0"
                "052awny0qwkbcbpd5jg2cd7vl5ry26pq4hz1nfsgf10c3qhbnawf"))

(define rust-icu-provider-2.2.0
  (crate-source "icu_provider" "2.2.0"
                "08dl8pxbwr8zsz4c5vphqb7xw0hykkznwi4rw7bk6pwb3krlr70k"))

(define rust-idna-adapter-1.2.2
  (crate-source "idna_adapter" "1.2.2"
                "0557p76l8hj35r9zn1yv7c6x1c0qbrsffmg80n0yy8361ly3fs6b"))

(define rust-indexmap-2.14.0
  (crate-source "indexmap" "2.14.0"
                "1na9z6f0d5pkjr1lgsni470v98gv2r7c41j8w48skr089x2yjrnl"))

(define rust-inout-0.1.4
  (crate-source "inout" "0.1.4"
                "008xfl1jn9rxsq19phnhbimccf4p64880jmnpg59wqi07kk117w7"))

(define rust-integer-encoding-3.0.4
  (crate-source "integer-encoding" "3.0.4"
                "00ng7jmv6pqwqc8w1297f768bn0spwwicdr7hb40baax00r3gc4b"))

(define rust-io-uring-0.7.12
  (crate-source "io-uring" "0.7.12"
                "0qibx47w7x3bvkn8zhisa772ic3hfj741r3hnwnris5cgs7vj2ad"))

(define rust-ipnet-2.12.0
  (crate-source "ipnet" "2.12.0"
                "1qpq2y0asyv0jppw7zww9y96fpnpinwap8a0phhqqgyy3znnz3yr"))

(define rust-jieba-macros-0.9.0
  (crate-source "jieba-macros" "0.9.0"
                "1h1gl0p71dlwbcidclyjs4kqxc5nlqgl2qq3z33091l9rmfzr752"))

(define rust-jieba-rs-0.9.0
  (crate-source "jieba-rs" "0.9.0"
                "18hhm6hmncfsclymmymlrf676f9lgsvdcj1qlbbcpynms7lxci9j"))

(define rust-jiff-0.2.24
  (crate-source "jiff" "0.2.24"
                "0g87al8yqp05m63dhqzi359xgsslc0grqz00nvfdyq8dcayms2zh"))

(define rust-jiff-static-0.2.24
  (crate-source "jiff-static" "0.2.24"
                "1mz6v0d1hd8wjgfzccgda5g9z01s1yxnyiizvahjw0pq1w1xw070"))

(define rust-jiff-tzdb-0.1.6
  (crate-source "jiff-tzdb" "0.1.6"
                "0xihzlnnyk0xnrzpq4xcyjdcmy8xc3ychzb9ayjkh4vgha2fy069"))

(define rust-jiff-tzdb-platform-0.1.3
  (crate-source "jiff-tzdb-platform" "0.1.3"
                "1s1ja692wyhbv7f60mc0x90h7kn1pv65xkqi2y4imarbmilmlnl7"))

(define rust-jsonb-0.5.6
  (crate-source "jsonb" "0.5.6"
                "0lv9rizv6iwjx3kzd4xf0czfihf062d4s9yis05c91v0cclzp67b"))

(define rust-jsonwebtoken-9.3.1
  (crate-source "jsonwebtoken" "9.3.1"
                "1plx2qf8mpg96bpsa1aalfsf4jvgwcnl6i37x7masysk91xcr1ss"))

(define rust-kanaria-0.2.0
  (crate-source "kanaria" "0.2.0"
                "1zwn86j7shw74lhv2yh7k5c9czd9mirqm6gdzp25l1a04mjxkyf0"))

(define rust-lance-namespace-reqwest-client-0.7.6
  (crate-source "lance-namespace-reqwest-client" "0.7.6"
                "1lk13vhc07k0scaxkw22lls3q96z67d6dxbwdsmivq0kmayk2ppn"))

(define rust-lexical-core-1.0.6
  (crate-source "lexical-core" "1.0.6"
                "1555cjyj1hb6wxh3n54cn7r55jxinxg4a13klxapx03z4xd153bx"))

(define rust-lexical-parse-float-1.0.6
  (crate-source "lexical-parse-float" "1.0.6"
                "0mpapd5cp4s5f1lsr46nq8d0fx5nkbwvbp1p06y51xfnzcrg5aaj"))

(define rust-lexical-parse-integer-1.0.6
  (crate-source "lexical-parse-integer" "1.0.6"
                "0d4v93wabi45vi759jhszrs2h6rw637grcnpdjcrrhdriygh6yls"))

(define rust-lexical-util-1.0.7
  (crate-source "lexical-util" "1.0.7"
                "05zdvc2wfhm7ndszhdsvcpxbk707amhsdmhvbpxi6kxidc9ds116"))

(define rust-lexical-write-float-1.0.6
  (crate-source "lexical-write-float" "1.0.6"
                "0qb3pgd5bbhl3g9znkk6h6mljhmlrqfvpazv2pa8hc81gk43ii2h"))

(define rust-lexical-write-integer-1.0.6
  (crate-source "lexical-write-integer" "1.0.6"
                "1pql9sn4xkz9rpxxjry8jax5qd0270sssz1pjrbjspa732k53620"))

(define rust-libbz2-rs-sys-0.2.3
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libbz2-rs-sys" "0.2.3"
                "0psnjha3lzksi5padx4y6a5s38d61v2k85cp7a7zqzh7cp0si9mk"))

(define rust-libloading-0.7.4
  (crate-source "libloading" "0.7.4"
                "17wbccnjvhjd9ibh019xcd8kjvqws8lqgq86lqkpbgig7gyq0wxn"))

(define rust-liblzma-0.4.6
  (crate-source "liblzma" "0.4.6"
                "16gqsypfx0nanl4cb9l5a5vg3gg7zfwlw0g8xbg5c7qxq9vkn0xn"))

(define rust-liblzma-sys-0.4.6
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "liblzma-sys" "0.4.6"
                "1dnkmpigzlwif837q2f89fpf4nsqzymqpax4xi35736d2lfqaq0s"))

(define rust-libm-0.2.16
  (crate-source "libm" "0.2.16"
                "10brh0a3qjmbzkr5mf5xqi887nhs5y9layvnki89ykz9xb1wxlmn"))

(define rust-libredox-0.1.16
  (crate-source "libredox" "0.1.16"
                "0v54zvgknag9310wcjykgv86pgq02qr3mzgkdg4r6m1k7ns3nbz0"))

(define rust-lindera-0.44.1
  (crate-source "lindera" "0.44.1"
                "0bphj604sg4m5dskqah3323qw8dr0rbgc8115xr808h587ps9ash"))

(define rust-lindera-cc-cedict-0.44.1
  (crate-source "lindera-cc-cedict" "0.44.1"
                "0l33nlkpr5kzwsdjd7nx5hfqqa4pk51i9ncah8ihzmhghfhffxsx"))

(define rust-lindera-dictionary-0.44.1
  (crate-source "lindera-dictionary" "0.44.1"
                "1bqab7sak24gmhd23hi7bg0y3vya8q4163kw77yz6akq484wg728"))

(define rust-lindera-ipadic-0.44.1
  (crate-source "lindera-ipadic" "0.44.1"
                "1ynx4mr0001g5xbhiiy89wsrszin13x88d6x9pwz1yhx8chhb1vq"))

(define rust-lindera-ipadic-neologd-0.44.1
  (crate-source "lindera-ipadic-neologd" "0.44.1"
                "11ax5cami7mfwk05yvylggyyrxvn03afiif228z6hp3f0p1kvjxb"))

(define rust-lindera-ko-dic-0.44.1
  (crate-source "lindera-ko-dic" "0.44.1"
                "1hlb3zfcaxa7lyw0sip7cdngg1573i95a84idljz055bicaid4z9"))

(define rust-lindera-unidic-0.44.1
  (crate-source "lindera-unidic" "0.44.1"
                "1yjww4d19h7z0ymadh03nnqns4kykp3lwpm2pdmjf53c2r2jjrsj"))

(define rust-litemap-0.8.2
  (crate-source "litemap" "0.8.2"
                "1w7628bc7wwcxc4n4s5kw0610xk06710nh2hn5kwwk2wa91z9nlj"))

(define rust-lock-api-0.4.14
  (crate-source "lock_api" "0.4.14"
                "0rg9mhx7vdpajfxvdjmgmlyrn20ligzqvn8ifmaz7dc79gkrjhr2"))

(define rust-loom-0.7.2
  (crate-source "loom" "0.7.2"
                "1jpszf9qxv8ydpsm2h9vcyvxvyxcfkhmmfbylzd4gfbc0k40v7j1"))

(define rust-lz4-1.28.1
  (crate-source "lz4" "1.28.1"
                "1x2svvs3gkn3krv61nd7ms4vmikibsnfl31mk0z480qdhqz542x2"))

(define rust-lz4-sys-1.11.1+lz4-1.10.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "lz4-sys" "1.11.1+lz4-1.10.0"
                "1rhqnhwq05fmlc2q39ipsq0vpi0xf6w6p22j6q5x637dqvbc1n3b"))

(define rust-lz4-flex-0.13.1
  (crate-source "lz4_flex" "0.13.1"
                "0zmrvmrcwnwgypldakvca19z5a2a11wch3dhds1giy39hvnx9w3y"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-matchit-0.7.3
  (crate-source "matchit" "0.7.3"
                "156bgdmmlv4crib31qhgg49nsjk88dxkdqp80ha2pk2rk6n6ax0f"))

(define rust-md-5-0.10.6
  (crate-source "md-5" "0.10.6"
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"))

(define rust-md5-0.8.0
  (crate-source "md5" "0.8.0"
                "1q6jfsa5w3993dzymxkv9jxpp7vyhgga6z35g6c0c8rk50w0i5mf"))

(define rust-memmap2-0.9.10
  (crate-source "memmap2" "0.9.10"
                "1qz0n4ch68pz2mp07sdwnk27imdjjqy6aqir3hp9j4g0iw19hh3i"))

(define rust-mio-1.2.0
  (crate-source "mio" "1.2.0"
                "1hanrh4fwsfkdqdaqfidz48zz1wdix23zwn3r2x78am0garfbdsh"))

(define rust-mock-instant-0.6.0
  (crate-source "mock_instant" "0.6.0"
                "1rjnp4a583k31lbj4qiaqf8yyc675p49sblx267kib2c14vdvrnw"))

(define rust-moka-0.12.15
  (crate-source "moka" "0.12.15"
                "0ijhgjfprcjkvrhgmm71m9gaph5lc9xjb3rwz4wyhbh42anjhwlm"))

(define rust-multimap-0.10.1
  (crate-source "multimap" "0.10.1"
                "1150lf0hjfjj4ksb8s3y0hl7a2nqzqlbh0is7vdym2iyjfrfr1qx"))

(define rust-ndarray-0.16.1
  (crate-source "ndarray" "0.16.1"
                "0ha8sg5ad501pgkxw0wczh8myc2ma3gyxgcny4mq8rckrqnxfbl8"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-num-bigint-dig-0.8.6
  (crate-source "num-bigint-dig" "0.8.6"
                "1dxh3d8pzjc5k0kpy8gy2qhhhqs7zw8a7m564zl3ib8gcjkdsqg6"))

(define rust-num-conv-0.2.1
  (crate-source "num-conv" "0.2.1"
                "0rqrr29brafaa2za352pbmhkk556n7f8z9rrkgmjp1idvdl3fry6"))

(define rust-num-iter-0.1.45
  (crate-source "num-iter" "0.1.45"
                "1gzm7vc5g9qsjjl3bqk9rz1h6raxhygbrcpbfl04swlh0i506a8l"))

(define rust-num-enum-0.7.6
  (crate-source "num_enum" "0.7.6"
                "09kg0c2y08npdv0c9dbm4m9a9wz8w2qaiqqxl4gj3v22hj1wl2sx"))

(define rust-num-enum-derive-0.7.6
  (crate-source "num_enum_derive" "0.7.6"
                "1y0x9z49s27vdas6mglqbv02sgkdmbr8ns2kwspzrp2ra81rh2b8"))

(define rust-object-0.37.3
  (crate-source "object" "0.37.3"
                "1zikiy9xhk6lfx1dn2gn2pxbnfpmlkn0byd7ib1n720x0cgj0xpz"))

(define rust-object-store-0.12.5
  (crate-source "object_store" "0.12.5"
                "001a3rdd57fivhp2ym2w6fdk3x7cm1yba459r07b1jpc1bsgzyzv"))

(define rust-object-store-0.13.2
  (crate-source "object_store" "0.13.2"
                "0jcajnvha22jrla3i9as7n9mrra0m864p00mxvi10g0d234wnak2"))

(define rust-object-store-opendal-0.55.0
  (crate-source "object_store_opendal" "0.55.0"
                "14c9db5xcf4hv2j8p3m29wssbg88vsc7nh2pbrcfwblpkrvb0fhi"))

(define rust-opendal-0.55.0
  (crate-source "opendal" "0.55.0"
                "02n89cw8clyrg15py0rrp7w2p9w6wjgln2nf3fyb8sis425anxfh"))

(define rust-openssl-probe-0.2.1
  (crate-source "openssl-probe" "0.2.1"
                "1gpwpb7smfhkscwvbri8xzbab39wcnby1jgz1s49vf1aqgsdx1vw"))

(define rust-ordered-float-2.10.1
  (crate-source "ordered-float" "2.10.1"
                "075i108hr95pr7hy4fgxivib5pky3b6b22rywya5qyd2wmkrvwb8"))

(define rust-ordered-float-5.3.0
  (crate-source "ordered-float" "5.3.0"
                "03mx5yg3ncp0g524y7zbyvhwcxpd8l9v30lgybm5bhqx2v551ndp"))

(define rust-ordered-multimap-0.7.3
  (crate-source "ordered-multimap" "0.7.3"
                "0ygg08g2h381r3zbclba4zx4amm25zd2hsqqmlxljc00mvf3q829"))

(define rust-outref-0.5.2
  (crate-source "outref" "0.5.2"
                "03pzw9aj4qskqhh0fkagy2mkgfwgj5a1m67ajlba5hw80h68100s"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.12.5
  (crate-source "parking_lot" "0.12.5"
                "06jsqh9aqmc94j2rlm8gpccilqm6bskbd67zf6ypfc0f4m9p91ck"))

(define rust-parking-lot-core-0.9.12
  (crate-source "parking_lot_core" "0.9.12"
                "1hb4rggy70fwa1w9nb0svbyflzdc69h047482v2z3sx2hmcnh896"))

(define rust-parquet-58.3.0
  (crate-source "parquet" "58.3.0"
                "0259r377fm580n53bzn4m4772dha19ajj60cvm3jmdl5238agbsx"))

(define rust-path-abs-0.5.1
  (crate-source "path_abs" "0.5.1"
                "1hrkjrk9w2mhgzwl84gl2lmajs7yagdrcpxnjf51vh1a6kv05vq5"))

(define rust-pbjson-0.8.0
  (crate-source "pbjson" "0.8.0"
                "1hyr4d14p9j53nqz6j9pikp2vajvjrz872c29rxaa2qdl0zsr2w9"))

(define rust-pbjson-build-0.8.0
  (crate-source "pbjson-build" "0.8.0"
                "1590n3gpnnk5jv934g8kwffcgi8q6cjzl3xvinki68jsca5d08mg"))

(define rust-pbjson-types-0.8.0
  (crate-source "pbjson-types" "0.8.0"
                "09nmx9hvdnas05jsnax366ccf2lchadz5f9vxqqa642g6wl8wx4f"))

(define rust-pbkdf2-0.12.2
  (crate-source "pbkdf2" "0.12.2"
                "1wms79jh4flpy1zi8xdp4h8ccxv4d85adc6zjagknvppc5vnmvgq"))

(define rust-pem-3.0.6
  (crate-source "pem" "3.0.6"
                "1glia9vv51wx79cysqxgdha6g1bwbbr20bfhijlk2nxw4qycac0x"))

(define rust-pem-rfc7468-0.7.0
  (crate-source "pem-rfc7468" "0.7.0"
                "04l4852scl4zdva31c1z6jafbak0ni5pi0j38ml108zwzjdrrcw8"))

(define rust-permutation-0.4.1
  (crate-source "permutation" "0.4.1"
                "1msp65z2q3f7p4fkz5b2h6agn03v01xz5mdgancki3jv1w5jn86z"))

(define rust-petgraph-0.8.3
  (crate-source "petgraph" "0.8.3"
                "0mblnaqbx1y20h5y7pz6y11hk9jjk6k87lsmn7jxaq3hm67ba0c7"))

(define rust-phf-0.12.1
  (crate-source "phf" "0.12.1"
                "1dz85g1wshfca83mrq3va9rm9n8qcdjlpv1i3908y5zc9j4p6cli"))

(define rust-phf-0.13.1
  (crate-source "phf" "0.13.1"
                "1pzswx5gdglgjgp4azyzwyr4gh031r0kcnpqq6jblga72z3jsmn1"))

(define rust-phf-codegen-0.13.1
  (crate-source "phf_codegen" "0.13.1"
                "1qfnsl2hiny0yg4lwn888xla5iwccszgxnx8dhbwl6s2h2fpzaj9"))

(define rust-phf-generator-0.13.1
  (crate-source "phf_generator" "0.13.1"
                "0dwpp11l41dy9mag4phkyyvhpf66lwbp79q3ik44wmhyfqxcwnhk"))

(define rust-phf-shared-0.12.1
  (crate-source "phf_shared" "0.12.1"
                "10cr16wpmbjxd7w6k98sxw9yw3zxnzscybl9jzyq3digi045a006"))

(define rust-phf-shared-0.13.1
  (crate-source "phf_shared" "0.13.1"
                "0rpjchnswm0x5l4mz9xqfpw0j4w68sjvyqrdrv13h7lqqmmyyzz5"))

(define rust-pin-project-1.1.12
  (crate-source "pin-project" "1.1.12"
                "1sbcs3s240z2w4jaga53c3jl5maw4qprf0a9kfcagcq0h7kdkw6b"))

(define rust-pin-project-internal-1.1.12
  (crate-source "pin-project-internal" "1.1.12"
                "12a3c85sa005ahk1qm673h1akx2fa8qfvpb0ybd5aj788cpy5459"))

(define rust-pin-project-lite-0.2.17
  (crate-source "pin-project-lite" "0.2.17"
                "1kfmwvs271si96zay4mm8887v5khw0c27jc9srw1a75ykvgj54x8"))

(define rust-pkcs1-0.7.5
  (crate-source "pkcs1" "0.7.5"
                "0zz4mil3nchnxljdfs2k5ab1cjqn7kq5lqp62n9qfix01zqvkzy8"))

(define rust-pkcs5-0.7.1
  (crate-source "pkcs5" "0.7.1"
                "19k9igzay529fqj90qdkgnvmvwp65wzw73h2vn3sigqq3b4y4iz8"))

(define rust-pkcs8-0.10.2
  (crate-source "pkcs8" "0.10.2"
                "1dx7w21gvn07azszgqd3ryjhyphsrjrmq5mmz1fbxkj5g0vv4l7r"))

(define rust-portable-atomic-util-0.2.7
  (crate-source "portable-atomic-util" "0.2.7"
                "0616j0fhy6y71hyxg3n86f6hng0fmsc269s3wp4gl8ww4p8hd8f2"))

(define rust-potential-utf-0.1.5
  (crate-source "potential_utf" "0.1.5"
                "0r0518fr32xbkgzqap509s3r60cr0iancsg9j1jgf37cyz7b20q1"))

(define rust-proc-macro-crate-3.5.0
  (crate-source "proc-macro-crate" "3.5.0"
                "0kv1g1d1zjwxlgcaba2qlshzyy32j03xic8rskqlcr5mnblsfyz6"))

(define rust-prost-0.14.3
  (crate-source "prost" "0.14.3"
                "0s057z9nzggzy7x4bbsiar852hg7zb81f4z4phcdb0ig99971snj"))

(define rust-prost-build-0.14.3
  (crate-source "prost-build" "0.14.3"
                "1rrf4rs74schd38jyaxglymi66vxzzg6hki00fdq7nkf0pbkng9l"))

(define rust-prost-derive-0.14.3
  (crate-source "prost-derive" "0.14.3"
                "02zvva6kb0pfvlyc4nac6gd37ncjrs8jq5scxcq4nbqkc8wh5ii7"))

(define rust-prost-types-0.14.3
  (crate-source "prost-types" "0.14.3"
                "1mrxrciryfgi6a0vmrgyj3g27r9hdhlgwkq71cgv3icbvg5w94c9"))

(define rust-psm-0.1.31
  (crate-source "psm" "0.1.31"
                "1sk1wzb8j64b9f3z863lv45cgri6ikhys5pgwdfrnv9ldr4bwpb4"))

(define rust-pyo3-0.28.3
  (crate-source "pyo3" "0.28.3"
                "04hwqcrfx9w3f67pnhjcg28y0iq1srpwv0drgwbd23mmlcw8xzci"))

(define rust-pyo3-build-config-0.28.3
  (crate-source "pyo3-build-config" "0.28.3"
                "07k16mnxn220x4aw0axzcss4mn4gckhknf7qlyyck67bzpfyfs73"))

(define rust-pyo3-ffi-0.28.3
  (crate-source "pyo3-ffi" "0.28.3"
                "07k5bxh8h2ax3v6gmb43x09wsgm003lar7pnyz57q7qbz05f2abz"))

(define rust-pyo3-macros-0.28.3
  (crate-source "pyo3-macros" "0.28.3"
                "04wqy9knmxkf2m12dfwbj4817p959chxhzgwsabmki27zw754vnz"))

(define rust-pyo3-macros-backend-0.28.3
  (crate-source "pyo3-macros-backend" "0.28.3"
                "1jrsh65i0vwinp5k6blbvypv8idgg0h853rkqa0qywrmv0cc5kf4"))

(define rust-pythonize-0.28.0
  (crate-source "pythonize" "0.28.0"
                "159zqj0zq32cghdg1yk9pdq6kfmnawdh30853ijqnv32r5qgcy8b"))

(define rust-quick-xml-0.38.4
  (crate-source "quick-xml" "0.38.4"
                "0772siy4d9vlq77842012c8cycs3y0szxkv62rh9sh2sqmc20v5n"))

(define rust-quinn-proto-0.11.14
  (crate-source "quinn-proto" "0.11.14"
                "1660jkxhzi1pnywzs13ifczwrlv6ds9qds111vsnxjciqpz44js3"))

(define rust-rand-0.8.6
  (crate-source "rand" "0.8.6"
                "12kd4rljn86m00rcaz4c1rcya4mb4gk5ig6i8xq00a8wjgxfr82w"))

(define rust-rand-0.9.4
  (crate-source "rand" "0.9.4"
                "1sknbxgs6nfg0nxdd7689lwbyr2i4vaswchrv4b34z8vpc3azia4"))

(define rust-rand-core-0.9.5
  (crate-source "rand_core" "0.9.5"
                "0g6qc5r3f0hdmz9b11nripyp9qqrzb0xqk9piip8w8qlvqkcibvn"))

(define rust-rand-distr-0.5.1
  (crate-source "rand_distr" "0.5.1"
                "0qvlzxq4a2rvrf3wq0xq1bfw8iy9zqm6jlmbywqzld6g1paib1ka"))

(define rust-rand-xoshiro-0.7.0
  (crate-source "rand_xoshiro" "0.7.0"
                "0h9dv9mn703zb2z5dys7vc4rzy3az8xg99fc5m8zbnh0axkg80zp"))

(define rust-random-word-0.5.2
  (crate-source "random_word" "0.5.2"
                "10fgg2fx9iha3vw9ldpqll7wjpgjrxmjs1l97j42ni2mvddkjyp4"))

(define rust-rangemap-1.7.1
  (crate-source "rangemap" "1.7.1"
                "0s7am2w72siggn668h03gn3g06gsinv6m1jaaxmnbj59177l6d4p"))

(define rust-rayon-1.12.0
  (crate-source "rayon" "1.12.0"
                "0vcj63xgnk72c30vdrak7dhl53snnaqv9x2faf1d94hzg1kb2fgv"))

(define rust-recursive-0.1.1
  (crate-source "recursive" "0.1.1"
                "0gmlaih5kyqc1pkbk0klqr9m65c4bvz6j0mwn68z8q5pxcys91h7"))

(define rust-recursive-proc-macro-impl-0.1.1
  (crate-source "recursive-proc-macro-impl" "0.1.1"
                "12z3wy2wa4l2dpfdb5vhaaiy78l130x5w9fflb0py1ql0sz9y03n"))

(define rust-redox-syscall-0.5.18
  (crate-source "redox_syscall" "0.5.18"
                "0b9n38zsxylql36vybw18if68yc9jczxmbyzdwyhb9sifmag4azd"))

(define rust-ref-cast-1.0.25
  (crate-source "ref-cast" "1.0.25"
                "0zdzc34qjva9xxgs889z5iz787g81hznk12zbk4g2xkgwq530m7k"))

(define rust-ref-cast-impl-1.0.25
  (crate-source "ref-cast-impl" "1.0.25"
                "1nkhn1fklmn342z5c4mzfzlxddv3x8yhxwwk02cj06djvh36065p"))

(define rust-regex-lite-0.1.9
  (crate-source "regex-lite" "0.1.9"
                "0wzr31ysmiy9sw48i36raqbm1iyk2xnq0lp4zbs6fzi47p3k9f6a"))

(define rust-regress-0.10.5
  (crate-source "regress" "0.10.5"
                "0j7v33qwc9j4lyi7r59r26nasy82p4d04f0m9ll97a38bqrb4mr0"))

(define rust-repr-offset-0.2.2
  (crate-source "repr_offset" "0.2.2"
                "1skj3cy77j7vwslnjjzgladq61z6jjvwlw89kp0zz7fjbdsp047v"))

(define rust-reqsign-0.16.5
  (crate-source "reqsign" "0.16.5"
                "00fpgvbwfiq4a0z468y17bnr1k6d5v8vhpy2hh35k9wh6nzisia3"))

(define rust-reqwest-0.12.28
  (crate-source "reqwest" "0.12.28"
                "0iqidijghgqbzl3bjg5hb4zmigwa4r612bgi0yiq0c90b6jkrpgd"))

(define rust-roaring-0.11.4
  (crate-source "roaring" "0.11.4"
                "1af4qdcpc7vb7z00457xygf44gr5p5djkwzmbvdkpjvfiijwbv8x"))

(define rust-robust-1.2.0
  (crate-source "robust" "1.2.0"
                "0fcqw67hgs1k9parr6rbkhzkjbd1my9n44fb1v7sv80wp65yw9sf"))

(define rust-rsa-0.9.10
  (crate-source "rsa" "0.9.10"
                "0bdikdwhcvl1gfh4637m5rdw3fgcl752aiygvzmwlgc8yl1kymxq"))

(define rust-rstar-0.12.2
  (crate-source "rstar" "0.12.2"
                "1fsx2z2l6nq2fd95g9yvw1a9qvypllq9q6aqb3x6vlng7k8h0522"))

(define rust-rust-ini-0.21.3
  (crate-source "rust-ini" "0.21.3"
                "1iw8yss8ncygd9yx5ay5gmr2jk7vcyv1d0d5pr1jlfcncqmqsvkr"))

(define rust-rustc-hash-2.1.2
  (crate-source "rustc-hash" "2.1.2"
                "1gjdc5bw9982cj176jvgz9rrqf9xvr1q1ddpzywf5qhs7yzhlc4l"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustls-0.23.40
  (crate-source "rustls" "0.23.40"
                "12qnv3ag4wrw7aj8jng74kgrilpjm2b1rfcjaac8h691frccv1pg"))

(define rust-rustls-native-certs-0.8.3
  (crate-source "rustls-native-certs" "0.8.3"
                "0qrajg2n90bcr3bcq6j95gjm7a9lirfkkdmjj32419dyyzan0931"))

(define rust-rustls-pki-types-1.14.1
  (crate-source "rustls-pki-types" "1.14.1"
                "1a9pr54y0f3qr97bxpd3ahjldq0gqdld0h799xbnwdzbwxx1k9rh"))

(define rust-rustls-webpki-0.103.13
  (crate-source "rustls-webpki" "0.103.13"
                "0vkm7z9pnxz5qz66p2kmyy2pwx0g4jnsbqk5xzfhs4czcjl2ki31"))

(define rust-ryu-1.0.23
  (crate-source "ryu" "1.0.23"
                "0zs70sg00l2fb9jwrf6cbkdyscjs53anrvai2hf7npyyfi5blx4p"))

(define rust-salsa20-0.10.2
  (crate-source "salsa20" "0.10.2"
                "04w211x17xzny53f83p8f7cj7k2hi8zck282q5aajwqzydd2z8lp"))

(define rust-schannel-0.1.29
  (crate-source "schannel" "0.1.29"
                "0ffrzz5vf2s3gnzvphgb5gg8fqifvryl07qcf7q3x1scj3jbghci"))

(define rust-schemars-0.9.0
  (crate-source "schemars" "0.9.0"
                "0pqncln5hqbzbl2r3yayyr4a82jjf93h2cfxrn0xamvx77wr3lac"))

(define rust-schemars-1.2.1
  (crate-source "schemars" "1.2.1"
                "1k16qzpdpy6p9hrh18q2l6cwawxzyqi25f8masa13l0wm8v2zd52"))

(define rust-scoped-tls-1.0.1
  (crate-source "scoped-tls" "1.0.1"
                "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1"))

(define rust-scrypt-0.11.0
  (crate-source "scrypt" "0.11.0"
                "07zxfaqpns9jn0mnxm7wj3ksqsinyfpirkav1f7kc2bchs2s65h5"))

(define rust-security-framework-3.7.0
  (crate-source "security-framework" "3.7.0"
                "07fd0j29j8yczb3hd430vwz784lx9knb5xwbvqna1nbkbivvrx5p"))

(define rust-security-framework-sys-2.17.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "security-framework-sys" "2.17.0"
                "1qr0w0y9iwvmv3hwg653q1igngnc5b74xcf0679cbv23z0fnkqkc"))

(define rust-semver-1.0.28
  (crate-source "semver" "1.0.28"
                "1kaimrpy876bcgi8bfj0qqfxk77zm9iz2zhn1hp9hj685z854y4a"))

(define rust-seq-macro-0.3.6
  (crate-source "seq-macro" "0.3.6"
                "1k4sshn0x2i6a9g97sy5jl7ghlqgmmh3n76aj3rrjwxy1x0i3iqv"))

(define rust-serde-path-to-error-0.1.20
  (crate-source "serde_path_to_error" "0.1.20"
                "0mxls44p2ycmnxh03zpnlxxygq42w61ws7ir7r0ba6rp5s1gza8h"))

(define rust-serde-tokenstream-0.2.3
  (crate-source "serde_tokenstream" "0.2.3"
                "0sccagn1g06j6yhdhs57v9v5v27n2hzk7frfbhyg209cqn2rbi6p"))

(define rust-serde-with-3.20.0
  (crate-source "serde_with" "3.20.0"
                "1qnddis0nz2yg0dl06fnhf2q3hkim0vraq8ac3xzl8xjnwn1qb77"))

(define rust-serde-with-macros-3.20.0
  (crate-source "serde_with_macros" "3.20.0"
                "1b5z2zs1flszvyfk2i5pky6qdigg82y467zlc81gpd7c723lh35r"))

(define rust-serde-yaml-0.9.34+deprecated
  (crate-source "serde_yaml" "0.9.34+deprecated"
                "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))

(define rust-signal-hook-registry-1.4.8
  (crate-source "signal-hook-registry" "1.4.8"
                "06vc7pmnki6lmxar3z31gkyg9cw7py5x9g7px70gy2hil75nkny4"))

(define rust-signature-2.2.0
  (crate-source "signature" "2.2.0"
                "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p"))

(define rust-simd-adler32-0.3.9
  (crate-source "simd-adler32" "0.3.9"
                "0532ysdwcvzyp2bwpk8qz0hijplcdwpssr5gy5r7qwqqy5z5qgbh"))

(define rust-simdutf8-0.1.5
  (crate-source "simdutf8" "0.1.5"
                "0vmpf7xaa0dnaikib5jlx6y4dxd3hxqz6l830qb079g7wcsgxag3"))

(define rust-simple-asn1-0.6.4
  (crate-source "simple_asn1" "0.6.4"
                "07azmvch32mc0644cz2bs5h2fl9dn2xg2dg6bqybw45cn2bmjn0d"))

(define rust-siphasher-1.0.3
  (crate-source "siphasher" "1.0.3"
                "0jg6l9xyzca5vy4h6gf8r6p4kk84g98fk95pzig1kq6cr4z8grcf"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-snafu-0.9.0
  (crate-source "snafu" "0.9.0"
                "1ii9r99x5qcn754m624yzgb9hzvkqkrcygf0aqh0pyb9dbnvrm6i"))

(define rust-snafu-derive-0.9.0
  (crate-source "snafu-derive" "0.9.0"
                "0h0x61kyj4fvilcr2nj02l85shw1ika64vq9brf2gyna662ln9al"))

(define rust-snap-1.1.1
  (crate-source "snap" "1.1.1"
                "0fxw80m831l76a5zxcwmz2aq7mcwc1pp345pnljl4cv1kbxnfsqv"))

(define rust-socket2-0.6.3
  (crate-source "socket2" "0.6.3"
                "0gkjjcyn69hqhhlh5kl8byk5m0d7hyrp2aqwzbs3d33q208nwxis"))

(define rust-spade-2.15.1
  (crate-source "spade" "2.15.1"
                "1yn7z3034nw7ihzqa37kzvzkmjcy9w3mnqzmhjqh16rlv6gkk6cn"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-spki-0.7.3
  (crate-source "spki" "0.7.3"
                "17fj8k5fmx4w9mp27l970clrh5qa7r5sjdvbsln987xhb34dc7nr"))

(define rust-sqlparser-0.61.0
  (crate-source "sqlparser" "0.61.0"
                "1dqc419qs0cmbd62j8pwrqxn1giakb5fpayby4d8x03w9n6ymxfv"))

(define rust-sqlparser-derive-0.5.0
  (crate-source "sqlparser_derive" "0.5.0"
                "12fjbfbflmr3v6cav9p4a9gnmpxvri10w6dpzfdjjy8wzkc4bpd6"))

(define rust-stacker-0.1.24
  (crate-source "stacker" "0.1.24"
                "141i1f49xgnsfymvk5kbddg6xfaspwxwl0qqrddjzcdnjbfqq334"))

(define rust-std-prelude-0.2.12
  (crate-source "std_prelude" "0.2.12"
                "1ghcwnhnqn3rphyhlknmxpj5clzqva46z1vh25k5bpzzan2ff1w2"))

(define rust-stfu8-0.2.7
  (crate-source "stfu8" "0.2.7"
                "0y0rzzphh2mzfhjz0sxymnjn0s4ap21c74f469s9xycky24iw7z5"))

(define rust-substrait-0.62.2
  (crate-source "substrait" "0.62.2"
                "1l25w121szsfi9zsh38lv7yi48a7baagghxrrir9g6qj7944pz32"))

(define rust-tagptr-0.2.0
  (crate-source "tagptr" "0.2.0"
                "05r4mwvlsclx1ayj65hpzjv3dn4wpi8j4xm695vydccf9k7r683v"))

(define rust-tar-0.4.45
  (crate-source "tar" "0.4.45"
                "0wq90hif25348zrvmk88q01g8aj8v8pla7f1vxgsf7x2frj2ls92"))

(define rust-tempfile-3.27.0
  (crate-source "tempfile" "3.27.0"
                "1gblhnyfjsbg9wjg194n89wrzah7jy3yzgnyzhp56f3v9jd7wj9j"))

(define rust-thread-tree-0.3.3
  (crate-source "thread-tree" "0.3.3"
                "0c6n8m5xrxffxkvfqbn7z09n38r493hn77sdjljkm5a7p063gggz"))

(define rust-thrift-0.17.0
  (crate-source "thrift" "0.17.0"
                "02cydaqqlp25ri19y3ixi77a7nd85fwvbfn4fp0qpakzzj2vqm3y"))

(define rust-time-0.3.47
  (crate-source "time" "0.3.47"
                "0b7g9ly2iabrlgizliz6v5x23yq5d6bpp0mqz6407z1s526d8fvl"))

(define rust-time-core-0.1.8
  (crate-source "time-core" "0.1.8"
                "1jidl426mw48i7hjj4hs9vxgd9lwqq4vyalm4q8d7y4iwz7y353n"))

(define rust-time-macros-0.2.27
  (crate-source "time-macros" "0.2.27"
                "058ja265waq275wxvnfwavbz9r1hd4dgwpfn7a1a9a70l32y8w1f"))

(define rust-tiny-keccak-2.0.2
  (crate-source "tiny-keccak" "2.0.2"
                "0dq2x0hjffmixgyf6xv9wgsbcxkd65ld0wrfqmagji8a829kg79c"))

(define rust-tinystr-0.8.3
  (crate-source "tinystr" "0.8.3"
                "0vfr8x285w6zsqhna0a9jyhylwiafb2kc8pj2qaqaahw48236cn8"))

(define rust-tokio-1.52.3
  (crate-source "tokio" "1.52.3"
                "1zpzazypkg61sw91na1m85x5s4rsjym335fwwhwm1hcs70dz1iwg"))

(define rust-tokio-macros-2.7.0
  (crate-source "tokio-macros" "2.7.0"
                "15m4f37mdafs0gg36sh0rskm1i768lb7zmp8bw67kaxr3avnqniq"))

(define rust-tokio-stream-0.1.18
  (crate-source "tokio-stream" "0.1.18"
                "0w3cj33605ab58wqd382gnla5pnd9hnr00xgg333np5bka04knij"))

(define rust-tokio-util-0.7.18
  (crate-source "tokio-util" "0.7.18"
                "1600rd47pylwn7cap1k7s5nvdaa9j7w8kqigzp1qy7mh0p4cxscs"))

(define rust-toml-datetime-1.1.1+spec-1.1.0
  (crate-source "toml_datetime" "1.1.1+spec-1.1.0"
                "1mws2mkkf46l7inn77azhm0vdwxngv9vsbhbl0ah33p2c9gzcr9i"))

(define rust-toml-edit-0.25.11+spec-1.1.0
  (crate-source "toml_edit" "0.25.11+spec-1.1.0"
                "0awzffbkx33v9x4h19b5mfrwp3sn4ifr16y58sbk6j6l5v9c8n8b"))

(define rust-toml-parser-1.1.2+spec-1.1.0
  (crate-source "toml_parser" "1.1.2+spec-1.1.0"
                "09kmzc55a0j21whm290wlf5a8b18a0qc87a1s8sncrckc6wfkax2"))

(define rust-tower-0.5.3
  (crate-source "tower" "0.5.3"
                "1m5i3a2z1sgs8nnz1hgfq2nr4clpdmizlp1d9qsg358ma5iyzrgb"))

(define rust-tower-http-0.5.2
  (crate-source "tower-http" "0.5.2"
                "1xakj3x0anp55gjqibiwvzma5iz0w9pcjsr7qk97sx4qm4sd970y"))

(define rust-tower-http-0.6.10
  (crate-source "tower-http" "0.6.10"
                "0lfbddgrhmxhnb3afazawsl4cxqfcs8wvq5hm34ija0wz3czvmk8"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-chrome-0.7.2
  (crate-source "tracing-chrome" "0.7.2"
                "0977zy46gpawva2laffigxr2pph8v0xa51kfp6ghlifnsn7762mz"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-subscriber-0.3.23
  (crate-source "tracing-subscriber" "0.3.23"
                "06fkr0qhggvrs861d7f74pn3i3a10h5jsp4n70jj9ys5b675fzyb"))

(define rust-tstr-0.2.4
  (crate-source "tstr" "0.2.4"
                "19yvgfipfrqjymvfi7cs6ipdc91f1dw2s2nxs1vf9ajby6a053kz"))

(define rust-tstr-proc-macros-0.2.2
  (crate-source "tstr_proc_macros" "0.2.2"
                "0yklq0k0s3c4y0k5f0qm13lw7nvz5z97x3yhmyw1if0cdc3250g7"))

(define rust-twox-hash-2.1.2
  (crate-source "twox-hash" "2.1.2"
                "1721278f1yc5zvkpdb8gsb1x6nlfjdmwm5fk9ff3fismcxmi78wy"))

(define rust-typewit-1.15.2
  (crate-source "typewit" "1.15.2"
                "1xmmbcykcn6xrsrp8dd3bfdy6j70c4ccmf89cb0cp18p36ra0k11"))

(define rust-typify-0.5.0
  (crate-source "typify" "0.5.0"
                "0ad6g8ji229na1nbfn3iw4abkp4kkyr3k3q9lj5gmc9fyv3brmg6"))

(define rust-typify-impl-0.5.0
  (crate-source "typify-impl" "0.5.0"
                "0h12rf9pdqxp2zrvb3cn092mcm505ldim8bzjjz9wkzsgygkbsx1"))

(define rust-typify-macro-0.5.0
  (crate-source "typify-macro" "0.5.0"
                "0ymbz3z2ybp8q266ws81xjp76vaypfz630r23f608jsir3rk474i"))

(define rust-unicase-2.9.0
  (crate-source "unicase" "2.9.0"
                "0hh1wrfd7807mfph2q67jsxqgw8hm82xg2fb8ln8cvblkwxbri6v"))

(define rust-unicode-blocks-0.1.9
  (crate-source "unicode-blocks" "0.1.9"
                "06sqfv9iz0drwrgl969scjbb6qf2cg1bhsxvm5ik2dq6krfy04kb"))

(define rust-unicode-segmentation-1.13.2
  (crate-source "unicode-segmentation" "1.13.2"
                "135a26m4a0wj319gcw28j6a5aqvz00jmgwgmcs6szgxjf942facn"))

(define rust-unsafe-libyaml-0.2.11
  (crate-source "unsafe-libyaml" "0.2.11"
                "0qdq69ffl3v5pzx9kzxbghzn0fzn266i1xn70y88maybz9csqfk7"))

(define rust-url-2.5.8
  (crate-source "url" "2.5.8"
                "1v8f7nx3hpr1qh76if0a04sj08k86amsq4h8cvpw6wvk76jahrzz"))

(define rust-utf8-ranges-1.0.5
  (crate-source "utf8-ranges" "1.0.5"
                "1fk46654sqis2dqamihlj9b1sv162kp3brgmmqpa0lqfz4kwikvz"))

(define rust-uuid-1.23.1
  (crate-source "uuid" "1.23.1"
                "0xlwg23rmsfl3gx98qsyzpl24pf4bs9wi3mqx5c6i319hyb4mmyx"))

(define rust-vsimd-0.8.0
  (crate-source "vsimd" "0.8.0"
                "0r4wn54jxb12r0x023r5yxcrqk785akmbddqkcafz9fm03584c2w"))

(define rust-wasm-bindgen-futures-0.4.71
  (crate-source "wasm-bindgen-futures" "0.4.71"
                "1f3k8r13nqshrlxwq0naxpbh250b4l6p526wlw2m78pv7w6jsjcn"))

(define rust-web-sys-0.3.98
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "web-sys" "0.3.98"
                "1aijiwx7wsfzj37p1gnqn6wv4j2ppf4rqwhrzb8blf6gigzjsmsb"))

(define rust-webpki-roots-1.0.7
  (crate-source "webpki-roots" "1.0.7"
                "17gblaqmp51znxd2c18c04k8yfnf7s77c04n6hdmzxbcr52fxxaj"))

(define rust-winnow-1.0.2
  (crate-source "winnow" "1.0.2"
                "1l7xnfvlgy4da6gq5ip2bgcm8i9d0rwzaxg1p88nlw8lxy5p1q9f"))

(define rust-wkb-0.9.2
  (crate-source "wkb" "0.9.2"
                "1achz19020xmaiifmak4rm8cp3l37p12f16m4s8785xdqwvb6851"))

(define rust-wkt-0.14.0
  (crate-source "wkt" "0.14.0"
                "1mw19dz28lslwarqd3ncq0d9vfjml0raiylzalp330n8rhivkcpg"))

(define rust-writeable-0.6.3
  (crate-source "writeable" "0.6.3"
                "1i54d13h9bpap2hf13xcry1s4lxh7ap3923g8f3c0grd7c9fbyhz"))

(define rust-xattr-1.6.1
  (crate-source "xattr" "1.6.1"
                "0ml1mb43gqasawillql6b344m0zgq8mz0isi11wj8vbg43a5mr1j"))

(define rust-xmlparser-0.13.6
  (crate-source "xmlparser" "0.13.6"
                "1r796g21c70p983ax0j6rmhzmalg4rhx61mvd4farxdhfyvy1zk6"))

(define rust-xxhash-rust-0.8.15
  (crate-source "xxhash-rust" "0.8.15"
                "1lrmffpn45d967afw7f1p300rsx7ill66irrskxpcm1p41a0rlpx"))

(define rust-yada-0.5.1
  (crate-source "yada" "0.5.1"
                "1pgzmm965f5396q1mj5rfbxmmd7hmnynr435hx8h5a28ksyi3ldf"))

(define rust-yoke-0.8.2
  (crate-source "yoke" "0.8.2"
                "1jprcs7a98a5whvfs6r3jvfh1nnfp6zyijl7y4ywmn88lzywbs5b"))

(define rust-yoke-derive-0.8.2
  (crate-source "yoke-derive" "0.8.2"
                "13l5y5sz4lqm7rmyakjbh6vwgikxiql51xfff9hq2j485hk4r16y"))

(define rust-zerofrom-0.1.7
  (crate-source "zerofrom" "0.1.7"
                "1py40in4rirc9q8w36q67pld0zk8ssg024xhh0cncxgal7ra3yk9"))

(define rust-zerofrom-derive-0.1.7
  (crate-source "zerofrom-derive" "0.1.7"
                "18c4wsnznhdxx6m80piil1lbyszdiwsshgjrybqcm4b6qic22lqi"))

(define rust-zerotrie-0.2.4
  (crate-source "zerotrie" "0.2.4"
                "1gr0pkcn3qsr6in6iixqyp0vbzwf2j1jzyvh7yl2yydh3p9m548g"))

(define rust-zerovec-0.11.6
  (crate-source "zerovec" "0.11.6"
                "0fdjsy6b31q9i0d73sl7xjd12xadbwi45lkpfgqnmasrqg5i3ych"))

(define rust-zerovec-derive-0.11.3
  (crate-source "zerovec-derive" "0.11.3"
                "0m85qj92mmfvhjra6ziqky5b1p4kcmp5069k7kfadp5hr8jw8pb2"))

(define rust-zlib-rs-0.6.3
  (crate-source "zlib-rs" "0.6.3"
                "04qmv85amq6sv73bzqgvnlsk9mnrl97rygzf2v4zjcx1807d9qrv"))

(define rust-zstd-0.13.3
  (crate-source "zstd" "0.13.3"
                "12n0h4w9l526li7jl972rxpyf012jw3nwmji2qbjghv9ll8y67p9"))

(define rust-zstd-safe-7.2.4
  (crate-source "zstd-safe" "7.2.4"
                "179vxmkzhpz6cq6mfzvgwc99bpgllkr6lwxq7ylh5dmby3aw8jcg"))

(define rust-zstd-sys-2.0.16+zstd.1.5.7
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "zstd-sys" "2.0.16+zstd.1.5.7"
                "0j1pd2iaqpvaxlgqmmijj68wma7xwdv9grrr63j873yw5ay9xqci"))

(define rust-arc-swap-1.9.1
  (crate-source "arc-swap" "1.9.1"
                "01xjlahcya8igdalxmda375lnlhjqwjz0cdqhy0bc1jkyzb1yfka"))

(define rust-argminmax-0.6.3
  (crate-source "argminmax" "0.6.3"
                "0rcy6nq86wqwfbqpxzpdq8lpmx76c66ifd7fg7nd5j0slh83vwbh"))

(define rust-array-init-cursor-0.2.1
  (crate-source "array-init-cursor" "0.2.1"
                "1hqzgcw4930bp8gw2qy10nfyw7c3kwgwaf5yd2klw7ad487zwlgd"))

(define rust-async-convert-1.0.0
  (crate-source "async-convert" "1.0.0"
                "1bhb6745qwljbmgciccmfavzkf32w56qfhnda8qy84kpx7p6yhbd"))

(define rust-async-openai-0.20.0
  (crate-source "async-openai" "0.20.0"
                "14c437jmv3013siqcrrb9p1zv9k1pfhhnw79mq62dqqfbsf7zs8i"))

(define rust-atoi-simd-0.15.6
  (crate-source "atoi_simd" "0.15.6"
                "1a98kvaqyhb1shi2c6qhvklahc7ckvpmibcy319i6g1i9xqkgq4s"))

(define rust-aws-config-1.8.16
  (crate-source "aws-config" "1.8.16"
                "17fax6amdj3q3w5nql4l4qig2745mk21dr1ylmd5zx9cvnn5dwah"))

(define rust-aws-credential-types-1.2.14
  (crate-source "aws-credential-types" "1.2.14"
                "1xyagxr44jzl9li8z1vk2m0zj2h9qahgn19hzqhy26rs6ydpj84g"))

(define rust-aws-runtime-1.7.3
  (crate-source "aws-runtime" "1.7.3"
                "0r18xr6jxwixmrgnwy61h23mmgkrrryhd213ac9p9b094b497kax"))

(define rust-aws-sdk-bedrockruntime-1.130.0
  (crate-source "aws-sdk-bedrockruntime" "1.130.0"
                "03sl4rsrvg8f3bbzvnbwjgdvl3wbpw0mab6i1s7mqg1f4p57nbry"))

(define rust-aws-sdk-dynamodb-1.111.0
  (crate-source "aws-sdk-dynamodb" "1.111.0"
                "1680qqcgc0w0kv3yp9ibrp3vg234n2yaqhp6b5yqq96bwd386hgw"))

(define rust-aws-sdk-kms-1.106.0
  (crate-source "aws-sdk-kms" "1.106.0"
                "17fg4mwkxqx942q8zci23zqzxhd00zfv9zdpa8jrrkzgjq7kadi3"))

(define rust-aws-sdk-s3-1.132.0
  (crate-source "aws-sdk-s3" "1.132.0"
                "0h5bm44zipfnzq39npmvp9kmpzhd69cy7sv32h0zc4bb78588xam"))

(define rust-aws-sdk-sso-1.98.0
  (crate-source "aws-sdk-sso" "1.98.0"
                "12clzmpni6kqw79wr6pqvdv6wfs2ys23q8dknrl0qii0zam7g76n"))

(define rust-aws-sdk-ssooidc-1.100.0
  (crate-source "aws-sdk-ssooidc" "1.100.0"
                "098237dqzyzcnr69dn10zckh2sishiapa9h858ma4p3d6h4pnzhw"))

(define rust-aws-sdk-sts-1.103.0
  (crate-source "aws-sdk-sts" "1.103.0"
                "16zly0m93pl7mg5g517i222rphqym1iq8dqwqhkq0fp7la0rn962"))

(define rust-aws-sigv4-1.4.3
  (crate-source "aws-sigv4" "1.4.3"
                "0nhij1jfx4vxp5s65zwq76n4wc31rjfc1d8k0qbj1carff80pp38"))

(define rust-aws-smithy-async-1.2.14
  (crate-source "aws-smithy-async" "1.2.14"
                "1z5cb4dasm2s698x8py79mirhi94d8r0qh3835bq996xddiazz1g"))

(define rust-aws-smithy-checksums-0.64.7
  (crate-source "aws-smithy-checksums" "0.64.7"
                "0qnpk4i17rxpsn1b959d2n8x4l8r74m5dz7200bbhi70q77bpvqh"))

(define rust-aws-smithy-eventstream-0.60.20
  (crate-source "aws-smithy-eventstream" "0.60.20"
                "0j3m3n6alyrqjmrack36g8v3wc2rrnihb99dfsw7cbz3wms9vw7s"))

(define rust-aws-smithy-http-0.63.6
  (crate-source "aws-smithy-http" "0.63.6"
                "0cajps6ywn129gxmhh4k1s5vw49gqhrx703isbm4jdrc3kfb46ms"))

(define rust-aws-smithy-http-client-1.1.12
  (crate-source "aws-smithy-http-client" "1.1.12"
                "0sgpqnkznfd468d439krf7xg91qr3059v2cb09iz5rpfgxd1cbva"))

(define rust-aws-smithy-json-0.62.5
  (crate-source "aws-smithy-json" "0.62.5"
                "0sl553j1frrnd3vgprfy7a71ybc238mavijj822dvvm2haxv0j4n"))

(define rust-aws-smithy-observability-0.2.6
  (crate-source "aws-smithy-observability" "0.2.6"
                "176amda1ravk36bgrh7409q855cn32ks72ys40cvzvbks4aj6v50"))

(define rust-aws-smithy-query-0.60.15
  (crate-source "aws-smithy-query" "0.60.15"
                "1g8yyaj6msisn2g21jr2jhxis4hy1239vxrff9fxngpv8jbxfmhs"))

(define rust-aws-smithy-runtime-1.11.3
  (crate-source "aws-smithy-runtime" "1.11.3"
                "10i1sgibk600zbldnv222f8dmz3whnsilm064a66za7yyv5gbrmq"))

(define rust-aws-smithy-runtime-api-1.12.1
  (crate-source "aws-smithy-runtime-api" "1.12.1"
                "0ckcfypk6fgqk5wifwnignnsf9mcx40gcj9z18macffgkqbpq4fw"))

(define rust-aws-smithy-runtime-api-macros-1.0.0
  (crate-source "aws-smithy-runtime-api-macros" "1.0.0"
                "1dx7m2lcg329bwxyqmafjdsd7fkinvn8gsb0wii9wn00jpyrcwwd"))

(define rust-aws-smithy-schema-0.1.0
  (crate-source "aws-smithy-schema" "0.1.0"
                "1xclkkh4mlr8jvlzb3ix140slmk70iy102hlg21fpw1qhckcnhkl"))

(define rust-aws-smithy-types-1.4.8
  (crate-source "aws-smithy-types" "1.4.8"
                "02q5lklvlnyxz876b7a760sqm3xm5r0bn19b3q6cr09grvdncsq5"))

(define rust-aws-smithy-xml-0.60.15
  (crate-source "aws-smithy-xml" "0.60.15"
                "1cr27lfx4p0lkjbyicd12xgsjiihpvf83pwa5w17srx33bfjmq0c"))

(define rust-aws-types-1.3.15
  (crate-source "aws-types" "1.3.15"
                "1b2qvkyry9zv6sa9kxmhlaaxh6wd8ah44prx5n841sh4jfmbqjrg"))

(define rust-backoff-0.4.0
  (crate-source "backoff" "0.4.0"
                "1h80d9xn5wngxdgza2m8w4x1kyhk0x6k9ydvsj50j2pcn6fdnbdn"))

(define rust-base16ct-0.1.1
  (crate-source "base16ct" "0.1.1"
                "1klccxr7igf73wpi0x3asjd8n0xjg0v6a7vxgvfk5ybvgh1hd6il"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-brotli-3.5.0
  (crate-source "brotli" "3.5.0"
                "14f34ml3i8qbnh4hhlv5r6j10bkx420gspsl1cgznl1wqrdx4h6n"))

(define rust-brotli-decompressor-2.5.1
  (crate-source "brotli-decompressor" "2.5.1"
                "0kyyh9701dwqzwvn2frff4ww0zibikqd1s1xvl7n1pfpc3z4lbjf"))

(define rust-bytecheck-0.8.2
  (crate-source "bytecheck" "0.8.2"
                "12yasgcsxapxnpiv2ac3jzvmc69zy3gj7isss4ch9jpdq2i37ahc"))

(define rust-bytecheck-derive-0.8.2
  (crate-source "bytecheck_derive" "0.8.2"
                "1ncjsg08a6kawhxhy24rpxn287yclbprbc7034i1v0ninn15wf49"))

(define rust-candle-core-0.9.2
  (crate-source "candle-core" "0.9.2"
                "14g0kgp4gwgn9hmmxvn3x9ab9gpa6kjbp910vgmb556rh1dnfny1"))

(define rust-candle-nn-0.9.2
  (crate-source "candle-nn" "0.9.2"
                "00syqqangjpq24q4r7sns3s9c2znjav5czd21796g1gggaggli9h"))

(define rust-candle-transformers-0.9.2
  (crate-source "candle-transformers" "0.9.2"
                "00axg64qwwx4rg56yaifhqkqh64gi1210qnkvni1di07m15fqf5m"))

(define rust-cfg-if-0.1.10
  (crate-source "cfg-if" "0.1.10"
                "08h80ihs74jcyp24cd75wwabygbbdgl05k6p5dmq8akbr78vv1a7"))

(define rust-chacha20-0.10.0
  (crate-source "chacha20" "0.10.0"
                "00bn2rn8l68qvlq93mhq7b4ns4zy9qbjsyjbb9kljgl4hqr9i3bg"))

(define rust-chrono-tz-0.8.6
  (crate-source "chrono-tz" "0.8.6"
                "0vlksnmpb6rd4h55245agnfhphnpslwnq9al3aw3is43dd3f16nm"))

(define rust-chrono-tz-build-0.2.1
  (crate-source "chrono-tz-build" "0.2.1"
                "03rmzd69cn7fp0fgkjr5042b3g54s2l941afjm3001ls7kqkjgj3"))

(define rust-cmov-0.5.3
  (crate-source "cmov" "0.5.3"
                "0ipp2fzpcz2z9l4ywks98bd1viwpw81lfd5pdj3sdi0z04ys921z"))

(define rust-colored-3.1.1
  (crate-source "colored" "3.1.1"
                "0d5cpbgvyvmmky199s885s6385ykd75q6qg3d2kcxjxq563ldygs"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-const-str-1.1.0
  (crate-source "const-str" "1.1.0"
                "0br169v5x31pljc2wg5yrh0r83r7dv4cgpfd61161ncfjk4jrw8q"))

(define rust-convert-case-0.11.0
  (crate-source "convert_case" "0.11.0"
                "0jfv1ajyr65bjlx533n5alfkfjdl8ks4zxfywdiz1jnj1qcz1yxg"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-countio-0.3.0
  (crate-source "countio" "0.3.0"
                "1jyyxxpvjwy7cia3h0s90678j3wm9xj1as9gv00lqx0xbpp2lw5r"))

(define rust-crc-3.3.0
  (crate-source "crc" "3.3.0"
                "0xg6yg57lbyzf69y8znq5gjb333w1fnlis2gnjg38blwffrx644p"))

(define rust-crc-catalog-2.5.0
  (crate-source "crc-catalog" "2.5.0"
                "0lq8dl60g457za8la86pak6i7ydxanm2lrpkqh5kyjkbz7m9hxi1"))

(define rust-crc-fast-1.9.0
  (crate-source "crc-fast" "1.9.0"
                "0gd8hwfnqnpj1g4cmw5y5fsh31gcfkp89zx0bfzv20b05k52mn9g"))

(define rust-crossterm-0.29.0
  (crate-source "crossterm" "0.29.0"
                "0yzqxxd90k7d2ac26xq1awsznsaq0qika2nv1ik3p0vzqvjg5ffq"))

(define rust-crossterm-winapi-0.9.1
  (crate-source "crossterm_winapi" "0.9.1"
                "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))

(define rust-crypto-bigint-0.4.9
  (crate-source "crypto-bigint" "0.4.9"
                "1vqprgj0aj1340w186zyspi58397ih78jsc0iydvhs6zrlilnazg"))

(define rust-crypto-bigint-0.5.5
  (crate-source "crypto-bigint" "0.5.5"
                "0xmbdff3g6ii5sbxjxc31xfkv9lrmyril4arh3dzckd4gjsjzj8d"))

(define rust-ctor-0.6.3
  (crate-source "ctor" "0.6.3"
                "03jrw316acxl3vld3wvl5m8jkj0mwwbssx7i06sb5blg4ww02kj2"))

(define rust-ctor-1.0.5
  (crate-source "ctor" "1.0.5"
                "1w1lp5sjzg0qlqp6y5aclrzdrc16r7lxndm07bvfls14mrs0k3rp"))

(define rust-ctor-proc-macro-0.0.7
  (crate-source "ctor-proc-macro" "0.0.7"
                "1havwah6iryn0ang09y12xxr45jsp7ff27zflz4mhgk017ghlmjj"))

(define rust-ctutils-0.4.2
  (crate-source "ctutils" "0.4.2"
                "17m2s9jv7i780k26cq2fcyslg0pakv9plwdrmygdwha1hfiiambx"))

(define rust-daachorse-2.1.1
  (crate-source "daachorse" "2.1.1"
                "01ifbyxhrbnh8g0l98gwabg9hmngz0r423v63zrk27fqnxg6nxfv"))

(define rust-der-0.6.1
  (crate-source "der" "0.6.1"
                "1pnl3y52m1s6srxpfrfbazf6qilzq8fgksk5dv79nxaybjk6g97i"))

(define rust-document-features-0.2.12
  (crate-source "document-features" "0.2.12"
                "0qcgpialq3zgvjmsvar9n6v10rfbv6mk6ajl46dd4pj5hn3aif6l"))

(define rust-dtor-0.1.1
  (crate-source "dtor" "0.1.1"
                "00fkcw8zn0g10m4k8b0qgmn304g47xqwn1ihhzyjra48n3p04ka0"))

(define rust-dtor-proc-macro-0.0.6
  (crate-source "dtor-proc-macro" "0.0.6"
                "19fg0mivy9qyvbwmqj3ysj0qm5cay0gyp5fyw1imq89cj95cyy7n"))

(define rust-dyn-stack-0.13.2
  (crate-source "dyn-stack" "0.13.2"
                "1s4g722dxp0irrf5axbsrxm128npjdnac6i7p1rbm1i87vj16iqw"))

(define rust-dyn-stack-macros-0.1.3
  (crate-source "dyn-stack-macros" "0.1.3"
                "1adzl0xcy5qiag9wnb4n6afw4hk1d52b8fzr87qp5lq7sjs2dng1"))

(define rust-ecdsa-0.14.8
  (crate-source "ecdsa" "0.14.8"
                "0p1wxap2s6jm06y2w3cal8dkz6p9223ir9wws70rgx8h929h2cs1"))

(define rust-elliptic-curve-0.12.3
  (crate-source "elliptic-curve" "0.12.3"
                "1lwi108mh6drw5nzqzlz7ighdba5qxdg5vmwwnw1j2ihnn58ifz7"))

(define rust-enum-as-inner-0.6.1
  (crate-source "enum-as-inner" "0.6.1"
                "1g3cywc65d9w974l2xy86ij13njss3qjc7b0kfbzbws9qrjs5rm1"))

(define rust-enum-dispatch-0.3.13
  (crate-source "enum_dispatch" "0.3.13"
                "1kby2jz173ggg7wk41vjsskmkdyx7749ll8lhqhv6mb5qqmww65a"))

(define rust-eventsource-stream-0.2.3
  (crate-source "eventsource-stream" "0.2.3"
                "1awhkl4xh9f66j9m770qvn2l74cr4l59ssqmv4lz99a7j9bg9zkl"))

(define rust-fancy-regex-0.17.0
  (crate-source "fancy-regex" "0.17.0"
                "1f314z64ilbbnn17ic1hghpq9dm2sqyn8gspvjvjp1jwhqgldkvj"))

(define rust-fast-float-0.2.0
  (crate-source "fast-float" "0.2.0"
                "0g7kfll3xyh99kc7r352lhljnwvgayxxa6saifb6725inikmyxlm"))

(define rust-ff-0.12.1
  (crate-source "ff" "0.12.1"
                "0q3imz4m3dj2cy182i20wa8kbclgj13ddfngqb2miicc6cjzq4yh"))

(define rust-float8-0.6.1
  (crate-source "float8" "0.6.1"
                "1md7f62lqyxm1cavv29sk38amka5vc7sihk2g7l8jjiyq8y916ki"))

(define rust-foreign-vec-0.1.0
  (crate-source "foreign_vec" "0.1.0"
                "0wv6p8yfahcqbdg2wg7wxgj4dm32g2b6spa5sg5sxg34v35ha6zf"))

(define rust-fsst-7.0.0
  (crate-source "fsst" "7.0.0"
                "1nsxpbdy2iabw8flvpzhq991k23dqcsx8qnyzi2gs4mc941cxl5w"))

(define rust-futures-timer-3.0.3
  (crate-source "futures-timer" "3.0.3"
                "094vw8k37djpbwv74bwf2qb7n6v6ghif4myss6smd6hgyajb127j"))

(define rust-gearhash-0.1.3
  (crate-source "gearhash" "0.1.3"
                "04yj8ni60v8756qfc2qsky671kkmqxvi6ni9arg4h5ndfv7q5ky8"))

(define rust-gemm-0.19.0
  (crate-source "gemm" "0.19.0"
                "1jza153naf745ykipdpnsr0wbv7vd1x36s1v20yjc4jb6vdp61ma"))

(define rust-gemm-c32-0.19.0
  (crate-source "gemm-c32" "0.19.0"
                "130jxddzk0ryaxqfx2x9alg3rrbhysc0ychxv2m3g7mrvkdkcs88"))

(define rust-gemm-c64-0.19.0
  (crate-source "gemm-c64" "0.19.0"
                "0grsvd0zggj8iqifdld0j0d518cv09j7d0nrljymjna2xkpaxj10"))

(define rust-gemm-common-0.19.0
  (crate-source "gemm-common" "0.19.0"
                "0giairy4wpjvq8rjni93sfm3cfmv8syc98dab84b3j8cj4jpc0l8"))

(define rust-gemm-f16-0.19.0
  (crate-source "gemm-f16" "0.19.0"
                "0vj8v4jibrl0nx60xcsr510f3iz0r2ck7bl2kmrxcv1f41apmpz3"))

(define rust-gemm-f32-0.19.0
  (crate-source "gemm-f32" "0.19.0"
                "075887yzjxabypny9lzhyssqxwcy4nyfcb5bwgiwdghzvb4viq02"))

(define rust-gemm-f64-0.19.0
  (crate-source "gemm-f64" "0.19.0"
                "0snwmbnbsnr7v7lhj4nq02bwf815cp6lr01g6amvy8d5ybl32q85"))

(define rust-git-version-0.3.9
  (crate-source "git-version" "0.3.9"
                "06ddi3px6l2ip0srn8512bsh8wrx4rzi65piya0vrz5h7nm6im8s"))

(define rust-git-version-macro-0.3.9
  (crate-source "git-version-macro" "0.3.9"
                "1h1s08fgh9bkwnc2hmjxcldv69hlxpq7a09cqdxsd5hb235hq0ak"))

(define rust-group-0.12.1
  (crate-source "group" "0.12.1"
                "1ixspxqdpq0hxg0hd9s6rngrp6rll21v4jjnr7ar1lzvdhxgpysx"))

(define rust-h2-0.3.27
  (crate-source "h2" "0.3.27"
                "0b92141hilij015av6i5ziw9xfx4py3lbjy17yc35z5ih01sbv0b"))

(define rust-heapify-0.2.0
  (crate-source "heapify" "0.2.0"
                "0bn5bky8dnbp1zhanlba29h40i7y8wmv4xalnadcl0gjnxjv4j80"))

(define rust-hf-hub-0.4.3
  (crate-source "hf-hub" "0.4.3"
                "15wxsm4plm63gwr2c1p9ik8anixr785dxc6n6s0197fspqxqz7b2"))

(define rust-hf-xet-1.5.2
  (crate-source "hf-xet" "1.5.2"
                "1k9bc6qjj423xr9ih3lsgggik8iw1mn0n1v3sba9c9zrhkx362s3"))

(define rust-hmac-0.13.0
  (crate-source "hmac" "0.13.0"
                "0gw6avmix6ah63lf70dapxhml4dlcakl9f2lnm6b0hdf6abvq0v3"))

(define rust-hyper-0.14.32
  (crate-source "hyper" "0.14.32"
                "1rvcb0smz8q1i0y6p7rwxr02x5sclfg2hhxf3g0774zczn0cgps1"))

(define rust-hyper-rustls-0.24.2
  (crate-source "hyper-rustls" "0.24.2"
                "1475j4a2nczz4aajzzsq3hpwg1zacmzbqg393a14j80ff8izsgpc"))

(define rust-instant-0.1.13
  (crate-source "instant" "0.1.13"
                "08h27kzvb5jw74mh0ajv0nv9ggwvgqm8ynjsn2sa9jsks4cjh970"))

(define rust-itoap-1.0.1
  (crate-source "itoap" "1.0.1"
                "1f48gsd18kbvskwbnwszhqjpk1l4rdmahh7kaz86b432cj9g8a4h"))

(define rust-jni-0.22.4
  (crate-source "jni" "0.22.4"
                "161lza8gz071h22pgyqyx4n91ixd691z2dbb1pq2g97k5i49mzay"))

(define rust-jni-macros-0.22.4
  (crate-source "jni-macros" "0.22.4"
                "18v02mcn5c7mb2yw6r930xg6ynsn7hwkxv8z2kdhn3qprjn0j0d0"))

(define rust-jni-sys-0.4.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "jni-sys" "0.4.1"
                "1wlahx6f2zhczdjqyn8mk7kshb8x5vsd927sn3lvw41rrf47ldy6"))

(define rust-jni-sys-macros-0.4.1
  (crate-source "jni-sys-macros" "0.4.1"
                "0r32gbabrak15a7p487765b5wc0jcna2yv88mk6m1zjqyi1bkh1q"))

(define rust-jsonwebtoken-10.4.0
  (crate-source "jsonwebtoken" "10.4.0"
                "1z649wrq8mnglsklll15dnrcji8py3x744236hzcmspx9zxjp8zb"))

(define rust-konst-0.4.3
  (crate-source "konst" "0.4.3"
                "07fbpslpn69dq3nmskvbsscl0savg644k8glnsd2ymp3hzwdaq7n"))

(define rust-konst-proc-macros-0.4.1
  (crate-source "konst_proc_macros" "0.4.1"
                "0si52sx4v0nk0ffz4pyakvrc20b4bnfs0kkan54vvzfmv3hs4dz0"))

(define rust-lance-7.0.0
  (crate-source "lance" "7.0.0"
                "17q1b7gbkmh0s8m9fwmgnwk2hjk67vrjn75g0kdg8y2cdylaqi1r"))

(define rust-lance-arrow-7.0.0
  (crate-source "lance-arrow" "7.0.0"
                "1p7cszxkzkxsxzc2ipkq81d84i6nrak9qpp6j5drh32qf054lgr5"))

(define rust-lance-bitpacking-7.0.0
  (crate-source "lance-bitpacking" "7.0.0"
                "14xmxkj36bjpj8f124mcwww335sl12m5cnjivm0m155i44jx3i40"))

(define rust-lance-core-7.0.0
  (crate-source "lance-core" "7.0.0"
                "15759qhyyckynx6bqzl5i7c7rvc5jzh9c5yx0wplwj2sv8h41y0k"))

(define rust-lance-datafusion-7.0.0
  (crate-source "lance-datafusion" "7.0.0"
                "0xp5557bvg3x6rrswyavkkcqc19kpk2xmm4kg6c7ajjkcrx5jq3l"))

(define rust-lance-datagen-7.0.0
  (crate-source "lance-datagen" "7.0.0"
                "0fvfjb2mdyk3da1yrb8s57faxlsxafzyf3853aacsw92xl35avq4"))

(define rust-lance-encoding-7.0.0
  (crate-source "lance-encoding" "7.0.0"
                "1ri499dk5cwi10zwm3a4qrrq7rjpshsyndn6djjxdyfw8gglxxbs"))

(define rust-lance-file-7.0.0
  (crate-source "lance-file" "7.0.0"
                "0dnpycjxrs81l0fk33xmc8rhpsbqgx8gkbr8xg0mv687c8nswwh7"))

(define rust-lance-index-7.0.0
  (crate-source "lance-index" "7.0.0"
                "112ykhqvy6n2vvb4ry5nlv2gn5azdxqxmq2gab5h7acn22svy7z7"))

(define rust-lance-io-7.0.0
  (crate-source "lance-io" "7.0.0"
                "0682j24blcfj0yz36hi14mf9yz6zxyj3qzyj842v4w5qy67ckf5s"))

(define rust-lance-linalg-7.0.0
  (crate-source "lance-linalg" "7.0.0"
                "1wd1m6i9rimv202kj1cd7v096xly8wl8b92dvh10ny5cs3552k3b"))

(define rust-lance-namespace-7.0.0
  (crate-source "lance-namespace" "7.0.0"
                "1hhz3zrcj2cz4cabwwv975l4p1irhrhazlz089im0586r8r86kh1"))

(define rust-lance-namespace-impls-7.0.0
  (crate-source "lance-namespace-impls" "7.0.0"
                "0b981br8f7j8sbgpdk9brpv5m0y4155d39yd7pfr5kx30qcj7lg8"))

(define rust-lance-namespace-reqwest-client-0.7.7
  (crate-source "lance-namespace-reqwest-client" "0.7.7"
                "16dfm9zslzadji0bp2gy0a1qna6fc4yb9243aggixc9gd3jfwsb3"))

(define rust-lance-table-7.0.0
  (crate-source "lance-table" "7.0.0"
                "039557rs3r0x095jnia4vq5y20brr5c0rizz0jxlxsjaj1ai6vxi"))

(define rust-lance-testing-7.0.0
  (crate-source "lance-testing" "7.0.0"
                "1cp6c22a3akw3ls42425p9qqcjg326llziczh0yhkynirfmc551h"))

(define rust-lance-tokenizer-7.0.0
  (crate-source "lance-tokenizer" "7.0.0"
                "1ks0i6id4lcbck9jjkjczqfysrw2i2fmb6srpwbbgh6hv5g7z6xk"))

(define rust-libloading-0.9.0
  (crate-source "libloading" "0.9.0"
                "0q4bvhp4kqy2v3bw4cn2bmyq73hskqd1ansa9125gfq5x0ns4k3m"))

(define rust-lindera-3.0.7
  (crate-source "lindera" "3.0.7"
                "05lj78v37h45c4xvxn3hy0i8sgycfgv2yaad9r0rpsb1f6fsgkbl"))

(define rust-lindera-dictionary-3.0.7
  (crate-source "lindera-dictionary" "3.0.7"
                "0c35ffyisl3r77xxsf2pbf6y5mgxa9miaprc0wlprs4zr9b58f72"))

(define rust-litrs-1.0.0
  (crate-source "litrs" "1.0.0"
                "14p0kzzkavnngvybl88nvfwv031cc2qx4vaxpfwsiifm8grdglqi"))

(define rust-lru-0.16.4
  (crate-source "lru" "0.16.4"
                "0fgg35wrpfdrkv9hcabkg92g3sv4867g1rir7ay9lq1zs3ayhrkz"))

(define rust-lzma-sys-0.1.20
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "lzma-sys" "0.1.20"
                "09sxp20waxyglgn3cjz8qjkspb3ryz2fwx4rigkwvrk46ymh9njz"))

(define rust-md-5-0.11.0
  (crate-source "md-5" "0.11.0"
                "166yqj8b11pawpys7knnn77cr618cby2iywpp0dq4dh3b4gl9dk9"))

(define rust-mea-0.6.3
  (crate-source "mea" "0.6.3"
                "0cnqisggj7w8v0jpvxlqgwsgrfa1m4wmzcmngssf2mni453gaiv7"))

(define rust-memmap2-0.7.1
  (crate-source "memmap2" "0.7.1"
                "1il82b0mw304jlwvl0m89aa8bj5dgmm3vbb0jg8lqlrk0p98i4zl"))

(define rust-more-asserts-0.3.1
  (crate-source "more-asserts" "0.3.1"
                "0zj0f9z73nsn1zxk2y21f0mmafvz7dz5v93prlxwdndb3jbadbqz"))

(define rust-multiversion-0.7.4
  (crate-source "multiversion" "0.7.4"
                "0hm1y7dhdbam2yvaxmxzd0bj7gv777y0zn82jjzx0fhxl5hi31f4"))

(define rust-multiversion-macros-0.7.4
  (crate-source "multiversion-macros" "0.7.4"
                "142yhgdxvy9qjyi8n4wg2hi1dsckay816g1jg0jpvhp0x7g4v9vr"))

(define rust-munge-0.4.7
  (crate-source "munge" "0.4.7"
                "032sj47l2174dirkjkhi18x92wlgdqdld4b4l5n9bfly4lgl05sy"))

(define rust-munge-macro-0.4.7
  (crate-source "munge_macro" "0.4.7"
                "0cgrm4q8a6qm0802d08pacbv2mpcq4c47hrxc3avannlrdfg4s25"))

(define rust-napi-3.9.0
  (crate-source "napi" "3.9.0"
                "18hpn2mzgmgaljxkwaavwrxslzf5gfis2w8hvlw6yl94713rblzi"))

(define rust-napi-build-2.3.2
  (crate-source "napi-build" "2.3.2"
                "1q85nnml8vkr9jnnn6pr528jzd89ams5zxrdcgx8c2y6r396dhy9"))

(define rust-napi-derive-3.5.6
  (crate-source "napi-derive" "3.5.6"
                "0hjv98i4w67a1flgcz1va6kdaxalz2j2s7hq1vdfcrs6w1kggcw9"))

(define rust-napi-derive-backend-5.0.4
  (crate-source "napi-derive-backend" "5.0.4"
                "1bsvipl31r5my62mb83y23qv0qkshx66vkvpfg737ygd0c2z6nhd"))

(define rust-napi-sys-3.2.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "napi-sys" "3.2.1"
                "0aqcp55khzjpgxkgm78pmqv8ym4n8qvz3fshbvjdl7kw9nw05dlf"))

(define rust-nohash-hasher-0.2.0
  (crate-source "nohash-hasher" "0.2.0"
                "0lf4p6k01w4wm7zn4grnihzj8s7zd5qczjmzng7wviwxawih5x9b"))

(define rust-now-0.1.3
  (crate-source "now" "0.1.3"
                "1l135786rb43rjfhwfdj7hi3b5zxxyl9gwf15yjz18cp8f3yk2bd"))

(define rust-ntapi-0.4.3
  (crate-source "ntapi" "0.4.3"
                "1bl0d73avwla7laa4pkqvzvifjbs0avg65w01zxjydgx3likbcy3"))

(define rust-objc2-core-foundation-0.3.2
  (crate-source "objc2-core-foundation" "0.3.2"
                "0dnmg7606n4zifyjw4ff554xvjmi256cs8fpgpdmr91gckc0s61a"))

(define rust-objc2-io-kit-0.3.2
  (crate-source "objc2-io-kit" "0.3.2"
                "05dvfcf97w39daaj5qsbfc399lw9hbx3s4h9nwgxrmlpjnizpyik"))

(define rust-objc2-system-configuration-0.3.2
  (crate-source "objc2-system-configuration" "0.3.2"
                "15m39m325yhkjpcagcygbv3qx19vr4ym4kdqramwqm6src8vs5kj"))

(define rust-object-store-opendal-0.56.0
  (crate-source "object_store_opendal" "0.56.0"
                "1hh7r6hxhhd0hias27260kcm772ghd432fdapjamr4z5xrs8ha88"))

(define rust-oneshot-0.1.13
  (crate-source "oneshot" "0.1.13"
                "01x1rp6s5hxx87n2pc5101lxgdrj0gnxj45zss2qb8li4m6cm6r6"))

(define rust-onig-6.5.3
  (crate-source "onig" "6.5.3"
                "1qh9qyv59ca5py5x5bvmfpi9sjyhls80r20sq638jhzrk3vcphqc"))

(define rust-onig-sys-69.9.3
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "onig_sys" "69.9.3"
                "1mzd6l3vjb8iw7jwk7l99jvj279wh0d8wf4nb2w56zp70iv32s0y"))

(define rust-opendal-0.56.0
  (crate-source "opendal" "0.56.0"
                "0dm2j0szlkz4nq85hdyrirbq0yv0ym3ndhiyny1mva4riqyivcwp"))

(define rust-opendal-core-0.56.0
  (crate-source "opendal-core" "0.56.0"
                "1jh0zr2yyl58xd6ir7d06zfvjixflcdwwpxgsdvffwz1hwkdsj8q"))

(define rust-opendal-layer-concurrent-limit-0.56.0
  (crate-source "opendal-layer-concurrent-limit" "0.56.0"
                "1scqi43m3cdlx8gmn631sfdyl0ndd1mf9bx9h3fkn9h3qllip2q4"))

(define rust-opendal-layer-logging-0.56.0
  (crate-source "opendal-layer-logging" "0.56.0"
                "0yy42npwc9c1dxpp1swchsmbpkzs57jrlrz20shys4lbk3f5lr6j"))

(define rust-opendal-layer-retry-0.56.0
  (crate-source "opendal-layer-retry" "0.56.0"
                "1m0anwrilk66z1951j0nji5l55jr66jq82k4389sdpadz97i7b2f"))

(define rust-opendal-layer-timeout-0.56.0
  (crate-source "opendal-layer-timeout" "0.56.0"
                "0k0gm5q4k4iqrh5i7zbq8w4vqmw9n6xfl63dkw0f7hl0fjmqd5b1"))

(define rust-opendal-service-azblob-0.56.0
  (crate-source "opendal-service-azblob" "0.56.0"
                "0fxclb09d5dbpz34b8243lvrw7lk4mwa3bfrkan83z8wqqzbylkl"))

(define rust-opendal-service-azdls-0.56.0
  (crate-source "opendal-service-azdls" "0.56.0"
                "0wj7828h8bhswk0wkqmrpaikp1n5v9vwhybx0yxs52ygv318964g"))

(define rust-opendal-service-azure-common-0.56.0
  (crate-source "opendal-service-azure-common" "0.56.0"
                "032fqnsckxhqychgz909w50fbrl0rcdj83m25p76dkwddify9c7z"))

(define rust-opendal-service-gcs-0.56.0
  (crate-source "opendal-service-gcs" "0.56.0"
                "0fk3qq08hlhhyzv9rr69z4bhfrzm2xhi61nijqc46qq1l5vr993h"))

(define rust-opendal-service-hf-0.56.0
  (crate-source "opendal-service-hf" "0.56.0"
                "12530sy588dd5lw3g722sv8brb4qww6mrnzlgqjzw7d1m2ibfakv"))

(define rust-opendal-service-oss-0.56.0
  (crate-source "opendal-service-oss" "0.56.0"
                "01dc7xl1hc57fgb4dna0n14zw081rcr8am9rnqhnvl4sh8bskj19"))

(define rust-opendal-service-s3-0.56.0
  (crate-source "opendal-service-s3" "0.56.0"
                "1j2in3whq7ri9mksgky1wi3wmpf4k3119nbxj8q0s2xmkgmxvbcx"))

(define rust-p256-0.11.1
  (crate-source "p256" "0.11.1"
                "151mqd8m25c8ib97saz4fwkg4nhw098i051gazg2l7pm13flxx2i"))

(define rust-parquet-format-safe-0.2.4
  (crate-source "parquet-format-safe" "0.2.4"
                "07wf6wf4jrxlq5p3xldxsnabp7jl06my2qp7kiwy9m3x2r5wac8i"))

(define rust-parse-zoneinfo-0.3.1
  (crate-source "parse-zoneinfo" "0.3.1"
                "093cs8slbd6kyfi6h12isz0mnaayf5ha8szri1xrbqj4inqhaahz"))

(define rust-pin-project-1.1.13
  (crate-source "pin-project" "1.1.13"
                "09091qp946lpmjz4yp0xil1r5v4hgc91fi19dg5csayhdqrv4ri4"))

(define rust-pin-project-internal-1.1.13
  (crate-source "pin-project-internal" "1.1.13"
                "12rzlh07i1sdgrvzj6wgkka5bjqyvbfsl8knq6qi7g16m7q9aqy9"))

(define rust-pkcs8-0.9.0
  (crate-source "pkcs8" "0.9.0"
                "1fm4sigvcd0zpzg9jcp862a8p272kk08b9lgcs1dm1az19cjrjly"))

(define rust-planus-0.3.1
  (crate-source "planus" "0.3.1"
                "17x8mr175b9clg998xpi5z45f9fsspb0ncfnx2644bz817fr25pw"))

(define rust-polars-0.39.2
  (crate-source "polars" "0.39.2"
                "1d9d3zybndxm5q1b7rib32yv998ss402inqjgbhw1f8nif2ip8hf"))

(define rust-polars-arrow-0.39.2
  (crate-source "polars-arrow" "0.39.2"
                "0x8gx2aspsn78qgnpdlncs7rvm2q7jvblyz2csdjfcggnpr0jnvj"))

(define rust-polars-arrow-format-0.1.0
  (crate-source "polars-arrow-format" "0.1.0"
                "0k5qci66rcwqv4ycb43zlkk128wrdvcqkc95j2qrd4xgfhjfzc0r"))

(define rust-polars-compute-0.39.2
  (crate-source "polars-compute" "0.39.2"
                "1zmlxwv7vvnnllggi2g86vk2pjnxdxp41w0yp5wvnkxi2idr95m7"))

(define rust-polars-core-0.39.2
  (crate-source "polars-core" "0.39.2"
                "0p736fm04myn43qfyvv9asywif46292vln638cd0nvbbx79p0ps6"))

(define rust-polars-error-0.39.2
  (crate-source "polars-error" "0.39.2"
                "0mq7bcyix521zpszl7z1g4cspj42bwdswlarnxw6z2kbbv8da92j"))

(define rust-polars-io-0.39.2
  (crate-source "polars-io" "0.39.2"
                "12rk05vjdh5wqb01g269qcpai102ljlb4r4d4944mglc86g5ij5j"))

(define rust-polars-lazy-0.39.2
  (crate-source "polars-lazy" "0.39.2"
                "0zzbrwfy967m0nkjkbh37p3cmqxxixnr33szil2y4s7n38mn7cl9"))

(define rust-polars-ops-0.39.2
  (crate-source "polars-ops" "0.39.2"
                "0py9zg8zzbphfhfrvvmpnlxn9az8bapig3nfw319n44jk96xpnzg"))

(define rust-polars-parquet-0.39.2
  (crate-source "polars-parquet" "0.39.2"
                "0spbs5hf9frr99ixxm3m2pj8yc7qhn24qqfvcbhxyvvqdwcx48dl"))

(define rust-polars-pipe-0.39.2
  (crate-source "polars-pipe" "0.39.2"
                "0p01193rbycnn4vqdwb00dsr452l15f4c7sq3r2iasnmbcfhyw28"))

(define rust-polars-plan-0.39.2
  (crate-source "polars-plan" "0.39.2"
                "0zf978xyri6qrsd60prs7kkaqpz7jsfav35yspplvi105qqf5f1g"))

(define rust-polars-row-0.39.2
  (crate-source "polars-row" "0.39.2"
                "0bpsmi5ywlyvvlka50p26y0wlw9v3xh8j3g77lp71qraik3bs5d5"))

(define rust-polars-sql-0.39.2
  (crate-source "polars-sql" "0.39.2"
                "1drd4f1b1cv796zs59b9h7yyh585qpgg3cjk34yh5hq43k6bfjvv"))

(define rust-polars-time-0.39.2
  (crate-source "polars-time" "0.39.2"
                "1wbknraagmvfdzw7dkrhf2x5jra35p11d3yqi7dmbv1fv4x8xhgg"))

(define rust-polars-utils-0.39.2
  (crate-source "polars-utils" "0.39.2"
                "1s0snasy3712wbd4a96rw77wwkmiim0gnv1xv6xzpqngk33bcq67"))

(define rust-ptr-meta-0.3.1
  (crate-source "ptr_meta" "0.3.1"
                "0yaa4bvghj0rygqjlcd4lkcid58pcywxmjzisihsz5hibbwhr6hb"))

(define rust-ptr-meta-derive-0.3.1
  (crate-source "ptr_meta_derive" "0.3.1"
                "1qbg3malg24dmiszfddd7n69g3rb7vl7nxj67gchh4ky19yqcivk"))

(define rust-pulp-0.22.2
  (crate-source "pulp" "0.22.2"
                "0cl6qd072f1qgw9d9mdbkanjpkqvfw0j5hl4wmanr4av1nrmn81f"))

(define rust-pulp-wasm-simd-flag-0.1.0
  (crate-source "pulp-wasm-simd-flag" "0.1.0"
                "1q42s7gr4hvy0pz8brxjfl0x61zllwcgkvcn6hczp29dd3p4xqj0"))

(define rust-pyo3-async-runtimes-0.28.0
  (crate-source "pyo3-async-runtimes" "0.28.0"
                "0y1ijwxkcrx9s53kpygwv0lmlwgrgyfz1c7rpdvq63phbfln8wwy"))

(define rust-pyo3-async-runtimes-macros-0.28.0
  (crate-source "pyo3-async-runtimes-macros" "0.28.0"
                "1y5ldpva6cjbnidxvx523ndzh1fxv2ly9kl4mk83377a1sbrjcy2"))

(define rust-quick-xml-0.39.4
  (crate-source "quick-xml" "0.39.4"
                "0plfhnna58ad2hlym3q02zrmmh7xdpikzs7hll4x6w7nwba8vk6d"))

(define rust-rancor-0.1.1
  (crate-source "rancor" "0.1.1"
                "1vl1y0yhw40j3g6b2h9jgkfjp0pg000cia8xashc49qm71rflqx0"))

(define rust-rand-0.10.1
  (crate-source "rand" "0.10.1"
                "01r22vdpw6z69jzy6khnyr0ljq9im337h4j0mkyz26lnqyyfis6j"))

(define rust-rand-core-0.10.1
  (crate-source "rand_core" "0.10.1"
                "0s9wiacxrr100icl7i41308gcj85nlcclrc5jx1jd6p10dhigf33"))

(define rust-rand-distr-0.4.3
  (crate-source "rand_distr" "0.4.3"
                "0cgfwg3z0pkqhrl0x90c77kx70r6g9z4m6fxq9v0h2ibr2dhpjrj"))

(define rust-random-word-0.4.3
  (crate-source "random_word" "0.4.3"
                "0a8zcp3m3f0d69y0d5db98yybcnmv2n75h35gwycrqnx2rxddvh7"))

(define rust-raw-cpuid-11.6.0
  (crate-source "raw-cpuid" "11.6.0"
                "11j1lmrjqqnc43bxkrz0xai1g9piw3z9aap53qsj8cnpb7fd1329"))

(define rust-reborrow-0.5.5
  (crate-source "reborrow" "0.5.5"
                "0c14ccj3fdf47a1ya21bkxqv7s2hxrcfhaw98aqd6jqg029i2983"))

(define rust-redb-3.1.3
  (crate-source "redb" "3.1.3"
                "0jhga6ghcbyazlq1gi28pypl6mcnnfrisq0frk9iacv9q70kk8jb"))

(define rust-relative-path-1.9.3
  (crate-source "relative-path" "1.9.3"
                "1limlh8fzwi21g0473fqzd6fln9iqkwvzp3816bxi31pkilz6fds"))

(define rust-rend-0.5.3
  (crate-source "rend" "0.5.3"
                "1rknl9l1s3x67zizrxz1n3k8w5z7z54dqzsdlrahgwn22zrxxnna"))

(define rust-reqsign-aliyun-oss-3.0.0
  (crate-source "reqsign-aliyun-oss" "3.0.0"
                "15bc2x0d9cvbrkq26rzw4kk369xmw2518m5m2gia42hlydbjgb2p"))

(define rust-reqsign-aws-v4-3.0.0
  (crate-source "reqsign-aws-v4" "3.0.0"
                "0hwz3r8n20xisdfqx9f1v5wvzbjks5c9d154y54mll4l5qwcmsj4"))

(define rust-reqsign-azure-storage-3.0.0
  (crate-source "reqsign-azure-storage" "3.0.0"
                "1rl2y3ys6fm8h55ff6gxcy515r1x59rc95dg9b9nnnax8201jcks"))

(define rust-reqsign-core-3.0.0
  (crate-source "reqsign-core" "3.0.0"
                "00p1m1w80d47mb2rrr2kc4lg3ssv7hncj7r1p9976zkx1b7h40xi"))

(define rust-reqsign-file-read-tokio-3.0.0
  (crate-source "reqsign-file-read-tokio" "3.0.0"
                "1i9xc4h1km1q7jb69j1j472qjgc4bpjqvk2i32ivwyninfar5n72"))

(define rust-reqsign-google-3.0.0
  (crate-source "reqsign-google" "3.0.0"
                "0bi4wc0xhksksi43basmfwzfvlcjyw1mlxzbmkn7d7n696dn1k1m"))

(define rust-reqwest-0.13.3
  (crate-source "reqwest" "0.13.3"
                "1h7fgnllk7ihw7836b7z73h9fb5vk90y3irvcm0ysan2l8g05q32"))

(define rust-reqwest-eventsource-0.6.0
  (crate-source "reqwest-eventsource" "0.6.0"
                "1pkbxjk0vjdfialjm155q4nnmh87r507n3k4j5b2fi5vdms5ab33"))

(define rust-reqwest-middleware-0.5.1
  (crate-source "reqwest-middleware" "0.5.1"
                "0vq5jq5w0sbh043ibycvfkm67ixi76g9gmq4rk835d9nll2dm78r"))

(define rust-rfc6979-0.3.1
  (crate-source "rfc6979" "0.3.1"
                "1fzsp705b5lhwd2r9il9grc3lj6rm3b2r89vh0xv181gy5xg2hvp"))

(define rust-rkyv-0.8.16
  (crate-source "rkyv" "0.8.16"
                "1qqn1ylmdbqykgzis7p6indgx48k8yqbbdas4wczjr76k469wf3k"))

(define rust-rkyv-derive-0.8.16
  (crate-source "rkyv_derive" "0.8.16"
                "1ina2cfv6iiz915n6wgq91ji472d64nyh8fhdfrmyc9586sx0bjx"))

(define rust-rstest-0.23.0
  (crate-source "rstest" "0.23.0"
                "0d90hr3i2yajzgpzvsh6p2yjzmcb3nm8884xdbb5sswvwmdmhb0a"))

(define rust-rstest-macros-0.23.0
  (crate-source "rstest_macros" "0.23.0"
                "0nmdm7a4ysihnh0zz6w6gqrmw205zfp7xqkb2id3858vg20afpl2"))

(define rust-rustls-0.21.12
  (crate-source "rustls" "0.21.12"
                "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz"))

(define rust-rustls-platform-verifier-0.7.0
  (crate-source "rustls-platform-verifier" "0.7.0"
                "181v4d0vl53vdh2wq56vghal1zyhdgqvy4xa8r45zwz4di9y5l96"))

(define rust-rustls-platform-verifier-android-0.1.1
  (crate-source "rustls-platform-verifier-android" "0.1.1"
                "13vq6sxsgz9547xm2zbdxiw8x7ad1g8n8ax6xvxsjqszk7q6awgq"))

(define rust-rustls-webpki-0.101.7
  (crate-source "rustls-webpki" "0.101.7"
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))

(define rust-safe-transmute-0.11.3
  (crate-source "safe-transmute" "0.11.3"
                "0zdb839pfgxgfi7bzwqnkalld52byi7cnfmsk849707sz1pq4i1r"))

(define rust-safetensors-0.7.0
  (crate-source "safetensors" "0.7.0"
                "1xc796dgbl28pvxx3zyndpiqdz4pkwczk97y3s90nqmvxb0mcmk7"))

(define rust-scc-2.4.0
  (crate-source "scc" "2.4.0"
                "1k2nwz3bysf1s3r5g437vq9xfm9i4sadfzn5c0k8xx7ynx3g1rj6"))

(define rust-sct-0.7.1
  (crate-source "sct" "0.7.1"
                "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s"))

(define rust-sdd-3.0.10
  (crate-source "sdd" "3.0.10"
                "1jj1brjjasx7r3lf6iyhhrpglx47vzr0z1qi1n0fcszjzv5wy3a9"))

(define rust-sec1-0.3.0
  (crate-source "sec1" "0.3.0"
                "0a09lk5w3nyggpyz54m10nnlg9v8qbh6kw3v1bgla31988c4rqiv"))

(define rust-secrecy-0.8.0
  (crate-source "secrecy" "0.8.0"
                "07p9h2bpkkg61f1fzzdqqbf74kwv1gg095r1cdmjzzbcl17cblcv"))

(define rust-serde-json-1.0.150
  (crate-source "serde_json" "1.0.150"
                "1ffgfhy9kndjnrz8lmy95pr758p2zk8dxv6yi99x0vkkni24w0g8"))

(define rust-serde-plain-1.0.2
  (crate-source "serde_plain" "1.0.2"
                "0l4d4nbw00pz6n43icrc605bhgynfmlyq39sn8i10qasnrnzrqcw"))

(define rust-serde-yaml-ng-0.10.0
  (crate-source "serde_yaml_ng" "0.10.0"
                "07ylpzx9xykdj4fqfa0vb6xz4c1pazrqqibv78hd8dlbp4kvckbv"))

(define rust-serial-test-3.4.0
  (crate-source "serial_test" "3.4.0"
                "0bx30wia5a40q849xj7frqwz6s4r7qxilsbvmbrs6w0hpxwxj6wi"))

(define rust-serial-test-derive-3.4.0
  (crate-source "serial_test_derive" "3.4.0"
                "1ng66dgrl7dj555b9xja7rmjnchdni4f8ibld3xx5c45kfa92z8a"))

(define rust-sha1-0.11.0
  (crate-source "sha1" "0.11.0"
                "05025pf8d8zr2qq5xyh5m3wqls1fn7813gz1mfs7551mk724rk5a"))

(define rust-sha2-asm-0.6.4
  (crate-source "sha2-asm" "0.6.4"
                "1ay1vai08d802avl41r0s6r1nrcnzv7jnj5xna34d03mc56j2idq"))

(define rust-shellexpand-3.1.2
  (crate-source "shellexpand" "3.1.2"
                "1n3y55yvh2s8cqmqb6bnz4wrlhchjd489fn1dpcc9rhnbsmlz0ij"))

(define rust-signature-1.6.4
  (crate-source "signature" "1.6.4"
                "0z3xg405pg827g6hfdprnszsdqkkbrsfx7f1dl04nv9g7cxks8vl"))

(define rust-simd-cesu8-1.1.1
  (crate-source "simd_cesu8" "1.1.1"
                "0crcbgvyycmazji2vqj9vxn2czdyl3gxmicp4xqdzkc7pdbh3ycl"))

(define rust-smartstring-1.0.1
  (crate-source "smartstring" "1.0.1"
                "0agf4x0jz79r30aqibyfjm1h9hrjdh0harcqcvb2vapv7rijrdrz"))

(define rust-snafu-0.8.9
  (crate-source "snafu" "0.8.9"
                "18p1y5qxwjn5j902wqsdr75n17b29lxpdipa0p7a3wybxbsb713f"))

(define rust-snafu-derive-0.8.9
  (crate-source "snafu-derive" "0.8.9"
                "0lg4s58jzx6w48ig4qp8jasrrs886pifqqd58k5b2jzlvd3pgjf1"))

(define rust-socket2-0.5.10
  (crate-source "socket2" "0.5.10"
                "0y067ki5q946w91xlz2sb175pnfazizva6fi3kfp639mxnmpc8z2"))

(define rust-spin-0.10.0
  (crate-source "spin" "0.10.0"
                "14g5sdsjf4wk2ys5dq8ivkq4rz57gphab2gcdzar5hnrk35lrznm"))

(define rust-spki-0.6.0
  (crate-source "spki" "0.6.0"
                "0ar1ldkl7svp8l3gfw2hyiiph7n2nqynjnjgdv1pscvsmjxh5kv7"))

(define rust-sqlparser-0.39.0
  (crate-source "sqlparser" "0.39.0"
                "1mrbqjdqr179qnhy43d0dnrl3yipsp4qyji5rc68j4fyrg14sfvl"))

(define rust-statrs-0.18.0
  (crate-source "statrs" "0.18.0"
                "0pikgp74gg9a3jp2hhh5z6wdfjn96gdkahw7n1kff4k5ik1ffgra"))

(define rust-streaming-decompression-0.1.2
  (crate-source "streaming-decompression" "0.1.2"
                "1wscqj3s30qknda778wf7z99mknk65p0h9hhs658l4pvkfqw6v5z"))

(define rust-streaming-iterator-0.1.9
  (crate-source "streaming-iterator" "0.1.9"
                "0845zdv8qb7zwqzglpqc0830i43xh3fb6vqms155wz85qfvk28ib"))

(define rust-strength-reduce-0.2.4
  (crate-source "strength_reduce" "0.2.4"
                "10jdq9dijjdkb20wg1dmwg447rnj37jbq0mwvbadvqi2gys5x2gy"))

(define rust-strum-0.28.0
  (crate-source "strum" "0.28.0"
                "1ggr0if083c1mz9w33hkdjsp0iqk2fz9n49bvb73knwihydxwa4n"))

(define rust-strum-macros-0.25.3
  (crate-source "strum_macros" "0.25.3"
                "184y62g474zqb2f7n16x3ghvlyjbh50viw32p9w9l5lwmjlizp13"))

(define rust-strum-macros-0.28.0
  (crate-source "strum_macros" "0.28.0"
                "0r7n6v5b3x85m52isyc8wq78irmr22g0hmj1xn3pbq8f4yhfx1db"))

(define rust-symlink-0.1.0
  (crate-source "symlink" "0.1.0"
                "02h1i0b81mxb4vns4xrvrfibpcvs7jqqav8p3yilwik8cv73r5x7"))

(define rust-sysctl-0.6.0
  (crate-source "sysctl" "0.6.0"
                "1z0x1lim5929fs60wjxnvd31z59d120p1v16d0mwcdxjxcnql681"))

(define rust-sysinfo-0.30.13
  (crate-source "sysinfo" "0.30.13"
                "1csbkx1hdlacgzw5ynjyfvgc1xg58w3h1rgh5gm2pysmxvd4snqa"))

(define rust-sysinfo-0.38.4
  (crate-source "sysinfo" "0.38.4"
                "0bx5wjp16cyckr9c0fxzrfcx54g4aa15f1k47kmqsl7yicpnmawj"))

(define rust-system-configuration-0.7.0
  (crate-source "system-configuration" "0.7.0"
                "12rwilylzc625qnxl30h5kf8wj5ka61zjrwpmb034cd0mc6ksgx1"))

(define rust-system-configuration-sys-0.6.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "system-configuration-sys" "0.6.0"
                "1i5sqrmgy58l4704hibjbl36hclddglh73fb3wx95jnmrq81n7cf"))

(define rust-target-features-0.1.6
  (crate-source "target-features" "0.1.6"
                "1m8y0ksw30gnkidjsjvnmhlpj165mgyj8ylk0lbs0qy4qprvkfy1"))

(define rust-test-log-0.2.20
  (crate-source "test-log" "0.2.20"
                "1cydyx3hzjrc7n52g34b8gcj6d76sm7xaxphjazzwjha9x3vyiig"))

(define rust-test-log-core-0.2.20
  (crate-source "test-log-core" "0.2.20"
                "0k1b20bybf1m6z4k3sllnmlrbjf0ziw1c0m926ic92xl409x9m1p"))

(define rust-test-log-macros-0.2.20
  (crate-source "test-log-macros" "0.2.20"
                "086kskh5sqha10izys0r9x24ixdw368a8014skzhshqym14r5swv"))

(define rust-tokenizers-0.19.1
  (crate-source "tokenizers" "0.19.1"
                "1zg6ffpllygijb5bh227m9p4lrhf0pjkysky68kddwrsvp8zl075"))

(define rust-tokio-retry-0.3.1
  (crate-source "tokio-retry" "0.3.1"
                "0h463h66srhgmnz29s9j8n67a3abjmf97y723a1rdlz9cb3l9xj0"))

(define rust-tokio-rustls-0.24.1
  (crate-source "tokio-rustls" "0.24.1"
                "10bhibg57mqir7xjhb2xmf24xgfpx6fzpyw720a4ih8a737jg0y2"))

(define rust-tracing-appender-0.2.5
  (crate-source "tracing-appender" "0.2.5"
                "0g4a6q5s3wafid5lqw1ljzvh1nhk3a4zmb627fxv96dr7qcqc1h5"))

(define rust-tracing-serde-0.2.0
  (crate-source "tracing-serde" "0.2.0"
                "1wbgzi364vzfswfkvy48a3p0z5xmv98sx342r57sil70ggmiljvh"))

(define rust-typed-path-0.12.3
  (crate-source "typed-path" "0.12.3"
                "03k051dafrnyg3lbm4c85zg0mpfhbn6l9aq4ryq8yyy8h2dzha4f"))

(define rust-unicode-reverse-1.0.9
  (crate-source "unicode-reverse" "1.0.9"
                "0xhcybbgy0l8s8n7sfd6hxi854f8znlxqkspzfnr8c62xf44hvsb"))

(define rust-untrusted-0.7.1
  (crate-source "untrusted" "0.7.1"
                "0jkbqaj9d3v5a91pp3wp9mffvng1nhycx6sh4qkdd9qyr62ccmm1"))

(define rust-wasi-0.14.7+wasi-0.2.4
  (crate-source "wasi" "0.14.7+wasi-0.2.4"
                "133fq3mq7h65mzrsphcm7bbbx1gsz7srrbwh01624zin43g7hd48"))

(define rust-wasite-1.0.2
  (crate-source "wasite" "1.0.2"
                "0hhsyylwsnbyz6dsr7i0gadzgk34nw4ljhnmafkji03b98mr1zk6"))

(define rust-wasm-streams-0.5.0
  (crate-source "wasm-streams" "0.5.0"
                "1fqbcx33w8ys5i5dv3p28a82g4yiclmhn80fcfp137kwa7vc87lx"))

(define rust-webpki-root-certs-1.0.7
  (crate-source "webpki-root-certs" "1.0.7"
                "0b59x5mzsilk42w59nif3lfhc24pgzb0v35pi6p01qy37z7424gk"))

(define rust-whoami-2.1.2
  (crate-source "whoami" "2.1.2"
                "03d4qn43gr4fnqnrq9k2769565234hycbac20rdiy3bli3png1wr"))

(define rust-windows-0.52.0
  (crate-source "windows" "0.52.0"
                "1gnh210qjlprpd1szaq04rjm1zqgdm9j7l9absg0kawi2rwm72p4"))

(define rust-windows-0.62.2
  (crate-source "windows" "0.62.2"
                "10457l9ihrbw8j79z2v4plyjxkf6xvb5npd0lqwmkh702gpaszsj"))

(define rust-windows-collections-0.3.2
  (crate-source "windows-collections" "0.3.2"
                "0436rjbkqn3j9m2v2lcmwwk0l3n2r57yvqb7fcy4m8d8y5ddkci3"))

(define rust-windows-core-0.52.0
  (crate-source "windows-core" "0.52.0"
                "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))

(define rust-windows-future-0.3.2
  (crate-source "windows-future" "0.3.2"
                "1jq5qs2dwzf6rl60f8gr49z2mifxsrdh4y4yfdws467ya41gkmp1"))

(define rust-windows-numerics-0.3.1
  (crate-source "windows-numerics" "0.3.1"
                "09hgbg8pf89r4090yyhh9q29ppi7yyxkgmga9ascshy19a240bkf"))

(define rust-windows-registry-0.6.1
  (crate-source "windows-registry" "0.6.1"
                "082p7l615qk8a4g8g15yipc5lghga6cgfhm74wm7zknwzgvjnx82"))

(define rust-windows-threading-0.2.1
  (crate-source "windows-threading" "0.2.1"
                "0dsvsy33vxs0153z4n39sqkzx382cjjkrd46rb3z3zfak5dvsj9r"))

(define rust-winnow-1.0.3
  (crate-source "winnow" "1.0.3"
                "1wajycd3krn6h699vydjv7hm0ll5l31p899qzpk59y2is74y34h5"))

(define rust-xet-client-1.5.2
  (crate-source "xet-client" "1.5.2"
                "1j4vlmvwl708gqn9x95jymzmwws6lvhlibyzmhbr1876rdnlj7iy"))

(define rust-xet-core-structures-1.5.2
  (crate-source "xet-core-structures" "1.5.2"
                "1vpv0fv121vmls984186fr40gm5a7h0cz10m62pk1mv7xfl8m0yb"))

(define rust-xet-data-1.5.2
  (crate-source "xet-data" "1.5.2"
                "137d6j0fi9c9mwwsn0wgcyr6q0xq1da9hdq1v6li2532xydl1zb7"))

(define rust-xet-runtime-1.5.2
  (crate-source "xet-runtime" "1.5.2"
                "0gd678ld0cbm40cp5pgzyh59rpg2s1qbwykkidjgfriqqchz3n0m"))

(define rust-zerofrom-0.1.8
  (crate-source "zerofrom" "0.1.8"
                "0wjjdj7gdmd0iq91gzkxl7dlv0nhkk80l4bmdpzh3a1yh48mmh0f"))

(define rust-zeroize-derive-1.4.3
  (crate-source "zeroize_derive" "1.4.3"
                "0bl5vd1lz27p4z336nximg5wrlw5j7jc8fxh7iv6r1wrhhav99c5"))

(define rust-zip-7.2.0
  (crate-source "zip" "7.2.0"
                "1q1dy2z9wl36ify50278pcx5hcp2bh8ikvy2271m01iaqbpk6bn4"))

(define rust-anstyle-wincon-3.0.6
  (crate-source "anstyle-wincon" "3.0.6"
                "099ir0w3lbpsp1nxdzbf4anq98ww8ykyc9pd1g03xgkj1v7dn291"))

(define rust-bitflags-2.6.0
  (crate-source "bitflags" "2.6.0"
                "1pkidwzn3hnxlsl8zizh0bncgbjnw7c41cx7bby26ncbzmiznj5h"))

(define rust-bumpalo-3.16.0
  (crate-source "bumpalo" "3.16.0"
                "0b015qb4knwanbdlp1x48pkb4pm57b8gidbhhhxr900q2wb6fabr"))

(define rust-cc-1.2.6
  (crate-source "cc" "1.2.6"
                "0cx32v9pcslavf8y10sb3y883v7377mw48q3dpw5b1cgidibnvcd"))

(define rust-console-0.15.10
  (crate-source "console" "0.15.10"
                "06q4ag46machxp5w381x1v9l2g7d801q6sawvxcpidarh36nwg7a"))

(define rust-darling-0.20.10
  (crate-source "darling" "0.20.10"
                "1299h2z88qn71mizhh05j26yr3ik0wnqmw11ijds89l8i9nbhqvg"))

(define rust-darling-core-0.20.10
  (crate-source "darling_core" "0.20.10"
                "1rgr9nci61ahnim93yh3xy6fkfayh7sk4447hahawah3m1hkh4wm"))

(define rust-darling-macro-0.20.10
  (crate-source "darling_macro" "0.20.10"
                "01kq3ibbn47czijj39h3vxyw0c2ksd0jvc097smcrk7n2jjs4dnk"))

(define rust-either-1.13.0
  (crate-source "either" "1.13.0"
                "1w2c1mybrd7vljyxk77y9f4w9dyjrmp3yp82mk7bcm8848fazcb0"))

(define rust-env-logger-0.11.6
  (crate-source "env_logger" "0.11.6"
                "1q30cqb2dfs3qrs0s30qdmqwi7n2gz4pniwd8a9gvhygwgcf7bnw"))

(define rust-errno-0.3.10
  (crate-source "errno" "0.3.10"
                "0pgblicz1kjz9wa9m0sghkhh2zw1fhq1mxzj7ndjm746kg5m5n1k"))

(define rust-humantime-2.1.0
  (crate-source "humantime" "2.1.0"
                "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls"))

(define rust-indicatif-0.17.9
  (crate-source "indicatif" "0.17.9"
                "10dj53x2a2bfhnfi78nhi4kb4zgc3rq6jm2wgg3d6d6rbsw7bxnb"))

(define rust-indoc-2.0.5
  (crate-source "indoc" "2.0.5"
                "1dgjk49rkmx4kjy07k4b90qb5vl89smgb5rcw02n0q0x9ligaj5j"))

(define rust-itoa-1.0.14
  (crate-source "itoa" "1.0.14"
                "0x26kr9m062mafaxgcf2p6h2x7cmixm0zw95aipzn2hr3d5jlnnp"))

(define rust-js-sys-0.3.76
  (crate-source "js-sys" "0.3.76"
                "1dz7v777h2j38wkf8k5iwkfxskn6nff2cdv2jsslyxkpn2svc5v7"))

(define rust-libc-0.2.169
  (crate-source "libc" "0.2.169"
                "02m253hs8gw0m1n8iyrsc4n15yzbqwhddi7w1l0ds7i92kdsiaxm"))

(define rust-linux-raw-sys-0.4.14
  (crate-source "linux-raw-sys" "0.4.14"
                "12gsjgbhhjwywpqcrizv80vrp7p7grsz5laqq773i33wphjsxcvq"))

(define rust-log-0.4.22
  (crate-source "log" "0.4.22"
                "093vs0wkm1rgyykk7fjbqp2lwizbixac1w52gv109p5r4jh0p9x7"))

(define rust-macro-rules-attribute-0.2.0
  (crate-source "macro_rules_attribute" "0.2.0"
                "04waa4qm28adwnxsxhx9135ki68mwkikr6m5pi5xhcy0gcgjg0la"))

(define rust-macro-rules-attribute-proc-macro-0.2.0
  (crate-source "macro_rules_attribute-proc_macro" "0.2.0"
                "0s45j4zm0a5d041g3vcbanvr76p331dfjb7gw9qdmh0w8mnqbpdq"))

(define rust-matrixmultiply-0.3.9
  (crate-source "matrixmultiply" "0.3.9"
                "06msav241ybxvsqfwm4hfmb1pbws71v0inhmyk0i0vg9wc8vk04k"))

(define rust-monostate-0.1.13
  (crate-source "monostate" "0.1.13"
                "07hfvh2202477mx1ff47b6f04gihqcdrmdndv10x0b2msw3q880d"))

(define rust-monostate-impl-0.1.13
  (crate-source "monostate-impl" "0.1.13"
                "1q3lxbfzpqcsy30gpyqkb2yppqzjj6ags6niflsi4kzdfnwn9km7"))

(define rust-numpy-0.23.0
  (crate-source "numpy" "0.23.0"
                "0y1yw681b9vhaihmq15qa5h5af0yi6iyc1mg6dys167r0plalk5r"))

(define rust-once-cell-1.20.2
  (crate-source "once_cell" "1.20.2"
                "0xb7rw1aqr7pa4z3b00y7786gyf8awx2gca3md73afy76dzgwq8j"))

(define rust-onig-6.4.0
  (crate-source "onig" "6.4.0"
                "0kyaz2fwa5dkr04rvk5ga2yv5jkqn1ymblvpdlf1gn9afb432jwc"))

(define rust-onig-sys-69.8.1
  (crate-source "onig_sys" "69.8.1"
                "1rw6y2qkb765gzylmrydbbd90hdzhnqyvs2y65z4riwwgqyrx0kv"))

(define rust-pkg-config-0.3.31
  (crate-source "pkg-config" "0.3.31"
                "1wk6yp2phl91795ia0lwkr3wl4a9xkrympvhqq8cxk4d75hwhglm"))

(define rust-portable-atomic-1.10.0
  (crate-source "portable-atomic" "1.10.0"
                "1rjfim62djiakf5rcq3r526hac0d1dd9hwa1jmiin7q7ad2c4398"))

(define rust-ppv-lite86-0.2.20
  (crate-source "ppv-lite86" "0.2.20"
                "017ax9ssdnpww7nrl1hvqh2lzncpv04nnsibmnw9nxjnaqlpp5bp"))

(define rust-proc-macro2-1.0.92
  (crate-source "proc-macro2" "1.0.92"
                "1c1vjy5wg8iy7kxsxda564qf4ljp0asysmbn2i7caj177x5m9lrp"))

(define rust-quote-1.0.38
  (crate-source "quote" "1.0.38"
                "1k0s75w61k6ch0rs263r4j69b7vj1wadqgb9dia4ylc9mymcqk8f"))

(define rust-rustc-hash-2.1.0
  (crate-source "rustc-hash" "2.1.0"
                "15yln6fmqlbg0k35r748h8g9xsd637ri23xihq81jb03ncwq1yy7"))

(define rust-rustix-0.38.42
  (crate-source "rustix" "0.38.42"
                "11fvprv3p450ggyqacp7sdpjbbsgm5zvfjwnzy8bfbmbrf7c6ggr"))

(define rust-ryu-1.0.18
  (crate-source "ryu" "1.0.18"
                "17xx2s8j1lln7iackzd9p0sv546vjq71i779gphjq923vjh5pjzk"))

(define rust-serde-1.0.217
  (crate-source "serde" "1.0.217"
                "0w2ck1p1ajmrv1cf51qf7igjn2nc51r0izzc00fzmmhkvxjl5z02"))

(define rust-serde-derive-1.0.217
  (crate-source "serde_derive" "1.0.217"
                "180r3rj5gi5s1m23q66cr5wlfgc5jrs6n1mdmql2njnhk37zg6ss"))

(define rust-serde-json-1.0.134
  (crate-source "serde_json" "1.0.134"
                "0z8wk61rzpqjmnwhv6k9zikhsfmsb6lr6qbg84aqpr1fqisl23yh"))

(define rust-smallvec-1.13.2
  (crate-source "smallvec" "1.13.2"
                "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))

(define rust-syn-2.0.93
  (crate-source "syn" "2.0.93"
                "0n6hk0yipq1q6cc8wb9jhw54l9vlvwiyc0182fqns3gfv9i60y4w"))

(define rust-tempfile-3.14.0
  (crate-source "tempfile" "3.14.0"
                "037f9jm13bmfc6xq9w86dp0nylrddh6ynvl6db4gm1xwzi8y5k18"))

(define rust-thiserror-2.0.9
  (crate-source "thiserror" "2.0.9"
                "1k5j0ri0kjrnlblv5ikaglbkg1sxxwh0qrxbidxgc38rs0zn8wph"))

(define rust-thiserror-impl-2.0.9
  (crate-source "thiserror-impl" "2.0.9"
                "1m77z5vb4w7xn7y12zxnbwncva4bwbi45y45xvkf5aki20kzll3v"))

(define rust-unicode-ident-1.0.14
  (crate-source "unicode-ident" "1.0.14"
                "10ywa1pg0glgkr4l3dppjxizr9r2b7im0ycbfa0137l69z5fdfdd"))

(define rust-unicode-width-0.2.0
  (crate-source "unicode-width" "0.2.0"
                "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))

(define rust-unindent-0.2.3
  (crate-source "unindent" "0.2.3"
                "1km2iy6fr6gsh2wvr1mxz86pm4wrlh3fjkinb35qfi3mw5rpvpn7"))

(define rust-wasm-bindgen-0.2.99
  (crate-source "wasm-bindgen" "0.2.99"
                "15k3rzb3kjrxyqnh0916gq99mrpwhwy62smawxxc2w0x3llgcx54"))

(define rust-wasm-bindgen-backend-0.2.99
  (crate-source "wasm-bindgen-backend" "0.2.99"
                "0ycwa4c68j34687k513djgyy2asn3fw3yp4g9rkq2kvbchwbp2az"))

(define rust-wasm-bindgen-macro-0.2.99
  (crate-source "wasm-bindgen-macro" "0.2.99"
                "1znlcrk5bvisr3vscwlqkdby959n3sb367zgdzpjwjd7v4giiiic"))

(define rust-wasm-bindgen-macro-support-0.2.99
  (crate-source "wasm-bindgen-macro-support" "0.2.99"
                "1hihsgyg0kf46kjhgfv8x5g9x0q1d0aizj6n7s84ag1xfrdskmrh"))

(define rust-wasm-bindgen-shared-0.2.99
  (crate-source "wasm-bindgen-shared" "0.2.99"
                "19h61snrhh1qhb5gz6zyb89l7fbj1fhmxcvi09p9l0mav8zsnfll"))

(define rust-zerocopy-0.7.35
  (crate-source "zerocopy" "0.7.35"
                "1w36q7b9il2flg0qskapgi9ymgg7p985vniqd09vi0mwib8lz6qv"))

(define rust-zerocopy-derive-0.7.35
  (crate-source "zerocopy-derive" "0.7.35"
                "0gnf2ap2y92nwdalzz3x7142f2b83sni66l39vxp2ijd6j080kzs"))

(define-cargo-inputs lookup-myguix-cargo-inputs
                     (python-tokenizers =>
                                      (list rust-aho-corasick-1.1.3
                                    rust-anstream-0.6.18
                                    rust-anstyle-1.0.10
                                    rust-anstyle-parse-0.2.6
                                    rust-anstyle-query-1.1.2
                                    rust-anstyle-wincon-3.0.6
                                    rust-autocfg-1.4.0
                                    rust-base64-0.13.1
                                    rust-bitflags-1.3.2
                                    rust-bitflags-2.6.0
                                    rust-bumpalo-3.16.0
                                    rust-byteorder-1.5.0
                                    rust-cc-1.2.6
                                    rust-cfg-if-1.0.0
                                    rust-colorchoice-1.0.3
                                    rust-console-0.15.10
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.21
                                    rust-darling-0.20.10
                                    rust-darling-core-0.20.10
                                    rust-darling-macro-0.20.10
                                    rust-derive-builder-0.20.2
                                    rust-derive-builder-core-0.20.2
                                    rust-derive-builder-macro-0.20.2
                                    rust-either-1.13.0
                                    rust-encode-unicode-1.0.0
                                    rust-env-filter-0.1.3
                                    rust-env-logger-0.11.6
                                    rust-errno-0.3.10
                                    rust-esaxx-rs-0.1.10
                                    rust-fastrand-2.3.0
                                    rust-fnv-1.0.7
                                    rust-getrandom-0.2.15
                                    rust-heck-0.5.0
                                    rust-humantime-2.1.0
                                    rust-ident-case-1.0.1
                                    rust-indicatif-0.17.9
                                    rust-indoc-2.0.5
                                    rust-is-terminal-polyfill-1.70.1
                                    rust-itertools-0.11.0
                                    rust-itertools-0.12.1
                                    rust-itertools-0.13.0
                                    rust-itoa-1.0.14
                                    rust-js-sys-0.3.76
                                    rust-lazy-static-1.5.0
                                    rust-libc-0.2.169
                                    rust-linux-raw-sys-0.4.14
                                    rust-log-0.4.22
                                    rust-macro-rules-attribute-0.2.0
                                    rust-macro-rules-attribute-proc-macro-0.2.0
                                    rust-matrixmultiply-0.3.9
                                    rust-memchr-2.7.4
                                    rust-memoffset-0.9.1
                                    rust-minimal-lexical-0.2.1
                                    rust-monostate-0.1.13
                                    rust-monostate-impl-0.1.13
                                    rust-ndarray-0.16.1
                                    rust-nom-7.1.3
                                    rust-num-complex-0.4.6
                                    rust-num-integer-0.1.46
                                    rust-num-traits-0.2.19
                                    rust-number-prefix-0.4.0
                                    rust-numpy-0.23.0
                                    rust-once-cell-1.20.2
                                    rust-onig-6.4.0
                                    rust-onig-sys-69.8.1
                                    rust-paste-1.0.15
                                    rust-pkg-config-0.3.31
                                    rust-portable-atomic-1.10.0
                                    rust-portable-atomic-util-0.2.4
                                    rust-ppv-lite86-0.2.20
                                    rust-proc-macro2-1.0.92
                                    rust-pyo3-0.23.5
                                    rust-pyo3-build-config-0.23.5
                                    rust-pyo3-ffi-0.23.5
                                    rust-pyo3-macros-0.23.5
                                    rust-pyo3-macros-backend-0.23.5
                                    rust-quote-1.0.38
                                    rust-rand-0.8.5
                                    rust-rand-chacha-0.3.1
                                    rust-rand-core-0.6.4
                                    rust-rawpointer-0.2.1
                                    rust-rayon-1.10.0
                                    rust-rayon-cond-0.3.0
                                    rust-rayon-core-1.12.1
                                    rust-regex-1.11.1
                                    rust-regex-automata-0.4.9
                                    rust-regex-syntax-0.8.5
                                    rust-rustc-hash-2.1.0
                                    rust-rustix-0.38.42
                                    rust-ryu-1.0.18
                                    rust-serde-1.0.217
                                    rust-serde-derive-1.0.217
                                    rust-serde-json-1.0.134
                                    rust-shlex-1.3.0
                                    rust-smallvec-1.13.2
                                    rust-spm-precompiled-0.1.4
                                    rust-strsim-0.11.1
                                    rust-syn-2.0.93
                                    rust-target-lexicon-0.12.16
                                    rust-tempfile-3.14.0
                                    rust-thiserror-2.0.9
                                    rust-thiserror-impl-2.0.9
                                    rust-tokenizers-0.21.1
                                    rust-unicode-ident-1.0.14
                                    rust-unicode-normalization-alignments-0.1.12
                                    rust-unicode-segmentation-1.12.0
                                    rust-unicode-width-0.2.0
                                    rust-unicode-categories-0.1.1
                                    rust-unindent-0.2.3
                                    rust-utf8parse-0.2.2
                                    rust-wasi-0.11.0+wasi-snapshot-preview1
                                    rust-wasm-bindgen-0.2.99
                                    rust-wasm-bindgen-backend-0.2.99
                                    rust-wasm-bindgen-macro-0.2.99
                                    rust-wasm-bindgen-macro-support-0.2.99
                                    rust-wasm-bindgen-shared-0.2.99
                                    rust-web-time-1.1.0
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
                                    rust-zerocopy-0.7.35
                                    rust-zerocopy-derive-0.7.35))
                     (pylance =>
                              (list rust-abi-stable-0.11.3
                                    rust-abi-stable-derive-0.11.3
                                    rust-abi-stable-shared-0.11.0
                                    rust-adler2-2.0.1
                                    rust-aes-0.8.4
                                    rust-ahash-0.8.12
                                    rust-aho-corasick-1.1.4
                                    rust-alloc-no-stdlib-2.0.4
                                    rust-alloc-stdlib-0.2.2
                                    rust-allocator-api2-0.2.21
                                    rust-android-system-properties-0.1.5
                                    rust-anstream-1.0.0
                                    rust-anstyle-1.0.14
                                    rust-anstyle-parse-1.0.0
                                    rust-anstyle-query-1.1.5
                                    rust-anstyle-wincon-3.0.11
                                    rust-anyhow-1.0.102
                                    rust-approx-0.5.1
                                    rust-ar-archive-writer-0.5.1
                                    rust-arrayref-0.3.9
                                    rust-arrayvec-0.7.6
                                    rust-arrow-58.3.0
                                    rust-arrow-arith-58.3.0
                                    rust-arrow-array-58.3.0
                                    rust-arrow-buffer-58.3.0
                                    rust-arrow-cast-58.3.0
                                    rust-arrow-csv-58.3.0
                                    rust-arrow-data-58.3.0
                                    rust-arrow-ipc-58.3.0
                                    rust-arrow-json-58.3.0
                                    rust-arrow-ord-58.3.0
                                    rust-arrow-pyarrow-58.3.0
                                    rust-arrow-row-58.3.0
                                    rust-arrow-schema-58.3.0
                                    rust-arrow-select-58.3.0
                                    rust-arrow-string-58.3.0
                                    rust-as-derive-utils-0.11.0
                                    rust-async-channel-2.5.0
                                    rust-async-compression-0.4.42
                                    rust-async-ffi-0.5.0
                                    rust-async-lock-3.4.2
                                    rust-async-recursion-1.1.1
                                    rust-async-trait-0.1.89
                                    rust-async-cell-0.2.3
                                    rust-atoi-2.0.0
                                    rust-atomic-waker-1.1.2
                                    rust-autocfg-1.5.0
                                    rust-aws-config-1.8.14
                                    rust-aws-credential-types-1.2.13
                                    rust-aws-lc-rs-1.16.3
                                    rust-aws-lc-sys-0.40.0
                                    rust-aws-runtime-1.7.1
                                    rust-aws-sdk-dynamodb-1.107.0
                                    rust-aws-sdk-sso-1.95.0
                                    rust-aws-sdk-ssooidc-1.97.0
                                    rust-aws-sdk-sts-1.99.0
                                    rust-aws-sigv4-1.4.1
                                    rust-aws-smithy-async-1.2.13
                                    rust-aws-smithy-http-0.63.5
                                    rust-aws-smithy-http-client-1.1.11
                                    rust-aws-smithy-json-0.62.4
                                    rust-aws-smithy-observability-0.2.5
                                    rust-aws-smithy-query-0.60.14
                                    rust-aws-smithy-runtime-1.10.2
                                    rust-aws-smithy-runtime-api-1.11.5
                                    rust-aws-smithy-types-1.4.5
                                    rust-aws-smithy-xml-0.60.14
                                    rust-aws-types-1.3.13
                                    rust-axum-0.7.9
                                    rust-axum-core-0.4.5
                                    rust-backon-1.6.0
                                    rust-base64-0.22.1
                                    rust-base64-simd-0.8.0
                                    rust-base64ct-1.8.3
                                    rust-bigdecimal-0.4.10
                                    rust-bincode-2.0.1
                                    rust-bincode-derive-2.0.1
                                    rust-bitflags-1.3.2
                                    rust-bitflags-2.11.1
                                    rust-bitpacking-0.9.3
                                    rust-bitvec-1.0.1
                                    rust-blake2-0.10.6
                                    rust-blake3-1.8.5
                                    rust-block-buffer-0.10.4
                                    rust-block-padding-0.3.3
                                    rust-brotli-8.0.2
                                    rust-brotli-decompressor-5.0.0
                                    rust-bs58-0.5.1
                                    rust-bumpalo-3.20.2
                                    rust-bytemuck-1.25.0
                                    rust-byteorder-1.5.0
                                    rust-bytes-1.11.1
                                    rust-bytes-utils-0.1.4
                                    rust-bzip2-0.6.1
                                    rust-cbc-0.1.2
                                    rust-cc-1.2.62
                                    rust-cedarwood-0.4.6
                                    rust-cfg-if-1.0.4
                                    rust-cfg-aliases-0.2.1
                                    rust-chrono-0.4.44
                                    rust-chrono-tz-0.10.4
                                    rust-cipher-0.4.4
                                    rust-cmake-0.1.58
                                    rust-colorchoice-1.0.5
                                    rust-comfy-table-7.2.2
                                    rust-compression-codecs-0.4.38
                                    rust-compression-core-0.4.32
                                    rust-concurrent-queue-2.5.0
                                    rust-const-oid-0.9.6
                                    rust-const-random-0.1.18
                                    rust-const-random-macro-0.1.16
                                    rust-const-panic-0.2.15
                                    rust-constant-time-eq-0.4.2
                                    rust-core-foundation-0.10.1
                                    rust-core-foundation-sys-0.8.7
                                    rust-core-extensions-1.5.4
                                    rust-core-extensions-proc-macros-1.5.4
                                    rust-cpufeatures-0.2.17
                                    rust-cpufeatures-0.3.0
                                    rust-crc32c-0.6.8
                                    rust-crc32fast-1.5.0
                                    rust-crossbeam-channel-0.5.15
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-queue-0.3.12
                                    rust-crossbeam-skiplist-0.1.3
                                    rust-crossbeam-utils-0.8.21
                                    rust-crunchy-0.2.4
                                    rust-crypto-common-0.1.7
                                    rust-csv-1.4.0
                                    rust-csv-core-0.1.13
                                    rust-darling-0.20.11
                                    rust-darling-0.23.0
                                    rust-darling-core-0.20.11
                                    rust-darling-core-0.23.0
                                    rust-darling-macro-0.20.11
                                    rust-darling-macro-0.23.0
                                    rust-dashmap-6.1.0
                                    rust-datafusion-53.1.0
                                    rust-datafusion-catalog-53.1.0
                                    rust-datafusion-catalog-listing-53.1.0
                                    rust-datafusion-common-53.1.0
                                    rust-datafusion-common-runtime-53.1.0
                                    rust-datafusion-datasource-53.1.0
                                    rust-datafusion-datasource-arrow-53.1.0
                                    rust-datafusion-datasource-csv-53.1.0
                                    rust-datafusion-datasource-json-53.1.0
                                    rust-datafusion-datasource-parquet-53.1.0
                                    rust-datafusion-doc-53.1.0
                                    rust-datafusion-execution-53.1.0
                                    rust-datafusion-expr-53.1.0
                                    rust-datafusion-expr-common-53.1.0
                                    rust-datafusion-ffi-53.1.0
                                    rust-datafusion-functions-53.1.0
                                    rust-datafusion-functions-aggregate-53.1.0
                                    rust-datafusion-functions-aggregate-common-53.1.0
                                    rust-datafusion-functions-nested-53.1.0
                                    rust-datafusion-functions-table-53.1.0
                                    rust-datafusion-functions-window-53.1.0
                                    rust-datafusion-functions-window-common-53.1.0
                                    rust-datafusion-macros-53.1.0
                                    rust-datafusion-optimizer-53.1.0
                                    rust-datafusion-physical-expr-53.1.0
                                    rust-datafusion-physical-expr-adapter-53.1.0
                                    rust-datafusion-physical-expr-common-53.1.0
                                    rust-datafusion-physical-optimizer-53.1.0
                                    rust-datafusion-physical-plan-53.1.0
                                    rust-datafusion-proto-53.1.0
                                    rust-datafusion-proto-common-53.1.0
                                    rust-datafusion-pruning-53.1.0
                                    rust-datafusion-session-53.1.0
                                    rust-datafusion-sql-53.1.0
                                    rust-datafusion-substrait-53.1.0
                                    rust-deepsize-0.2.0
                                    rust-deepsize-derive-0.1.2
                                    rust-der-0.7.10
                                    rust-deranged-0.5.8
                                    rust-derive-builder-0.20.2
                                    rust-derive-builder-core-0.20.2
                                    rust-derive-builder-macro-0.20.2
                                    rust-digest-0.10.7
                                    rust-dirs-6.0.0
                                    rust-dirs-sys-0.5.0
                                    rust-displaydoc-0.2.5
                                    rust-dlv-list-0.5.2
                                    rust-dunce-1.0.5
                                    rust-dyn-clone-1.0.20
                                    rust-earcutr-0.4.3
                                    rust-either-1.15.0
                                    rust-encoding-0.2.33
                                    rust-encoding-index-japanese-1.20141219.5
                                    rust-encoding-index-korean-1.20141219.5
                                    rust-encoding-index-simpchinese-1.20141219.5
                                    rust-encoding-index-singlebyte-1.20141219.5
                                    rust-encoding-index-tradchinese-1.20141219.5
                                    rust-encoding-index-tests-0.1.4
                                    rust-encoding-rs-0.8.35
                                    rust-encoding-rs-io-0.1.7
                                    rust-env-filter-1.0.1
                                    rust-env-logger-0.11.10
                                    rust-equivalent-1.0.2
                                    rust-errno-0.3.14
                                    rust-ethnum-1.5.3
                                    rust-event-listener-5.4.1
                                    rust-event-listener-strategy-0.5.4
                                    rust-fast-float2-0.2.3
                                    rust-fastrand-2.4.1
                                    rust-filetime-0.2.28
                                    rust-find-msvc-tools-0.1.9
                                    rust-fixedbitset-0.5.7
                                    rust-flatbuffers-25.12.19
                                    rust-flate2-1.1.9
                                    rust-float-next-after-1.0.0
                                    rust-fnv-1.0.7
                                    rust-foldhash-0.1.5
                                    rust-foldhash-0.2.0
                                    rust-form-urlencoded-1.2.2
                                    rust-fs-extra-1.3.0
                                    rust-fst-0.4.7
                                    rust-funty-2.0.0
                                    rust-futures-0.3.32
                                    rust-futures-channel-0.3.32
                                    rust-futures-core-0.3.32
                                    rust-futures-executor-0.3.32
                                    rust-futures-io-0.3.32
                                    rust-futures-macro-0.3.32
                                    rust-futures-sink-0.3.32
                                    rust-futures-task-0.3.32
                                    rust-futures-util-0.3.32
                                    rust-generational-arena-0.2.9
                                    rust-generator-0.8.8
                                    rust-generic-array-0.14.7
                                    rust-geo-0.31.0
                                    rust-geo-traits-0.3.0
                                    rust-geo-types-0.7.19
                                    rust-geoarrow-array-0.8.0
                                    rust-geoarrow-expr-geo-0.8.0
                                    rust-geoarrow-schema-0.8.0
                                    rust-geodatafusion-0.4.0
                                    rust-geographiclib-rs-0.2.7
                                    rust-geohash-0.13.1
                                    rust-getrandom-0.2.17
                                    rust-getrandom-0.3.4
                                    rust-getrandom-0.4.2
                                    rust-glob-0.3.3
                                    rust-gloo-timers-0.3.0
                                    rust-h2-0.4.14
                                    rust-half-2.7.1
                                    rust-hash32-0.3.1
                                    rust-hashbrown-0.12.3
                                    rust-hashbrown-0.14.5
                                    rust-hashbrown-0.15.5
                                    rust-hashbrown-0.16.1
                                    rust-hashbrown-0.17.1
                                    rust-heapless-0.8.0
                                    rust-heck-0.5.0
                                    rust-hermit-abi-0.5.2
                                    rust-hex-0.4.3
                                    rust-hmac-0.12.1
                                    rust-home-0.5.12
                                    rust-http-0.2.12
                                    rust-http-1.4.0
                                    rust-http-body-0.4.6
                                    rust-http-body-1.0.1
                                    rust-http-body-util-0.1.3
                                    rust-httparse-1.10.1
                                    rust-httpdate-1.0.3
                                    rust-humantime-2.3.0
                                    rust-hyper-1.9.0
                                    rust-hyper-rustls-0.27.9
                                    rust-hyper-util-0.1.20
                                    rust-hyperloglogplus-0.4.1
                                    rust-i-float-1.15.0
                                    rust-i-key-sort-0.6.0
                                    rust-i-overlay-4.0.7
                                    rust-i-shape-1.14.0
                                    rust-i-tree-0.16.0
                                    rust-iana-time-zone-0.1.65
                                    rust-iana-time-zone-haiku-0.1.2
                                    rust-icu-collections-2.2.0
                                    rust-icu-locale-core-2.2.0
                                    rust-icu-normalizer-2.2.0
                                    rust-icu-normalizer-data-2.2.0
                                    rust-icu-properties-2.2.0
                                    rust-icu-properties-data-2.2.0
                                    rust-icu-provider-2.2.0
                                    rust-id-arena-2.3.0
                                    rust-ident-case-1.0.1
                                    rust-idna-1.1.0
                                    rust-idna-adapter-1.2.2
                                    rust-indexmap-1.9.3
                                    rust-indexmap-2.14.0
                                    rust-inout-0.1.4
                                    rust-integer-encoding-3.0.4
                                    rust-io-uring-0.7.12
                                    rust-ipnet-2.12.0
                                    rust-is-terminal-polyfill-1.70.2
                                    rust-itertools-0.11.0
                                    rust-itertools-0.13.0
                                    rust-itertools-0.14.0
                                    rust-itoa-1.0.18
                                    rust-jieba-macros-0.9.0
                                    rust-jieba-rs-0.9.0
                                    rust-jiff-0.2.24
                                    rust-jiff-static-0.2.24
                                    rust-jiff-tzdb-0.1.6
                                    rust-jiff-tzdb-platform-0.1.3
                                    rust-jobserver-0.1.34
                                    rust-js-sys-0.3.98
                                    rust-jsonb-0.5.6
                                    rust-jsonwebtoken-9.3.1
                                    rust-kanaria-0.2.0
                                    rust-lance-namespace-reqwest-client-0.7.6
                                    rust-lazy-static-1.5.0
                                    rust-leb128fmt-0.1.0
                                    rust-lexical-core-1.0.6
                                    rust-lexical-parse-float-1.0.6
                                    rust-lexical-parse-integer-1.0.6
                                    rust-lexical-util-1.0.7
                                    rust-lexical-write-float-1.0.6
                                    rust-lexical-write-integer-1.0.6
                                    rust-libbz2-rs-sys-0.2.3
                                    rust-libc-0.2.186
                                    rust-libloading-0.7.4
                                    rust-liblzma-0.4.6
                                    rust-liblzma-sys-0.4.6
                                    rust-libm-0.2.16
                                    rust-libredox-0.1.16
                                    rust-lindera-0.44.1
                                    rust-lindera-cc-cedict-0.44.1
                                    rust-lindera-dictionary-0.44.1
                                    rust-lindera-ipadic-0.44.1
                                    rust-lindera-ipadic-neologd-0.44.1
                                    rust-lindera-ko-dic-0.44.1
                                    rust-lindera-unidic-0.44.1
                                    rust-linux-raw-sys-0.12.1
                                    rust-litemap-0.8.2
                                    rust-lock-api-0.4.14
                                    rust-log-0.4.29
                                    rust-loom-0.7.2
                                    rust-lru-slab-0.1.2
                                    rust-lz4-1.28.1
                                    rust-lz4-sys-1.11.1+lz4-1.10.0
                                    rust-lz4-flex-0.13.1
                                    rust-matchers-0.2.0
                                    rust-matchit-0.7.3
                                    rust-matrixmultiply-0.3.10
                                    rust-md-5-0.10.6
                                    rust-md5-0.8.0
                                    rust-memchr-2.8.0
                                    rust-memmap2-0.9.10
                                    rust-mime-0.3.17
                                    rust-mime-guess-2.0.5
                                    rust-miniz-oxide-0.8.9
                                    rust-mio-1.2.0
                                    rust-mock-instant-0.6.0
                                    rust-moka-0.12.15
                                    rust-multimap-0.10.1
                                    rust-ndarray-0.16.1
                                    rust-nom-8.0.0
                                    rust-nu-ansi-term-0.50.3
                                    rust-num-bigint-0.4.6
                                    rust-num-bigint-dig-0.8.6
                                    rust-num-complex-0.4.6
                                    rust-num-conv-0.2.1
                                    rust-num-integer-0.1.46
                                    rust-num-iter-0.1.45
                                    rust-num-traits-0.2.19
                                    rust-num-cpus-1.17.0
                                    rust-num-enum-0.7.6
                                    rust-num-enum-derive-0.7.6
                                    rust-object-0.37.3
                                    rust-object-store-0.12.5
                                    rust-object-store-0.13.2
                                    rust-object-store-opendal-0.55.0
                                    rust-once-cell-1.21.4
                                    rust-once-cell-polyfill-1.70.2
                                    rust-opendal-0.55.0
                                    rust-openssl-probe-0.2.1
                                    rust-option-ext-0.2.0
                                    rust-ordered-float-2.10.1
                                    rust-ordered-float-5.3.0
                                    rust-ordered-multimap-0.7.3
                                    rust-outref-0.5.2
                                    rust-parking-2.2.1
                                    rust-parking-lot-0.12.5
                                    rust-parking-lot-core-0.9.12
                                    rust-parquet-58.3.0
                                    rust-paste-1.0.15
                                    rust-path-abs-0.5.1
                                    rust-pbjson-0.8.0
                                    rust-pbjson-build-0.8.0
                                    rust-pbjson-types-0.8.0
                                    rust-pbkdf2-0.12.2
                                    rust-pem-3.0.6
                                    rust-pem-rfc7468-0.7.0
                                    rust-percent-encoding-2.3.2
                                    rust-permutation-0.4.1
                                    rust-petgraph-0.8.3
                                    rust-phf-0.12.1
                                    rust-phf-0.13.1
                                    rust-phf-codegen-0.13.1
                                    rust-phf-generator-0.13.1
                                    rust-phf-shared-0.12.1
                                    rust-phf-shared-0.13.1
                                    rust-pin-project-1.1.12
                                    rust-pin-project-internal-1.1.12
                                    rust-pin-project-lite-0.2.17
                                    rust-pin-utils-0.1.0
                                    rust-pkcs1-0.7.5
                                    rust-pkcs5-0.7.1
                                    rust-pkcs8-0.10.2
                                    rust-pkg-config-0.3.33
                                    rust-portable-atomic-1.13.1
                                    rust-portable-atomic-util-0.2.7
                                    rust-potential-utf-0.1.5
                                    rust-powerfmt-0.2.0
                                    rust-ppv-lite86-0.2.21
                                    rust-prettyplease-0.2.37
                                    rust-proc-macro-crate-3.5.0
                                    rust-proc-macro2-1.0.106
                                    rust-prost-0.14.3
                                    rust-prost-build-0.14.3
                                    rust-prost-derive-0.14.3
                                    rust-prost-types-0.14.3
                                    rust-psm-0.1.31
                                    rust-pyo3-0.28.3
                                    rust-pyo3-build-config-0.28.3
                                    rust-pyo3-ffi-0.28.3
                                    rust-pyo3-macros-0.28.3
                                    rust-pyo3-macros-backend-0.28.3
                                    rust-pythonize-0.28.0
                                    rust-quick-xml-0.37.5
                                    rust-quick-xml-0.38.4
                                    rust-quinn-0.11.9
                                    rust-quinn-proto-0.11.14
                                    rust-quinn-udp-0.5.14
                                    rust-quote-1.0.45
                                    rust-r-efi-5.3.0
                                    rust-r-efi-6.0.0
                                    rust-radium-0.7.0
                                    rust-rand-0.8.6
                                    rust-rand-0.9.4
                                    rust-rand-chacha-0.3.1
                                    rust-rand-chacha-0.9.0
                                    rust-rand-core-0.6.4
                                    rust-rand-core-0.9.5
                                    rust-rand-distr-0.5.1
                                    rust-rand-xoshiro-0.7.0
                                    rust-random-word-0.5.2
                                    rust-rangemap-1.7.1
                                    rust-rawpointer-0.2.1
                                    rust-rayon-1.12.0
                                    rust-rayon-core-1.13.0
                                    rust-recursive-0.1.1
                                    rust-recursive-proc-macro-impl-0.1.1
                                    rust-redox-syscall-0.5.18
                                    rust-redox-users-0.5.2
                                    rust-ref-cast-1.0.25
                                    rust-ref-cast-impl-1.0.25
                                    rust-regex-1.12.3
                                    rust-regex-automata-0.4.14
                                    rust-regex-lite-0.1.9
                                    rust-regex-syntax-0.8.10
                                    rust-regress-0.10.5
                                    rust-repr-offset-0.2.2
                                    rust-reqsign-0.16.5
                                    rust-reqwest-0.12.28
                                    rust-ring-0.17.14
                                    rust-roaring-0.11.4
                                    rust-robust-1.2.0
                                    rust-rsa-0.9.10
                                    rust-rstar-0.12.2
                                    rust-rust-ini-0.21.3
                                    rust-rust-stemmers-1.2.0
                                    rust-rustc-hash-2.1.2
                                    rust-rustc-version-0.4.1
                                    rust-rustix-1.1.4
                                    rust-rustls-0.23.40
                                    rust-rustls-native-certs-0.8.3
                                    rust-rustls-pemfile-2.2.0
                                    rust-rustls-pki-types-1.14.1
                                    rust-rustls-webpki-0.103.13
                                    rust-rustversion-1.0.22
                                    rust-ryu-1.0.23
                                    rust-salsa20-0.10.2
                                    rust-same-file-1.0.6
                                    rust-schannel-0.1.29
                                    rust-schemars-0.8.22
                                    rust-schemars-0.9.0
                                    rust-schemars-1.2.1
                                    rust-schemars-derive-0.8.22
                                    rust-scoped-tls-1.0.1
                                    rust-scopeguard-1.2.0
                                    rust-scrypt-0.11.0
                                    rust-security-framework-3.7.0
                                    rust-security-framework-sys-2.17.0
                                    rust-semver-1.0.28
                                    rust-seq-macro-0.3.6
                                    rust-serde-1.0.228
                                    rust-serde-core-1.0.228
                                    rust-serde-derive-1.0.228
                                    rust-serde-derive-internals-0.29.1
                                    rust-serde-json-1.0.149
                                    rust-serde-path-to-error-0.1.20
                                    rust-serde-repr-0.1.20
                                    rust-serde-tokenstream-0.2.3
                                    rust-serde-urlencoded-0.7.1
                                    rust-serde-with-3.20.0
                                    rust-serde-with-macros-3.20.0
                                    rust-serde-yaml-0.9.34+deprecated
                                    rust-sha1-0.10.6
                                    rust-sha2-0.10.9
                                    rust-sharded-slab-0.1.7
                                    rust-shlex-1.3.0
                                    rust-signal-hook-registry-1.4.8
                                    rust-signature-2.2.0
                                    rust-simd-adler32-0.3.9
                                    rust-simdutf8-0.1.5
                                    rust-simple-asn1-0.6.4
                                    rust-siphasher-1.0.3
                                    rust-slab-0.4.12
                                    rust-smallvec-1.15.1
                                    rust-snafu-0.9.0
                                    rust-snafu-derive-0.9.0
                                    rust-snap-1.1.1
                                    rust-socket2-0.6.3
                                    rust-spade-2.15.1
                                    rust-spin-0.9.8
                                    rust-spki-0.7.3
                                    rust-sqlparser-0.61.0
                                    rust-sqlparser-derive-0.5.0
                                    rust-stable-deref-trait-1.2.1
                                    rust-stacker-0.1.24
                                    rust-std-prelude-0.2.12
                                    rust-stfu8-0.2.7
                                    rust-strsim-0.11.1
                                    rust-strum-0.26.3
                                    rust-strum-0.27.2
                                    rust-strum-macros-0.26.4
                                    rust-strum-macros-0.27.2
                                    rust-substrait-0.62.2
                                    rust-subtle-2.6.1
                                    rust-syn-1.0.109
                                    rust-syn-2.0.117
                                    rust-sync-wrapper-1.0.2
                                    rust-synstructure-0.13.2
                                    rust-tagptr-0.2.0
                                    rust-tap-1.0.1
                                    rust-tar-0.4.45
                                    rust-target-lexicon-0.13.5
                                    rust-tempfile-3.27.0
                                    rust-thiserror-1.0.69
                                    rust-thiserror-2.0.18
                                    rust-thiserror-impl-1.0.69
                                    rust-thiserror-impl-2.0.18
                                    rust-thread-tree-0.3.3
                                    rust-thread-local-1.1.9
                                    rust-thrift-0.17.0
                                    rust-time-0.3.47
                                    rust-time-core-0.1.8
                                    rust-time-macros-0.2.27
                                    rust-tiny-keccak-2.0.2
                                    rust-tinystr-0.8.3
                                    rust-tinyvec-1.11.0
                                    rust-tinyvec-macros-0.1.1
                                    rust-tokio-1.52.3
                                    rust-tokio-macros-2.7.0
                                    rust-tokio-rustls-0.26.4
                                    rust-tokio-stream-0.1.18
                                    rust-tokio-util-0.7.18
                                    rust-toml-datetime-1.1.1+spec-1.1.0
                                    rust-toml-edit-0.25.11+spec-1.1.0
                                    rust-toml-parser-1.1.2+spec-1.1.0
                                    rust-tower-0.5.3
                                    rust-tower-http-0.5.2
                                    rust-tower-http-0.6.10
                                    rust-tower-layer-0.3.3
                                    rust-tower-service-0.3.3
                                    rust-tracing-0.1.44
                                    rust-tracing-attributes-0.1.31
                                    rust-tracing-chrome-0.7.2
                                    rust-tracing-core-0.1.36
                                    rust-tracing-log-0.2.0
                                    rust-tracing-subscriber-0.3.23
                                    rust-try-lock-0.2.5
                                    rust-tstr-0.2.4
                                    rust-tstr-proc-macros-0.2.2
                                    rust-twox-hash-2.1.2
                                    rust-typed-arena-2.0.2
                                    rust-typenum-1.20.0
                                    rust-typewit-1.15.2
                                    rust-typify-0.5.0
                                    rust-typify-impl-0.5.0
                                    rust-typify-macro-0.5.0
                                    rust-unicase-2.9.0
                                    rust-unicode-blocks-0.1.9
                                    rust-unicode-ident-1.0.24
                                    rust-unicode-normalization-0.1.25
                                    rust-unicode-segmentation-1.13.2
                                    rust-unicode-width-0.2.2
                                    rust-unicode-xid-0.2.6
                                    rust-unsafe-libyaml-0.2.11
                                    rust-untrusted-0.9.0
                                    rust-unty-0.0.4
                                    rust-url-2.5.8
                                    rust-urlencoding-2.1.3
                                    rust-utf8-ranges-1.0.5
                                    rust-utf8-iter-1.0.4
                                    rust-utf8parse-0.2.2
                                    rust-uuid-1.23.1
                                    rust-valuable-0.1.1
                                    rust-version-check-0.9.5
                                    rust-virtue-0.0.18
                                    rust-vsimd-0.8.0
                                    rust-walkdir-2.5.0
                                    rust-want-0.3.1
                                    rust-wasi-0.11.1+wasi-snapshot-preview1
                                    rust-wasip2-1.0.3+wasi-0.2.9
                                    rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                                    rust-wasm-bindgen-0.2.121
                                    rust-wasm-bindgen-futures-0.4.71
                                    rust-wasm-bindgen-macro-0.2.121
                                    rust-wasm-bindgen-macro-support-0.2.121
                                    rust-wasm-bindgen-shared-0.2.121
                                    rust-wasm-encoder-0.244.0
                                    rust-wasm-metadata-0.244.0
                                    rust-wasm-streams-0.4.2
                                    rust-wasmparser-0.244.0
                                    rust-web-sys-0.3.98
                                    rust-web-time-1.1.0
                                    rust-webpki-roots-1.0.7
                                    rust-winapi-0.3.9
                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                    rust-winapi-util-0.1.11
                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                    rust-windows-core-0.62.2
                                    rust-windows-implement-0.60.2
                                    rust-windows-interface-0.59.3
                                    rust-windows-link-0.2.1
                                    rust-windows-result-0.4.1
                                    rust-windows-strings-0.5.1
                                    rust-windows-sys-0.52.0
                                    rust-windows-sys-0.60.2
                                    rust-windows-sys-0.61.2
                                    rust-windows-targets-0.52.6
                                    rust-windows-targets-0.53.5
                                    rust-windows-aarch64-gnullvm-0.52.6
                                    rust-windows-aarch64-gnullvm-0.53.1
                                    rust-windows-aarch64-msvc-0.52.6
                                    rust-windows-aarch64-msvc-0.53.1
                                    rust-windows-i686-gnu-0.52.6
                                    rust-windows-i686-gnu-0.53.1
                                    rust-windows-i686-gnullvm-0.52.6
                                    rust-windows-i686-gnullvm-0.53.1
                                    rust-windows-i686-msvc-0.52.6
                                    rust-windows-i686-msvc-0.53.1
                                    rust-windows-x86-64-gnu-0.52.6
                                    rust-windows-x86-64-gnu-0.53.1
                                    rust-windows-x86-64-gnullvm-0.52.6
                                    rust-windows-x86-64-gnullvm-0.53.1
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-windows-x86-64-msvc-0.53.1
                                    rust-winnow-1.0.2
                                    rust-wit-bindgen-0.51.0
                                    rust-wit-bindgen-0.57.1
                                    rust-wit-bindgen-core-0.51.0
                                    rust-wit-bindgen-rust-0.51.0
                                    rust-wit-bindgen-rust-macro-0.51.0
                                    rust-wit-component-0.244.0
                                    rust-wit-parser-0.244.0
                                    rust-wkb-0.9.2
                                    rust-wkt-0.14.0
                                    rust-writeable-0.6.3
                                    rust-wyz-0.5.1
                                    rust-xattr-1.6.1
                                    rust-xmlparser-0.13.6
                                    rust-xxhash-rust-0.8.15
                                    rust-yada-0.5.1
                                    rust-yoke-0.8.2
                                    rust-yoke-derive-0.8.2
                                    rust-zerocopy-0.8.48
                                    rust-zerocopy-derive-0.8.48
                                    rust-zerofrom-0.1.7
                                    rust-zerofrom-derive-0.1.7
                                    rust-zeroize-1.8.2
                                    rust-zerotrie-0.2.4
                                    rust-zerovec-0.11.6
                                    rust-zerovec-derive-0.11.3
                                    rust-zlib-rs-0.6.3
                                    rust-zmij-1.0.21
                                    rust-zstd-0.13.3
                                    rust-zstd-safe-7.2.4
                                    rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (lancedb-python =>
                              (list rust-adler2-2.0.1
                                    rust-aes-0.8.4
                                    rust-ahash-0.8.12
                                    rust-aho-corasick-1.1.4
                                    rust-alloc-no-stdlib-2.0.4
                                    rust-alloc-stdlib-0.2.2
                                    rust-allocator-api2-0.2.21
                                    rust-android-system-properties-0.1.5
                                    rust-anstream-1.0.0
                                    rust-anstyle-1.0.14
                                    rust-anstyle-parse-1.0.0
                                    rust-anstyle-query-1.1.5
                                    rust-anstyle-wincon-3.0.11
                                    rust-anyhow-1.0.102
                                    rust-approx-0.5.1
                                    rust-ar-archive-writer-0.5.1
                                    rust-arc-swap-1.9.1
                                    rust-argminmax-0.6.3
                                    rust-array-init-cursor-0.2.1
                                    rust-arrayref-0.3.9
                                    rust-arrayvec-0.7.6
                                    rust-arrow-58.3.0
                                    rust-arrow-arith-58.3.0
                                    rust-arrow-array-58.3.0
                                    rust-arrow-buffer-58.3.0
                                    rust-arrow-cast-58.3.0
                                    rust-arrow-csv-58.3.0
                                    rust-arrow-data-58.3.0
                                    rust-arrow-ipc-58.3.0
                                    rust-arrow-json-58.3.0
                                    rust-arrow-ord-58.3.0
                                    rust-arrow-pyarrow-58.3.0
                                    rust-arrow-row-58.3.0
                                    rust-arrow-schema-58.3.0
                                    rust-arrow-select-58.3.0
                                    rust-arrow-string-58.3.0
                                    rust-async-channel-2.5.0
                                    rust-async-compression-0.4.42
                                    rust-async-convert-1.0.0
                                    rust-async-lock-3.4.2
                                    rust-async-openai-0.20.0
                                    rust-async-recursion-1.1.1
                                    rust-async-trait-0.1.89
                                    rust-async-cell-0.2.3
                                    rust-atoi-2.0.0
                                    rust-atoi-simd-0.15.6
                                    rust-atomic-waker-1.1.2
                                    rust-autocfg-1.5.0
                                    rust-aws-config-1.8.16
                                    rust-aws-credential-types-1.2.14
                                    rust-aws-lc-rs-1.16.3
                                    rust-aws-lc-sys-0.40.0
                                    rust-aws-runtime-1.7.3
                                    rust-aws-sdk-bedrockruntime-1.130.0
                                    rust-aws-sdk-dynamodb-1.111.0
                                    rust-aws-sdk-kms-1.106.0
                                    rust-aws-sdk-s3-1.132.0
                                    rust-aws-sdk-sso-1.98.0
                                    rust-aws-sdk-ssooidc-1.100.0
                                    rust-aws-sdk-sts-1.103.0
                                    rust-aws-sigv4-1.4.3
                                    rust-aws-smithy-async-1.2.14
                                    rust-aws-smithy-checksums-0.64.7
                                    rust-aws-smithy-eventstream-0.60.20
                                    rust-aws-smithy-http-0.63.6
                                    rust-aws-smithy-http-client-1.1.12
                                    rust-aws-smithy-json-0.62.5
                                    rust-aws-smithy-observability-0.2.6
                                    rust-aws-smithy-query-0.60.15
                                    rust-aws-smithy-runtime-1.11.3
                                    rust-aws-smithy-runtime-api-1.12.1
                                    rust-aws-smithy-runtime-api-macros-1.0.0
                                    rust-aws-smithy-schema-0.1.0
                                    rust-aws-smithy-types-1.4.8
                                    rust-aws-smithy-xml-0.60.15
                                    rust-aws-types-1.3.15
                                    rust-axum-0.7.9
                                    rust-axum-core-0.4.5
                                    rust-backoff-0.4.0
                                    rust-backon-1.6.0
                                    rust-base16ct-0.1.1
                                    rust-base64-0.13.1
                                    rust-base64-0.21.7
                                    rust-base64-0.22.1
                                    rust-base64-simd-0.8.0
                                    rust-base64ct-1.8.3
                                    rust-bigdecimal-0.4.10
                                    rust-bit-set-0.8.0
                                    rust-bit-vec-0.8.0
                                    rust-bitflags-1.3.2
                                    rust-bitflags-2.11.1
                                    rust-bitpacking-0.9.3
                                    rust-bitvec-1.0.1
                                    rust-blake2-0.10.6
                                    rust-blake3-1.8.5
                                    rust-block-buffer-0.10.4
                                    rust-block-buffer-0.12.0
                                    rust-block-padding-0.3.3
                                    rust-brotli-3.5.0
                                    rust-brotli-8.0.2
                                    rust-brotli-decompressor-2.5.1
                                    rust-brotli-decompressor-5.0.0
                                    rust-bs58-0.5.1
                                    rust-bstr-1.12.1
                                    rust-bumpalo-3.20.2
                                    rust-bytecheck-0.8.2
                                    rust-bytecheck-derive-0.8.2
                                    rust-bytemuck-1.25.0
                                    rust-bytemuck-derive-1.10.2
                                    rust-byteorder-1.5.0
                                    rust-bytes-1.11.1
                                    rust-bytes-utils-0.1.4
                                    rust-candle-core-0.9.2
                                    rust-candle-nn-0.9.2
                                    rust-candle-transformers-0.9.2
                                    rust-cbc-0.1.2
                                    rust-cc-1.2.62
                                    rust-cedarwood-0.4.6
                                    rust-cfg-if-0.1.10
                                    rust-cfg-if-1.0.4
                                    rust-cfg-aliases-0.2.1
                                    rust-chacha20-0.10.0
                                    rust-chrono-0.4.44
                                    rust-chrono-tz-0.8.6
                                    rust-chrono-tz-0.10.4
                                    rust-chrono-tz-build-0.2.1
                                    rust-cipher-0.4.4
                                    rust-clap-4.6.1
                                    rust-clap-builder-4.6.0
                                    rust-clap-derive-4.6.1
                                    rust-clap-lex-1.1.0
                                    rust-cmake-0.1.58
                                    rust-cmov-0.5.3
                                    rust-colorchoice-1.0.5
                                    rust-colored-3.1.1
                                    rust-combine-4.6.7
                                    rust-comfy-table-7.2.2
                                    rust-compression-codecs-0.4.38
                                    rust-compression-core-0.4.32
                                    rust-concurrent-queue-2.5.0
                                    rust-console-0.15.11
                                    rust-const-oid-0.9.6
                                    rust-const-oid-0.10.2
                                    rust-const-random-0.1.18
                                    rust-const-random-macro-0.1.16
                                    rust-const-str-1.1.0
                                    rust-const-panic-0.2.15
                                    rust-constant-time-eq-0.4.2
                                    rust-convert-case-0.11.0
                                    rust-core-foundation-0.9.4
                                    rust-core-foundation-0.10.1
                                    rust-core-foundation-sys-0.8.7
                                    rust-countio-0.3.0
                                    rust-cpufeatures-0.2.17
                                    rust-cpufeatures-0.3.0
                                    rust-crc-3.3.0
                                    rust-crc-catalog-2.5.0
                                    rust-crc-fast-1.9.0
                                    rust-crc32c-0.6.8
                                    rust-crc32fast-1.5.0
                                    rust-crossbeam-channel-0.5.15
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-queue-0.3.12
                                    rust-crossbeam-skiplist-0.1.3
                                    rust-crossbeam-utils-0.8.21
                                    rust-crossterm-0.29.0
                                    rust-crossterm-winapi-0.9.1
                                    rust-crunchy-0.2.4
                                    rust-crypto-bigint-0.4.9
                                    rust-crypto-bigint-0.5.5
                                    rust-crypto-common-0.1.7
                                    rust-crypto-common-0.2.1
                                    rust-csv-1.4.0
                                    rust-csv-core-0.1.13
                                    rust-ctor-0.6.3
                                    rust-ctor-1.0.5
                                    rust-ctor-proc-macro-0.0.7
                                    rust-ctutils-0.4.2
                                    rust-daachorse-2.1.1
                                    rust-darling-0.20.11
                                    rust-darling-0.23.0
                                    rust-darling-core-0.20.11
                                    rust-darling-core-0.23.0
                                    rust-darling-macro-0.20.11
                                    rust-darling-macro-0.23.0
                                    rust-dashmap-6.1.0
                                    rust-datafusion-53.1.0
                                    rust-datafusion-catalog-53.1.0
                                    rust-datafusion-catalog-listing-53.1.0
                                    rust-datafusion-common-53.1.0
                                    rust-datafusion-common-runtime-53.1.0
                                    rust-datafusion-datasource-53.1.0
                                    rust-datafusion-datasource-arrow-53.1.0
                                    rust-datafusion-datasource-csv-53.1.0
                                    rust-datafusion-datasource-json-53.1.0
                                    rust-datafusion-doc-53.1.0
                                    rust-datafusion-execution-53.1.0
                                    rust-datafusion-expr-53.1.0
                                    rust-datafusion-expr-common-53.1.0
                                    rust-datafusion-functions-53.1.0
                                    rust-datafusion-functions-aggregate-53.1.0
                                    rust-datafusion-functions-aggregate-common-53.1.0
                                    rust-datafusion-functions-nested-53.1.0
                                    rust-datafusion-functions-table-53.1.0
                                    rust-datafusion-functions-window-53.1.0
                                    rust-datafusion-functions-window-common-53.1.0
                                    rust-datafusion-macros-53.1.0
                                    rust-datafusion-optimizer-53.1.0
                                    rust-datafusion-physical-expr-53.1.0
                                    rust-datafusion-physical-expr-adapter-53.1.0
                                    rust-datafusion-physical-expr-common-53.1.0
                                    rust-datafusion-physical-optimizer-53.1.0
                                    rust-datafusion-physical-plan-53.1.0
                                    rust-datafusion-pruning-53.1.0
                                    rust-datafusion-session-53.1.0
                                    rust-datafusion-sql-53.1.0
                                    rust-deepsize-0.2.0
                                    rust-deepsize-derive-0.1.2
                                    rust-der-0.6.1
                                    rust-der-0.7.10
                                    rust-deranged-0.5.8
                                    rust-derive-builder-0.20.2
                                    rust-derive-builder-core-0.20.2
                                    rust-derive-builder-macro-0.20.2
                                    rust-digest-0.10.7
                                    rust-digest-0.11.3
                                    rust-dirs-6.0.0
                                    rust-dirs-sys-0.5.0
                                    rust-displaydoc-0.2.5
                                    rust-dlv-list-0.5.2
                                    rust-document-features-0.2.12
                                    rust-dtor-0.1.1
                                    rust-dtor-proc-macro-0.0.6
                                    rust-dunce-1.0.5
                                    rust-dyn-clone-1.0.20
                                    rust-dyn-stack-0.13.2
                                    rust-dyn-stack-macros-0.1.3
                                    rust-ecdsa-0.14.8
                                    rust-either-1.15.0
                                    rust-elliptic-curve-0.12.3
                                    rust-encode-unicode-1.0.0
                                    rust-encoding-rs-0.8.35
                                    rust-encoding-rs-io-0.1.7
                                    rust-enum-as-inner-0.6.1
                                    rust-enum-dispatch-0.3.13
                                    rust-env-filter-1.0.1
                                    rust-env-logger-0.11.10
                                    rust-equivalent-1.0.2
                                    rust-errno-0.3.14
                                    rust-esaxx-rs-0.1.10
                                    rust-ethnum-1.5.3
                                    rust-event-listener-5.4.1
                                    rust-event-listener-strategy-0.5.4
                                    rust-eventsource-stream-0.2.3
                                    rust-fallible-streaming-iterator-0.1.9
                                    rust-fancy-regex-0.17.0
                                    rust-fast-float-0.2.0
                                    rust-fast-float2-0.2.3
                                    rust-fastrand-2.4.1
                                    rust-ff-0.12.1
                                    rust-find-msvc-tools-0.1.9
                                    rust-fixedbitset-0.5.7
                                    rust-flatbuffers-25.12.19
                                    rust-flate2-1.1.9
                                    rust-float8-0.6.1
                                    rust-fnv-1.0.7
                                    rust-foldhash-0.1.5
                                    rust-foldhash-0.2.0
                                    rust-foreign-vec-0.1.0
                                    rust-form-urlencoded-1.2.2
                                    rust-fs-extra-1.3.0
                                    rust-fsst-7.0.0
                                    rust-fst-0.4.7
                                    rust-funty-2.0.0
                                    rust-futures-0.3.32
                                    rust-futures-channel-0.3.32
                                    rust-futures-core-0.3.32
                                    rust-futures-executor-0.3.32
                                    rust-futures-io-0.3.32
                                    rust-futures-macro-0.3.32
                                    rust-futures-sink-0.3.32
                                    rust-futures-task-0.3.32
                                    rust-futures-timer-3.0.3
                                    rust-futures-util-0.3.32
                                    rust-gearhash-0.1.3
                                    rust-gemm-0.19.0
                                    rust-gemm-c32-0.19.0
                                    rust-gemm-c64-0.19.0
                                    rust-gemm-common-0.19.0
                                    rust-gemm-f16-0.19.0
                                    rust-gemm-f32-0.19.0
                                    rust-gemm-f64-0.19.0
                                    rust-generator-0.8.8
                                    rust-generic-array-0.14.7
                                    rust-getrandom-0.2.17
                                    rust-getrandom-0.3.4
                                    rust-getrandom-0.4.2
                                    rust-git-version-0.3.9
                                    rust-git-version-macro-0.3.9
                                    rust-glob-0.3.3
                                    rust-gloo-timers-0.3.0
                                    rust-group-0.12.1
                                    rust-h2-0.3.27
                                    rust-h2-0.4.14
                                    rust-half-2.7.1
                                    rust-hashbrown-0.12.3
                                    rust-hashbrown-0.14.5
                                    rust-hashbrown-0.15.5
                                    rust-hashbrown-0.16.1
                                    rust-hashbrown-0.17.1
                                    rust-heapify-0.2.0
                                    rust-heck-0.4.1
                                    rust-heck-0.5.0
                                    rust-hermit-abi-0.5.2
                                    rust-hex-0.4.3
                                    rust-hf-hub-0.4.3
                                    rust-hf-xet-1.5.2
                                    rust-hmac-0.12.1
                                    rust-hmac-0.13.0
                                    rust-home-0.5.12
                                    rust-http-0.2.12
                                    rust-http-1.4.0
                                    rust-http-body-0.4.6
                                    rust-http-body-1.0.1
                                    rust-http-body-util-0.1.3
                                    rust-httparse-1.10.1
                                    rust-httpdate-1.0.3
                                    rust-humantime-2.3.0
                                    rust-hybrid-array-0.4.12
                                    rust-hyper-0.14.32
                                    rust-hyper-1.9.0
                                    rust-hyper-rustls-0.24.2
                                    rust-hyper-rustls-0.27.9
                                    rust-hyper-util-0.1.20
                                    rust-hyperloglogplus-0.4.1
                                    rust-iana-time-zone-0.1.65
                                    rust-iana-time-zone-haiku-0.1.2
                                    rust-icu-collections-2.2.0
                                    rust-icu-locale-core-2.2.0
                                    rust-icu-normalizer-2.2.0
                                    rust-icu-normalizer-data-2.2.0
                                    rust-icu-properties-2.2.0
                                    rust-icu-properties-data-2.2.0
                                    rust-icu-provider-2.2.0
                                    rust-id-arena-2.3.0
                                    rust-ident-case-1.0.1
                                    rust-idna-1.1.0
                                    rust-idna-adapter-1.2.2
                                    rust-indexmap-1.9.3
                                    rust-indexmap-2.14.0
                                    rust-indicatif-0.17.11
                                    rust-inout-0.1.4
                                    rust-instant-0.1.13
                                    rust-io-uring-0.7.12
                                    rust-ipnet-2.12.0
                                    rust-is-terminal-polyfill-1.70.2
                                    rust-itertools-0.11.0
                                    rust-itertools-0.12.1
                                    rust-itertools-0.13.0
                                    rust-itertools-0.14.0
                                    rust-itoa-1.0.18
                                    rust-itoap-1.0.1
                                    rust-jieba-macros-0.9.0
                                    rust-jieba-rs-0.9.0
                                    rust-jiff-0.2.24
                                    rust-jiff-static-0.2.24
                                    rust-jiff-tzdb-0.1.6
                                    rust-jiff-tzdb-platform-0.1.3
                                    rust-jni-0.22.4
                                    rust-jni-macros-0.22.4
                                    rust-jni-sys-0.4.1
                                    rust-jni-sys-macros-0.4.1
                                    rust-jobserver-0.1.34
                                    rust-js-sys-0.3.98
                                    rust-jsonb-0.5.6
                                    rust-jsonwebtoken-10.4.0
                                    rust-kanaria-0.2.0
                                    rust-konst-0.4.3
                                    rust-konst-proc-macros-0.4.1
                                    rust-lance-7.0.0
                                    rust-lance-arrow-7.0.0
                                    rust-lance-bitpacking-7.0.0
                                    rust-lance-core-7.0.0
                                    rust-lance-datafusion-7.0.0
                                    rust-lance-datagen-7.0.0
                                    rust-lance-encoding-7.0.0
                                    rust-lance-file-7.0.0
                                    rust-lance-index-7.0.0
                                    rust-lance-io-7.0.0
                                    rust-lance-linalg-7.0.0
                                    rust-lance-namespace-7.0.0
                                    rust-lance-namespace-impls-7.0.0
                                    rust-lance-namespace-reqwest-client-0.7.7
                                    rust-lance-table-7.0.0
                                    rust-lance-testing-7.0.0
                                    rust-lance-tokenizer-7.0.0
                                    rust-lazy-static-1.5.0
                                    rust-leb128fmt-0.1.0
                                    rust-lexical-core-1.0.6
                                    rust-lexical-parse-float-1.0.6
                                    rust-lexical-parse-integer-1.0.6
                                    rust-lexical-util-1.0.7
                                    rust-lexical-write-float-1.0.6
                                    rust-lexical-write-integer-1.0.6
                                    rust-libc-0.2.186
                                    rust-libloading-0.9.0
                                    rust-libm-0.2.16
                                    rust-libredox-0.1.16
                                    rust-lindera-3.0.7
                                    rust-lindera-dictionary-3.0.7
                                    rust-linux-raw-sys-0.12.1
                                    rust-litemap-0.8.2
                                    rust-litrs-1.0.0
                                    rust-lock-api-0.4.14
                                    rust-log-0.4.29
                                    rust-loom-0.7.2
                                    rust-lru-0.16.4
                                    rust-lru-slab-0.1.2
                                    rust-lz4-1.28.1
                                    rust-lz4-sys-1.11.1+lz4-1.10.0
                                    rust-lz4-flex-0.13.1
                                    rust-lzma-sys-0.1.20
                                    rust-macro-rules-attribute-0.2.2
                                    rust-macro-rules-attribute-proc-macro-0.2.2
                                    rust-matchers-0.2.0
                                    rust-matchit-0.7.3
                                    rust-matrixmultiply-0.3.10
                                    rust-md-5-0.10.6
                                    rust-md-5-0.11.0
                                    rust-mea-0.6.3
                                    rust-memchr-2.8.0
                                    rust-memmap2-0.7.1
                                    rust-memmap2-0.9.10
                                    rust-mime-0.3.17
                                    rust-mime-guess-2.0.5
                                    rust-minimal-lexical-0.2.1
                                    rust-miniz-oxide-0.8.9
                                    rust-mio-1.2.0
                                    rust-moka-0.12.15
                                    rust-monostate-0.1.18
                                    rust-monostate-impl-0.1.18
                                    rust-more-asserts-0.3.1
                                    rust-multimap-0.10.1
                                    rust-multiversion-0.7.4
                                    rust-multiversion-macros-0.7.4
                                    rust-munge-0.4.7
                                    rust-munge-macro-0.4.7
                                    rust-napi-3.9.0
                                    rust-napi-build-2.3.2
                                    rust-napi-derive-3.5.6
                                    rust-napi-derive-backend-5.0.4
                                    rust-napi-sys-3.2.1
                                    rust-ndarray-0.16.1
                                    rust-nohash-hasher-0.2.0
                                    rust-nom-7.1.3
                                    rust-nom-8.0.0
                                    rust-now-0.1.3
                                    rust-ntapi-0.4.3
                                    rust-nu-ansi-term-0.50.3
                                    rust-num-bigint-0.4.6
                                    rust-num-bigint-dig-0.8.6
                                    rust-num-complex-0.4.6
                                    rust-num-conv-0.2.1
                                    rust-num-integer-0.1.46
                                    rust-num-iter-0.1.45
                                    rust-num-traits-0.2.19
                                    rust-num-cpus-1.17.0
                                    rust-number-prefix-0.4.0
                                    rust-objc2-core-foundation-0.3.2
                                    rust-objc2-io-kit-0.3.2
                                    rust-objc2-system-configuration-0.3.2
                                    rust-object-0.37.3
                                    rust-object-store-0.13.2
                                    rust-object-store-opendal-0.56.0
                                    rust-once-cell-1.21.4
                                    rust-once-cell-polyfill-1.70.2
                                    rust-oneshot-0.1.13
                                    rust-onig-6.5.3
                                    rust-onig-sys-69.9.3
                                    rust-opendal-0.56.0
                                    rust-opendal-core-0.56.0
                                    rust-opendal-layer-concurrent-limit-0.56.0
                                    rust-opendal-layer-logging-0.56.0
                                    rust-opendal-layer-retry-0.56.0
                                    rust-opendal-layer-timeout-0.56.0
                                    rust-opendal-service-azblob-0.56.0
                                    rust-opendal-service-azdls-0.56.0
                                    rust-opendal-service-azure-common-0.56.0
                                    rust-opendal-service-gcs-0.56.0
                                    rust-opendal-service-hf-0.56.0
                                    rust-opendal-service-oss-0.56.0
                                    rust-opendal-service-s3-0.56.0
                                    rust-openssl-probe-0.2.1
                                    rust-option-ext-0.2.0
                                    rust-ordered-float-5.3.0
                                    rust-ordered-multimap-0.7.3
                                    rust-os-str-bytes-6.6.1
                                    rust-outref-0.5.2
                                    rust-p256-0.11.1
                                    rust-parking-2.2.1
                                    rust-parking-lot-0.12.5
                                    rust-parking-lot-core-0.9.12
                                    rust-parquet-format-safe-0.2.4
                                    rust-parse-zoneinfo-0.3.1
                                    rust-paste-1.0.15
                                    rust-path-abs-0.5.1
                                    rust-pbkdf2-0.12.2
                                    rust-pem-3.0.6
                                    rust-pem-rfc7468-0.7.0
                                    rust-percent-encoding-2.3.2
                                    rust-permutation-0.4.1
                                    rust-petgraph-0.8.3
                                    rust-phf-0.11.3
                                    rust-phf-0.12.1
                                    rust-phf-0.13.1
                                    rust-phf-codegen-0.11.3
                                    rust-phf-codegen-0.13.1
                                    rust-phf-generator-0.11.3
                                    rust-phf-generator-0.13.1
                                    rust-phf-shared-0.11.3
                                    rust-phf-shared-0.12.1
                                    rust-phf-shared-0.13.1
                                    rust-pin-project-1.1.13
                                    rust-pin-project-internal-1.1.13
                                    rust-pin-project-lite-0.2.17
                                    rust-pin-utils-0.1.0
                                    rust-pkcs1-0.7.5
                                    rust-pkcs5-0.7.1
                                    rust-pkcs8-0.9.0
                                    rust-pkcs8-0.10.2
                                    rust-pkg-config-0.3.33
                                    rust-planus-0.3.1
                                    rust-polars-0.39.2
                                    rust-polars-arrow-0.39.2
                                    rust-polars-arrow-format-0.1.0
                                    rust-polars-compute-0.39.2
                                    rust-polars-core-0.39.2
                                    rust-polars-error-0.39.2
                                    rust-polars-io-0.39.2
                                    rust-polars-lazy-0.39.2
                                    rust-polars-ops-0.39.2
                                    rust-polars-parquet-0.39.2
                                    rust-polars-pipe-0.39.2
                                    rust-polars-plan-0.39.2
                                    rust-polars-row-0.39.2
                                    rust-polars-sql-0.39.2
                                    rust-polars-time-0.39.2
                                    rust-polars-utils-0.39.2
                                    rust-portable-atomic-1.13.1
                                    rust-portable-atomic-util-0.2.7
                                    rust-potential-utf-0.1.5
                                    rust-powerfmt-0.2.0
                                    rust-ppv-lite86-0.2.21
                                    rust-prettyplease-0.2.37
                                    rust-proc-macro-crate-3.5.0
                                    rust-proc-macro2-1.0.106
                                    rust-prost-0.14.3
                                    rust-prost-build-0.14.3
                                    rust-prost-derive-0.14.3
                                    rust-prost-types-0.14.3
                                    rust-psm-0.1.31
                                    rust-ptr-meta-0.3.1
                                    rust-ptr-meta-derive-0.3.1
                                    rust-pulp-0.22.2
                                    rust-pulp-wasm-simd-flag-0.1.0
                                    rust-pyo3-0.28.3
                                    rust-pyo3-async-runtimes-0.28.0
                                    rust-pyo3-async-runtimes-macros-0.28.0
                                    rust-pyo3-build-config-0.28.3
                                    rust-pyo3-ffi-0.28.3
                                    rust-pyo3-macros-0.28.3
                                    rust-pyo3-macros-backend-0.28.3
                                    rust-quick-xml-0.38.4
                                    rust-quick-xml-0.39.4
                                    rust-quinn-0.11.9
                                    rust-quinn-proto-0.11.14
                                    rust-quinn-udp-0.5.14
                                    rust-quote-1.0.45
                                    rust-r-efi-5.3.0
                                    rust-r-efi-6.0.0
                                    rust-radium-0.7.0
                                    rust-rancor-0.1.1
                                    rust-rand-0.8.6
                                    rust-rand-0.9.4
                                    rust-rand-0.10.1
                                    rust-rand-chacha-0.3.1
                                    rust-rand-chacha-0.9.0
                                    rust-rand-core-0.6.4
                                    rust-rand-core-0.9.5
                                    rust-rand-core-0.10.1
                                    rust-rand-distr-0.4.3
                                    rust-rand-distr-0.5.1
                                    rust-rand-xoshiro-0.7.0
                                    rust-random-word-0.4.3
                                    rust-random-word-0.5.2
                                    rust-rangemap-1.7.1
                                    rust-raw-cpuid-11.6.0
                                    rust-rawpointer-0.2.1
                                    rust-rayon-1.12.0
                                    rust-rayon-cond-0.3.0
                                    rust-rayon-core-1.13.0
                                    rust-reborrow-0.5.5
                                    rust-recursive-0.1.1
                                    rust-recursive-proc-macro-impl-0.1.1
                                    rust-redb-3.1.3
                                    rust-redox-syscall-0.5.18
                                    rust-redox-users-0.5.2
                                    rust-ref-cast-1.0.25
                                    rust-ref-cast-impl-1.0.25
                                    rust-regex-1.12.3
                                    rust-regex-automata-0.4.14
                                    rust-regex-lite-0.1.9
                                    rust-regex-syntax-0.8.10
                                    rust-relative-path-1.9.3
                                    rust-rend-0.5.3
                                    rust-reqsign-aliyun-oss-3.0.0
                                    rust-reqsign-aws-v4-3.0.0
                                    rust-reqsign-azure-storage-3.0.0
                                    rust-reqsign-core-3.0.0
                                    rust-reqsign-file-read-tokio-3.0.0
                                    rust-reqsign-google-3.0.0
                                    rust-reqwest-0.12.28
                                    rust-reqwest-0.13.3
                                    rust-reqwest-eventsource-0.6.0
                                    rust-reqwest-middleware-0.5.1
                                    rust-rfc6979-0.3.1
                                    rust-ring-0.17.14
                                    rust-rkyv-0.8.16
                                    rust-rkyv-derive-0.8.16
                                    rust-roaring-0.11.4
                                    rust-rsa-0.9.10
                                    rust-rstest-0.23.0
                                    rust-rstest-macros-0.23.0
                                    rust-rust-ini-0.21.3
                                    rust-rust-stemmers-1.2.0
                                    rust-rustc-hash-2.1.2
                                    rust-rustc-version-0.4.1
                                    rust-rustix-1.1.4
                                    rust-rustls-0.21.12
                                    rust-rustls-0.23.40
                                    rust-rustls-native-certs-0.8.3
                                    rust-rustls-pki-types-1.14.1
                                    rust-rustls-platform-verifier-0.7.0
                                    rust-rustls-platform-verifier-android-0.1.1
                                    rust-rustls-webpki-0.101.7
                                    rust-rustls-webpki-0.103.13
                                    rust-rustversion-1.0.22
                                    rust-ryu-1.0.23
                                    rust-safe-transmute-0.11.3
                                    rust-safetensors-0.7.0
                                    rust-salsa20-0.10.2
                                    rust-same-file-1.0.6
                                    rust-scc-2.4.0
                                    rust-schannel-0.1.29
                                    rust-schemars-0.9.0
                                    rust-schemars-1.2.1
                                    rust-scoped-tls-1.0.1
                                    rust-scopeguard-1.2.0
                                    rust-scrypt-0.11.0
                                    rust-sct-0.7.1
                                    rust-sdd-3.0.10
                                    rust-sec1-0.3.0
                                    rust-secrecy-0.8.0
                                    rust-security-framework-3.7.0
                                    rust-security-framework-sys-2.17.0
                                    rust-semver-1.0.28
                                    rust-seq-macro-0.3.6
                                    rust-serde-1.0.228
                                    rust-serde-core-1.0.228
                                    rust-serde-derive-1.0.228
                                    rust-serde-json-1.0.150
                                    rust-serde-path-to-error-0.1.20
                                    rust-serde-plain-1.0.2
                                    rust-serde-repr-0.1.20
                                    rust-serde-urlencoded-0.7.1
                                    rust-serde-with-3.20.0
                                    rust-serde-with-macros-3.20.0
                                    rust-serde-yaml-ng-0.10.0
                                    rust-serial-test-3.4.0
                                    rust-serial-test-derive-3.4.0
                                    rust-sha1-0.10.6
                                    rust-sha1-0.11.0
                                    rust-sha2-0.10.9
                                    rust-sha2-0.11.0
                                    rust-sha2-asm-0.6.4
                                    rust-sharded-slab-0.1.7
                                    rust-shellexpand-3.1.2
                                    rust-shlex-1.3.0
                                    rust-signal-hook-registry-1.4.8
                                    rust-signature-1.6.4
                                    rust-signature-2.2.0
                                    rust-simd-adler32-0.3.9
                                    rust-simd-cesu8-1.1.1
                                    rust-simdutf8-0.1.5
                                    rust-simple-asn1-0.6.4
                                    rust-siphasher-1.0.3
                                    rust-slab-0.4.12
                                    rust-smallvec-1.15.1
                                    rust-smartstring-1.0.1
                                    rust-snafu-0.8.9
                                    rust-snafu-0.9.0
                                    rust-snafu-derive-0.8.9
                                    rust-snafu-derive-0.9.0
                                    rust-socket2-0.5.10
                                    rust-socket2-0.6.3
                                    rust-socks-0.3.4
                                    rust-spin-0.9.8
                                    rust-spin-0.10.0
                                    rust-spki-0.6.0
                                    rust-spki-0.7.3
                                    rust-spm-precompiled-0.1.4
                                    rust-sqlparser-0.39.0
                                    rust-sqlparser-0.61.0
                                    rust-sqlparser-derive-0.5.0
                                    rust-stable-deref-trait-1.2.1
                                    rust-stacker-0.1.24
                                    rust-static-assertions-1.1.0
                                    rust-statrs-0.18.0
                                    rust-std-prelude-0.2.12
                                    rust-stfu8-0.2.7
                                    rust-streaming-decompression-0.1.2
                                    rust-streaming-iterator-0.1.9
                                    rust-strength-reduce-0.2.4
                                    rust-strsim-0.11.1
                                    rust-strum-0.26.3
                                    rust-strum-0.28.0
                                    rust-strum-macros-0.25.3
                                    rust-strum-macros-0.26.4
                                    rust-strum-macros-0.28.0
                                    rust-subtle-2.6.1
                                    rust-symlink-0.1.0
                                    rust-syn-1.0.109
                                    rust-syn-2.0.117
                                    rust-sync-wrapper-1.0.2
                                    rust-synstructure-0.13.2
                                    rust-sysctl-0.6.0
                                    rust-sysinfo-0.30.13
                                    rust-sysinfo-0.38.4
                                    rust-system-configuration-0.7.0
                                    rust-system-configuration-sys-0.6.0
                                    rust-tagptr-0.2.0
                                    rust-tap-1.0.1
                                    rust-target-features-0.1.6
                                    rust-target-lexicon-0.13.5
                                    rust-tempfile-3.27.0
                                    rust-test-log-0.2.20
                                    rust-test-log-core-0.2.20
                                    rust-test-log-macros-0.2.20
                                    rust-thiserror-1.0.69
                                    rust-thiserror-2.0.18
                                    rust-thiserror-impl-1.0.69
                                    rust-thiserror-impl-2.0.18
                                    rust-thread-tree-0.3.3
                                    rust-thread-local-1.1.9
                                    rust-time-0.3.47
                                    rust-time-core-0.1.8
                                    rust-time-macros-0.2.27
                                    rust-tiny-keccak-2.0.2
                                    rust-tinystr-0.8.3
                                    rust-tinyvec-1.11.0
                                    rust-tinyvec-macros-0.1.1
                                    rust-tokenizers-0.19.1
                                    rust-tokio-1.52.3
                                    rust-tokio-macros-2.7.0
                                    rust-tokio-retry-0.3.1
                                    rust-tokio-rustls-0.24.1
                                    rust-tokio-rustls-0.26.4
                                    rust-tokio-stream-0.1.18
                                    rust-tokio-util-0.7.18
                                    rust-toml-datetime-1.1.1+spec-1.1.0
                                    rust-toml-edit-0.25.11+spec-1.1.0
                                    rust-toml-parser-1.1.2+spec-1.1.0
                                    rust-tower-0.5.3
                                    rust-tower-http-0.5.2
                                    rust-tower-http-0.6.10
                                    rust-tower-layer-0.3.3
                                    rust-tower-service-0.3.3
                                    rust-tracing-0.1.44
                                    rust-tracing-appender-0.2.5
                                    rust-tracing-attributes-0.1.31
                                    rust-tracing-core-0.1.36
                                    rust-tracing-log-0.2.0
                                    rust-tracing-serde-0.2.0
                                    rust-tracing-subscriber-0.3.23
                                    rust-try-lock-0.2.5
                                    rust-twox-hash-2.1.2
                                    rust-typed-path-0.12.3
                                    rust-typenum-1.20.0
                                    rust-typewit-1.15.2
                                    rust-unicase-2.9.0
                                    rust-unicode-blocks-0.1.9
                                    rust-unicode-ident-1.0.24
                                    rust-unicode-normalization-0.1.25
                                    rust-unicode-normalization-alignments-0.1.12
                                    rust-unicode-reverse-1.0.9
                                    rust-unicode-segmentation-1.13.2
                                    rust-unicode-width-0.2.2
                                    rust-unicode-xid-0.2.6
                                    rust-unicode-categories-0.1.1
                                    rust-unsafe-libyaml-0.2.11
                                    rust-untrusted-0.7.1
                                    rust-untrusted-0.9.0
                                    rust-ureq-2.12.1
                                    rust-url-2.5.8
                                    rust-urlencoding-2.1.3
                                    rust-utf8-ranges-1.0.5
                                    rust-utf8-iter-1.0.4
                                    rust-utf8parse-0.2.2
                                    rust-uuid-1.23.1
                                    rust-valuable-0.1.1
                                    rust-version-check-0.9.5
                                    rust-vsimd-0.8.0
                                    rust-walkdir-2.5.0
                                    rust-want-0.3.1
                                    rust-wasi-0.11.1+wasi-snapshot-preview1
                                    rust-wasi-0.14.7+wasi-0.2.4
                                    rust-wasip2-1.0.3+wasi-0.2.9
                                    rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                                    rust-wasite-1.0.2
                                    rust-wasm-bindgen-0.2.121
                                    rust-wasm-bindgen-futures-0.4.71
                                    rust-wasm-bindgen-macro-0.2.121
                                    rust-wasm-bindgen-macro-support-0.2.121
                                    rust-wasm-bindgen-shared-0.2.121
                                    rust-wasm-encoder-0.244.0
                                    rust-wasm-metadata-0.244.0
                                    rust-wasm-streams-0.4.2
                                    rust-wasm-streams-0.5.0
                                    rust-wasmparser-0.244.0
                                    rust-web-sys-0.3.98
                                    rust-web-time-1.1.0
                                    rust-webpki-root-certs-1.0.7
                                    rust-webpki-roots-0.26.11
                                    rust-webpki-roots-1.0.7
                                    rust-whoami-2.1.2
                                    rust-winapi-0.3.9
                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                    rust-winapi-util-0.1.11
                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                    rust-windows-0.52.0
                                    rust-windows-0.62.2
                                    rust-windows-collections-0.3.2
                                    rust-windows-core-0.52.0
                                    rust-windows-core-0.62.2
                                    rust-windows-future-0.3.2
                                    rust-windows-implement-0.60.2
                                    rust-windows-interface-0.59.3
                                    rust-windows-link-0.2.1
                                    rust-windows-numerics-0.3.1
                                    rust-windows-registry-0.6.1
                                    rust-windows-result-0.4.1
                                    rust-windows-strings-0.5.1
                                    rust-windows-sys-0.52.0
                                    rust-windows-sys-0.59.0
                                    rust-windows-sys-0.60.2
                                    rust-windows-sys-0.61.2
                                    rust-windows-targets-0.52.6
                                    rust-windows-targets-0.53.5
                                    rust-windows-threading-0.2.1
                                    rust-windows-aarch64-gnullvm-0.52.6
                                    rust-windows-aarch64-gnullvm-0.53.1
                                    rust-windows-aarch64-msvc-0.52.6
                                    rust-windows-aarch64-msvc-0.53.1
                                    rust-windows-i686-gnu-0.52.6
                                    rust-windows-i686-gnu-0.53.1
                                    rust-windows-i686-gnullvm-0.52.6
                                    rust-windows-i686-gnullvm-0.53.1
                                    rust-windows-i686-msvc-0.52.6
                                    rust-windows-i686-msvc-0.53.1
                                    rust-windows-x86-64-gnu-0.52.6
                                    rust-windows-x86-64-gnu-0.53.1
                                    rust-windows-x86-64-gnullvm-0.52.6
                                    rust-windows-x86-64-gnullvm-0.53.1
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-windows-x86-64-msvc-0.53.1
                                    rust-winnow-1.0.3
                                    rust-wit-bindgen-0.51.0
                                    rust-wit-bindgen-0.57.1
                                    rust-wit-bindgen-core-0.51.0
                                    rust-wit-bindgen-rust-0.51.0
                                    rust-wit-bindgen-rust-macro-0.51.0
                                    rust-wit-component-0.244.0
                                    rust-wit-parser-0.244.0
                                    rust-writeable-0.6.3
                                    rust-wyz-0.5.1
                                    rust-xet-client-1.5.2
                                    rust-xet-core-structures-1.5.2
                                    rust-xet-data-1.5.2
                                    rust-xet-runtime-1.5.2
                                    rust-xmlparser-0.13.6
                                    rust-xxhash-rust-0.8.15
                                    rust-yoke-0.8.2
                                    rust-yoke-derive-0.8.2
                                    rust-zerocopy-0.8.48
                                    rust-zerocopy-derive-0.8.48
                                    rust-zerofrom-0.1.8
                                    rust-zerofrom-derive-0.1.7
                                    rust-zeroize-1.8.2
                                    rust-zeroize-derive-1.4.3
                                    rust-zerotrie-0.2.4
                                    rust-zerovec-0.11.6
                                    rust-zerovec-derive-0.11.3
                                    rust-zip-7.2.0
                                    rust-zmij-1.0.21
                                    rust-zstd-0.13.3
                                    rust-zstd-safe-7.2.4
                                    rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (bibtex-parser =>
                              (list rust-ahash-0.8.12
                                    rust-aho-corasick-1.1.3
                                    rust-anes-0.1.6
                                    rust-anstyle-1.0.10
                                    rust-autocfg-1.4.0
                                    rust-biblatex-0.11.0
                                    rust-bit-set-0.8.0
                                    rust-bit-vec-0.8.0
                                    rust-bitflags-2.9.1
                                    rust-bumpalo-3.17.0
                                    rust-bytecount-0.6.9
                                    rust-cast-0.3.0
                                    rust-cc-1.2.62
                                    rust-cfg-if-1.0.0
                                    rust-ciborium-0.2.2
                                    rust-ciborium-io-0.2.2
                                    rust-ciborium-ll-0.2.2
                                    rust-clap-4.5.39
                                    rust-clap-builder-4.5.39
                                    rust-clap-lex-0.7.4
                                    rust-console-0.15.11
                                    rust-criterion-0.5.1
                                    rust-criterion-plot-0.5.0
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.21
                                    rust-crunchy-0.2.3
                                    rust-diff-0.1.13
                                    rust-either-1.15.0
                                    rust-encode-unicode-1.0.0
                                    rust-errno-0.3.12
                                    rust-fastrand-2.3.0
                                    rust-find-msvc-tools-0.1.9
                                    rust-fnv-1.0.7
                                    rust-getrandom-0.2.16
                                    rust-getrandom-0.3.3
                                    rust-half-2.6.0
                                    rust-heck-0.5.0
                                    rust-hermit-abi-0.5.1
                                    rust-indoc-2.0.7
                                    rust-insta-1.43.1
                                    rust-is-terminal-0.4.16
                                    rust-itertools-0.10.5
                                    rust-itoa-1.0.15
                                    rust-js-sys-0.3.77
                                    rust-lazy-static-1.5.0
                                    rust-libc-0.2.172
                                    rust-libmimalloc-sys-0.1.47
                                    rust-linux-raw-sys-0.9.4
                                    rust-log-0.4.27
                                    rust-memchr-2.7.4
                                    rust-memoffset-0.9.1
                                    rust-mimalloc-0.1.50
                                    rust-minimal-lexical-0.2.1
                                    rust-nom-7.1.3
                                    rust-nom-8.0.0
                                    rust-nom-bibtex-0.6.0
                                    rust-nom-language-0.1.0
                                    rust-nom-tracable-0.9.1
                                    rust-nom-tracable-macros-0.9.1
                                    rust-nom-locate-4.2.0
                                    rust-nom-locate-5.0.0
                                    rust-num-traits-0.2.19
                                    rust-once-cell-1.21.3
                                    rust-oorandom-11.1.5
                                    rust-paste-1.0.15
                                    rust-phf-0.11.3
                                    rust-phf-generator-0.11.3
                                    rust-phf-macros-0.11.3
                                    rust-phf-shared-0.11.3
                                    rust-plotters-0.3.7
                                    rust-plotters-backend-0.3.7
                                    rust-plotters-svg-0.3.7
                                    rust-portable-atomic-1.13.1
                                    rust-ppv-lite86-0.2.21
                                    rust-pretty-assertions-1.4.1
                                    rust-proc-macro2-1.0.95
                                    rust-proptest-1.6.0
                                    rust-pyo3-0.27.2
                                    rust-pyo3-build-config-0.27.2
                                    rust-pyo3-ffi-0.27.2
                                    rust-pyo3-macros-0.27.2
                                    rust-pyo3-macros-backend-0.27.2
                                    rust-quick-error-1.2.3
                                    rust-quick-error-2.0.1
                                    rust-quote-1.0.40
                                    rust-r-efi-5.2.0
                                    rust-rand-0.8.5
                                    rust-rand-chacha-0.3.1
                                    rust-rand-core-0.6.4
                                    rust-rand-xorshift-0.3.0
                                    rust-rayon-1.10.0
                                    rust-rayon-core-1.12.1
                                    rust-regex-1.11.1
                                    rust-regex-automata-0.4.9
                                    rust-regex-syntax-0.8.5
                                    rust-roman-numerals-rs-3.1.0
                                    rust-rustix-1.0.7
                                    rust-rustversion-1.0.21
                                    rust-rusty-fork-0.3.0
                                    rust-ryu-1.0.20
                                    rust-same-file-1.0.6
                                    rust-serde-1.0.219
                                    rust-serde-bibtex-0.7.1
                                    rust-serde-derive-1.0.219
                                    rust-serde-json-1.0.140
                                    rust-shlex-1.3.0
                                    rust-similar-2.7.0
                                    rust-siphasher-1.0.1
                                    rust-strum-0.27.2
                                    rust-strum-macros-0.27.2
                                    rust-syn-1.0.109
                                    rust-syn-2.0.101
                                    rust-target-lexicon-0.13.5
                                    rust-tempfile-3.20.0
                                    rust-thiserror-1.0.69
                                    rust-thiserror-impl-1.0.69
                                    rust-tinytemplate-1.2.1
                                    rust-tinyvec-1.9.0
                                    rust-tinyvec-macros-0.1.1
                                    rust-unarray-0.1.4
                                    rust-unicase-2.8.1
                                    rust-unicode-ident-1.0.18
                                    rust-unicode-normalization-0.1.24
                                    rust-unindent-0.2.4
                                    rust-unscanny-0.1.0
                                    rust-version-check-0.9.5
                                    rust-wait-timeout-0.2.1
                                    rust-walkdir-2.5.0
                                    rust-wasi-0.11.0+wasi-snapshot-preview1
                                    rust-wasi-0.14.2+wasi-0.2.4
                                    rust-wasm-bindgen-0.2.100
                                    rust-wasm-bindgen-backend-0.2.100
                                    rust-wasm-bindgen-macro-0.2.100
                                    rust-wasm-bindgen-macro-support-0.2.100
                                    rust-wasm-bindgen-shared-0.2.100
                                    rust-web-sys-0.3.77
                                    rust-winapi-util-0.1.9
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
                                    rust-winnow-0.5.40
                                    rust-wit-bindgen-rt-0.39.0
                                    rust-yansi-1.0.1
                                    rust-zerocopy-0.8.25
                                    rust-zerocopy-derive-0.8.25))
                     (citerra =>
                              (list rust-ahash-0.8.12
                                    rust-aho-corasick-1.1.3
                                    rust-anes-0.1.6
                                    rust-anstyle-1.0.10
                                    rust-autocfg-1.4.0
                                    rust-biblatex-0.11.0
                                    rust-bit-set-0.8.0
                                    rust-bit-vec-0.8.0
                                    rust-bitflags-2.9.1
                                    rust-bumpalo-3.17.0
                                    rust-bytecount-0.6.9
                                    rust-cast-0.3.0
                                    rust-cc-1.2.62
                                    rust-cfg-if-1.0.0
                                    rust-ciborium-0.2.2
                                    rust-ciborium-io-0.2.2
                                    rust-ciborium-ll-0.2.2
                                    rust-clap-4.5.39
                                    rust-clap-builder-4.5.39
                                    rust-clap-lex-0.7.4
                                    rust-console-0.15.11
                                    rust-criterion-0.5.1
                                    rust-criterion-plot-0.5.0
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.21
                                    rust-crunchy-0.2.3
                                    rust-diff-0.1.13
                                    rust-either-1.15.0
                                    rust-encode-unicode-1.0.0
                                    rust-errno-0.3.12
                                    rust-fastrand-2.3.0
                                    rust-find-msvc-tools-0.1.9
                                    rust-fnv-1.0.7
                                    rust-getrandom-0.2.16
                                    rust-getrandom-0.3.3
                                    rust-half-2.6.0
                                    rust-heck-0.5.0
                                    rust-hermit-abi-0.5.1
                                    rust-indoc-2.0.7
                                    rust-insta-1.43.1
                                    rust-is-terminal-0.4.16
                                    rust-itertools-0.10.5
                                    rust-itoa-1.0.15
                                    rust-js-sys-0.3.77
                                    rust-lazy-static-1.5.0
                                    rust-libc-0.2.172
                                    rust-libmimalloc-sys-0.1.47
                                    rust-linux-raw-sys-0.9.4
                                    rust-log-0.4.27
                                    rust-memchr-2.7.4
                                    rust-memoffset-0.9.1
                                    rust-mimalloc-0.1.50
                                    rust-minimal-lexical-0.2.1
                                    rust-nom-7.1.3
                                    rust-nom-8.0.0
                                    rust-nom-bibtex-0.6.0
                                    rust-nom-language-0.1.0
                                    rust-nom-tracable-0.9.1
                                    rust-nom-tracable-macros-0.9.1
                                    rust-nom-locate-4.2.0
                                    rust-nom-locate-5.0.0
                                    rust-num-traits-0.2.19
                                    rust-once-cell-1.21.3
                                    rust-oorandom-11.1.5
                                    rust-paste-1.0.15
                                    rust-phf-0.11.3
                                    rust-phf-generator-0.11.3
                                    rust-phf-macros-0.11.3
                                    rust-phf-shared-0.11.3
                                    rust-plotters-0.3.7
                                    rust-plotters-backend-0.3.7
                                    rust-plotters-svg-0.3.7
                                    rust-portable-atomic-1.13.1
                                    rust-ppv-lite86-0.2.21
                                    rust-pretty-assertions-1.4.1
                                    rust-proc-macro2-1.0.95
                                    rust-proptest-1.6.0
                                    rust-pyo3-0.27.2
                                    rust-pyo3-build-config-0.27.2
                                    rust-pyo3-ffi-0.27.2
                                    rust-pyo3-macros-0.27.2
                                    rust-pyo3-macros-backend-0.27.2
                                    rust-quick-error-1.2.3
                                    rust-quick-error-2.0.1
                                    rust-quote-1.0.40
                                    rust-r-efi-5.2.0
                                    rust-rand-0.8.5
                                    rust-rand-chacha-0.3.1
                                    rust-rand-core-0.6.4
                                    rust-rand-xorshift-0.3.0
                                    rust-rayon-1.10.0
                                    rust-rayon-core-1.12.1
                                    rust-regex-1.11.1
                                    rust-regex-automata-0.4.9
                                    rust-regex-syntax-0.8.5
                                    rust-roman-numerals-rs-3.1.0
                                    rust-rustix-1.0.7
                                    rust-rustversion-1.0.21
                                    rust-rusty-fork-0.3.0
                                    rust-ryu-1.0.20
                                    rust-same-file-1.0.6
                                    rust-serde-1.0.219
                                    rust-serde-bibtex-0.7.1
                                    rust-serde-derive-1.0.219
                                    rust-serde-json-1.0.140
                                    rust-shlex-1.3.0
                                    rust-similar-2.7.0
                                    rust-siphasher-1.0.1
                                    rust-strum-0.27.2
                                    rust-strum-macros-0.27.2
                                    rust-syn-1.0.109
                                    rust-syn-2.0.101
                                    rust-target-lexicon-0.13.5
                                    rust-tempfile-3.20.0
                                    rust-thiserror-1.0.69
                                    rust-thiserror-impl-1.0.69
                                    rust-tinytemplate-1.2.1
                                    rust-tinyvec-1.9.0
                                    rust-tinyvec-macros-0.1.1
                                    rust-unarray-0.1.4
                                    rust-unicase-2.8.1
                                    rust-unicode-ident-1.0.18
                                    rust-unicode-normalization-0.1.24
                                    rust-unindent-0.2.4
                                    rust-unscanny-0.1.0
                                    rust-version-check-0.9.5
                                    rust-wait-timeout-0.2.1
                                    rust-walkdir-2.5.0
                                    rust-wasi-0.11.0+wasi-snapshot-preview1
                                    rust-wasi-0.14.2+wasi-0.2.4
                                    rust-wasm-bindgen-0.2.100
                                    rust-wasm-bindgen-backend-0.2.100
                                    rust-wasm-bindgen-macro-0.2.100
                                    rust-wasm-bindgen-macro-support-0.2.100
                                    rust-wasm-bindgen-shared-0.2.100
                                    rust-web-sys-0.3.77
                                    rust-winapi-util-0.1.9
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
                                    rust-winnow-0.5.40
                                    rust-wit-bindgen-rt-0.39.0
                                    rust-yansi-1.0.1
                                    rust-zerocopy-0.8.25
                                    rust-zerocopy-derive-0.8.25))
                     (pathrs =>
                             (list rust-bitflags-2.10.0
                                   rust-bitflags-1.3.2
                                   rust-anyhow-1.0.100
                                   rust-atty-0.2.14
                                   rust-bytemuck-1.24.0
                                   rust-bytemuck-derive-1.10.2
                                   rust-cfg-if-1.0.4
                                   rust-clap-3.2.25
                                   rust-clap-lex-0.2.4
                                   rust-autocfg-1.5.0
                                   rust-errno-0.3.14
                                   rust-either-1.15.0
                                   rust-fastrand-2.3.0
                                   rust-getrandom-0.3.4
                                   rust-hashbrown-0.12.3
                                   rust-hermit-abi-0.1.19
                                   rust-indexmap-1.9.3
                                   rust-indoc-2.0.7
                                   rust-itertools-0.14.0
                                   rust-libc-0.2.177
                                   rust-linux-raw-sys-0.11.0
                                   rust-memchr-2.7.6
                                   rust-once-cell-1.20.3
                                   rust-open-enum-0.3.0
                                   rust-open-enum-derive-0.3.0
                                   rust-os-str-bytes-6.6.1
                                   rust-paste-1.0.15
                                   rust-path-clean-1.0.1
                                   rust-ppv-lite86-0.2.21
                                   rust-pretty-assertions-1.4.1
                                   rust-proc-macro2-1.0.101
                                   rust-quote-1.0.41
                                   rust-r-efi-5.3.0
                                   rust-rand-0.9.2
                                   rust-rand-chacha-0.9.0
                                   rust-rand-core-0.9.3
                                   rust-rustix-1.1.2
                                   rust-rustversion-1.0.22
                                   rust-static-assertions-1.1.0
                                   rust-syn-1.0.109
                                   rust-syn-2.0.106
                                   rust-tempfile-3.23.0
                                   rust-termcolor-1.4.1
                                   rust-textwrap-0.16.1
                                   rust-thiserror-2.0.17
                                   rust-thiserror-impl-2.0.17
                                   rust-unicode-ident-1.0.18
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-util-0.1.11
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-link-0.2.1
                                   rust-windows-sys-0.60.2
                                   rust-windows-sys-0.61.2
                                   rust-windows-targets-0.53.5
                                   rust-windows-aarch64-gnullvm-0.53.1
                                   rust-windows-aarch64-msvc-0.53.1
                                   rust-windows-i686-gnu-0.53.1
                                   rust-windows-i686-gnullvm-0.53.1
                                   rust-windows-i686-msvc-0.53.1
                                   rust-windows-x86-64-gnu-0.53.1
                                   rust-windows-x86-64-gnullvm-0.53.1
                                   rust-windows-x86-64-msvc-0.53.1
                                   rust-wasip2-1.0.1+wasi-0.2.4
                                   rust-wit-bindgen-0.46.0
                                   rust-yansi-1.0.1
                                   rust-diff-0.1.13
                                   rust-zerocopy-0.8.27
                                   rust-zerocopy-derive-0.8.27
                                   rust-strsim-0.10.0))
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
                                            rust-pyo3-0.26.0
                                            rust-pyo3-build-config-0.26.0
                                            rust-pyo3-ffi-0.26.0
                                            rust-pyo3-macros-0.26.0
                                            rust-pyo3-macros-backend-0.26.0
                                            rust-quote-1.0.40
                                            rust-regex-1.11.1
                                            rust-regex-automata-0.4.9
                                            rust-regex-syntax-0.8.5
                                            rust-rustc-hash-2.1.1
                                            rust-serde-1.0.219
                                            rust-serde-derive-1.0.219
                                            rust-syn-2.0.106
                                            rust-target-lexicon-0.13.3
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
                                     rust-zerovec-derive-0.11.1))
                     (openai-harmony =>
                                     (list rust-addr2line-0.24.2
                                      rust-adler2-2.0.0
                                      rust-aho-corasick-1.1.3
                                      rust-aligned-vec-0.5.0
                                      rust-android-tzdata-0.1.1
                                      rust-android-system-properties-0.1.5
                                      rust-anstream-0.6.18
                                      rust-anstyle-1.0.10
                                      rust-anstyle-parse-0.2.6
                                      rust-anstyle-query-1.1.2
                                      rust-anstyle-wincon-3.0.7
                                      rust-anyhow-1.0.98
                                      rust-arbitrary-1.4.1
                                      rust-arg-enum-proc-macro-0.3.4
                                      rust-arrayvec-0.7.6
                                      rust-autocfg-1.4.0
                                      rust-av1-grain-0.2.3
                                      rust-avif-serialize-0.8.3
                                      rust-backtrace-0.3.75
                                      rust-base64-0.22.1
                                      rust-bit-set-0.5.3
                                      rust-bit-vec-0.6.3
                                      rust-bit-field-0.10.2
                                      rust-bitflags-1.3.2
                                      rust-bitflags-2.9.0
                                      rust-bitstream-io-2.6.0
                                      rust-block-buffer-0.10.4
                                      rust-bstr-1.12.0
                                      rust-built-0.7.7
                                      rust-bumpalo-3.17.0
                                      rust-bytemuck-1.22.0
                                      rust-byteorder-lite-0.1.0
                                      rust-bytes-1.10.1
                                      rust-cc-1.2.18
                                      rust-cfg-expr-0.15.8
                                      rust-cfg-if-1.0.0
                                      rust-cfg-aliases-0.2.1
                                      rust-chrono-0.4.40
                                      rust-clap-4.5.38
                                      rust-clap-builder-4.5.38
                                      rust-clap-derive-4.5.32
                                      rust-clap-lex-0.7.4
                                      rust-color-quant-1.1.0
                                      rust-colorchoice-1.0.3
                                      rust-core-foundation-sys-0.8.7
                                      rust-cpufeatures-0.2.17
                                      rust-crc32fast-1.4.2
                                      rust-crossbeam-deque-0.8.6
                                      rust-crossbeam-epoch-0.9.18
                                      rust-crossbeam-utils-0.8.21
                                      rust-crunchy-0.2.3
                                      rust-crypto-common-0.1.6
                                      rust-darling-0.20.11
                                      rust-darling-core-0.20.11
                                      rust-darling-macro-0.20.11
                                      rust-deranged-0.4.0
                                      rust-diff-0.1.13
                                      rust-digest-0.10.7
                                      rust-displaydoc-0.2.5
                                      rust-either-1.15.0
                                      rust-equivalent-1.0.2
                                      rust-exr-1.73.0
                                      rust-fancy-regex-0.13.0
                                      rust-fdeflate-0.3.7
                                      rust-flate2-1.1.1
                                      rust-fnv-1.0.7
                                      rust-form-urlencoded-1.2.1
                                      rust-futures-0.3.31
                                      rust-futures-channel-0.3.31
                                      rust-futures-core-0.3.31
                                      rust-futures-executor-0.3.31
                                      rust-futures-io-0.3.31
                                      rust-futures-macro-0.3.31
                                      rust-futures-sink-0.3.31
                                      rust-futures-task-0.3.31
                                      rust-futures-util-0.3.31
                                      rust-generic-array-0.14.7
                                      rust-getrandom-0.2.15
                                      rust-getrandom-0.3.2
                                      rust-gif-0.13.1
                                      rust-gimli-0.31.1
                                      rust-half-2.5.0
                                      rust-hashbrown-0.12.3
                                      rust-hashbrown-0.15.2
                                      rust-heck-0.5.0
                                      rust-hex-0.4.3
                                      rust-http-1.3.1
                                      rust-http-body-1.0.1
                                      rust-http-body-util-0.1.3
                                      rust-httparse-1.10.1
                                      rust-hyper-1.6.0
                                      rust-hyper-rustls-0.27.5
                                      rust-hyper-util-0.1.11
                                      rust-iana-time-zone-0.1.63
                                      rust-iana-time-zone-haiku-0.1.2
                                      rust-icu-collections-2.0.0
                                      rust-icu-locale-core-2.0.0
                                      rust-icu-normalizer-2.0.0
                                      rust-icu-normalizer-data-2.0.0
                                      rust-icu-properties-2.0.0
                                      rust-icu-properties-data-2.0.0
                                      rust-icu-provider-2.0.0
                                      rust-ident-case-1.0.1
                                      rust-idna-1.0.3
                                      rust-idna-adapter-1.2.1
                                      rust-image-0.25.6
                                      rust-image-webp-0.2.1
                                      rust-imgref-1.11.0
                                      rust-indexmap-1.9.3
                                      rust-indexmap-2.9.0
                                      rust-indoc-2.0.6
                                      rust-interpolate-name-0.2.4
                                      rust-ipnet-2.11.0
                                      rust-is-terminal-polyfill-1.70.1
                                      rust-itertools-0.12.1
                                      rust-itoa-1.0.15
                                      rust-jobserver-0.1.33
                                      rust-jpeg-decoder-0.3.1
                                      rust-js-sys-0.3.77
                                      rust-lebe-0.5.2
                                      rust-libc-0.2.171
                                      rust-libfuzzer-sys-0.4.9
                                      rust-litemap-0.8.0
                                      rust-log-0.4.27
                                      rust-loop9-0.1.5
                                      rust-maybe-rayon-0.1.1
                                      rust-memchr-2.7.4
                                      rust-memoffset-0.9.1
                                      rust-mime-0.3.17
                                      rust-mime-guess-2.0.5
                                      rust-minimal-lexical-0.2.1
                                      rust-miniz-oxide-0.8.7
                                      rust-mio-1.0.3
                                      rust-new-debug-unreachable-1.0.6
                                      rust-nom-7.1.3
                                      rust-noop-proc-macro-0.3.0
                                      rust-num-bigint-0.4.6
                                      rust-num-conv-0.1.0
                                      rust-num-derive-0.4.2
                                      rust-num-integer-0.1.46
                                      rust-num-rational-0.4.2
                                      rust-num-traits-0.2.19
                                      rust-object-0.36.7
                                      rust-once-cell-1.21.3
                                      rust-paste-1.0.15
                                      rust-percent-encoding-2.3.1
                                      rust-pin-project-lite-0.2.16
                                      rust-pin-utils-0.1.0
                                      rust-pkg-config-0.3.32
                                      rust-png-0.17.16
                                      rust-portable-atomic-1.11.0
                                      rust-potential-utf-0.1.2
                                      rust-powerfmt-0.2.0
                                      rust-ppv-lite86-0.2.21
                                      rust-pretty-assertions-1.4.1
                                      rust-proc-macro2-1.0.94
                                      rust-profiling-1.0.16
                                      rust-profiling-procmacros-1.0.16
                                      rust-pyo3-0.25.0
                                      rust-pyo3-build-config-0.25.0
                                      rust-pyo3-ffi-0.25.0
                                      rust-pyo3-macros-0.25.0
                                      rust-pyo3-macros-backend-0.25.0
                                      rust-qoi-0.4.1
                                      rust-quick-error-2.0.1
                                      rust-quinn-0.11.6
                                      rust-quinn-proto-0.11.9
                                      rust-quinn-udp-0.5.10
                                      rust-quote-1.0.40
                                      rust-r-efi-5.2.0
                                      rust-rand-0.8.5
                                      rust-rand-chacha-0.3.1
                                      rust-rand-core-0.6.4
                                      rust-rav1e-0.7.1
                                      rust-ravif-0.11.11
                                      rust-rayon-1.10.0
                                      rust-rayon-core-1.12.1
                                      rust-regex-1.11.1
                                      rust-regex-automata-0.4.9
                                      rust-regex-syntax-0.8.5
                                      rust-reqwest-0.12.15
                                      rust-rgb-0.8.50
                                      rust-ring-0.17.14
                                      rust-rustc-demangle-0.1.24
                                      rust-rustc-hash-1.1.0
                                      rust-rustc-hash-2.1.1
                                      rust-rustls-0.23.27
                                      rust-rustls-pemfile-2.2.0
                                      rust-rustls-pki-types-1.12.0
                                      rust-rustls-webpki-0.103.3
                                      rust-rustversion-1.0.20
                                      rust-ryu-1.0.20
                                      rust-serde-1.0.219
                                      rust-serde-wasm-bindgen-0.6.5
                                      rust-serde-derive-1.0.219
                                      rust-serde-json-1.0.140
                                      rust-serde-spanned-0.6.8
                                      rust-serde-urlencoded-0.7.1
                                      rust-serde-with-3.12.0
                                      rust-serde-with-macros-3.12.0
                                      rust-sha1-0.10.6
                                      rust-sha2-0.10.9
                                      rust-shlex-1.3.0
                                      rust-simd-adler32-0.3.7
                                      rust-simd-helpers-0.1.0
                                      rust-slab-0.4.9
                                      rust-smallvec-1.15.0
                                      rust-socket2-0.5.9
                                      rust-stable-deref-trait-1.2.0
                                      rust-strsim-0.11.1
                                      rust-subtle-2.6.1
                                      rust-syn-2.0.100
                                      rust-sync-wrapper-1.0.2
                                      rust-synstructure-0.13.2
                                      rust-system-deps-6.2.2
                                      rust-target-lexicon-0.12.16
                                      rust-target-lexicon-0.13.2
                                      rust-thiserror-1.0.69
                                      rust-thiserror-2.0.12
                                      rust-thiserror-impl-1.0.69
                                      rust-thiserror-impl-2.0.12
                                      rust-tiff-0.9.1
                                      rust-time-0.3.41
                                      rust-time-core-0.1.4
                                      rust-time-macros-0.2.22
                                      rust-tinystr-0.8.1
                                      rust-tinyvec-1.8.1
                                      rust-tinyvec-macros-0.1.1
                                      rust-tokio-1.45.0
                                      rust-tokio-rustls-0.26.2
                                      rust-tokio-util-0.7.15
                                      rust-toml-0.8.20
                                      rust-toml-datetime-0.6.8
                                      rust-toml-edit-0.22.24
                                      rust-tower-0.5.2
                                      rust-tower-layer-0.3.3
                                      rust-tower-service-0.3.3
                                      rust-tracing-0.1.41
                                      rust-tracing-core-0.1.33
                                      rust-try-lock-0.2.5
                                      rust-typenum-1.18.0
                                      rust-unicase-2.8.1
                                      rust-unicode-ident-1.0.18
                                      rust-unindent-0.2.4
                                      rust-untrusted-0.9.0
                                      rust-url-2.5.4
                                      rust-utf8-iter-1.0.4
                                      rust-utf8parse-0.2.2
                                      rust-v-frame-0.3.8
                                      rust-version-compare-0.2.0
                                      rust-version-check-0.9.5
                                      rust-want-0.3.1
                                      rust-wasi-0.11.0+wasi-snapshot-preview1
                                      rust-wasi-0.14.2+wasi-0.2.4
                                      rust-wasm-bindgen-0.2.100
                                      rust-wasm-bindgen-backend-0.2.100
                                      rust-wasm-bindgen-futures-0.4.50
                                      rust-wasm-bindgen-macro-0.2.100
                                      rust-wasm-bindgen-macro-support-0.2.100
                                      rust-wasm-bindgen-shared-0.2.100
                                      rust-wasm-streams-0.4.2
                                      rust-web-sys-0.3.77
                                      rust-web-time-1.1.0
                                      rust-webpki-roots-0.26.8
                                      rust-weezl-0.1.8
                                      rust-windows-core-0.61.0
                                      rust-windows-implement-0.60.0
                                      rust-windows-interface-0.59.1
                                      rust-windows-link-0.1.1
                                      rust-windows-registry-0.4.0
                                      rust-windows-result-0.3.2
                                      rust-windows-strings-0.3.1
                                      rust-windows-strings-0.4.0
                                      rust-windows-sys-0.52.0
                                      rust-windows-sys-0.59.0
                                      rust-windows-targets-0.52.6
                                      rust-windows-targets-0.53.0
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
                                      rust-winnow-0.7.6
                                      rust-wit-bindgen-rt-0.39.0
                                      rust-writeable-0.6.1
                                      rust-yansi-1.0.1
                                      rust-yoke-0.8.0
                                      rust-yoke-derive-0.8.0
                                      rust-zerocopy-0.8.24
                                      rust-zerocopy-derive-0.8.24
                                      rust-zerofrom-0.1.6
                                      rust-zerofrom-derive-0.1.6
                                      rust-zeroize-1.8.1
                                      rust-zerotrie-0.2.2
                                      rust-zerovec-0.11.2
                                      rust-zerovec-derive-0.11.1
                                      rust-zune-core-0.4.12
                                      rust-zune-inflate-0.2.54
                                      rust-zune-jpeg-0.4.14)))
(define-cargo-inputs lookup-cargo-inputs
                     (refbox =>
                              (list rust-ahash-0.8.12
                                    rust-aho-corasick-1.1.4
                                    rust-anstream-1.0.0
                                    rust-anstyle-1.0.14
                                    rust-anstyle-parse-1.0.0
                                    rust-anstyle-query-1.1.5
                                    rust-anstyle-wincon-3.0.11
                                    rust-anyhow-1.0.102
                                    rust-bibtex-parser-0.4.0
                                    rust-bitflags-2.11.1
                                    rust-block-buffer-0.12.0
                                    rust-bstr-1.12.1
                                    rust-bumpalo-3.20.2
                                    rust-cc-1.2.62
                                    rust-cfg-if-1.0.4
                                    rust-clap-4.6.1
                                    rust-clap-builder-4.6.0
                                    rust-clap-derive-4.6.1
                                    rust-clap-lex-1.1.0
                                    rust-colorchoice-1.0.5
                                    rust-const-oid-0.10.2
                                    rust-cpufeatures-0.3.0
                                    rust-crypto-common-0.2.1
                                    rust-digest-0.11.3
                                    rust-fallible-iterator-0.3.0
                                    rust-fallible-streaming-iterator-0.1.9
                                    rust-find-msvc-tools-0.1.9
                                    rust-foldhash-0.2.0
                                    rust-getrandom-0.3.4
                                    rust-globset-0.4.18
                                    rust-hashbrown-0.16.1
                                    rust-hashlink-0.11.0
                                    rust-heck-0.5.0
                                    rust-hybrid-array-0.4.12
                                    rust-is-terminal-polyfill-1.70.2
                                    rust-itoa-1.0.18
                                    rust-js-sys-0.3.98
                                    rust-libc-0.2.186
                                    rust-libsqlite3-sys-0.37.0
                                    rust-log-0.4.29
                                    rust-memchr-2.8.0
                                    rust-once-cell-1.21.4
                                    rust-once-cell-polyfill-1.70.2
                                    rust-pkg-config-0.3.33
                                    rust-proc-macro2-1.0.106
                                    rust-quote-1.0.45
                                    rust-r-efi-5.3.0
                                    rust-regex-automata-0.4.14
                                    rust-regex-syntax-0.8.10
                                    rust-rsqlite-vfs-0.1.0
                                    rust-rusqlite-0.39.0
                                    rust-rustversion-1.0.22
                                    rust-serde-1.0.228
                                    rust-serde-core-1.0.228
                                    rust-serde-derive-1.0.228
                                    rust-serde-json-1.0.149
                                    rust-sha2-0.11.0
                                    rust-shlex-1.3.0
                                    rust-smallvec-1.15.1
                                    rust-sqlite-wasm-rs-0.5.3
                                    rust-strsim-0.11.1
                                    rust-syn-2.0.117
                                    rust-thiserror-1.0.69
                                    rust-thiserror-2.0.18
                                    rust-thiserror-impl-1.0.69
                                    rust-thiserror-impl-2.0.18
                                    rust-tinyvec-1.11.0
                                    rust-tinyvec-macros-0.1.1
                                    rust-typenum-1.20.0
                                    rust-unicode-ident-1.0.24
                                    rust-unicode-normalization-0.1.25
                                    rust-utf8parse-0.2.2
                                    rust-vcpkg-0.2.15
                                    rust-version-check-0.9.5
                                    rust-wasip2-1.0.3+wasi-0.2.9
                                    rust-wasm-bindgen-0.2.121
                                    rust-wasm-bindgen-macro-0.2.121
                                    rust-wasm-bindgen-macro-support-0.2.121
                                    rust-wasm-bindgen-shared-0.2.121
                                    rust-windows-link-0.2.1
                                    rust-windows-sys-0.61.2
                                    rust-winnow-0.5.40
                                    rust-wit-bindgen-0.57.1
                                    rust-zerocopy-0.8.48
                                    rust-zerocopy-derive-0.8.48
                                    rust-zmij-1.0.21))
                     (slipbox =>
                              (list rust-aho-corasick-1.1.4
                               rust-android-system-properties-0.1.5
                               rust-anstream-0.6.21
                               rust-anstyle-1.0.13
                               rust-anstyle-parse-0.2.7
                               rust-anstyle-query-1.1.5
                               rust-anstyle-wincon-3.0.11
                               rust-anyhow-1.0.102
                               rust-autocfg-1.5.0
                               rust-bitflags-2.11.0
                               rust-bumpalo-3.20.2
                               rust-cc-1.2.56
                               rust-cfg-if-1.0.4
                               rust-chrono-0.4.44
                               rust-clap-4.5.60
                               rust-clap-builder-4.5.60
                               rust-clap-derive-4.5.55
                               rust-clap-lex-1.0.0
                               rust-colorchoice-1.0.4
                               rust-core-foundation-sys-0.8.7
                               rust-equivalent-1.0.2
                               rust-errno-0.3.14
                               rust-fallible-iterator-0.3.0
                               rust-fallible-streaming-iterator-0.1.9
                               rust-fastrand-2.3.0
                               rust-find-msvc-tools-0.1.9
                               rust-foldhash-0.1.5
                               rust-getrandom-0.4.2
                               rust-hashbrown-0.15.5
                               rust-hashbrown-0.16.1
                               rust-hashlink-0.10.0
                               rust-heck-0.5.0
                               rust-id-arena-2.3.0
                               rust-iana-time-zone-0.1.65
                               rust-iana-time-zone-haiku-0.1.2
                               rust-indexmap-2.13.0
                               rust-is-terminal-polyfill-1.70.2
                               rust-itoa-1.0.17
                               rust-js-sys-0.3.91
                               rust-leb128fmt-0.1.0
                               rust-libc-0.2.182
                               rust-libsqlite3-sys-0.35.0
                               rust-linux-raw-sys-0.12.1
                               rust-log-0.4.29
                               rust-memchr-2.8.0
                               rust-num-traits-0.2.19
                               rust-once-cell-1.21.3
                               rust-once-cell-polyfill-1.70.2
                               rust-pkg-config-0.3.32
                               rust-prettyplease-0.2.37
                               rust-proc-macro2-1.0.106
                               rust-quote-1.0.45
                               rust-r-efi-6.0.0
                               rust-regex-1.12.3
                               rust-regex-automata-0.4.14
                               rust-regex-syntax-0.8.10
                               rust-rusqlite-0.37.0
                               rust-rustix-1.1.4
                               rust-rustversion-1.0.22
                               rust-same-file-1.0.6
                               rust-semver-1.0.27
                               rust-serde-1.0.228
                               rust-serde-core-1.0.228
                               rust-serde-derive-1.0.228
                               rust-serde-json-1.0.149
                               rust-shlex-1.3.0
                               rust-smallvec-1.15.1
                               rust-strsim-0.11.1
                               rust-syn-2.0.117
                               rust-tempfile-3.26.0
                               rust-thiserror-2.0.18
                               rust-thiserror-impl-2.0.18
                               rust-unicode-ident-1.0.24
                               rust-unicode-xid-0.2.6
                               rust-urlencoding-2.1.3
                               rust-utf8parse-0.2.2
                               rust-uuid-1.22.0
                               rust-vcpkg-0.2.15
                               rust-walkdir-2.5.0
                               rust-wasip2-1.0.2+wasi-0.2.9
                               rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                               rust-wasm-bindgen-0.2.114
                               rust-wasm-bindgen-macro-0.2.114
                               rust-wasm-bindgen-macro-support-0.2.114
                               rust-wasm-bindgen-shared-0.2.114
                               rust-wasm-encoder-0.244.0
                               rust-wasm-metadata-0.244.0
                               rust-wasmparser-0.244.0
                               rust-winapi-util-0.1.11
                               rust-windows-core-0.62.2
                               rust-windows-implement-0.60.2
                               rust-windows-interface-0.59.3
                               rust-windows-link-0.2.1
                               rust-windows-result-0.4.1
                               rust-windows-strings-0.5.1
                               rust-windows-sys-0.61.2
                               rust-wit-bindgen-0.51.0
                               rust-wit-bindgen-core-0.51.0
                               rust-wit-bindgen-rust-0.51.0
                               rust-wit-bindgen-rust-macro-0.51.0
                               rust-wit-component-0.244.0
                               rust-wit-parser-0.244.0
                               rust-zmij-1.0.21)))
