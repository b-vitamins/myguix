(define-module (myguix build jax-cuda)
  #:use-module (guix build utils)
  #:use-module (ice-9 textual-ports)
  #:export (prepare-jax-cuda13-repositories))

;; Generate the CUDA repositories expected by JAX's Bazel CUDA build.  The
;; repositories follow the names used by rules_ml_toolchain's NVIDIA
;; redistribution support, but point at Guix-provided CUDA, cuDNN, NCCL and
;; NVVM packages.
(define* (prepare-jax-cuda13-repositories
          #:key root cuda cuda-nvvm cccl cudnn nccl clang
          nvshmem rules-ml-toolchain binutils)
  (define (write-file file text)
    (call-with-output-file file
      (lambda (port)
        (display text port))))

  (define (force-symlink target link)
    (false-if-exception (delete-file link))
    (symlink target link))

  (define (workspace repo name)
    (write-file (string-append repo "/WORKSPACE")
                (string-append "workspace(name = \"" name "\")\n")))

  (define (version repo version)
    (write-file (string-append repo "/version.bzl")
                (string-append "VERSION = \"" version "\"\n")))

  (define (repo name)
    (let ((dir (string-append root "/" name)))
      (mkdir-p dir)
      (workspace dir name)
      dir))

  (define (make-tree-writable dir)
    (for-each make-file-writable
              (cons dir
                    (find-files dir "."
                                #:directories? #t))))

  (define (write-patched-rules-ml-toolchain)
    (when (and rules-ml-toolchain binutils)
      (let ((dir (string-append root "/rules_ml_toolchain")))
        (false-if-exception (delete-file-recursively dir))
        (copy-recursively rules-ml-toolchain dir
                          #:log (%make-void-port "w"))
        (make-tree-writable dir)
        (substitute* (string-append dir "/gpu/cuda/cuda_configure.bzl")
          (("cuda_defines\\[\"%\\{host_compiler_prefix\\}\"\\] = \"/usr/bin\"")
           (string-append
            "cuda_defines[\"%{host_compiler_prefix}\"] = \""
            binutils "/bin\""))
          (("\"-Wno-invalid-partial-specialization\"")
           (string-append
            "\"-Wno-invalid-partial-specialization\",\n"
            "          \"-Wno-unused-command-line-argument\""))))))

  (define (link-cuda-layout dir)
    (force-symlink (string-append cuda "/include")
                   (string-append dir "/include"))
    (force-symlink (string-append cuda "/lib")
                   (string-append dir "/lib"))
    (force-symlink (string-append cuda "/lib64")
                   (string-append dir "/lib64"))
    (force-symlink (string-append cuda "/bin")
                   (string-append dir "/bin")))

  (define (write-header-only-repo name version-number include-prefix)
    (let ((dir (repo name)))
      (link-cuda-layout dir)
      (version dir version-number)
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "filegroup(name = \"header_list\", "
        "srcs = glob([\"include/**/*.h\", \"include/**/*.hpp\", "
        "\"include/**/*.cuh\"], "
        "exclude = [\"include/cccl/**\"], allow_empty = True))\n"
        "cc_library(name = \"headers\", hdrs = [\":header_list\"], "
        "includes = [\"include\"], "
        "include_prefix = \"" include-prefix "\", "
        "strip_include_prefix = \"include\")\n"))))

  (define (write-cuda-cccl)
    (let ((dir (repo "cuda_cccl")))
      (version dir "3.2.0")
      (mkdir-p (string-append dir "/src"))
      (mkdir-p (string-append dir "/include"))
      (invoke "tar" "xf" cccl "-C" (string-append dir "/src")
              "--strip-components=1")
      (substitute* (string-append dir "/src/cub/cub/block/block_load.cuh")
        (("#include <cub/block/block_exchange\\.cuh>")
         "#include <cub/block/block_exchange.cuh>\n#include <cub/detail/uninitialized_copy.cuh>")
        (("new \\(&dst_items\\[i\\]\\) T\\(block_src_it\\[warp_offset \\+ tid \\+ \\(i \\* detail::warp_threads\\)\\]\\);")
         "detail::uninitialized_copy_single(dst_items + i, block_src_it[warp_offset + tid + (i * detail::warp_threads)]);")
        (("new \\(&dst_items\\[i\\]\\) T\\(block_src_it\\[src_pos\\]\\);")
         "detail::uninitialized_copy_single(dst_items + i, block_src_it[src_pos]);"))
      ;; CCCL 3.2's onesweep short-circuit path guards the value-copy branch
      ;; with a normal constant `if'.  Clang still type-checks the branch for
      ;; key-only sorts, where ValueT is cub::NullType, so discard it with
      ;; `if constexpr' instead.
      (substitute* (string-append
                    dir "/src/cub/cub/agent/agent_radix_sort_onesweep.cuh")
        (("if \\(!KEYS_ONLY\\)")
         "if constexpr (!KEYS_ONLY)"))
      (force-symlink (string-append dir "/src/cub/cub")
                     (string-append dir "/include/cub"))
      (force-symlink (string-append dir "/src/libcudacxx/include/cuda")
                     (string-append dir "/include/cuda"))
      (force-symlink (string-append dir "/src/libcudacxx/include/nv")
                     (string-append dir "/include/nv"))
      (force-symlink (string-append dir "/src/thrust/thrust")
                     (string-append dir "/include/thrust"))
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "filegroup(name = \"header_list\", "
        "srcs = glob([\"include/cub/**\", \"include/cuda/**\", "
        "\"include/nv/**\", \"include/thrust/**\"], "
        "allow_empty = False))\n"
        "cc_library(name = \"headers\", hdrs = [\":header_list\"], "
        "includes = [\"include\"], "
        "include_prefix = \"third_party/gpus/cuda/include\", "
        "strip_include_prefix = \"include\")\n"))))

  (define (write-shared-repo name version-number targets)
    (let ((dir (repo name)))
      (link-cuda-layout dir)
      (version dir version-number)
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "filegroup(name = \"header_list\", "
        "srcs = glob([\"include/**/*.h\", \"include/**/*.hpp\", "
        "\"include/**/*.cuh\"], "
        "exclude = [\"include/cccl/**\"], allow_empty = True))\n"
        "cc_library(name = \"headers\", hdrs = [\":header_list\"], "
        "includes = [\"include\"], "
        "include_prefix = \"third_party/gpus/cuda/include\", "
        "strip_include_prefix = \"include\")\n"
        (apply string-append
                  (map (lambda (target)
                         (let ((name (car target))
                               (library (cadr target)))
                           (string-append
                            "cc_import(name = \"" name "_shared\", "
                            "hdrs = [\":headers\"], shared_library = \""
                            library "\")\n"
                            "cc_library(name = \"" name
                            "\", deps = [\":" name "_shared\"])\n")))
                    targets))))))

  (define (write-cuda-cudart)
    (let ((dir (repo "cuda_cudart")))
      (link-cuda-layout dir)
      (version dir "13")
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "exports_files([\"include/cuda.h\"])\n"
        "filegroup(name = \"static\", srcs = [\"lib/libcudart_static.a\"])\n"
        "filegroup(name = \"header_list\", "
        "srcs = glob([\"include/**/*.h\", \"include/**/*.hpp\", "
        "\"include/**/*.cuh\"], "
        "exclude = [\"include/cccl/**\"], allow_empty = True))\n"
        "cc_library(name = \"headers\", hdrs = [\":header_list\"], "
        "includes = [\"include\"], "
        "include_prefix = \"third_party/gpus/cuda/include\", "
        "strip_include_prefix = \"include\")\n"
        "cc_import(name = \"cuda_stub\", "
        "interface_library = \"lib/stubs/libcuda.so\", system_provided = 1)\n"
        "cc_import(name = \"cudart_shared_library\", hdrs = [\":headers\"], "
        "shared_library = \"lib/libcudart.so.13\")\n"
        "cc_library(name = \"cuda_driver\", deps = [\":cuda_stub\"])\n"
        "cc_library(name = \"cudart\", "
        "deps = [\":cuda_driver\", \":cudart_shared_library\"])\n"))))

  (define (write-cuda-cudnn)
    (let ((dir (repo "cuda_cudnn")))
      (force-symlink (string-append cudnn "/include")
                     (string-append dir "/include"))
      (force-symlink (string-append cudnn "/lib")
                     (string-append dir "/lib"))
      (version dir "9")
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "filegroup(name = \"header_list\", "
        "srcs = glob([\"include/cudnn*.h\"], allow_empty = True))\n"
        "cc_library(name = \"headers\", hdrs = [\":header_list\"], "
        "includes = [\"include\"], "
        "include_prefix = \"third_party/gpus/cudnn\", "
        "strip_include_prefix = \"include\")\n"
        "cc_import(name = \"cudnn_main\", hdrs = [\":headers\"], "
        "shared_library = \"lib/libcudnn.so.9\")\n"
        "cc_import(name = \"cudnn_ops\", hdrs = [\":headers\"], "
        "shared_library = \"lib/libcudnn_ops.so.9\")\n"
        "cc_import(name = \"cudnn_cnn\", hdrs = [\":headers\"], "
        "shared_library = \"lib/libcudnn_cnn.so.9\")\n"
        "cc_import(name = \"cudnn_adv\", hdrs = [\":headers\"], "
        "shared_library = \"lib/libcudnn_adv.so.9\")\n"
        "cc_import(name = \"cudnn_graph\", hdrs = [\":headers\"], "
        "shared_library = \"lib/libcudnn_graph.so.9\")\n"
        "cc_import(name = \"cudnn_engines_precompiled\", hdrs = [\":headers\"], "
        "shared_library = \"lib/libcudnn_engines_precompiled.so.9\")\n"
        "cc_import(name = \"cudnn_engines_runtime_compiled\", "
        "hdrs = [\":headers\"], "
        "shared_library = \"lib/libcudnn_engines_runtime_compiled.so.9\")\n"
        "cc_import(name = \"cudnn_heuristic\", hdrs = [\":headers\"], "
        "shared_library = \"lib/libcudnn_heuristic.so.9\")\n"
        "cc_library(name = \"cudnn\", hdrs = [\":header_list\"], "
        "deps = [\":cudnn_main\", \":cudnn_ops\", \":cudnn_cnn\", "
        "\":cudnn_adv\", \":cudnn_graph\", "
        "\":cudnn_engines_precompiled\", "
        "\":cudnn_engines_runtime_compiled\", "
        "\":cudnn_heuristic\", \"@cuda_nvrtc//:nvrtc\"])\n"))))

  (define (write-cuda-cupti)
    (let ((dir (repo "cuda_cupti")))
      (link-cuda-layout dir)
      (version dir "13")
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "filegroup(name = \"header_list\", "
        "srcs = glob([\"include/cupti*.h\", \"include/Openacc/**\", "
        "\"include/Openmp/**\"], allow_empty = True))\n"
        "cc_library(name = \"headers\", hdrs = [\":header_list\"], "
        "includes = [\"include\"], "
        "include_prefix = \"third_party/gpus/cuda/extras/CUPTI/include\", "
        "strip_include_prefix = \"include\")\n"
        "cc_import(name = \"cupti_shared_library\", "
        "hdrs = [\":headers\"], shared_library = \"lib64/libcupti.so.13\")\n"
        "cc_library(name = \"cupti\", "
        "deps = [\":cupti_shared_library\"])\n"))))

  (define (write-cuda-nvml)
    (let ((dir (repo "cuda_nvml")))
      (link-cuda-layout dir)
      (version dir "1")
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "filegroup(name = \"header_list\", "
        "srcs = [\"include/nvml.h\"])\n"
        "cc_library(name = \"headers\", hdrs = [\":header_list\"], "
        "includes = [\"include\"], "
        "include_prefix = \"third_party/gpus/cuda/nvml/include\", "
        "strip_include_prefix = \"include\")\n"))))

  (define (write-cuda-nvcc)
    (let ((dir (repo "cuda_nvcc")))
      (link-cuda-layout dir)
      (force-symlink (string-append cuda-nvvm "/nvvm")
                     (string-append dir "/nvvm"))
      (version dir "13")
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "exports_files([\"bin/nvcc\"])\n"
        "filegroup(name = \"nvvm\", "
        "srcs = glob([\"nvvm/**\"], allow_empty = True))\n"
        "filegroup(name = \"nvdisasm\", srcs = [\"bin/nvdisasm\"])\n"
        "filegroup(name = \"nvlink\", srcs = [\"bin/nvlink\"])\n"
        "filegroup(name = \"fatbinary\", srcs = [\"bin/fatbinary\"])\n"
        "filegroup(name = \"bin2c\", srcs = [\"bin/bin2c\"])\n"
        "filegroup(name = \"ptxas\", srcs = [\"bin/ptxas\"])\n"
        "filegroup(name = \"bin\", "
        "srcs = glob([\"bin/**\", \"nvvm/bin/**\"], allow_empty = True))\n"
        "filegroup(name = \"link_stub\", srcs = [\"bin/crt/link.stub\"])\n"
        "filegroup(name = \"feature\")\n"
        "filegroup(name = \"header_list\", "
        "srcs = glob([\"include/**/*.h\", \"include/**/*.hpp\", "
        "\"include/**/*.cuh\"], "
        "exclude = [\"include/cccl/**\"], allow_empty = True))\n"
        "cc_library(name = \"headers\", hdrs = [\":header_list\"], "
        "includes = [\"include\"], "
        "include_prefix = \"third_party/gpus/cuda/include\", "
        "strip_include_prefix = \"include\")\n"))))

  (define (write-cuda-nvvm)
    (let ((dir (repo "cuda_nvvm")))
      (force-symlink (string-append cuda-nvvm "/nvvm")
                     (string-append dir "/nvvm"))
      (version dir "13")
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "exports_files([\"nvvm/bin/cicc\", "
        "\"nvvm/libdevice/libdevice.10.bc\"])\n"
        "filegroup(name = \"cicc\", srcs = [\"nvvm/bin/cicc\"])\n"
        "filegroup(name = \"nvvm\", "
        "srcs = [\"nvvm/libdevice/libdevice.10.bc\"])\n"))))

  (define (write-cuda-driver)
    (let ((dir (repo "cuda_driver")))
      (link-cuda-layout dir)
      (version dir "1")
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "config_setting(name = \"cuda_umd_libs\", "
        "define_values = {\"cuda_umd\": \"true\"})\n"
        "cc_library(name = \"nvidia_driver\")\n"
        "cc_library(name = \"nvidia_ptxjitcompiler\")\n"))))

  (define (write-cuda-nccl name)
    (let ((dir (repo name)))
      (force-symlink (string-append nccl "/include")
                     (string-append dir "/include"))
      (force-symlink (string-append nccl "/lib")
                     (string-append dir "/lib"))
      (mkdir-p (string-append dir "/third_party/nccl"))
      (force-symlink "../../include/nccl.h"
                     (string-append dir "/third_party/nccl/nccl.h"))
      (force-symlink "../../include/nccl_device.h"
                     (string-append dir "/third_party/nccl/nccl_device.h"))
      (force-symlink "../../include/nccl_device"
                     (string-append dir "/third_party/nccl/nccl_device"))
      (version dir "2")
      (write-file
       (string-append dir "/nccl_config.h")
       "#define TF_NCCL_VERSION \"2\"\n")
      (force-symlink "../../nccl_config.h"
                     (string-append dir "/third_party/nccl/nccl_config.h"))
      (write-file (string-append dir "/version.txt") "2\n")
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "package(default_visibility = [\"//visibility:public\"])\n"
        "exports_files([\"version.txt\"])\n"
        "filegroup(name = \"header_list\", "
        "srcs = glob([\"include/nccl*.h\"], allow_empty = True))\n"
        "filegroup(name = \"compat_header_list\", "
        "srcs = glob([\"third_party/nccl/**/*.h\"], allow_empty = True))\n"
        "cc_library(name = \"compat_headers\", "
        "hdrs = [\":compat_header_list\"], includes = [\".\"])\n"
        "cc_library(name = \"headers\", "
        "hdrs = [\":header_list\"], "
        "includes = [\"include\"], include_prefix = \"third_party/nccl\", "
        "strip_include_prefix = \"include\", "
        "deps = [\":compat_headers\", "
        "\"@local_config_cuda//cuda:cuda_headers\"])\n"
        "cc_import(name = \"nccl_shared_library\", "
        "hdrs = [\":headers\", \":compat_header_list\"], "
        "shared_library = \"lib/libnccl.so.2\")\n"
        "cc_library(name = \"nccl\", "
        "hdrs = [\":header_list\", \":compat_header_list\"], "
        "deps = [\":compat_headers\", \":nccl_shared_library\"])\n"
        "cc_library(name = \"nccl_via_stub\", deps = [\":nccl\"])\n"
        "cc_library(name = \"nccl_headers\", "
        "deps = [\":compat_headers\", \":headers\"])\n"
        "cc_library(name = \"nccl_config\", hdrs = [\"nccl_config.h\"], "
        "include_prefix = \"third_party/nccl\")\n"))))

  (define (write-nvidia-nvshmem)
    (let ((dir (repo "nvidia_nvshmem")))
      (force-symlink (string-append nvshmem "/include")
                     (string-append dir "/include"))
      (force-symlink (string-append nvshmem "/lib")
                     (string-append dir "/lib"))
      (force-symlink (string-append nvshmem "/bin")
                     (string-append dir "/bin"))
      (version dir "3.3.20")
      (write-file
       (string-append dir "/BUILD")
       (string-append
        "licenses([\"restricted\"])\n"
        "load(\"@rules_ml_toolchain//gpu:nvidia_common_rules.bzl\", "
        "\"cuda_rpath_flags\")\n"
        "package(default_visibility = [\"//visibility:public\"])\n\n"
        "filegroup(name = \"libnvshmem_device\", "
        "srcs = [\"lib/libnvshmem_device.bc\"])\n"
        "filegroup(name = \"header_list\", "
        "srcs = glob([\"include/**\"], allow_empty = False))\n"
        "cc_library(name = \"headers\", hdrs = [\":header_list\"], "
        "includes = [\"include\"], include_prefix = \"third_party/nvshmem\", "
        "strip_include_prefix = \"include\")\n"
        "cc_import(name = \"nvshmem_host_shared_library\", "
        "hdrs = [\":headers\"], "
        "shared_library = \"lib/libnvshmem_host.so.3\")\n"
        "cc_import(name = \"nvshmem_bootstrap_uid_shared_library\", "
        "hdrs = [\":headers\"], "
        "shared_library = \"lib/nvshmem_bootstrap_uid.so.3\")\n"
        "cc_import(name = \"nvshmem_transport_ibrc_shared_library\", "
        "hdrs = [\":headers\"], "
        "shared_library = \"lib/nvshmem_transport_ibrc.so.3\")\n"
        "cc_library(name = \"nvshmem\", deps = ["
        "\":nvshmem_host_shared_library\", "
        "\":nvshmem_bootstrap_uid_shared_library\", "
        "\":nvshmem_transport_ibrc_shared_library\"], "
        "linkopts = cuda_rpath_flags(\"nvidia/nvshmem/lib\"))\n"))))

  (define (write-local-config-hooks)
    (chmod "WORKSPACE" #o644)
    (let ((workspace (call-with-input-file "WORKSPACE" get-string-all)))
      (call-with-output-file "WORKSPACE"
        (lambda (port)
          (display workspace port)
          (display
           "\nload(\"@rules_ml_toolchain//gpu/cuda:cuda_configure.bzl\", \"cuda_configure\")\n"
           port)
          (display "cuda_configure(name = \"local_config_cuda\")\n" port)
          (display
           "load(\"@rules_ml_toolchain//gpu/nccl:nccl_configure.bzl\", \"nccl_configure\")\n"
           port)
          (display "nccl_configure(name = \"local_config_nccl\")\n" port)))))

  (define (write-local-config-rocm-cuda-selectors)
    (let ((dir (string-append root "/local_config_rocm/rocm")))
      (mkdir-p dir)
      (write-file
       (string-append dir "/build_defs.bzl")
       (string-append
        "load(\"@rules_cc//cc:cc_library.bzl\", \"cc_library\")\n\n"
        "def if_rocm(if_true, if_false = []):\n"
        "    return if_false\n\n"
        "def select_threshold(value, above_or_eq, threshold, below):\n"
        "    return below if value < threshold else above_or_eq\n\n"
        "def if_rocm_is_configured(if_true, if_false = []):\n"
        "    return if_false\n\n"
        "def if_gpu_is_configured(if_true, if_false = []):\n"
        "    return select({\n"
        "        \"@local_config_cuda//:is_cuda_enabled\": if_true,\n"
        "        \"//conditions:default\": if_false,\n"
        "    })\n\n"
        "def if_cuda_or_rocm(if_true, if_false = []):\n"
        "    return select({\n"
        "        \"@local_config_cuda//:is_cuda_enabled\": if_true,\n"
        "        \"//conditions:default\": if_false,\n"
        "    })\n\n"
        "def is_rocm_configured():\n"
        "    return False\n\n"
        "def rocm_library(copts = [], deps = [], **kwargs):\n"
        "    if \"@local_config_rocm//rocm:rocm_headers\" not in deps:\n"
        "        deps = deps + [\"@local_config_rocm//rocm:rocm_headers\"]\n"
        "    cc_library(copts = copts, deps = deps, **kwargs)\n\n"
        "def rocm_version_number():\n"
        "    return 0\n\n"
        "def get_rbe_amdgpu_pool(is_single_gpu = False):\n"
        "    return \"\"\n"))))

  (define (patch-xla-cuda-sources)
    (define (patch-one xla-root)
      (let ((bfloat (string-append
                     xla-root
                     "/xla/stream_executor/cuda/buffer_debug_float_check_kernel_cuda.cu.cc"))
            (gpu-build (string-append xla-root "/xla/pjrt/gpu/BUILD")))
        (when (file-exists? bfloat)
          (format #t "patching XLA CUDA bfloat source in ~a~%" xla-root)
          (make-file-writable bfloat)
          (substitute* bfloat
            (("// Neither the constructor of __nv_bfloat16 nor the CUDART_INF_BF16 constants")
             "// CUDA's raw bfloat16 wrapper supports constexpr host/device initialization.")
            (("// are constexpr, so we can't use them directly\\. However:")
             "// Use the raw IEEE bfloat16 representation for +infinity to avoid")
            (("// - Eigen::bfloat16 \\*does\\* have a valid numeric_limits::infinity\\(\\),")
             "// instantiating std::is_trivially_copyable from absl::bit_cast in device code.")
            (("// - absl::bit_cast is constexpr,")
             "//")
            (("// - __nv_bfloat16 and Eigen::bfloat16 are bitwise identical")
             "//")
            (("// So we can get a constant this way\\.")
             "//")
            (("    absl::bit_cast<__nv_bfloat16>\\(kInfinity<Eigen::bfloat16>\\);")
             "    __nv_bfloat16(__nv_bfloat16_raw{0x7f80});")))
        (when (file-exists? gpu-build)
          (make-file-writable gpu-build)
          (unless (string-contains
                   (call-with-input-file gpu-build get-string-all)
                   "\"//xla/service/gpu:kernel_reuse_cache_proto_cc\"")
            (format #t "adding XLA kernel reuse cache dependency in ~a~%"
                    gpu-build)
            (substitute* gpu-build
              (("        \"//xla/service/gpu:gpu_compiler\",")
               (string-append
                "        \"//xla/service/gpu:gpu_compiler\",\n"
                "        \"//xla/service/gpu:kernel_reuse_cache_proto_cc\",")))))))
    (for-each patch-one
              (list (string-append root "/xla")
                    (string-append root "/output/external/xla"))))

  (setenv "TF_NEED_CUDA" "1")
  (setenv "TF_NCCL_USE_STUB" "0")
  (setenv "HERMETIC_CUDA_VERSION" "13.0.0")
  (setenv "HERMETIC_CUDA_COMPUTE_CAPABILITIES"
          "sm_75,sm_80,sm_86,sm_89,sm_90,sm_100,compute_120")
  (setenv "CC" (string-append clang "/bin/clang"))
  (setenv "CXX" (string-append clang "/bin/clang++"))
  (setenv "CLANG_CUDA_COMPILER_PATH"
          (string-append clang "/bin/clang"))

  (substitute* "jaxlib/tools/BUILD.bazel"
    (("cuda_major_version = \"0\"")
     "cuda_major_version = \"13\""))
  (substitute* (list ".bazelrc"
                     "jaxlib/tools/BUILD.bazel")
    (("@local_config_cuda//:enable_cuda")
     "@rules_ml_toolchain//common:enable_cuda"))
  (patch-xla-cuda-sources)
  (write-patched-rules-ml-toolchain)
  (write-local-config-hooks)
  (write-local-config-rocm-cuda-selectors)
  (write-cuda-cccl)
  (write-header-only-repo "cuda_crt" "13"
                          "third_party/gpus/cuda/include")
  (write-cuda-cudart)
  (write-shared-repo "cuda_cublas" "13"
                     '(("cublas" "lib/libcublas.so.13")
                       ("cublasLt" "lib/libcublasLt.so.13")))
  (write-cuda-cudnn)
  (write-shared-repo "cuda_cufft" "12" '(("cufft" "lib/libcufft.so.12")))
  (write-cuda-cupti)
  (write-shared-repo "cuda_curand" "10" '(("curand" "lib/libcurand.so.10")))
  (write-shared-repo "cuda_cusolver" "12"
                     '(("cusolver" "lib/libcusolver.so.12")))
  (write-shared-repo "cuda_cusparse" "12"
                     '(("cusparse" "lib/libcusparse.so.12")))
  (write-cuda-driver)
  (write-cuda-nccl "cuda_nccl")
  (write-cuda-nccl "nccl_archive")
  (write-nvidia-nvshmem)
  (write-cuda-nvcc)
  (write-cuda-nvvm)
  (write-shared-repo "cuda_nvjitlink" "13"
                     '(("nvjitlink" "lib/libnvJitLink.so.13")))
  (write-cuda-nvml)
  (write-header-only-repo "cuda_nvtx" "13"
                          "third_party/gpus/cuda/include")
  (write-shared-repo "cuda_nvrtc" "13" '(("nvrtc" "lib/libnvrtc.so.13")))
  (let ((dir (repo "cuda_nvdisasm")))
    (link-cuda-layout dir)
    (version dir "13")
    (write-file (string-append dir "/BUILD")
                (string-append
                 "package(default_visibility = [\"//visibility:public\"])\n"
                 "filegroup(name = \"nvdisasm\", srcs = [\"bin/nvdisasm\"])\n")))
  (let ((dir (repo "cuda_nvprune")))
    (link-cuda-layout dir)
    (version dir "13")
    (write-file (string-append dir "/BUILD")
                (string-append
                 "package(default_visibility = [\"//visibility:public\"])\n"
                 "filegroup(name = \"nvprune\", "
                 "srcs = glob([\"bin/nvprune\"], allow_empty = True))\n")))
  (write-header-only-repo "cuda_profiler_api" "13"
                          "third_party/gpus/cuda/include")
  (for-each
   (lambda (name)
     (let ((dir (repo name)))
       (version dir "21")
       (write-file
        (string-append dir "/BUILD")
        (string-append
         "package(default_visibility = [\"//visibility:public\"])\n"
         "filegroup(name = \"cuda_wrappers_headers\")\n"))))
   '("llvm_linux_aarch64" "llvm_linux_x86_64")))
