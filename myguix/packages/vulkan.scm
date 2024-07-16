(define-module (myguix packages vulkan)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages vulkan))

(define-public vulkan-memory-allocator-a6bfc2
  (let ((version "3.0")
        (revision "1")
        (commit "a6bfc237255a6bac1513f7c1ebde6d8aed6b5191"))
    (package
			(inherit vulkan-memory-allocator)
      (name "vulkan-memory-allocator")
      (version (git-version version revision commit))
      (home-page "https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1hpzjwl5bgqv9hmf1fdldihfllcbdg515f391a200klg0rnixdds")))))))
