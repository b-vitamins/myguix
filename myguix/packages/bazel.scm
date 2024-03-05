;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (myguix packages bazel)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rpc))

;; TODO: all these jars need to be replaced with packages from Guix.
(define-public bazel-6
  (package
    (name "bazel")
    (version "6.3.2")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/bazelbuild/bazel/"
                           "releases/download/"
                           version
                           "/bazel-"
                           version
                           "-dist.zip"))
       (sha256
        (base32 "0j190j7vjknlw1cgynb3r8vlv0j6i9lac6s5payf4fqrb2ngxmwc"))
       (patches (search-patches "patches/bazel-mock-repos.patch"
                                "patches/bazel-workspace.patch"))
       (snippet '(for-each delete-file
                           '("third_party/apache_commons_collections/commons-collections-3.2.2.jar"
                             "third_party/apache_commons_compress/apache-commons-compress-1.19.jar"
                             "third_party/apache_commons_io/commons-io-2.4.jar"
                             "third_party/apache_commons_lang/commons-lang-2.6.jar"
                             "third_party/hamcrest/hamcrest-core-1.3.jar"
                             "third_party/jsr305/jsr-305.jar"
                             "third_party/xz/xz-1.9.jar")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'prepare-jars
                     (lambda* (#:key inputs #:allow-other-keys)
                       (copy-file (search-input-file inputs
                                   "/share/java/commons-collections-3.2.2.jar")
                        "third_party/apache_commons_collections/commons-collections-3.2.2.jar")
                       (copy-file (search-input-file inputs
                                   "/lib/m2/org/apache/commons/commons-compress/1.21/commons-compress-1.21.jar")
                        "third_party/apache_commons_compress/apache-commons-compress-1.19.jar")
                       (copy-file (search-input-file inputs
                                   "/lib/m2/commons-io/commons-io/2.5/commons-io-2.5.jar")
                        "third_party/apache_commons_io/commons-io-2.4.jar")
                       (copy-file (search-input-file inputs
                                   "/share/java/commons-lang-2.6.jar")
                        "third_party/apache_commons_lang/commons-lang-2.6.jar")
                       (copy-file (search-input-file inputs
                                   "/lib/m2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar")
                        "third_party/hamcrest/hamcrest-core-1.3.jar")
                       (copy-file (search-input-file inputs
                                   "/lib/m2/com/google/code/findbugs/jsr305/3.0.1/jsr305-3.0.1.jar")
                                  "third_party/jsr305/jsr-305.jar")
                       (copy-file (search-input-file inputs
                                   "/lib/m2/org/tukaani/xz/1.9/xz-1.9.jar")
                                  "third_party/xz/xz-1.9.jar")))
                   (delete 'configure)
                   (replace 'build
                     (lambda _
                       (setenv "EXTRA_BAZEL_ARGS"
                               (string-append
                                "--tool_java_runtime_version=local_jdk --noremote_accept_cached --verbose_failures --subcommands --action_env=PATH --action_env=LIBRARY_PATH --action_env=C_INCLUDE_PATH --action_env=CPLUS_INCLUDE_PATH --action_env=GUIX_LOCPATH --host_action_env=PATH --host_action_env=LIBRARY_PATH --host_action_env=C_INCLUDE_PATH --host_action_env=CPLUS_INCLUDE_PATH --host_action_env=GUIX_LOCPATH --define=distribution=debian
--override_repository=com_google_protobuf="
                                (getcwd)
                                "/tools/distributions/debian/protobuf --override_repository=remote_java_tools_linux="
                                (getcwd)
                                "/mock_repos/remote_java_tools_linux --override_repository=io_bazel_skydoc="
                                (getcwd)
                                "/mock_repos/bazel_skydoc --override_repository=rules_cc="
                                (getcwd)
                                "/mock_repos/rules_cc --override_repository=rules_java="
                                (getcwd)
                                "/mock_repos/rules_java --override_repository=rules_proto="
                                (getcwd)
                                "/mock_repos/rules_proto --override_repository=platforms="
                                (getcwd)
                                "/mock_repos/platforms "))
                       (substitute* "tools/distributions/debian/deps.bzl"
                         (("/usr/include/google/protobuf")
                          (string-append #$(this-package-native-input
                                            "protobuf")
                                         "/include/google/protobuf"))
                         (("\"grpc_java_plugin.*")
                          "")
                         (("/usr/bin/protoc")
                          (string-append #$(this-package-native-input
                                            "protobuf") "/bin/protoc"))
                         (("/usr/bin/grpc_cpp_plugin")
                          (string-append #$(this-package-input "grpc")
                                         "/bin/grpc_cpp_plugin"))
                         (("\"java\": \"/usr/share/java\",")
                          (string-append "\"jars\": \""
                                         (getcwd) "/derived/jars\",")))
                       (substitute* "tools/distributions/distribution_rules.bzl"
                         (("if \"debian\"")
                          "if \"shmebian\""))
                       (substitute* "third_party/grpc-java/compiler/src/java_plugin/cpp/java_generator.cpp"
                         (("google/protobuf/compiler/java/java_names.h")
                          "google/protobuf/compiler/java/names.h"))
                       (substitute* "tools/distributions/debian/debian_java.BUILD"
                         (("\"java/protobuf-util.jar\",")
                          (string-append "\""
                           "jars/com_google_protobuf/java/util/libutil.jar"
                           "\","))
                         (("\"java/protobuf.jar\",")
                          (string-append "\""
                           "jars/com_google_protobuf/java/core/libcore.jar"
                           "\","))
                         (("jars = \\[\"java/protobuf-util.jar\"")
                          (string-append "jars = [\""
                           "jars/com_google_protobuf/java/util/libutil.jar"
                           "\""))
                         (("jars = \\[\"java/protobuf.jar\"")
                          (string-append "jars = [\""
                           "jars/com_google_protobuf/java/core/libcore.jar"
                           "\", \""
                           "jars/com_google_protobuf/java/core/liblite.jar"
                           "\",")))
                       (substitute* "third_party/grpc/build_defs.bzl"
                         (("command = \"cp" m)
                          (string-append "use_default_shell_env = True,\n" m)))
                       (substitute* "src/main/java/com/google/devtools/build/lib/BUILD"
                         (("\"@com_google_protobuf//java/util\",")
                          ""))
                       (substitute* '("src/conditions/BUILD"
                                      "src/conditions/BUILD.tools")
                         (("@platforms//cpu:riscv64")
                          "@platforms//cpu:x86_64")
                         (("@platforms//cpu:mips64")
                          "@platforms//cpu:x86_64"))
                       (substitute* "src/main/java/com/google/devtools/build/lib/runtime/commands/license/merge_licenses.bzl"
                         (("\"OUT\": ctx.outputs.out.path," m)
                          (string-append m "\n\"PATH\": \""
                                         (getenv "PATH") "\",")))
                       (substitute* "scripts/bootstrap/compile.sh"
                         (("#!/bin/sh")
                          (string-append "#!"
                                         (which "sh"))))
                       (substitute* '("src/main/java/com/google/devtools/build/lib/analysis/BashCommandConstructor.java"
                                      "tools/build_rules/java_rules_skylark.bzl"
                                      "tools/build_rules/test_rules.bzl"
                                      "tools/android/android_sdk_repository_template.bzl")
                         (("#!/bin/bash")
                          (string-append "#!"
                                         (which "bash"))))
                       (substitute* '("src/main/java/com/google/devtools/build/lib/analysis/CommandHelper.java"
                                      "src/main/java/com/google/devtools/build/lib/analysis/ShellConfiguration.java"
                                      "src/main/java/com/google/devtools/build/lib/bazel/rules/BazelRuleClassProvider.java"
                                      "src/main/java/com/google/devtools/build/lib/bazel/rules/sh/BazelShRuleClasses.java"
                                      "src/main/java/com/google/devtools/build/lib/util/CommandBuilder.java")
                         (("\"/bin/bash\"")
                          (string-append "\""
                                         (which "bash") "\"")))
                       (substitute* '("src/main/java/com/google/devtools/build/lib/bazel/rules/python/BazelPythonSemantics.java"
                                      "src/main/java/com/google/devtools/build/lib/starlarkbuildapi/python/PyRuntimeInfoApi.java"
                                      "src/main/java/com/google/devtools/build/lib/bazel/rules/java/java_stub_template.txt"
                                      "src/test/java/com/google/devtools/build/lib/standalone/StandaloneSpawnStrategyTest.java"
                                      "src/test/java/com/google/devtools/build/lib/bazel/rules/python/BazelPyBinaryConfiguredTargetTest.java"
                                      "tools/python/toolchain.bzl")
                         (("/usr/bin/env")
                          (which "env")))
                       (invoke "proot"
                               "-b"
                               (string-append #$static-bash "/bin/bash"
                                              ":/bin/bash")
                               "-b"
                               (string-append #$coreutils "/bin/env"
                                              ":/usr/bin/env")
                               "bash"
                               "compile.sh")))
                   (replace 'install
                     (lambda _
                       (install-file "output/bazel"
                                     (string-append #$output "/bin")))))))
    (inputs (list `(,openjdk11 "jdk")
                  grpc
                  python
                  java-commons-collections
                  java-commons-compress
                  java-commons-io
                  java-commons-lang
                  java-hamcrest-core
                  java-jsr305
                  java-xz
                  zlib))
    (native-inputs (list proot
                         protobuf
                         unzip
                         util-linux
                         which
                         zip))
    (home-page "https://bazel.build")
    (synopsis "Build and test tool")
    (description
     "Bazel is a build and test tool similar to Make, Maven, and
Gradle.  It uses a human-readable, high-level build language.  Bazel
supports projects in multiple languages and builds outputs for
multiple platforms.  Bazel supports large codebases across multiple
repositories, and large numbers of users.")
    (license license:asl2.0)))
