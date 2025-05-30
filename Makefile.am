# myguix --- Personal Guix channel
# Copyright © 2025 Ayan Das <bvits@riseup.net>
#
# This file is part of myguix.
#
# myguix is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# myguix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with myguix.  If not, see <http://www.gnu.org/licenses/>.

MAKEFLAGS = -j$(shell nproc)

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'			        \
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]abs_top_builddir[@],$(abs_top_builddir),g'

nodist_noinst_SCRIPTS =				\
  pre-inst-env					\
  test-env

# Verbosity control for compilation
AM_V_GUILEC = $(AM_V_GUILEC_@AM_V@)
AM_V_GUILEC_ = $(AM_V_GUILEC_@AM_DEFAULT_V@)
AM_V_GUILEC_0 = @echo "  GUILEC  " $@;

# Modules organized by dependency order (base modules first)
# This ensures dependent modules can use compiled versions of their dependencies
BASE_MODULES =					\
  myguix/licenses.scm				\
  myguix/utils.scm				\
  myguix/modules.scm				\
  myguix/download.scm				\
  myguix/gexp.scm				\
  myguix/packages.scm

BUILD_SYSTEM_MODULES =				\
  myguix/build/utils.scm			\
  myguix/build/bazel-build-system.scm		\
  myguix/build/binary-build-system.scm		\
  myguix/build/chromium-binary-build-system.scm	\
  myguix/build-system/bazel.scm		        \
  myguix/build-system/binary.scm		\
  myguix/build-system/chromium-binary.scm

PACKAGE_MODULES =				\
  myguix/packages/base.scm			\
  myguix/packages/bazel.scm			\
  myguix/packages/benchmark.scm		        \
  myguix/packages/chrome.scm			\
  myguix/packages/compression.scm		\
  myguix/packages/emacs-pqrs.scm		\
  myguix/packages/firmware.scm			\
  myguix/packages/fonts.scm			\
  myguix/packages/huggingface.scm		\
  myguix/packages/java-pqrs.scm		        \
  myguix/packages/linux.scm			\
  myguix/packages/llvm-pqrs.scm		        \
  myguix/packages/machine-learning.scm		\
  myguix/packages/maths.scm			\
  myguix/packages/messaging.scm		        \
  myguix/packages/mozilla.scm			\
  myguix/packages/nlp.scm			\
  myguix/packages/nvidia.scm			\
  myguix/packages/perl-pqrs.scm		        \
  myguix/packages/productivity.scm		\
  myguix/packages/python-pqrs.scm		\
  myguix/packages/rust-pqrs.scm		        \
  myguix/packages/video.scm			\
  myguix/packages/vpn.scm			\
  myguix/packages/wasm.scm

SERVICE_MODULES =				\
  myguix/services/authentication.scm		\
  myguix/services/base.scm			\
  myguix/services/desktop.scm			\
  myguix/services/mcron.scm			\
  myguix/services/nvidia.scm			\
  myguix/services/oci-containers.scm		\
  myguix/services/vpn.scm

HOME_SYSTEM_MODULES =				\
  myguix/home.scm				\
  myguix/home/services/emacs.scm		\
  myguix/home/services/pipewire.scm		\
  myguix/system/install.scm			\
  myguix/system/linux-initrd.scm		\
  myguix/ci.scm

# All modules in dependency order
MODULES = $(BASE_MODULES) $(BUILD_SYSTEM_MODULES) $(PACKAGE_MODULES) \
          $(SERVICE_MODULES) $(HOME_SYSTEM_MODULES)

# Patch files
PATCHES =					                        \
  myguix/packages/patches/bazel-mock-repos.patch		        \
  myguix/packages/patches/bazel-recreate-markers.patch	 		\
  myguix/packages/patches/bazel-workspace.patch				\
  myguix/packages/patches/firefox-esr-compare-paths.patch		\
  myguix/packages/patches/firefox-esr-use-system-wide-dir.patch		\
  myguix/packages/patches/firefox-ge-138-compare-paths.patch		\
  myguix/packages/patches/firefox-restore-desktop-files.patch		\
  myguix/packages/patches/firefox-use-system-wide-dir.patch		\
  myguix/packages/patches/miniz-for-pytorch-2.7.0.patch			\
  myguix/packages/patches/nvidia-cudnn-frontend_find_built_dlpack.patch	\
  myguix/packages/patches/nvidia-cudnn-frontend_use_store_so.patch	\
  myguix/packages/patches/nvidia-cutlass-3.4.0_disable_static_lib.patch	\
  myguix/packages/patches/python-pytorch-fix-codegen-2.7.0.patch	\
  myguix/packages/patches/python-pytorch-runpath-2.7.0.patch		\
  myguix/packages/patches/python-pytorch-system-libraries-2.7.0.patch	\
  myguix/packages/patches/python-pytorch-without-kineto-2.7.0.patch	\
  myguix/packages/patches/python-trition-disable-amd-backend.patch	\
  myguix/packages/patches/ruff-embed-salsa-and-lsp-types.patch

# Auxiliary files
AUX_FILES =					\
  myguix/etc/deduplicate.gscm			\
  myguix/etc/new-client-cert.gscm		\
  myguix/etc/style.gscm				\
  myguix/etc/zsh/zshenv				\
  myguix/etc/zsh/zshrc				\
  myguix/system/keys/bordeaux.guix.gnu.org.pub	\
  myguix/system/keys/ci.guix.gnu.org.pub	\
  myguix/system/keys/substitutes.myguix.bvits.in.pub

GOBJECTS = $(MODULES:%.scm=%.go)

nobase_dist_guilemodule_DATA = $(MODULES) $(PATCHES) $(AUX_FILES)
nobase_nodist_guileobject_DATA = $(GOBJECTS)

# Test files
SCM_TESTS =					\
  tests/gexp.scm

TESTS = $(SCM_TESTS)
TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" GUILE_AUTO_COMPILE=0

SCM_LOG_DRIVER =				\
  $(top_builddir)/test-env --quiet-stderr	\
  $(GUILE) --no-auto-compile -e main		\
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

# OPTIMIZED COMPILATION RULE
# Key changes:
# 1. Use guild compile instead of raw compile-file
# 2. Don't unset GUILE_LOAD_COMPILED_PATH - instead add our build dir to it
# 3. Add optimization flags for faster code
# 4. Enable warnings to catch issues early
%.go: %.scm
	$(AM_V_GUILEC)$(MKDIR_P) "`dirname "$@"`" ;			\
	export GUILE_LOAD_COMPILED_PATH="$(abs_top_builddir):$$GUILE_LOAD_COMPILED_PATH" ; \
	export GUILE_LOAD_PATH="$(abs_top_builddir):$(abs_top_srcdir):$$GUILE_LOAD_PATH" ; \
	LC_ALL=C							\
	$(top_builddir)/pre-inst-env					\
	guild compile							\
	  -L "$(abs_top_builddir)" -L "$(abs_top_srcdir)"		\
	  -Wunbound-variable -Warity-mismatch -Wformat			\
	  -O2								\
	  -o "$@" "$<"

# Alternative: Batch compilation for even more speed
# This compiles multiple files in one go, reducing startup overhead
GUILD_COMPILE_FLAGS = -L "$(abs_top_builddir)" -L "$(abs_top_srcdir)" \
                      -Wunbound-variable -Warity-mismatch -Wformat -O2

# Compile base modules first (they're dependencies for others)
compile-base: $(BASE_MODULES:%.scm=%.go)

# Then compile everything else in parallel
compile-rest: compile-base
	$(MAKE) $(BUILD_SYSTEM_MODULES:%.scm=%.go) \
	        $(PACKAGE_MODULES:%.scm=%.go) \
	        $(SERVICE_MODULES:%.scm=%.go) \
	        $(HOME_SYSTEM_MODULES:%.scm=%.go)

# Main compilation target
all-am: compile-base compile-rest

# Clean compiled files
clean-go:
	-$(RM) -f $(GOBJECTS)
	@find . -path ./test-tmp -prune -o -name '*.go' -print | \
	  if test -t 1; then \
	    xargs -r echo -e "\033[31mwarning:\033[0m stray .go files:"; \
	  else \
	    xargs -r echo "warning: stray .go files:"; \
	  fi

# Speed up clean by using find instead of individual rm
clean-local: clean-go

CLEANFILES = $(SCM_TESTS:tests/%.scm=%.log)

# Extra files to distribute
EXTRA_DIST =					\
  .dir-locals.el				\
  .gitignore   					\
  .gitattributes				\
  .guix-authorizations				\
  .guix-channel					\
  AUTHORS					\
  COPYING					\
  HACKING					\
  LICENSE					\
  NEWS						\
  README					\
  THANKS					\
  TODO						\
  bootstrap					\
  build-aux/pre-inst-env.in			\
  build-aux/test-driver.scm			\
  build-aux/test-env.in				\
  build-aux/config.rpath			\
  m4/.gitkeep					\
  $(TESTS)

ACLOCAL_AMFLAGS = -I m4

.PHONY: clean-go compile-base compile-rest

# Lint all packages
lint:
	@for pkg in $$(./pre-inst-env guix package -L . -A | cut -f1); do \
	  echo "Linting $$pkg..."; \
	  ./pre-inst-env guix lint -L . $$pkg || true; \
	done

# Format all packages  
style:
	@for file in myguix/packages/*.scm; do \
	  echo "Styling $$file..."; \
	  ./pre-inst-env guix style -f $$file; \
	done

.PHONY: lint style

# Optional: Add dependency tracking
# This creates .d files that track module dependencies
DEPDIR = .deps
DEPFILES = $(MODULES:%.scm=$(DEPDIR)/%.d)

$(DEPDIR)/%.d: %.scm
	@$(MKDIR_P) $(DEPDIR)/`dirname "$*"`
	@echo "Generating dependencies for $<"
	@$(GUILE) -c "(use-modules (ice-9 session) (ice-9 regex)) \
	  (define deps (call-with-input-file \"$<\" \
	    (lambda (port) \
	      (let loop ((deps '())) \
	        (define line (read-line port)) \
	        (if (eof-object? line) deps \
	          (let ((m (string-match \"^[[:space:]]*\\(use-modules[[:space:]]+(.*)\\)\" line))) \
	            (if m (loop (cons line deps)) (loop deps)))))))) \
	  (format #t \"$*.go: $<~%\")" > $@

-include $(DEPFILES)
