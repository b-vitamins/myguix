#!@PYTHON@ -I
# SPDX-License-Identifier: GPL-3.0-or-later
"""Make a downloaded Codex primary runtime executable on Guix System.

ChatGPT downloads a runtime bundle (Node.js, Python, LibreOffice, Poppler,
Git) built for FHS distributions.  Executables are patched once to use
GUIX/ld.so as their interpreter and to search GUIX/lib, where GUIX is a
per-user directory holding a symlink farm of the Guix libraries this package
was built with; shared libraries are patched only when they need more than
what executables preload.  The bundle may be patched while still in the
installer's staging directory and is moved afterwards, so nothing written
into it may refer to its own location.  Package upgrades only refresh the
symlinks, so binaries are never patched twice (patchelf cannot reliably grow
an already patched file).  Shebangs naming interpreters absent from Guix
System are repaired as well.
"""

import fcntl
import json
import os
import re
import shutil
import stat
import subprocess
import sys

PATCHELF = "@PATCHELF@"
INTERPRETER = "@INTERPRETER@"
LIBRARY_PATH = "@LIBRARY_PATH@"
FIXUP_ID = "@FIXUP_ID@"

# Bump when the patching scheme changes so existing bundles are redone.
FORMAT = "1"
# Libraries most bundled objects expect from the host system.
PRELOAD = ("libstdc++.so.6", "libgcc_s.so.1", "libz.so.1")
ELF_MACHINES = {"x86_64": 0x3E, "aarch64": 0xB7}
UPSTREAM_PREFIX = re.compile(rb"^#!\s*/\S*/codex-primary-runtime/(dependencies/\S+)")
SHEBANG = re.compile(rb"^#!\s*(/\S+)")


def is_elf(path, machine):
    """Return True for an executable or shared ELF object for MACHINE."""
    try:
        with open(path, "rb") as f:
            head = f.read(20)
    except OSError:
        return False
    return (len(head) == 20 and head[:4] == b"\x7fELF" and head[4] == 2
            and int.from_bytes(head[16:18], "little") in (2, 3)
            and int.from_bytes(head[18:20], "little") == machine)


def patchelf(*args):
    return subprocess.run([PATCHELF, *args], capture_output=True, text=True)


def unresolved(path, needed, entries):
    """Return the NEEDED entries of PATH not found via ENTRIES or glibc."""
    origin = os.path.dirname(path)
    directories = [entry.replace("${ORIGIN}", origin).replace("$ORIGIN", origin)
                   for entry in entries] + [os.path.dirname(INTERPRETER)]
    return [lib for lib in needed
            if "/" not in lib
            and not any(os.path.exists(os.path.join(directory, lib))
                        for directory in directories)]


def fix_elf(path, interpreter, libdir):
    """Patch PATH, touching shared libraries only when they need it.

    Rewriting some prebuilt libraries (sharp's libvips) breaks them, so
    executables load PRELOAD up front and libraries are patched only for
    other dependencies, such as LibreOffice's NSS and lcms2.
    """
    current = patchelf("--print-interpreter", path)
    executable = (current.returncode == 0
                  and os.path.basename(current.stdout.strip())
                  .startswith("ld-linux"))
    needed = patchelf("--print-needed", path).stdout.split()
    entries = [entry for entry
               in patchelf("--print-rpath", path).stdout.strip().split(":")
               if entry]
    changes = []
    if executable:
        if current.stdout.strip() != interpreter:
            changes += ["--set-interpreter", interpreter]
        for lib in PRELOAD:
            if lib not in needed and os.path.exists(os.path.join(libdir, lib)):
                changes += ["--add-needed", lib]
    missing = unresolved(path, needed, entries)
    if (executable or set(missing) - set(PRELOAD)) and libdir not in entries:
        changes += ["--set-rpath", ":".join(entries + [libdir])]
    if not changes:
        return None
    result = patchelf(*changes, path)
    return None if result.returncode == 0 else result.stderr.strip()


def fix_shebang(path, root):
    with open(path, "rb") as f:
        data = f.read()
    first, sep, rest = data.partition(b"\n")
    match = UPSTREAM_PREFIX.match(first)
    if match:
        # Console scripts naming the upstream build directory: run the
        # interpreter next to them through a relocatable sh trampoline, as
        # pip does for overlong shebangs.
        target = os.path.relpath(os.path.join(root, match.group(1).decode()),
                                 os.path.dirname(path))
        new = (b"#!/bin/sh\n'''exec' \"${0%/*}/" + target.encode()
               + b"\" \"$0\" \"$@\"\n' '''")
    else:
        match = SHEBANG.match(first)
        if not match or os.path.exists(match.group(1)):
            return
        name = os.path.basename(match.group(1))
        new = b"#!/usr/bin/env " + name + first[match.end():]
    with open(path, "wb") as f:
        f.write(new + sep + rest)


def writable(path, action):
    mode = os.lstat(path).st_mode
    if not mode & stat.S_IWUSR:
        os.chmod(path, mode | stat.S_IWUSR)
    try:
        return action()
    finally:
        if not mode & stat.S_IWUSR:
            os.chmod(path, stat.S_IMODE(mode))


def read(path):
    try:
        with open(path) as f:
            return f.read()
    except OSError:
        return None


def write(path, content):
    with open(path + ".tmp", "w") as f:
        f.write(content)
    os.replace(path + ".tmp", path)


def refresh_links(guix):
    """Point GUIX at the loader and libraries of this package build."""
    if read(os.path.join(guix, "id")) == FIXUP_ID:
        return
    staging = guix + ".new"
    shutil.rmtree(staging, ignore_errors=True)
    os.makedirs(os.path.join(staging, "lib"))
    os.symlink(INTERPRETER, os.path.join(staging, "ld.so"))
    for directory in LIBRARY_PATH.split(":"):
        for name in sorted(os.listdir(directory)):
            link = os.path.join(staging, "lib", name)
            if ".so" in name and not os.path.lexists(link):
                os.symlink(os.path.join(directory, name), link)
    # The bundled Git is built on Debian against libcurl-gnutls.  Guix's
    # libcurl uses GnuTLS too and has no symbol versions, so alias it.
    curl = os.path.join(staging, "lib", "libcurl.so.4")
    if os.path.lexists(curl):
        os.symlink(os.readlink(curl),
                   os.path.join(staging, "lib", "libcurl-gnutls.so.4"))
    write(os.path.join(staging, "id"), FIXUP_ID)
    if os.path.isdir(guix):
        old = guix + ".old"
        shutil.rmtree(old, ignore_errors=True)
        os.rename(guix, old)
        os.rename(staging, guix)
        shutil.rmtree(old, ignore_errors=True)
    else:
        os.rename(staging, guix)


def patch_bundle(root, guix):
    interpreter = os.path.join(guix, "ld.so")
    libdir = os.path.join(guix, "lib")
    machine = ELF_MACHINES.get(os.uname().machine)
    failures = 0
    for directory, subdirs, files in os.walk(root):
        for name in files:
            path = os.path.join(directory, name)
            if os.path.islink(path) or not os.path.isfile(path):
                continue
            try:
                if is_elf(path, machine):
                    error = writable(
                        path, lambda: fix_elf(path, interpreter, libdir))
                    if error:
                        failures += 1
                        print(f"{path}: {error}", file=sys.stderr)
                elif os.access(path, os.X_OK):
                    with open(path, "rb") as f:
                        if f.read(2) == b"#!":
                            writable(path, lambda: fix_shebang(path, root))
            except OSError as error:
                failures += 1
                print(f"{path}: {error}", file=sys.stderr)
    return failures


def guix_directory():
    cache = (os.environ.get("XDG_CACHE_HOME")
             or os.path.join(os.path.expanduser("~"), ".cache"))
    return os.path.join(cache, "chatgpt-desktop", "guix-runtime")


def main(root):
    root = os.path.realpath(root)
    try:
        with open(os.path.join(root, "runtime.json")) as f:
            version = json.load(f).get("bundleVersion") or "unknown"
    except (OSError, ValueError):
        return 0
    guix = guix_directory()
    os.makedirs(os.path.dirname(guix), exist_ok=True)
    stamp = os.path.join(root, ".guix-patched")
    with open(guix + ".lock", "a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        refresh_links(guix)
        expected = f"{version}\n{FORMAT}\n"
        if read(stamp) == expected:
            return 0
        failures = patch_bundle(root, guix)
        write(stamp, expected)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
