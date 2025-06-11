#!/bin/bash
set -euo pipefail

# setup-dev.sh - Set up a complete Guix development environment.
# Run once with network access. Installs GNU Guix and fetches
# channel dependencies so later runs can work offline.

if [[ "$(id -u)" -ne 0 ]]; then
  echo "This script must be run as root." >&2
  exit 1
fi

# Packages required for building and running Guix
DEPS=(build-essential git wget curl xz-utils bzip2 gnupg \
      guile-3.0 guile-3.0-dev guile-gcrypt guile-gnutls \
      guile-sqlite3 guile-json guile-zlib guile-lzlib guile-avahi \
      guile-git guile-ssh guile-zstd guile-library \
      autoconf automake gettext texinfo help2man graphviz \
      pkg-config libgcrypt20-dev libsqlite3-dev)

apt-get update -y
apt-get install -y --no-install-recommends "${DEPS[@]}"

# Import Guix release signing keys from a reliable key server
KEYSERVER="https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x"
KEYS=(3CE464558A84FDC69DB40CFB090B11993D9AEBB5 \ 
      27D586A4F8900854329FF09F1260E46482E63562)
for key in "${KEYS[@]}"; do
    if ! gpg --list-keys "$key" >/dev/null 2>&1; then
        curl -fsSL "${KEYSERVER}${key}" | gpg --import -
    fi
done

# Download and run the official Guix installer
INSTALLER=/tmp/guix-install.sh
wget -O "$INSTALLER" \
  'https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh?plain=1'
chmod +x "$INSTALLER"
# Use non-interactive installation
yes | "$INSTALLER"

# Ensure guix is reachable via /usr/bin
ln -sf /usr/local/bin/guix /usr/bin/guix || true

# Update to latest Guix and populate the store with channel deps
su - "$(logname)" -c "guix pull"
su - "$(logname)" -c "guix shell -m manifest.gscm --true"

echo "Development environment ready."

