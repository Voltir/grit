#!/usr/bin/env bash
# Installs the pinned enola binary into tools/enola -- project-local rather than on
# PATH, because grit has no devcontainer and no CI to carry a system install.
# Re-run to re-fetch.
#
# TRAP: two things drift out from under this pin. `enola upgrade` replaces the binary in
# place, so anyone who runs it diverges from what this script installed. And a release
# carries an EXTRACTOR version of its own (`enola --version --json`) which decides how
# facts are read -- so a bump here can move the snapshot with no repository code
# changing. Until the baseline is re-pinned, `enola check` declines to grade (exit 3) and
# scripts/enola-law.sh fails, printing how to re-pin.
#
# TRAP: the pinned version is NOT a free upgrade decision. grit uses capture-checking syntax
# project-wide, and up to 0.4.24 enola's Scala extractor silently degraded on it, dropping and
# mis-owning symbols while the receipt said `parse_errors: 0`. The receipt cannot report this
# damage, so a bump is measured first, against the version it replaces:
# - a controlled fixture: files of capture syntax beside the same files without it, and a pair
#   with separation-checking syntax (`consume`, `^` on parameters, `caps.Mutable` and its
#   `update` methods) beside its plain twin. Each version must extract the same symbols with
#   the same owners from both sides of a pair, and the new version no fewer than the old;
# - grit's import-edge and module facts (they gate): every difference explained;
# - scripts/enola-plant: every plant caught.
# The current pin passed all three; the commit that set it records the numbers.
set -euo pipefail

ENOLA_VERSION=0.4.38
# The release's published digests, pinned here as well, so a re-uploaded asset fails the check.
ENOLA_SHA256_AMD64=2e66b79afc8b6840129bddf98092724e359499d074cd79f18833a181b87422cc
ENOLA_SHA256_ARM64=d63ef65f60440b5c80ed91a5043b8e1eeb545c413143d262e6f2a2a33b3dfffa
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dest="$repo_root/tools"

case "$(uname -m)" in
  x86_64) arch=amd64 sha256=$ENOLA_SHA256_AMD64 ;;
  aarch64 | arm64) arch=arm64 sha256=$ENOLA_SHA256_ARM64 ;;
  *) echo "unsupported arch: $(uname -m)" >&2; exit 1 ;;
esac

base="https://github.com/enola-labs/enola/releases/download/v${ENOLA_VERSION}"
name="enola-${ENOLA_VERSION}-linux-${arch}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

curl -fsSL -o "$work/enola.tar.gz" "${base}/${name}.tar.gz"
curl -fsSL -o "$work/enola.sha256" "${base}/${name}.sha256"

# The published checksum must be the pinned one, and the download must match it. The file
# names the release asset while the download is a temporary path, so only the digest is reused.
published="$(cut -d' ' -f1 "$work/enola.sha256")"
[ "$published" = "$sha256" ] || { echo "published sha256 $published is not the pinned $sha256" >&2; exit 1; }
echo "$sha256  $work/enola.tar.gz" | sha256sum -c - >/dev/null

tar -xzf "$work/enola.tar.gz" -C "$work" "$name"
mkdir -p "$dest"
install -m 0755 "$work/$name" "$dest/enola"
"$dest/enola" --version
