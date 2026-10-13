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
# project-wide, and up to 0.4.24 enola's Scala extractor silently degraded on it: on a
# controlled fixture (five files of capture syntax beside the same five without it), 0.4.8,
# 0.4.15 and 0.4.22 extracted 40 symbols against the plain files' 46, dropping 7 and
# mis-owning 11, while the receipt said `parse_errors: 0`. 0.4.29 extracts all 46 with their
# right owners, and its receipt says `parse_errors: 0` too: the receipt cannot report this
# damage, so a bump re-runs that comparison. It also diffs grit's import-edge and module facts
# against the previous version's (they gate) and reruns scripts/enola-plant.
set -euo pipefail

ENOLA_VERSION=0.4.29
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dest="$repo_root/tools"

case "$(uname -m)" in
  x86_64) arch=amd64 ;;
  aarch64 | arm64) arch=arm64 ;;
  *) echo "unsupported arch: $(uname -m)" >&2; exit 1 ;;
esac

base="https://github.com/enola-labs/enola/releases/download/v${ENOLA_VERSION}"
name="enola-${ENOLA_VERSION}-linux-${arch}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

curl -fsSL -o "$work/enola.tar.gz" "${base}/${name}.tar.gz"
curl -fsSL -o "$work/enola.sha256" "${base}/${name}.sha256"

# Published checksums, so they are checked. The file names the release asset while the
# download is a temporary path, so only the digest is reused.
echo "$(cut -d' ' -f1 "$work/enola.sha256")  $work/enola.tar.gz" | sha256sum -c - >/dev/null

tar -xzf "$work/enola.tar.gz" -C "$work" "$name"
mkdir -p "$dest"
install -m 0755 "$work/$name" "$dest/enola"
"$dest/enola" --version
