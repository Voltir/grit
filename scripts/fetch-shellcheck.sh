#!/usr/bin/env bash
# Installs the pinned shellcheck into tools/shellcheck, for grit.claudecode's ShellcheckTests,
# which lint the join script as the adapter serves it and the hook as the join installs it.
# Project-local, like scripts/fetch-enola.sh: grit has no CI to carry a system install. Re-run
# to re-fetch.
#
# The release publishes no checksum file; the digest below is GitHub's own for the asset
# (the release API's `digest`), pinned here, so a re-uploaded asset fails the check. A bump
# changes both, and is a commit of its own: a new version may find what the old did not.
set -euo pipefail

SHELLCHECK_VERSION=0.11.0
SHA256=b7af85e41cc99489dcc21d66c6d5f3685138f06d34651e6d34b42ec6d54fe6f6 # linux.x86_64.tar.gz
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dest="$repo_root/tools"

if [ "$(uname -s)-$(uname -m)" != Linux-x86_64 ]; then
  echo "only shellcheck's linux.x86_64 build is pinned; this is $(uname -s)-$(uname -m)" >&2
  exit 1
fi

name="shellcheck-v${SHELLCHECK_VERSION}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
curl -fsSL -o "$work/shellcheck.tar.gz" \
  "https://github.com/koalaman/shellcheck/releases/download/v${SHELLCHECK_VERSION}/${name}.linux.x86_64.tar.gz"
echo "$SHA256  $work/shellcheck.tar.gz" | sha256sum -c - >/dev/null
tar -xzf "$work/shellcheck.tar.gz" -C "$work" "$name/shellcheck"
mkdir -p "$dest"
install -m 0755 "$work/$name/shellcheck" "$dest/shellcheck"
"$dest/shellcheck" --version
