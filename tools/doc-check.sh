#!/usr/bin/env bash
# Snippet-check the full documentation corpus: repo docs + the published
# tutorial on the site. Kept as a script so nobody has to remember the
# file list — an incomplete list is the failure mode this guards against.
set -euo pipefail
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SITE="${SKALP_SITE_DIR:-$HOME/src/website/mikaana}"
export SKALP_STDLIB_PATH="${SKALP_STDLIB_PATH:-$REPO/crates/skalp-stdlib}"

files=(
  "$REPO/docs/LANGUAGE_SPECIFICATION.md"
  "$REPO/docs/user/guides/"*.md
  "$REPO/docs/user/reference/"*.md
)
if [ -d "$SITE/content/tutorial/skalp" ]; then
  files+=("$SITE/content/tutorial/skalp/"*.md)
else
  echo "note: site not found at $SITE — checking repo docs only (set SKALP_SITE_DIR)" >&2
fi

exec python3 "$REPO/tools/doc_snippet_check.py" "${files[@]}"
