#!/bin/bash

# Format Ruby source from stdin with the rubocop that lives inside the
# project's "api" Docker container. Everything is derived from the file's
# own path, so it works in any checkout/worktree of the project.
# Usage: docker-rubocop.sh <absolute-host-path-of-file>

set -euo pipefail

FILE="$1"
ROOT="$(cd "$(dirname "$FILE")" && git rev-parse --show-toplevel)"

# Rubocop needs the path relative to /app inside the container to
# resolve .rubocop.yml, so strip the host-side prefix.
REL="${FILE#"$ROOT"/api/}"

cd "$ROOT"
exec docker-compose exec -T api \
  bundle exec rubocop --stdin "$REL" --auto-correct --stderr \
  --format quiet --fail-level fatal
