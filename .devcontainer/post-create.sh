#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

# Named volumes start empty (and root-owned until post-start chown).
echo "==> Ensuring Coursier apps (sbt, metals, scala-cli, cellar)"
cs install sbt metals scala-cli
cs install --contrib cellar

echo "==> Installing npm dependencies (VitePress)"
npm ci

echo "==> Warming sbt (launcher, project deps, and a remote cache pull)"
if [[ -n "${BUILDBUDDY_API_KEY:-}" ]]; then
  echo "    BuildBuddy remote cache enabled"
else
  echo "    BUILDBUDDY_API_KEY unset — local disk cache only"
fi
# Non-fatal: an unreachable cache or a compile error should not fail container creation.
sbt --batch -Dsbt.supershell=false "core/compile; doobie/compile" \
  || echo "    warning: warm-up build failed; run 'sbt core/compile' manually"

echo "==> Dev container ready"
echo "    Scala tests:  sbt tests/test"
echo "    Docs site:    sbt docs/mdoc && npm run docs:dev"
echo "    Dep lookup:   cellar get --module core apparatus.core.Apparatus"
