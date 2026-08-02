#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

# Timestamps: every step here can stall on the network, and without them a
# hung container start is indistinguishable from a slow one.
log() { printf '==> [%s] %s\n' "$(date -u +%H:%M:%SZ)" "$*"; }

# The image installed these already. Re-running `cs install` still round-trips
# to Maven Central for metadata, which reads as a silent hang on a slow network.
log "Checking Coursier apps (sbt, metals, scala-cli, cellar)"
for app in sbt metals scala-cli; do
  command -v "$app" >/dev/null || cs install "$app"
done
# command -v cellar >/dev/null || cs install --contrib cellar

log "Installing npm dependencies (VitePress)"
npm ci --no-audit --no-fund

if [[ -n "${APPARATUS_SKIP_WARMUP:-}" ]]; then
  log "Skipping sbt warm-up (APPARATUS_SKIP_WARMUP set)"
else
  if [[ -n "${BUILDBUDDY_API_KEY:-}" ]]; then
    log "Warming sbt — BuildBuddy remote cache enabled"
  else
    log "Warming sbt — BUILDBUDDY_API_KEY unset, local disk cache only"
  fi
  # Bounded and non-fatal: a cold dependency download, an unreachable cache or a
  # compile error should cost time, not a container that never finishes creating.
  timeout 15m sbt --batch -Dsbt.supershell=false "core/compile; doobie/compile" \
    || log "warning: warm-up did not finish; run 'sbt core/compile' by hand"
fi

log "Dev container ready"
echo "    Scala tests:  sbt tests/test"
echo "    Docs site:    sbt docs/mdoc && npm run docs:dev"
echo "    Dep lookup:   cellar get --module core apparatus.core.Apparatus"
