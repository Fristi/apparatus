#!/usr/bin/env bash
# Runs on every container start: fix volume ownership + docker.sock access.
set -euo pipefail

log() { printf '==> [%s] %s\n' "$(date -u +%H:%M:%SZ)" "$*"; }

# Named volumes are created root-owned; sbt/coursier need write access.
# sbt 2 uses ~/.config/sbt as its global base and ~/.cache/sbt/v2 as the
# machine-wide task cache — neither lives under ~/.sbt anymore.
log "Fixing ownership of cache volumes (slow on a warm Coursier cache)"
for dir in \
  /home/vscode/.config/sbt \
  /home/vscode/.ivy2 \
  /home/vscode/.cache \
  /home/vscode/.cache/coursier \
  /home/vscode/.cache/sbt \
  /home/vscode/.local/share/coursier
do
  sudo mkdir -p "$dir"
done
sudo chown -R vscode:vscode \
  /home/vscode/.config/sbt \
  /home/vscode/.ivy2 \
  /home/vscode/.cache \
  /home/vscode/.local/share/coursier

# Mirror the injected key to disk so clients that attach without Coder's
# environment (plain devcontainer CLI, JetBrains Gateway) still get cache hits.
# Envbuilder (Coder) bakes this dev container into the workspace pod itself and
# rebuilds the image on every start, so only /workspaces survives. VS Code then
# arrives over plain Remote-SSH, which reads no part of customizations.vscode, so
# nothing here is set up on its behalf unless we do it.
if [[ -d /.envbuilder && -d /workspaces ]]; then
  SERVER_HOME="${HOME:-/root}/.vscode-server"

  # Keep the server and its extensions across a restart.
  log "Persisting the VS Code server on /workspaces"
  PERSISTED=/workspaces/.vscode-server
  mkdir -p "$PERSISTED"
  if [[ -e "$SERVER_HOME" && ! -L "$SERVER_HOME" ]]; then
    cp -a "$SERVER_HOME/." "$PERSISTED/"
    rm -rf "$SERVER_HOME"
  fi
  ln -sfn "$PERSISTED" "$SERVER_HOME"

  # Recreating the workspace wipes /workspaces too, so the image is the only
  # copy that always survives. The server rebuilds extensions.json by scanning
  # this directory, which is why dropping the folders in is enough.
  if [[ -d /opt/vscode-extensions ]]; then
    log "Seeding VS Code extensions from the image"
    mkdir -p "$SERVER_HOME/extensions"
    for seeded in /opt/vscode-extensions/*/; do
      target="$SERVER_HOME/extensions/$(basename "$seeded")"
      [[ -e "$target" ]] || cp -a "$seeded" "$target"
    done
  fi
fi

CREDENTIAL_FILE=/home/vscode/.config/sbt/buildbuddy_credential.txt
if [[ -n "${BUILDBUDDY_API_KEY:-}" ]]; then
  umask 077
  printf 'x-buildbuddy-api-key=%s\n' "$BUILDBUDDY_API_KEY" > "$CREDENTIAL_FILE"
  chmod 600 "$CREDENTIAL_FILE"
fi

log "Wiring up docker.sock"
SOCK=/var/run/docker.sock
if [[ ! -S "$SOCK" ]]; then
  echo "warning: $SOCK not mounted — Testcontainers/docker will not work"
  exit 0
fi

SOCK_GID="$(stat -c '%g' "$SOCK" 2>/dev/null || stat -f '%g' "$SOCK")"
if getent group "$SOCK_GID" >/dev/null; then
  GROUP_NAME="$(getent group "$SOCK_GID" | cut -d: -f1)"
else
  GROUP_NAME=dockerhost
  sudo groupadd -g "$SOCK_GID" "$GROUP_NAME" 2>/dev/null \
    || GROUP_NAME="$(getent group "$SOCK_GID" | cut -d: -f1)"
fi
sudo usermod -aG "$GROUP_NAME" vscode 2>/dev/null || true

# Bounded: an unresponsive daemon behind the socket blocks `docker info`.
if ! timeout 30s docker info >/dev/null 2>&1; then
  sudo chmod 666 "$SOCK" || true
fi

log "Container start hooks done"
