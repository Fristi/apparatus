FROM mcr.microsoft.com/devcontainers/java:1-21-bookworm

ARG TARGETARCH
ARG DOCKER_CLI_VERSION=29.7.0

# Arch-independent launcher (self-executing jar). Native cs-*-pc-linux.gz
# assets are x86_64-only in current coursier releases, which breaks arm64.
RUN curl -fL -o /usr/local/bin/cs \
      https://github.com/coursier/coursier/releases/latest/download/coursier \
  && chmod +x /usr/local/bin/cs

# Static Docker CLI only — daemon is the host's via mounted docker.sock.
# Avoids apt docker.io / DinD features that fail under JetBrains builds.
RUN DOCKER_ARCH="$( [ "${TARGETARCH}" = "arm64" ] && echo aarch64 || echo x86_64 )" \
  && curl -fL "https://download.docker.com/linux/static/stable/${DOCKER_ARCH}/docker-${DOCKER_CLI_VERSION}.tgz" \
    | tar -xz -C /tmp \
  && install -m 0755 /tmp/docker/docker /usr/local/bin/docker \
  && rm -rf /tmp/docker \
  && groupadd -f docker \
  && usermod -aG docker vscode

# Login shells reset PATH from /etc/profile, so the image-wide ENV below is not
# enough for terminals — profile.d restores it for bash and zsh alike.
RUN echo 'export PATH="$HOME/.local/share/coursier/bin:$PATH"' \
      > /etc/profile.d/coursier.sh

# The Coursier apps (sbt, metals, scala-cli, cellar) are installed by
# post-create.sh instead of here. Baking them in adds ~300 MB across several
# thousand files — Metals alone is 139 MB over 1885 files — and Envbuilder's
# Kaniko snapshotter stalls or gets OOM-killed hashing a layer that size.
ENV COURSIER_BIN_DIR=/home/vscode/.local/share/coursier/bin
ENV PATH="${COURSIER_BIN_DIR}:${PATH}"
