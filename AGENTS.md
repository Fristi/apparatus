# Agent guide

Apparatus is a Scala 3 library for composing basic, eventful and networked finite
state machines. The sbt modules are `core`, `doobie`, `examples`, `tests` and `docs`.

## Looking up Scala APIs

Two complementary tools are available in the dev container. Prefer them over
guessing type signatures.

**Metals MCP** — everything about *this* codebase: compilation, diagnostics,
tests, go-to-definition, find usages, symbol search. Metals starts its MCP server
automatically and writes the connection details to `.vscode/mcp.json` (or
`.cursor/mcp.json`). The port changes per session, so never hard-code it.

**cellar** — the public API of *external* Maven dependencies. It is a plain CLI,
not an MCP server, so call it through the shell:

```sh
cellar get-external org.typelevel:cats-core_3:2.13.0 cats.Monad
cellar list-external dev.zio:zio-blocks-schema_3:0.0.33 zio.blocks.schema
cellar search-external org.tpolecat:doobie-core_3:1.0.0-RC12 transactor
cellar get-source org.typelevel:cats-effect_3:3.5.4 cats.effect.Ref
cellar deps org.typelevel:cats-effect_3:3.5.4
```

Coordinates must be explicit (`group:artifact_3:version`); `latest` resolves the
newest release. For symbols in this project use `cellar get --module core <fqn>`,
though Metals is usually the better answer there.

## Common commands

```sh
sbt "core/test; doobie/test"   # what CI runs
sbt tests/test                 # integration tests (needs Docker for Testcontainers)
sbt docs/mdoc                  # regenerate docs/*.md from docs-src
npm run docs:dev               # VitePress dev server on port 5173
```

`tests/` uses Testcontainers with PostgreSQL, and `MermaidRenderSpec` shells out
to `docker run`. Both need a working Docker daemon.

## Remote build cache

The build runs on sbt 2, where every task result is cached to `~/.cache/sbt/v2`
and, when a BuildBuddy key is present, shared through a Bazel-compatible gRPC
cache. CI is the producer; workspaces are mostly consumers.

`build.sbt` looks for the key in `BUILDBUDDY_API_KEY` first, then in
`~/.config/sbt/buildbuddy_credential.txt`. With neither, `remoteCache` stays
`None` and the build falls back to the local disk cache, so a clone without an
account still works.

Success lines report where the results came from — `cache 100%, 12 disk cache
hits` means nothing was recompiled. To confirm the remote half is doing the
work, delete `~/.cache/sbt/v2` and build again; hits after that can only be
coming from BuildBuddy.

On Coder the key comes from `coder_agent.env`:

```terraform
variable "buildbuddy_api_key" {
  type      = string
  sensitive = true
}

resource "coder_agent" "dev" {
  env = {
    BUILDBUDDY_API_KEY = var.buildbuddy_api_key
  }
  # ...
}
```

That covers everything the agent spawns — terminals, the VS Code server, and the
sbt that Metals starts over BSP — so ordinary work gets remote cache hits. It does
*not* cover the warm-up compile in `post-create.sh`, which runs before the agent
exists (see below); that one falls back to the local disk cache, and the
credential file `post-start.sh` mirrors is never written there.

## Where the dev container actually runs

`.devcontainer/devcontainer.json` is consumed by three different clients, and
only one of them honours all of it.

**VS Code Dev Containers**, locally, is the reference implementation: image,
`containerEnv`, mounts and `customizations.vscode` all apply.

**JetBrains Gateway** builds the image and mounts but drops
`customizations.vscode` — IDEA supplies its own Scala support, so nothing is lost.

**Coder** runs this workspace through **Envbuilder**, which is not a dev container
at all. Envbuilder bakes the Dockerfile, the features and `containerEnv` directly
into the workspace pod, then hands over to the coder agent; VS Code connects
afterwards over plain Remote-SSH. Four consequences worth knowing:

- `customizations.vscode` is dropped entirely. Editor configuration that has to
  survive lives in `.vscode/` (`settings.json`, `extensions.json`), which
  Remote-SSH does read as workspace settings. Keep container-absolute paths such
  as `metals.javaHome` out of there — the same file is read on a laptop.
- Only `/workspaces` is a persistent volume; the rest of the pod is rebuilt from
  cache on every start. `post-start.sh` therefore relocates `~/.vscode-server`
  onto `/workspaces` so installed extensions survive a restart.
- Lifecycle scripts run before the agent, so agent-injected environment is absent
  during `postCreateCommand`, and `${localEnv:...}` in `containerEnv` resolves
  against nothing.
- There is no Docker daemon in the pod. `sbt tests/test` and `MermaidRenderSpec`
  cannot run on Coder; use a local dev container for those.

The checkout is at `/workspaces/apparatus` and the session user is `root`, while
the toolchain was installed for `vscode` and lives on `PATH` at
`/home/vscode/.local/share/coursier/bin`.

## Conventions

- Scala 3.8.3 across all modules; keep `commonSettings` as the single source.
- sbt 2.0.4; its global base is `~/.config/sbt`, not `~/.sbt`.
- `docs/*.md` is generated by mdoc from `docs-src/` — edit the source, not the output.
