# SPEC: Git Flow for apparatus

## 1. Objective

Apply the [nvie Git Flow model](https://nvie.com/posts/a-successful-git-branching-model/) to the `apparatus` sbt/Scala project using GitHub branch protection rules and GitHub Actions (GHA) workflows. The goal is a disciplined, tag-driven release process where `master` always reflects production and `develop` is the integration branch for upcoming work.

## 2. Branch Model

```
master          – production; every commit is a released version (tagged)
develop         – integration branch; next release accumulates here
feature/*       – new features branched from develop, merged back via PR
fix/*           – bug fixes for the next release, branched from develop
hotfix/*        – urgent fixes branched from master, merged to master + develop via PR
release/*       – release stabilisation; branched from develop, merged to master + develop via PR
```

### Rules
- `master` and `develop` are permanent protected branches.
- All merges into `master` or `develop` require a PR (no direct push, except the initial setup).
- Only maintainers can merge PRs into `master`.
- Tags of the form `vX.Y.Z` on `master` drive the Maven Central publish workflow.
- `hotfix/*` branches are the only branches that may be opened against `master`; all others target `develop`.

## 3. GHA Workflow Matrix

| Event | Workflow | Jobs |
|-------|----------|------|
| PR opened / updated (any branch) | `ci.yml` | test, site-build |
| Push to `develop` | `ci.yml` | test, site-build |
| Push to `master` | `ci.yml` | test, site-build, site-deploy |
| Tag push `v*` on `master` | `publish.yml` | GPG sign, publish to Maven Central, create GitHub Release |

### `ci.yml` triggers

```yaml
on:
  push:
    branches:
      - master
      - develop
      - "feature/**"
      - "fix/**"
      - "hotfix/**"
      - "release/**"
  pull_request:
```

### `publish.yml` triggers

```yaml
on:
  push:
    tags:
      - "v*"
```

## 4. Branch Protection Rules (GitHub UI / gh CLI)

### `master`
- Require PR before merging
- Required status checks: `test` (from `ci.yml`)
- Dismiss stale reviews on new commits
- Restrict who can merge: maintainers only
- No direct pushes (including admins, recommended)
- Do not allow force-push

### `develop`
- Require PR before merging
- Required status checks: `test`
- No direct pushes
- Do not allow force-push

## 5. Typical Workflows

### New feature
```
git checkout -b feature/my-thing develop
# work...
git push origin feature/my-thing
# open PR → develop
# merge via squash or merge commit
```

### Bug fix (next release)
```
git checkout -b fix/issue-42 develop
# open PR → develop
```

### Hotfix (production)
```
git checkout -b hotfix/critical-thing master
# open PR → master  (maintainer merges, then tags)
# open PR → develop (backport)
```

### Release
```
git checkout -b release/1.2.0 develop
# bump version in build.sbt if needed, update changelog
# open PR → master
# maintainer merges, then: git tag v1.2.0 && git push origin v1.2.0
# open backport PR → develop
```

## 6. Versioning Convention

- Tags: `vMAJOR.MINOR.PATCH` (semver)
- Hotfix bumps PATCH: `v1.2.0` → `v1.2.1`
- Feature/release bumps MINOR: `v1.2.0` → `v1.3.0`
- Breaking changes bump MAJOR

## 7. Acceptance Criteria

- [ ] `master` and `develop` have branch protection rules applied
- [ ] `ci.yml` runs on PRs and relevant branch pushes only
- [ ] `publish.yml` triggers on `v*` tag push, not on every master push
- [ ] A GitHub Release is created automatically on tag push with auto-generated release notes
- [ ] A hotfix PR can target `master` without touching `develop` first
- [ ] A feature PR targets `develop` and cannot be merged to `master` directly

## 8. Out of Scope

- Series/maintenance branches (`series/1.x`)
- Changelog generation tooling (beyond GitHub's auto-generated release notes)
- Automated version bumping (sbt-dynver handles snapshot versions; release tags are set manually)
