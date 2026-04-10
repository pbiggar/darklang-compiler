# Docker Development

Build and test commands require .NET 10, which lives in a Docker
devcontainer. VS Code users can open the folder as a devcontainer
directly (`.devcontainer/devcontainer.json`); command-line users can
run `docker compose up -d` from the repo root.

Either way, the repo ends up bind-mounted at `/workspace` inside the
container.

## VS Code devcontainer

The project includes `.devcontainer/devcontainer.json`, so "Reopen in
Container" just works. Recommended extensions (Ionide F#, C# DevKit)
install automatically.

## docker compose

```bash
# Build the image and start the container
docker compose up -d

# Shell in
docker compose exec dev bash

# Stop
docker compose down
```

## Running commands from the host

The `scripts/run-in-container` helper detects whether you're already
inside a container and dispatches accordingly. Use it from any script
that needs to call into the build/test environment:

```bash
scripts/run-in-container dotnet build --verbosity quiet
scripts/run-in-container ./run-tests
scripts/run-in-container ./dark -r -e "2 + 3"
```

Inside the container, the same command runs directly without extra
docker calls.

## Host policy

**Do not install toolchains on the host.** Everything goes in the
Dockerfile. If a tool is missing from the container, add it there and
rebuild the image.

## Codex / Claude inside the container

Both `@openai/codex` and `@anthropic-ai/claude-code` are preinstalled.
Their config and session data persist in Docker-managed volumes
(`codex-home`, `claude-home`), so you only have to log in once.

```bash
docker compose exec dev codex login    # first time
docker compose exec dev codex          # subsequent sessions

docker compose exec dev claude login   # first time
docker compose exec dev claude         # subsequent sessions
```

## Volumes and worktrees

`docker-compose.yml` bind-mounts the parent of the repo directory at
`/workspace`. If you use git worktrees (e.g. `main`, `wt-1`, `wt-2`),
each worktree gets its own `bin/` and `obj/` overlay volume so build
artifacts don't leak between worktrees.
