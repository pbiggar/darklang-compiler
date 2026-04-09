# Dark Compiler

## Architecture

The compiler targets ARM64 (macOS + Linux) and x86_64 (Linux). Backend is
selected at runtime via `PlatformTypes.detectArch()`.

Passes 1-5 (Parser → TypeCheck → ANF → MIR → LIR → RegisterAllocation) are
**fully shared**. Passes 6-8 are per-architecture in `passes/arm64/` and
`passes/x64/`.

See `docs/architecture.md` and `docs/compiler-passes.md` for details.

## Running Commands — Use the Devcontainer

All build/test commands require .NET 10, which lives inside the Docker
container. **Always run commands via `docker exec`.**

The container name may vary — check `docker ps` for the actual name.
The repo is mounted at `/workspace` inside the container.

```bash
# Find the container name
CONTAINER=$(docker ps --format "{{.Names}}" | head -1)

# Build and run all tests
docker exec -w /workspace $CONTAINER ./run-tests

# Run filtered tests
docker exec -w /workspace $CONTAINER ./run-tests --filter=x86 --quiet

# Quick expression test
docker exec -w /workspace $CONTAINER ./dark -r -e "2 + 3"

# Build only
docker exec -w /workspace $CONTAINER dotnet build --verbosity quiet
```

If the container isn't running: `docker compose up -d` in the repo root.

**Do not install tooling on the host.** If something is needed persistently,
add it to the Dockerfile.

## Current Status

See [TODOs.md](TODOs.md) for active work and backlog.
