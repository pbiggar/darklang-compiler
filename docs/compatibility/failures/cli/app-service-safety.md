# cli/app-service-safety.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark) · [File list](../../current-audit.md)

Executed 9 assertions: **0 passed, 9 failed**.
Of 9 previously disabled assertions, **0 passed and 9 failed**.

| Test | Observed failure |
| --- | --- |
| [L11](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark#L11) — Darklang.Cli.Apps.Model.isValidEntrypoint "Darklang.Cli.Apps.Examples.heartbeat" | Unknown function or value 'Darklang.Cli.Apps.Model.isValidEntrypoint' |
| [L14](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark#L14) — (Darklang.Cli.Apps.Model.isValidEntrypoint "x'; touch /tmp/pwned; echo '") | Unknown function or value 'Darklang.Cli.Apps.Model.isValidEntrypoint' |
| [L16](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark#L16) — (Darklang.Cli.Apps.Model.isValidEntrypoint "x\"; id; \"") | Unknown function or value 'Darklang.Cli.Apps.Model.isValidEntrypoint' |
| [L18](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark#L18) — (Darklang.Cli.Apps.Model.isValidEntrypoint "") | Unknown function or value 'Darklang.Cli.Apps.Model.isValidEntrypoint' |
| [L23](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark#L23) — (Darklang.Cli.Apps.Model.isValidAppName "Dark Packages HTTP Server") | Unknown function or value 'Darklang.Cli.Apps.Model.isValidAppName' |
| [L25](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark#L25) — Darklang.Cli.Apps.Model.isValidAppName "evil\nExecStartPre=/bin/sh -c id" | Unknown function or value 'Darklang.Cli.Apps.Model.isValidAppName' |
| [L28](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark#L28) — (Darklang.Cli.Apps.Model.isValidAppName "") | Unknown function or value 'Darklang.Cli.Apps.Model.isValidAppName' |
| [L33](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark#L33) — Darklang.Cli.Apps.Model.isValidEntrypointOf (Darklang.Cli.Apps.Model.Target.Foreground "anything at all") | Unknown function or value 'Darklang.Cli.Apps.Model.isValidEntrypointOf' |
| [L36](../../../../test/fixtures/e2e/upstream/cli/app-service-safety.dark#L36) — Darklang.Cli.Apps.Model.isValidEntrypointOf (Darklang.Cli.Apps.Model.Target.Daemon "x'; id; '") | Unknown function or value 'Darklang.Cli.Apps.Model.isValidEntrypointOf' |
