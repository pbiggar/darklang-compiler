# Pretty-printing parity

Current compiler source review: 2026-10-07 at `7154b0ea9c1a3f53d30984ed17b9e0cc5d8f0dce`.
See the [current audit](../current-audit.md) for post-port status and validation.
Older revision pairs and executed counts below are historical evidence, not
a fresh test result for this revision.

The compiler copies `Stdlib.Pretty` from darklang/dark release
`v0.0.35`, revision `0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`.

The public `Doc` and `Mode` types and the `empty`, `line`, `hardLine`,
`softLine`, `text`, `styled`, `concat`, `concatAll`, `concatSpace`,
`concatLine`, `nest`, `group`, `hsep`, `vsep`, `hang`, `join`, and `render`
values are present. Rendering uses `String.displayWidth`, so zero-width styling
and wide Unicode text do not distort group-fit decisions. `HardLine` forces an
enclosing group to break; `Line` becomes a space in flat mode; `SoftLine`
becomes empty in flat mode; and nesting is applied after broken lines.

The compiler source qualifies names, supplies a small number of type arguments
where inference is ambiguous, and names the internal group constructor
`PrettyGroup` to avoid collision with `Regex.Group`. The public `group`
constructor function and observable layouts match upstream.

The unchanged upstream fixture remains whole-file gated. A fresh probe at the
audited revision passes 34 cases and fails L186: the multiline nested `concat`
application reports “Expected TUnit, got TFunction”. This reproduces a specific
application-layout failure after the parser migration. See the
[current results](../current-audit.md#fresh-probes-of-whole-file-gates).
Focused E2E coverage in `stdlib/copied_pure_surfaces.e2e` renders flat and broken
groups, joined and vertically separated documents, nesting, Unicode widths,
and styled text. Re-enable passing cases after addressing the isolated failure.
