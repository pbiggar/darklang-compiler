# Pretty-printing parity

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

The unchanged upstream fixture remains gated because its shared helper contains
a call shape affected by the application-grouping frontend gap. Focused E2E
coverage renders flat and broken groups, joined and vertically separated
documents, nesting, Unicode widths, and styled text.
