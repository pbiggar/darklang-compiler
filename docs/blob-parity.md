# Blob, Base64, Crypto, and X509 parity

This contract records the binary API comparison made from compiler HEAD
`bcd4d46f49a12aaab8588844b72a9afa1f34a0db` and darklang/dark revision
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. DCB1 report commit `8a402797`
and the earlier compiler evidence revision
`51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` were used only to locate
candidates. Every retained result was revalidated against the pinned sources
and focused native probes. Performance differences are outside this contract.

## Public contract

`Blob` is the only binary runtime type. It is an immutable, managed handle with
the existing length/data/padding/refcount allocation layout. Strings and Blobs
have distinct source and IR ownership identities even though their variable-size
allocation shapes are deliberately similar.

| Name | Exact signature |
| --- | --- |
| `Blob.empty` | `Blob` value |
| `Blob.length` | `Blob -> Int` |
| `Blob.concat` | `List<Blob> -> Blob` |
| `Blob.slice` | `Blob -> Int -> Int -> Blob` |
| `Blob.toHex` | `Blob -> String` |
| `Blob.fromHex` | `String -> Result<Blob, String>` |
| `Blob.toBase64` | `Blob -> String` |
| `Blob.fromBase64` | `String -> Result<Blob, String>` |
| `Blob.toString` | `Blob -> Result<String, String>` |
| `Blob.fromString` | `String -> Blob` |
| `Blob.toList` | `Blob -> List<UInt8>` |
| `Blob.fromList` | `List<UInt8> -> Blob` |
| `Base64.decode` | `String -> Result<Blob, String>` |
| `Base64.encode` | `Blob -> String` |
| `Base64.urlEncode` | `Blob -> String` |
| `Crypto.md5` | `Blob -> Blob` |
| `Crypto.sha256` | `Blob -> Blob` |
| `Crypto.sha384` | `Blob -> Blob` |
| `Crypto.sha256hmac` | `Blob -> Blob -> Blob` (key, then data) |
| `Crypto.sha1hmac` | `Blob -> Blob -> Blob` (key, then data) |
| `X509.pemCertificatePublicKey` | `String -> Result<String, String>` |

All constructors and transformations return fresh handles, including empty
`concat`, `slice`, codec results, and list/string conversions. Rebinding or
passing a Blob preserves its handle. `Blob.empty` is the one immortal canonical
empty handle. Equality compares handles: two fresh equal payloads are unequal,
the same rebound handle is equal, and repeated `Blob.empty` references are
equal. Container equality applies that rule recursively. Native value rendering
is the interpreter spelling `<Blob: ephemeral>` and does not reveal bytes or a
process-local address.

## Bounds and failures

| Operation | Observable behavior |
| --- | --- |
| `Blob.slice` | Clamp start to `0..length`; clamp negative requested length to zero; cap length at the remaining bytes. |
| `Blob.concat` | Preserve list and byte order. |
| `Blob.toHex` | Uppercase hexadecimal. |
| `Blob.fromHex` odd length | `Invalid hex string: The input is not a valid hex string as its length is not a multiple of 2.` |
| `Blob.fromHex` non-hex | `Invalid hex string: The input is not a valid hex string as it contains a non-hex character.` |
| `Blob.toString` | Strict UTF-8; reject overlong, surrogate, truncated, bad-continuation, and out-of-range sequences before allocating a String. The error is `Invalid UTF-8: Unable to translate bytes [XX] at index N from specified code page to Unicode.` for the first invalid sequence. |
| `Blob.fromBase64` | Replace URL alphabet and infer padding before applying .NET-compatible whitespace handling. Errors retain the detailed `Invalid base64 string: ` prefix and pinned `FormatException` text. |
| `Base64.decode` | Accept standard/URL alphabets, padding, unpadded 2/3-character tails, and permissive unused bits. Reject all whitespace, illegal characters, length 1 modulo 4, excessive/nonterminal padding, and return only `Not a valid base64 string`. |
| `Base64.encode`, `urlEncode` | Always padded; URL encoding substitutes `-` and `_`. |
| `X509.pemCertificatePublicKey` | Select only a `CERTIFICATE` PEM block; reject malformed Base64/definite DER, truncated lengths, private keys, CSRs, and unsupported shapes as `No certificates`. Preserve the complete SubjectPublicKeyInfo TLV and emit 64-character PEM lines plus a trailing newline. |

## Explicit compiler extensions

These names are not parity API. They remain compiler extensions over `Blob`:

- `Bytes.create/fromList/get/hexEncode/length/set/toList` keep their existing
  `Int64`/`List<Int64>` bridge. `create` is deterministically zero-filled;
  `set` is immutable, returns the same handle for an out-of-bounds index, and
  truncates values to the low eight bits. `get` retains its unchecked legacy
  bounds contract.
- `Base64.urlDecode` delegates to the parity decoder.
- `Crypto.sha1`, `Crypto.bytesToHex`, and every `Crypto.debug*` declaration use
  Blob inputs/results where binary data is involved.

These are deliberately classified extensions, not interpreter divergences.
The Blob-family public divergence table is empty.

## Source evidence and probes

Interpreter public wrappers are pinned at
`packages/darklang/stdlib/blob.dark`, `base64.dark`, `crypto.dark`, and
`x509.dark`. Runtime behavior is pinned at
`backend/src/Builtins/Builtins.Pure/Libs/Blob.fs`, `Base64.fs`, `Crypto.fs`,
and `X509.fs`; identity and rendering come from
`backend/src/LibExecution/RuntimeTypes.fs`,
`backend/src/Builtins/Builtins.Pure/Libs/NoModule.fs`, and
`packages/darklang/prettyPrinter/runtimeTypes.dark`. Pinned behavior fixtures
are `packages/darklang/stdlib/*` plus `backend/tests/Tests/Blob.Tests.fs`.

Compiler type/value registration is in `src/DarkCompiler/AST.fs` and
`Stdlib.fs`. Name resolution and equality admission are in
`passes/1.5_TypeChecking.fs`; value lowering, structural equality, and Blob
ownership flow through `passes/2_AST_to_ANF.fs`, ANF/MIR/LIR, reference-count
insertion, and both native backends. Public implementations are
`stdlib/Blob.dark`, `Base64.dark`, `Crypto.dark`, and `X509.dark`; the retained
bridge is `stdlib/Bytes.dark`.

The focused executable probes live in `src/Tests/e2e/blob.e2e`, `x509.e2e`,
the migrated local suites, and the activated pinned
`src/Tests/e2e/upstream/stdlib/{bytes,base64,crypto,x509}.dark` sources. During
implementation the focused filters passed: Blob 37/37, Base64 70/70, Crypto
125/125, and X509 20/20. The complete native suite passed 5961/5961 after the
focused work. Routine benchmark and integration verification remain the
worker-owned post-commit gate.
