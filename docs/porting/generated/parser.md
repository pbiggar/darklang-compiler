# Parser porting inventory

## Current section

OCaml port in src/frontend/interpreter/, with compiler-specific adapters and split parser components.

## Intended section

- source: faithful-port
- behavior: identical
- freshness: follow-upstream
- destination: Retain the OCaml port with explicit upstream component mappings; final monorepo placement undecided.
- disposition: required

## Open questions

- Trial tokens, syntax trees, diagnostics, ranges, and validation comparisons.

## Source observations

| Identity | Upstream | Compiler candidates | Observation | Behavior | Source policy | Disposition |
| --- | --- | --- | --- | --- | --- | --- |
| parser:backend/src/LibParser/Lexer.fs | [backend/src/LibParser/Lexer.fs](https://github.com/darklang/dark/blob/a9d93efc853486244630dad693fe79f6f4882cf6/backend/src/LibParser/Lexer.fs) | [src/frontend/interpreter/Lexer.ml](../../../src/frontend/interpreter/Lexer.ml) | same-stem port candidate | unverified | faithful-port | required |
| parser:backend/src/LibParser/NameResolver.fs | [backend/src/LibParser/NameResolver.fs](https://github.com/darklang/dark/blob/a9d93efc853486244630dad693fe79f6f4882cf6/backend/src/LibParser/NameResolver.fs) | — | component mapping needed | unverified | faithful-port | required |
| parser:backend/src/LibParser/Package.fs | [backend/src/LibParser/Package.fs](https://github.com/darklang/dark/blob/a9d93efc853486244630dad693fe79f6f4882cf6/backend/src/LibParser/Package.fs) | — | component mapping needed | unverified | faithful-port | required |
| parser:backend/src/LibParser/Parser.fs | [backend/src/LibParser/Parser.fs](https://github.com/darklang/dark/blob/a9d93efc853486244630dad693fe79f6f4882cf6/backend/src/LibParser/Parser.fs) | [src/frontend/interpreter/Parser.ml](../../../src/frontend/interpreter/Parser.ml) | same-stem port candidate | unverified | faithful-port | required |
| parser:backend/src/LibParser/SourceFile.fs | [backend/src/LibParser/SourceFile.fs](https://github.com/darklang/dark/blob/a9d93efc853486244630dad693fe79f6f4882cf6/backend/src/LibParser/SourceFile.fs) | — | component mapping needed | unverified | faithful-port | required |
| parser:backend/src/LibParser/Tokenizer.fs | [backend/src/LibParser/Tokenizer.fs](https://github.com/darklang/dark/blob/a9d93efc853486244630dad693fe79f6f4882cf6/backend/src/LibParser/Tokenizer.fs) | [src/frontend/interpreter/Tokenizer.ml](../../../src/frontend/interpreter/Tokenizer.ml) | same-stem port candidate | unverified | faithful-port | required |
| parser:backend/src/LibParser/Validation.fs | [backend/src/LibParser/Validation.fs](https://github.com/darklang/dark/blob/a9d93efc853486244630dad693fe79f6f4882cf6/backend/src/LibParser/Validation.fs) | [src/frontend/interpreter/Validation.ml](../../../src/frontend/interpreter/Validation.ml) | same-stem port candidate | unverified | faithful-port | required |
| parser:backend/src/LibParser/WrittenTypes.fs | [backend/src/LibParser/WrittenTypes.fs](https://github.com/darklang/dark/blob/a9d93efc853486244630dad693fe79f6f4882cf6/backend/src/LibParser/WrittenTypes.fs) | [src/frontend/interpreter/WrittenTypes.ml](../../../src/frontend/interpreter/WrittenTypes.ml) | same-stem port candidate | unverified | faithful-port | required |
| parser:backend/src/LibParser/WrittenTypesToProgramTypes.fs | [backend/src/LibParser/WrittenTypesToProgramTypes.fs](https://github.com/darklang/dark/blob/a9d93efc853486244630dad693fe79f6f4882cf6/backend/src/LibParser/WrittenTypesToProgramTypes.fs) | — | component mapping needed | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/BindingPatternParser.ml | — | [src/frontend/interpreter/BindingPatternParser.ml](../../../src/frontend/interpreter/BindingPatternParser.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/DeclarationParser.ml | — | [src/frontend/interpreter/DeclarationParser.ml](../../../src/frontend/interpreter/DeclarationParser.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/DeclarationSupport.ml | — | [src/frontend/interpreter/DeclarationSupport.ml](../../../src/frontend/interpreter/DeclarationSupport.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/ExpressionCollections.ml | — | [src/frontend/interpreter/ExpressionCollections.ml](../../../src/frontend/interpreter/ExpressionCollections.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/ExpressionControl.ml | — | [src/frontend/interpreter/ExpressionControl.ml](../../../src/frontend/interpreter/ExpressionControl.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/ExpressionInterpolation.ml | — | [src/frontend/interpreter/ExpressionInterpolation.ml](../../../src/frontend/interpreter/ExpressionInterpolation.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/ExpressionPrecedence.ml | — | [src/frontend/interpreter/ExpressionPrecedence.ml](../../../src/frontend/interpreter/ExpressionPrecedence.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/ExpressionPrimary.ml | — | [src/frontend/interpreter/ExpressionPrimary.ml](../../../src/frontend/interpreter/ExpressionPrimary.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/FileParser.ml | — | [src/frontend/interpreter/FileParser.ml](../../../src/frontend/interpreter/FileParser.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/LibExecution_Effects.ml | — | [src/frontend/interpreter/LibExecution_Effects.ml](../../../src/frontend/interpreter/LibExecution_Effects.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/ParserDependencies.ml | — | [src/frontend/interpreter/ParserDependencies.ml](../../../src/frontend/interpreter/ParserDependencies.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/ParserSupport.ml | — | [src/frontend/interpreter/ParserSupport.ml](../../../src/frontend/interpreter/ParserSupport.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/PatternParser.ml | — | [src/frontend/interpreter/PatternParser.ml](../../../src/frontend/interpreter/PatternParser.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/Prelude.ml | — | [src/frontend/interpreter/Prelude.ml](../../../src/frontend/interpreter/Prelude.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/ProgramTypesShim.ml | — | [src/frontend/interpreter/ProgramTypesShim.ml](../../../src/frontend/interpreter/ProgramTypesShim.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/TraitDeclarationParser.ml | — | [src/frontend/interpreter/TraitDeclarationParser.ml](../../../src/frontend/interpreter/TraitDeclarationParser.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/TypeDefinitionParser.ml | — | [src/frontend/interpreter/TypeDefinitionParser.ml](../../../src/frontend/interpreter/TypeDefinitionParser.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/TypeParameterParser.ml | — | [src/frontend/interpreter/TypeParameterParser.ml](../../../src/frontend/interpreter/TypeParameterParser.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
| parser:compiler/src/frontend/interpreter/TypeParser.ml | — | [src/frontend/interpreter/TypeParser.ml](../../../src/frontend/interpreter/TypeParser.ml) | compiler split component or adapter; classify | unverified | faithful-port | required |
