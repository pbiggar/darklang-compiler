// ParserDependencies.fs - Small interfaces used by the copied interpreter parser.
//
// These definitions preserve the interpreter parser's existing calls without
// changing the copied source files. They are limited to symbols used by the
// lexer, parser, WrittenTypes, and validation modules.

module NEList

type NEList<'a> = { head: 'a; tail: 'a list }

let ofList head tail : NEList<'a> = { head = head; tail = tail }

let toList (items: NEList<'a>) : 'a list = items.head :: items.tail

let ofListWithDefault fallback items : NEList<'a> =
    match items with
    | head :: tail -> ofList head tail
    | [] -> ofList fallback []
