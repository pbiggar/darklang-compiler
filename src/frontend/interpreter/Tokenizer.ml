(*
   Shared lexer types for the range-complete parser: source positions and the
   `Token` DU (produced by `Lexer`, consumed by `Parser` and the
   syntax-highlighter's token-kind classifier).
*)
(* Tokenizer.ml - Complete range and literal token domain. *)
type pos = { row : int; column : int }
type tokenRange = { start : pos; end_ : pos }

(*
   Token types for the lexer.
   Default integer — arbitrary-precision `Int` (bare `1`)
   64-bit signed: 1L
   128-bit signed: 1Q
   8-bit signed: 1y
   16-bit signed: 1s
   32-bit signed: 1l
   8-bit unsigned: 1uy
   16-bit unsigned: 1us
   32-bit unsigned: 1ul
   64-bit unsigned: 1UL
   128-bit unsigned: 1Z
   String literal token
   Char literal: 'x' (stores UTF-8 string for EGC support)
   Interpolated string `$"Hello {name}!"`. No payload: the parser re-reads the
   token's source text and re-scans the `{expr}` bodies itself.
   ++ (string concatenation)
   ** (exponentiation)
   val (declaration-scope value; never a local let-expression)
   elif
   then
   else
   type (type definition)
   :: (list cons pattern)
   : (type annotation)
   , (parameter separator)
   ; (statement separator; record/dict field separator)
   . (tuple/record access)
   { (record literal)
   } (record literal)
   | (sum type variant separator / pattern separator)
   of (sum type payload)
   match (pattern matching)
   with (pattern matching)
   fun (interpreter-style lambda)
   -> (pattern matching)
   _ (wildcard pattern)
   when (guard clause in pattern matching)
   [ (list literal)
   ] (list literal)
   = (assignment in let)
   == (equality comparison)
   !=
   <
   >
   <=
   >=
   &&
   ||
   !
   |> (pipe operator)
   ... (rest pattern in lists)
   % (modulo)
   << (left shift)
   >> (right shift)
   & (bitwise and)
   ^ (bitwise xor)
   ~ (bitwise not)
   @ (list append)
*)
type token =
  | TInt of Z.t
  | TInt64 of int64
  | TInt128 of Z.t
  | TInt8 of int
  | TInt16 of int
  | TInt32 of int32
  | TUInt8 of int
  | TUInt16 of int
  | TUInt32 of int64
  | TUInt64 of int64
  | TUInt128 of Z.t
  | TFloat of float
  | TStringLit of string
  | TCharLit of string
  | TInterpString
  | TTrue
  | TFalse
  | TPlus
  | TPlusPlus
  | TMinus
  | TStar
  | TStarStar
  | TSlash
  | TLParen
  | TRParen
  | TLet
  | TVal
  | TIn
  | TIf
  | TElif
  | TThen
  | TElse
  | TType
  | TCons
  | TColon
  | TComma
  | TSemicolon
  | TDot
  | TLBrace
  | TRBrace
  | TBar
  | TOf
  | TMatch
  | TWith
  | TFun
  | TArrow
  | TUnderscore
  | TWhen
  | TLBracket
  | TRBracket
  | TEquals
  | TEqEq
  | TNeq
  | TLt
  | TGt
  | TLte
  | TGte
  | TAnd
  | TOr
  | TNot
  | TPipe
  | TDotDotDot
  | TPercent
  | TShl
  | TShr
  | TBitAnd
  | TBitXor
  | TBitNot
  | TAt
  | TIdent of string
  | TEOF
