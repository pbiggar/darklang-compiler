(* Tokenizer.mli - Complete range and literal token domain. *)
type pos = { row : int; column : int }
type tokenRange = { start : pos; end_ : pos }
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
