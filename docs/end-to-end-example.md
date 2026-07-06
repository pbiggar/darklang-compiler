## Abridged End-to-End Example

Here's how `2 + 3 * 4` transforms through the shared IR pipeline on the current
compiler:

```
Source:     "2 + 3 * 4"

AST:        BinOp(Add, Int64Literal(2),
              BinOp(Mul, Int64Literal(3), Int64Literal(4)))

ANF before
opt:        let TempId 0 = 3 * 4
            let TempId 1 = 2 + t0
            return t1

ANF after
opt:        return 14

ANF after
print:      let TempId 2000 = print(14, type=TInt64)
            return 14

MIR:        Print(14, type=TInt64)
            ret 14

LIR:        X0 <- Mov(Imm 14)
            PrintInt64(X0)
            X0 <- Mov(Imm 14)
            Ret

Allocated
LIR:        X0 <- Mov(Imm 14)
            PrintInt64(X0)
            X0 <- Mov(Imm 14)
            Ret

Result:     Executable that prints 14 and exits with code 0
```
