## Complete End-to-End Example

Here's how `2 + 3 * 4` transforms through the entire pipeline:

```
Source:     "2 + 3 * 4"

AST:        BinOp(Add, IntLiteral(2),
              BinOp(Mul, IntLiteral(3), IntLiteral(4)))

ANF:        let tmp0 = 3 * 4
            let tmp1 = 2 + tmp0
            return tmp1

MIR:        v0 <- 3
            v1 <- 4
            v2 <- v0 * v1
            v3 <- 2
            v4 <- v3 + v2
            ret v4

LIR:        v0 <- Mov(Imm 3)
            v1 <- Mov(Imm 4)
            v2 <- Mul(v0, v1)
            v3 <- Mov(Imm 2)
            v4 <- Add(v3, v2)
            ret v4

Allocated:  X0 <- Mov(Imm 3)
            X1 <- Mov(Imm 4)
            X2 <- Mul(X0, X1)
            X3 <- Mov(Imm 2)
            X4 <- Add(X3, X2)
            ret X4

ARM64:      MOVZ X0, #3, LSL #0
            MOVZ X1, #4, LSL #0
            MUL X2, X0, X1
            MOVZ X3, #2, LSL #0
            ADD X4, X3, X2
            MOV X0, X4
            RET

Machine     0xD2800060  // MOVZ X0, #3
Code:       0xD2800081  // MOVZ X1, #4
            0x9B017C02  // MUL X2, X0, X1
            0xD2800043  // MOVZ X3, #2
            0x8B020064  // ADD X4, X3, X2
            0xAA0403E0  // MOV X0, X4
            0xD65F03C0  // RET

Result:     Executable that exits with code 14
```
