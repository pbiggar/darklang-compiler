# Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
import re
import sys
PRECEDENCE = {"<":1, "+":2, "-":2, "*":3, "/":3}

def lex(source):
    tokens = re.findall(r"[0-9]+|[a-zA-Z]+|[^\s]",source)
    return [int(t) if t.isdecimal() else t for t in tokens]

class Parser:
    def __init__(self,tokens): self.tokens,self.index = tokens,0
    def primary(self):
        token = self.tokens[self.index]; self.index += 1
        if isinstance(token,int): return token
        if token == "x": return self.x
        if token == "y": return self.y
        if token == "(":
            value = self.expression(0)
            assert self.tokens[self.index] == ")"
            self.index += 1
            return value
        raise ValueError("invalid primary")
    def expression(self,minimum):
        left = self.primary()
        while self.index < len(self.tokens):
            op = self.tokens[self.index]
            precedence = PRECEDENCE.get(op,0)
            if precedence == 0 or precedence < minimum: break
            self.index += 1
            right = self.expression(precedence+1)
            if op == "+": left += right
            elif op == "-": left -= right
            elif op == "*": left *= right
            elif op == "/": left = (abs(left)//abs(right))*(-1 if (left<0)!=(right<0) else 1)
            else: left = int(left < right)
        return left
    def evaluate(self,iteration):
        result = index = 0
        while self.index < len(self.tokens):
            self.x = (iteration*17+index*13)%97+3
            self.y = (iteration*29+index*7)%89+5
            value = self.expression(0)
            assert self.tokens[self.index] == ";"
            self.index += 1
            result = (result+value*(index+1)) % 1_000_000_007
            index += 1
        return result
n,runs = map(int,sys.argv[1:])
tokens = lex("x * x + y * 3 + (x + y) * (x - y) + x / 2;\n"*n)
print(sum(Parser(tokens).evaluate(i) for i in range(runs)) % 1_000_000_007)
