class CPU:
    def __init__(self, pointer, instrs):
        self.registers = [0, 0, 0, 0, 0, 0]
        self.pointer = pointer
        self.ip = 0

        self.ops = {
           'addr': self.addr,
           'addi': self.addi,
           'mulr': self.mulr,
           'muli': self.muli,
           'banr': self.banr,
           'bani': self.bani,
           'borr': self.borr,
           'bori': self.bori,
           'setr': self.setr,
           'seti': self.seti,
           'gtri': self.gtri,
           'gtrr': self.gtrr,
           'gtir': self.gtir,
           'eqir': self.eqir,
           'eqri': self.eqri,
           'eqrr': self.eqrr
            }

        self.instrs = instrs

    def next(self):
        instr = self.instrs[self.ip]
        self.registers[self.pointer] = self.ip
        self.ops[instr['code']](*instr['args'])
        self.ip = self.registers[self.pointer]+1

        print(self.ip, self.registers)

    def clear(self):
        self.load([0, 0, 0, 0, 0, 0])
        self.ip = 0

    def load(self, state):
        self.registers = state[:]

    def addr(self, A, B, C):
        self.addi(A, self.registers[B], C)

    def addi(self, A, B, C):
        self.registers[C] = self.registers[A] + B

    def mulr(self, A, B, C):
        self.muli(A, self.registers[B], C)

    def muli(self, A, B, C):
        self.registers[C] = self.registers[A] * B

    def banr(self, A, B, C):
        self.bani(A, self.registers[B], C)

    def bani(self, A, B, C):
        self.registers[C] = self.registers[A] & B

    def borr(self, A, B, C):
        self.bori(A, self.registers[B], C)

    def bori(self, A, B, C):
        self.registers[C] = self.registers[A] | B

    def setr(self, A, B, C):
        self.seti(self.registers[A], B, C)

    def seti(self, A, B, C):
        self.registers[C] = A

    def gtir(self, A, B, C):
        if A > self.registers[B]:
            self.registers[C] = 1
        else:
            self.registers[C] = 0

    def gtri(self, A, B, C):
        if self.registers[A] > B:
            self.registers[C] = 1
        else:
            self.registers[C] = 0

    def gtrr(self, A, B, C):
        self.gtri(A, self.registers[B], C)
        
    def eqir(self, A, B, C):
        if A == self.registers[B]:
            self.registers[C] = 1
        else:
            self.registers[C] = 0

    def eqri(self, A, B, C):
        if self.registers[A] == B:
            self.registers[C] = 1
        else:
            self.registers[C] = 0

    def eqrr(self, A, B, C):
        self.eqri(A, self.registers[B], C)

    def execute_instrs(self):
        try:
            while True:
                self.next()
        except KeyError:
            pass

    def compare(self, state):
        return self.registers == state