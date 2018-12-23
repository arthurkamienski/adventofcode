import re

class ThreeBehaviours(Exception):
    pass

def interpret(instr):
    line = instr.readline()

    samples = []

    while line != '\n':
        before = re.match('.*([0-9]+),\ ([0-9]+),\ ([0-9]+),\ ([0-9]+)', line).groups()
        operation = re.match('([0-9]+)\ ([0-9]+)\ ([0-9]+)\ ([0-9]+)', instr.readline()).groups()
        after = re.match('.*([0-9]+),\ ([0-9]+),\ ([0-9]+),\ ([0-9]+)', instr.readline()).groups()

        example = {
            'before': [int(x) for x in before],
            'opcode': int(operation[0]),
            'args': [int(x) for x in operation[1:4]],
            'after': [int(x) for x in after]
        }

        samples.append(example)
        
        instr.readline()
        line = instr.readline()

    program = []
    instr.readline()

    for line in instr:
        instr = re.match('([0-9]+)\ ([0-9]+)\ ([0-9]+)\ ([0-9]+)', line).groups()
        program.append([int(x) for x in instr])

    return samples, program

class CPU:
    def __init__(self):
        self.registers = [0, 0, 0, 0]

        self.ops = ([self.addr, self.addi, self.mulr, self.muli, self.banr,
            self.bani, self.borr, self.bori, self.setr, self.seti, self.gtri,
            self.gtrr, self.gtir, self.eqir, self.eqri, self.eqrr])

    def clear(self):
        self.load([0, 0, 0, 0])

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

    def compare(self, state):
        return self.registers == state

def equal_codes(samples, cpu):
    ops = {}

    op_num = 0
    for e in samples:
        equal = 0
        for f in cpu.ops:
            cpu.load(e['before'])
            f(*e['args'])

            if cpu.compare(e['after']):
                equal = equal + 1
                ops[e['opcode']] = ops.get(e['opcode'], set())
                ops[e['opcode']].add(f)
        if equal >= 3:
            op_num = op_num + 1

    print('Samples behaving like >=3 operations:', op_num)

    return ops

def find_op_codes(ops):
    op_num = {}
    

    while ops != {}:
        _, op = min([(len(fs), op) for op, fs in ops.items()])

        f = ops[op].pop()

        op_num[op] = f

        for o in ops.values():
            try:
                o.remove(f)
            except KeyError:
                pass

        ops.pop(op)

    for op, f in op_num.items():
        print(op, f.__name__)

    return op_num

def main():
    with open('input.txt', 'r') as instructions:
        samples, program = interpret(instructions)
        cpu = CPU()

        ops = equal_codes(samples, cpu)

        op_num = find_op_codes(ops)

        cpu.clear()

        for instr in program:
            f = op_num[instr[0]]
            
            f(*instr[1:4])

        print(cpu.registers[0])


if __name__ == '__main__':
    main()