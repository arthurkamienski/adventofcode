from cpu import CPU
import re
import time

def extract(data):
    ip = int(data.readline().split(' ')[1])
    instrs = {}
    
    for i, line in enumerate(data):
        match = re.match('(.*)\ ([0-9]+)\ ([0-9]+)\ ([0-9]+)$', line).groups()
        instrs[i] = {
            'code': match[0],
            'args': [int(m) for m in match[1:4]]
        }

    return ip, instrs

def main():
    with open('input.txt', 'r') as raw_data:
        ip, instrs = extract(raw_data)
        cpu = CPU(ip, instrs)

        cpu.load([0, 0, 0, 0, 0, 0])
        
        cpu.execute_instrs()
        # while cpu.ip != 28:
        #     cpu.next()


if __name__ == '__main__':
    main()