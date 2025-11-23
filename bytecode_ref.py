class XC:
    BC_LOAD_IMM = 0
    BC_CALL = 1
    BC_JUC = 2
    BC_JCT = 3
    BC_ADD = 4
    BC_MOV = 5
    BC_CG = 6

X = XC()

BC_HALT_OFFSET = 0xC0FE_1DEA

prog = [
    (X.BC_LOAD_IMM, 0, 1),    # 0
    (X.BC_LOAD_IMM, 1, 1),    # 1
    (X.BC_LOAD_IMM, 2, 100),  # 2
    (X.BC_ADD, 3, 0, 1),  # 3
    (X.BC_MOV, 1, 0),  # 4
    (X.BC_MOV, 0, 3),  # 5
    (X.BC_CALL, "print", 0),  # 6
    (X.BC_CG, 4, 0, 2),  # 7
    (X.BC_JCT, 4, BC_HALT_OFFSET),  # 8
    (X.BC_JUC, 3-9),  # 9
]

regs = [0 for _ in range(5)]

ip = 0

while ip < len(prog):
    inst = prog[ip]
    # print(ip, inst, regs)
    match inst[0]:
        case X.BC_LOAD_IMM:
            regs[inst[1]] = inst[2]
        case X.BC_CALL:
            assert inst[1], "This silly half experiment/half demo supports only print."
            print(regs[inst[2]])
        case X.BC_JUC:
            if inst[1] == BC_HALT_OFFSET: break
            ip += inst[1]
            continue
        case X.BC_JCT:
            if regs[inst[1]]:
                if inst[2] == BC_HALT_OFFSET: break
                ip += inst[2]
                continue
        case X.BC_ADD:
            regs[inst[1]] = regs[inst[2]] + regs[inst[3]]
        case X.BC_MOV:
            regs[inst[1]] = regs[inst[2]]
        case X.BC_CG:
            regs[inst[1]] = 1 if regs[inst[2]] > regs[inst[3]] else 0
    ip += 1
