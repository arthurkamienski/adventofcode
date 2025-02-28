#!/usr/bin/awk -f

@include "utils.awk"

/Register [A-C]: [0-9]+/ {
    register[substr($2, 1, 1)] = $3
}

/Program: ([0-7],?)+/ {
    split($2, program, ",");
}

function combo(operand) {
    if (operand <= 3) {
        return operand;
    } else {
        return register[operand_to_register[operand]];
    }
}

function div(operand, reg,      num, den, pow, res) {
    num = register["A"];

    den = 2 ** combo(operand);
    res = num / den;

    register[reg] = int(res);
}

function compute_opcode(opcode, operand) {
    switch (opcode) {
    case 0:
        div(operand, "A");
        break;
    case 1:
        register["B"] = xor(register["B"], operand);
        break;
    case 2:
        register["B"] = combo(operand) % 8;
        break;
    case 3:
        if (register["A"] != 0) {
            pointer = operand-2;
        }
        break;
    case 4:
        register["B"] = xor(register["B"], register["C"]);
        break;
    case 5:
        return combo(operand) % 8;
        break;
    case 6:
        div(operand, "B");
        break;
    case 7:
        div(operand, "C");
        break;
    }
    return -1;
}

function state_code() {
    return register["A"] SUBSEP register["B"] SUBSEP register["C"] SUBSEP pointer;
}

function run_program(reg_a,     out) {
    pointer = 0;
    register["A"] = reg_a;
    register["B"] = 0;
    register["C"] = 0;

    start_state = state_code();
    delete curr_states;

    out = "";

    while(pointer < (length(program)-1)) {
        curr_states[state_code()] = "";

        opcode = program[pointer+1];
        operand = program[pointer+2];

        res = compute_opcode(opcode, operand);

        if (res != -1) {
            out = out res ",";
        }

        pointer+=2;
    }
    out = substr(out, 1, length(out)-1);
    return out;
}

function test_values(it) {
    expected = program[length(program)-it];

    for(i=length(program)-it+1; i<=length(program); i++) {
        expected = expected "," program[i];
    }

    for (i in possibilities[it-1]) {
        for (j=0; j<8; j++) {
            value = 8* i + j;
            res = run_program(value);
            if(res == expected) {
                possibilities[it][value] = 1;
            }
        }
    }
}

END {
    operand_to_register[4] = "A";
    operand_to_register[5] = "B";
    operand_to_register[6] = "C";

    print run_program(register["A"]);

    possibilities[-1][0] = 1;
    for (k=0; k<length(program); k++) {
        test_values(k);
    }

    min = 2**63;
    for(p in possibilities[k-1]) {
        if (p < min) {
            min = p;
        }
    }
    print min;
}
