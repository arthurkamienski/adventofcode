#!/usr/bin/awk -f

@include "utils.awk"

match($0, /([a-z0-9]+): ([01])/, matches) {
    wire_val[matches[1]] = matches[2];
}

match($0, /([a-z0-9]+) ([A-Z]+) ([a-z0-9]+) -> ([a-z0-9]+)/, matches) {
    gates[matches[4]]["inputs"][matches[1]] = 1;
    gates[matches[4]]["inputs"][matches[3]] = 1;
    gates[matches[4]]["op"] = matches[2];
}

function apply_gate(op, inputs) {
    if(op == "AND") {
        return inputs[0] && inputs[1];
    } else if(op == "OR") {
        return inputs[0] || inputs[1];
    } else if(op == "XOR") {
        return xor(inputs[0], inputs[1]);
    }
}

function run(     missing_gates, gate, inputs, solved_gates, missing_at_start) {
    missing_gates = length(gates);

    while(missing_gates > 0) {
        missing_at_start = missing_gates;
        for(gate in gates) {
            delete inputs;
            if (gate in solved_gates) continue;

            for(input in gates[gate]["inputs"]) {
                if(input in wire_val) {
                    inputs[length(inputs)] = wire_val[input];
                }
            }

            if(length(inputs) == 2) {
                solved_gates[gate] = 1;
                wire_val[gate] = apply_gate(gates[gate]["op"], inputs);
                missing_gates--;
            }
        }

        if(missing_gates == missing_at_start) break;
    }

    return missing_gates == 0;
}

function to_binary(n, c,    i, str) {
    if(n == 0 || n == 1) {
        str = n;
        for (i=0; i<c; i++) {
            str = "0" str;
        }
        return str;
    }
    return to_binary(int(n/2), c-1) "" (n%2);
}

function load(prefix, bin, n,    i, b) {
    for(i=1; i<=(n+1); i++) {
        if (i-1 < 10) b = "0" (i-1);
        else b = (i-1);

        if (i > length(bin)) {
            wire_val[prefix b] = 0;
            continue;
        }

        wire_val[prefix b] = substr(bin, length(bin)-i+1, 1) + 0;
    }
}

function output_str(n,    i, out, b) {
    out = "";
    for(i=1; i<=(n+1); i++) {
        if(i-1 < 10) b = "0" (i-1);
        else b = (i-1);

        out = wire_val["z" b] out;
    }
    return out;
}

function binary_to_int(b,    bin, i, n) {
    n = 0;
    split(b, bin, "");
    for(i=length(bin); i>0; i--) {
        n += bin[i] * (2**(length(bin)-i));
    }
    return n;
}

function get_gates_that_produce(gate, a,     g) {
    if (!(gate in gates)) {
        return;
    }

    for (g in gates[gate]["inputs"]) {
        a[g] = 1;
        get_gates_that_produce(g, a);
    }
}

function test_sum(x, y, n_bits) {
    delete wire_val;

    res = x + y;

    expected = to_binary(res, n_bits);

    load("x", to_binary(x), 44);
    load("y", to_binary(y), 44);

    succeeded = run();

    if(!succeeded) return 0;

    out = output_str(n_bits);

    return out == expected;
}

function test_bits(n_bits, prev_failed,   x, a, b, c, d) {
    x = 2**(n_bits-1);
    a = test_sum(x*2, 0, n_bits+1);
    b = test_sum(0, x*2, n_bits+1);
    c = test_sum(x, x, n_bits+1);
    if(!prev_failed && n_bits > 1) {
        d = test_sum(x+x/2, x/2, n_bits+1);
    } else {
        d = 1;
    }
    return a && b && c && d;
}

function swap_gates(g1, g2,    i, g1_op, aux) {
    g1_op = gates[g1]["op"];
    gates[g1]["op"] = gates[g2]["op"];
    gates[g2]["op"] = g1_op;

    for(i in gates[g1]["inputs"]) {
        aux[i] = 1;
    }
    delete gates[g1]["inputs"];
    for(i in gates[g2]["inputs"]) {
        gates[g1]["inputs"][i] = 1;
    }
    delete gates[g2]["inputs"];
    for(i in aux) {
        gates[g2]["inputs"][i] = 1;
    }
}

function gates_from(gs,   g, i, inputs) {
    start_len = length(gs);
    for (g in gates) {
        delete inputs;
        for(i in gates[g]["inputs"]) {
            if (i in gs) {
                inputs[i] = 1;
            }
        }
        if (length(inputs) >= 1) {
            gs[g] = 1;
        }
    }

    if(length(gs) != start_len) gates_from(gs);
}

END {
    run();
    print "Part 1:", binary_to_int(output_str(45));
    print "";

    # swap_gates("z14", "vss");
    # swap_gates("kdh", "hjf")
    # swap_gates("z31", "kpp");
    # swap_gates("z35", "sgj");
    # hjf,kdh,kpp,sgj,vss,z14,z31,z35

    delete safe;
    delete unsafe;
    for(i=1; i<45; i++) {
        res = test_bits(i, (i-2) in wrong_results);

        if(!res) {
            wrong_results[i] = 1;
        } else {
            delete top_up;
            lead_zero = ((i-1) < 10 ? "0" : "");
            top_up["x" lead_zero (i-1)] = 1;
            top_up["y" lead_zero (i-1)] = 1;

            lead_zero = (i < 10 ? "0" : "");
            top_up["x" lead_zero i] = 1;
            top_up["y" lead_zero i] = 1;

            gates_from(top_up);

            delete bottom_down;
            get_gates_that_produce("z" lead_zero i, bottom_down);
            bottom_down["z" lead_zero i] = 1;

            for(g in bottom_down) {
                if(g in top_up) {
                    safe_gates[g] = 1;
                }
            }
        }
    }

    print "Wrong outputs:";
    for (i in wrong_results) {
        print i;
    }
    print "";

    print "Finding possible swaps";
    for(i in wrong_results) {
        gate = "z" (i < 10 ? "0" i : i);
        delete unsafe_gates;
        unsafe_gates[gate] = 1;
        get_gates_that_produce(gate, unsafe_gates);

        for(g in unsafe_gates) {
            if(substr(g, 1, 1) == "x" || substr(g, 1, 1) == "y") delete unsafe_gates[g];
            if(g in safe_gates) {
                delete unsafe_gates[g];
            }
        }
        print "Output", i "; gates to check:", length(unsafe_gates);
        
        for(g1 in unsafe_gates) {
            for(g2 in gates) {
                if(g2 in safe_gates) continue;
                if(g2 == g1) continue;

                swap_gates(g1, g2);
                res = test_bits(i);
                swap_gates(g1, g2);

                if(res) {
                    delete aux;
                    aux[1] = g1;
                    aux[2] = g2;
                    asort(aux);
                    possible_swaps[aux[1] "," aux[2]]++;
                }
            }
        }
    }

    for(swp in possible_swaps) {
        if(possible_swaps[swp] == 1) {
            delete possible_swaps[swp];
        }
    }

    print "Possible swaps: " length(possible_swaps);

    for(s1 in possible_swaps) {
        for(s2 in possible_swaps) {
            for(s3 in possible_swaps) {
                for(s4 in possible_swaps) {
                    split(s1 "," s2 "," s3 "," s4, swps, ",");

                    delete gs;
                    for(i=1; i<=length(swps); i++) {
                        gs[swps[i]]++;
                    }

                    is_valid = 1;
                    for(g in gs) {
                        if(gs[g] > 1) is_valid = 0;
                    }
                    if(!is_valid) continue;

                    delete aux;
                    aux[1] = s1;
                    aux[2] = s2;
                    aux[3] = s3;
                    aux[4] = s4;
                    asort(aux);

                    combinations[aux[1] ":" aux[2] ":" aux[3] ":" aux[4]] = 1;
                }
            }
        }
    }

    for(c in combinations) {
        print "Trying combination", c;
        delete swps;
        split(c, swps, ":");

        for(i=1; i<=length(swps); i++) {
            delete s;
            split(swps[i], s, ",");
            swap_gates(s[1], s[2]);
        }

        for(i=1; i<45; i++) {
            res = test_bits(i, 1);
            if(!res) {
                print "Failed for", i;
                break;
            }
        }

        if(res) {
            print "Found solution";

            for(i=1; i<=length(swps); i++) {
                delete s;
                split(swps[i], s, ",");
                ans[s[1]] = 1;
                ans[s[2]] = 1;
            }

            asorti(ans);

            for(i=1; i<=length(ans); i++) {
                printf ans[i] (i==length(ans) ? "\n" : ",");
            }

            break;
        }

        for(i=1; i<=length(swps); i++) {
            delete s;
            split(swps[i], s, ",");
            swap_gates(s[1], s[2]);
        }
    }
}
