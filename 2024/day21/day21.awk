#!/usr/bin/awk -f

@include "utils.awk"


function dist(curr_pos, next_pos,     c, n) {
    split(curr_pos, c, ",")
    split(next_pos, n, ",")
    return coord(n[1] - c[1], n[2] - c[2])
}

function is_v_first(x, y) {
    if(x < 0 && y > 0) return 1;
    if(x > 0 && y > 0) return 1;

    return 0;
}


BEGIN {
    FS = "";
    SUBSEP = ",";

    dir_to_coord["^"] = coord(1, 1);
    dir_to_coord["A"] = coord(1, 2);
    dir_to_coord["<"] = coord(0, 0);
    dir_to_coord["v"] = coord(0, 1);
    dir_to_coord[">"] = coord(0, 2);

    num_to_coord["A"] = coord(0, 2);
    num_to_coord[0]   = coord(0, 1);
    num_to_coord[1]   = coord(1, 0);
    num_to_coord[2]   = coord(1, 1);
    num_to_coord[3]   = coord(1, 2);
    num_to_coord[4]   = coord(2, 0);
    num_to_coord[5]   = coord(2, 1);
    num_to_coord[6]   = coord(2, 2);
    num_to_coord[7]   = coord(3, 0);
    num_to_coord[8]   = coord(3, 1);
    num_to_coord[9]   = coord(3, 2);

    for(i=-3; i<=3; i++) {
        for(j=-2; j<=2; j++) {
            if(i == 0 && j == 0) continue;

            v_comp = "";
            for (k=0; k<abs(i); k++) {
                v_comp = v_comp (i > 0 ? "^" : "v");
            }
            h_comp = "";
            for (k=0; k<abs(j); k++) {
                h_comp = h_comp (j > 0 ? ">" : "<");
            }

            str = v_comp h_comp;
            curr_pos = coord(1, 2);
            n = 0;

            if(i != 0) {
                target = i > 0? dir_to_coord["^"] : dir_to_coord["v"];

                moves[str][n++] = dist(curr_pos, target);
                for(k=0; k < abs(i); k++) {
                    moves[str][n++] = "A";
                }
                curr_pos = target;
            }

            if(j != 0) {
                target = j > 0? dir_to_coord[">"] : dir_to_coord["<"];

                moves[str][n++] = dist(curr_pos, target);
                for(k=0; k < abs(j); k++) {
                    moves[str][n++] = "A";
                }
                curr_pos = target;
            }

            moves[str][n++] = dist(target, coord(1, 2));
            moves[str][n++] = "A";

            str = h_comp v_comp;
            curr_pos = coord(1, 2);
            n = 0;

            if(j != 0) {
                target = j > 0? dir_to_coord[">"] : dir_to_coord["<"];
                moves[str][n++] = dist(curr_pos, target);

                for(k=0; k < abs(j); k++) {
                    moves[str][n++] = "A";
                }
                curr_pos = target;
            }

            if(i != 0) {
                target = i > 0? dir_to_coord["^"] : dir_to_coord["v"];

                moves[str][n++] = dist(curr_pos, target);
                for(k=0; k < abs(i); k++) {
                    moves[str][n++] = "A";
                }
                curr_pos = target;
            }

            moves[str][n++] = dist(target, coord(1, 2));
            moves[str][n++] = "A";

            if(!(i == 1 && j == 2) && (is_v_first(i, j) || (i == -1 && j == -2))) {
                str = v_comp h_comp;
                move_to_str[coord(i, j)] = str;
            } else {
                str = h_comp v_comp;
                move_to_str[coord(i, j)] = str;
            }
        }
    }

    for (s in moves) {
        str = "";
        n = 0;
        for (i=0; i<length(moves[s]); i++) {
            m = moves[s][i];

            if(m == "A") {
                expanded[s "A"][n++] = str "A";
                str = "";
            } else {
                mov = move_to_str[m]

                if(s ~ /<+\^+/ && mov == "^>") {
                    mov = ">^";
                }

                if(s ~ /\^+<+/ && mov == "<v"){
                    mov = "v<";
                }

                str = str mov;
            }
        }
    }

    expanded["A"][0] = "A";
}

{
    curr_pos = num_to_coord["A"];

    delete keyset;
    num = substr($0, 1, length($0) - 1) + 0;

    str = "";
    for(i=1; i<=NF; i++) {
        move = dist(curr_pos, num_to_coord[$i]);

        delete c;
        delete mv;
        split(curr_pos, c, ",");
        split(move, mv, ",");

        vertical_first_allowed = coord(c[1] + mv[1], c[2]) != coord(0, 0);
        horizontal_first_allowed = coord(c[1], c[2] + mv[2]) != coord(0, 0);

        vertical_first = is_v_first(mv[1], mv[2]);

        if(vertical_first) {
            vertical_first = vertical_first_allowed;
        } else {
            vertical_first = !horizontal_first_allowed;
        }

        v_comp = "";
        for (k=0; k<abs(mv[1]); k++) {
            v_comp = v_comp (mv[1] > 0 ? "^" : "v");
        }
        h_comp = "";
        for (k=0; k<abs(mv[2]); k++) {
            h_comp = h_comp (mv[2] > 0 ? ">" : "<");
        }

        if(vertical_first) {
            keyset[1][v_comp h_comp "A"]++;
        } else {
            keyset[1][h_comp v_comp "A"]++;
        }

        curr_pos = num_to_coord[$i];
    }

    for(i=0; i<2; i++) {
        delete keyset[i%2];
        for (k in keyset[!(i%2)]) {
            for (v in expanded[k]) {
                mov = expanded[k][v];

                keyset[i%2][mov] += keyset[!(i%2)][k];
            }
        }
    }

    acc = 0;
    for (k in keyset[!(i%2)]) {
        acc += length(k) * (keyset[!(i%2)][k] + 0);
    } 
    res[num][0] = acc;

    for(; i<25; i++) {
        delete keyset[i%2];
        for (k in keyset[!(i%2)]) {
            for (v in expanded[k]) {
                mov = expanded[k][v];

                keyset[i%2][mov] += keyset[!(i%2)][k];
            }
        }
    }

    acc = 0;
    for (k in keyset[!(i%2)]) {
        acc += length(k) * (keyset[!(i%2)][k] + 0);
    } 
    res[num][1] = acc;
}

END {
    acc1 = 0;
    acc2 = 0;
    for (r in res) {
        acc1 += r * res[r][0];
        acc2 += r * res[r][1];
    }
    print acc1;
    print acc2;
}


# +---+---+---+
# | 7 | 8 | 9 |
# +---+---+---+
# | 4 | 5 | 6 |
# +---+---+---+
# | 1 | 2 | 3 |
# +---+---+---+
#     | 0 | A |
#     +---+---+
# 
# <<^^A>A>AvvA
# v<<AA^>AA>AvA^AvA^Av<AA^>A
#
#     +---+---+
#     | ^ | A |
# +---+---+---+
# | < | v | > |
# +---+---+---+

# <<^^A>A>AvvA
# v<<AA ^>AA >A vA^AvA^A v<AA^>A
