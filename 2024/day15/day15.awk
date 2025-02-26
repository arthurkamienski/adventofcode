#!/usr/bin/awk -f

@include "utils.awk"

BEGIN { FS = ""; SUBSEP = "," }

/[#\.O@]+/ {
    for(i=0; i<NF; i++) {
        warehouse[coord(NR-1, i)] = $(i+1);
        if($(i+1) == "@") {
            curr_pos = coord(NR-1, i);
            curr_pos_wide = coord(NR-1, i*2);
            warehouse_wide[coord(NR-1, i*2)] = $(i+1);
            warehouse_wide[coord(NR-1, i*2+1)] =  ".";
        } else if ($(i+1) == "O") {
            warehouse_wide[coord(NR-1, i*2)] = "[";
            warehouse_wide[coord(NR-1, i*2+1)] =  "]";
        } else {
            warehouse_wide[coord(NR-1, i*2)] = $(i+1);
            warehouse_wide[coord(NR-1, i*2+1)] =  $(i+1);
        }
    }
}

function get_coord(c, dir,     x, y, pos) {
    split(c, pos, ",");
    x = pos[1];
    y = pos[2];

    if (dir == "^") {
        x -= 1;
    } else if (dir == "v") {
        x += 1;
    } else if (dir == "<") {
        y -= 1;
    } else if (dir == ">") {
        y += 1;
    }

    return coord(x, y);
}

function maybe_move(c, dir,     next_coord, next_pos, can_move) {
    next_coord = get_coord(c, dir);
    next_pos = warehouse[next_coord];

    if(next_pos == "#") {
        can_move = 0;
    } else if (next_pos == ".") {
        can_move = 1;
    } else if (next_pos == "O") {
        can_move = maybe_move(next_coord, dir);
    } 

    if (can_move) {
        warehouse[next_coord] = warehouse[c];
        warehouse[c] = ".";
    }

    return can_move;
}

function can_move_wide(c, dir,    next_coord, next_pos, other_half, move_allowed, other_half_dir) {
    to_move[c] = 1;

    next_coord = get_coord(c, dir);

    next_pos = warehouse_wide[next_coord];

    if(next_pos == "#") {
        return 0;
    } else if (next_pos == ".") {
        return 1;
    } else if (next_pos == "[" || next_pos == "]") {
        move_allowed = can_move_wide(next_coord, dir);

        if (dir == "^" || dir == "v") {
            other_half_dir = (next_pos == "[") ? ">" : "<";
            other_half = get_coord(next_coord, other_half_dir);

            if (!(other_half in to_move)) {
                move_allowed = move_allowed && can_move_wide(other_half, dir); 
            }
        }
        return move_allowed;
    }
}

function move_wide(c, dir,       next_coord, next_pos, other_half, other_half_dir) {
    moved[c] = 1;
    next_coord = get_coord(c, dir);
    next_pos = warehouse_wide[next_coord];

    if (next_pos == "[" || next_pos == "]") {
        move_wide(next_coord, dir);

        if (dir == "^" || dir == "v") {
            other_half_dir = (next_pos == "[") ? ">" : "<";
            other_half = get_coord(next_coord, other_half_dir);
            if (!(other_half in moved)) {
                move_wide(other_half, dir);
            }
        }
    }

    warehouse_wide[next_coord] = warehouse_wide[c];
    warehouse_wide[c] = ".";
}

/[\^><v]+/ {
    for(i=0; i<NF; i++) {
        dir = $(i+1);
        has_moved = maybe_move(curr_pos, dir);

        if(has_moved) {
            curr_pos = get_coord(curr_pos, dir);
        }

        delete to_move;
        delete moved;
        if(can_move_wide(curr_pos_wide, dir)) {
            move_wide(curr_pos_wide, dir);
            curr_pos_wide = get_coord(curr_pos_wide, dir);
        }
    }
}

END {
    for (i=0; i<NR; i++) {
        for (j=0; j<NF; j++) {
            if(warehouse[coord(i, j)] == "O") {
                gps += (i*100) + j;
            }
        }
    }
    print gps;

    for (i=0; i<NR; i++) {
        for (j=0; j<NF*2; j++) {
            if(warehouse_wide[coord(i, j)] == "[") {
                gps_wide += (i*100) + j;
            }
        }
    }
    print gps_wide;
}
