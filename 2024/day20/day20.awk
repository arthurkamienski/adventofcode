#!/usr/bin/awk -f

@include "utils.awk"

BEGIN {
    FS = "";
    SUBSEP = ",";
}

{
    for (i = 1; i <= NF; i++) {
        if ($i == ".") {
            path[NR, i] = 1;
        } else if ($i == "S") {
            start = coord(NR, i);
            path[NR, i] = 1;
        } else if ($i == "E") {
            end = coord(NR, i);
            path[NR, i] = 1;
        }
    }
}

function find_path() {
    delete queue;
    delete times;

    insert(queue, start, 0);
    times[0] = start;
    positions[start] = 0;

    visited[start] = 1;

    while(peek_element(queue) != end) {
        current = peek_element(queue);
        time = peek_priority(queue);
        pop(queue);

        delete ns;
        get_neighbors(current, ns);

        for (n in ns) {
            if (n in visited) continue;
            if (!(n in path)) continue;

            visited[n] = 1;
            times[time+1] = n;
            positions[n] = time + 1;

            insert(queue, n, time + 1);
        }
    }
}

function get_shortcuts(thr,    i, j, k, pos, new_pos, dist, time_saved, shortcuts) {
    for (i=0; i<length(times); i++) {
        split(times[i], pos, ",");

        for (j=-thr; j<=thr; j++) {
            for (k=-thr; k<=thr; k++) {
                dist = abs(j) + abs(k);

                if (dist > thr) continue;

                new_pos = coord(pos[1] + k, pos[2] + j);

                if ((new_pos in path) && positions[new_pos] > positions[times[i]]) {
                    time_saved = positions[new_pos] - positions[times[i]] - dist;
                    if (time_saved >= 100) {
                        shortcuts++;
                    }
                }
            }
        }
    }

    return shortcuts;
}



END {
    find_path();

    print get_shortcuts(2);
    print get_shortcuts(20);
}
