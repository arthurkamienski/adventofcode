#!/usr/bin/awk -f

@include "utils.awk"

BEGIN {
    SUBSEP = ",";
    FS = ",";
    size = 70;
    end = coord(size, size);
}

{
    bytes[NR] = coord($1, $2);
}

function is_valid(c) {
    split(c, a, SUBSEP);
    x = a[1];
    y = a[2];
    return x >= 0 && x <= size && y >= 0 && y <= size;
}

function shortest_path(start, n_bytes,    path, p) {
    delete queue;
    delete visited;
    delete elements;
    insert(queue, 0, 0);
    elements[0]["coord"] = start;
    elements[0]["path"] = start;
    visited[start] = 1;

    while((length(queue) > 0) && (elements[peek_element(queue)]["coord"] != end)) {
        el = peek_element(queue);
        current = elements[el]["coord"];
        priority = peek_priority(queue);
        pop(queue);

        delete ns;
        get_neighbors(current, ns, 0);

        for (n in ns) {
            if (ordered_bytes[n_bytes][n] == 1) continue;

            if(is_valid(n) && !(n in visited)) {
                visited[n] = 1;

                el_n = length(elements);
                elements[el_n]["coord"] = n;
                elements[el_n]["path"] = elements[el]["path"] "|" n;

                insert(queue, el_n, priority + 1);
            }
        }
    }

    if (length(queue) > 0) {
        split(elements[peek_element(queue)]["path"], path, "|");

        for(i=1; i<length(path); i++) {
            p = path[i];
            best_path[p] = 1;
        }
    }
}

END {
    for (i = 1; i <= NR; i++) {
        for (j = i; j <= NR; j++) {
            ordered_bytes[j][bytes[i]] = 1;
        }
    }
    shortest_path(coord(0, 0), 1024);
    print peek_priority(queue);

    found = 0;
    for (i = 1024; i <= NR && !found; i++) {
        if (bytes[i] in best_path) {
            delete best_path;
            shortest_path(coord(0, 0), i);
            found = length(queue) == 0;
        }
    }
    print bytes[i-1];
}
