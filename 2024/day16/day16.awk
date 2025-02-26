#!/usr/bin/awk -f

@include "utils.awk"

BEGIN { FS=""; SUBSEP="," }

{
    for (i = 1; i <= NF; i++) {
        map[NR, i] = $i;
        if ($i == "S") {
            start_pos = coord(NR, i);
        } else if ($i == "E") {
            end_pos = coord(NR, i);
        }
    }
}

function get_next_dir(pos, next_pos) {
    split(pos, a, SUBSEP);
    split(next_pos, b, SUBSEP);

    x_diff = b[1] - a[1];
    y_diff = b[2] - a[2];

    if (x_diff == 0) {
        return y_diff > 0 ? "E" : "W";
    } else {
        return x_diff > 0 ? "S" : "N";
    }
}

function reverse_dir(dir) {
    if (dir == "N") return "S";
    if (dir == "S") return "N";
    if (dir == "E") return "W";
    if (dir == "W") return "E";
}

function create_graph(start, start_dir, curr_pos, steps, dir, prev, tiles,    ns, next_dir) {
    if (curr_pos == end_pos) {
        graph[start][start_dir]["dest"] = curr_pos;
        graph[start][start_dir]["weight"] = steps;
        graph[start][start_dir]["dir"] = dir;
        graph[start][start_dir]["tiles"] = tiles;
        return;
    }

    visited[curr_pos] = 1;
    get_neighbors(curr_pos, ns);

    for (n in ns) {
        if (map[n] == "#") delete ns[n];
    }

    if (length(ns) == 1) {
        return;
    } else if (length(ns) == 2 && curr_pos != start_pos) {
        for (n in ns) {
            if (n == prev) continue;
            next_dir = get_next_dir(curr_pos, n);
            if (next_dir != dir) {
                if(curr_pos == start)  {
                    start_dir = next_dir;
                } else {
                    steps+=1000;
                }
            }
            create_graph(start, start_dir, n, steps + 1, next_dir, curr_pos, tiles "|" n);
        }
    } else {
        if (curr_pos != start) {
            graph[start][start_dir]["dest"] = curr_pos;
            graph[start][start_dir]["weight"] = steps;
            graph[start][start_dir]["dir"] = dir;
            graph[start][start_dir]["tiles"] = tiles "|" curr_pos "|" start;
            graph[curr_pos][reverse_dir(dir)]["dest"] = start;
            graph[curr_pos][reverse_dir(dir)]["weight"] = steps;
            graph[curr_pos][reverse_dir(dir)]["dir"] = reverse_dir(start_dir);
            graph[curr_pos][reverse_dir(dir)]["tiles"] = tiles "|" curr_pos "|" start;
        }
        for (n in ns) {
            if (!(n in visited)) {
                next_dir = get_next_dir(curr_pos, n);
                create_graph(curr_pos, next_dir, n, 1, next_dir, curr_pos, curr_pos "|" n);
            }
        }
    }
}

function dist(x, y,   a, b) {
    split(x, a, SUBSEP);
    split(y, b, SUBSEP);

    return abs(a[1] - b[1]) + abs(a[2] - b[2]);
}

function heuristic(x, y) {
    return 0;
}

function find_best_path(start_pos, start_dir,        curr_pos, curr_dir, curr_priority, a) {
    delete h[0];
    insert(h, 1, heuristic(start_pos, end_pos));

    path[1]["pos"] = start_pos;
    path[1]["dir"] = start_dir;
    path[1]["path"] = "";
    path[1]["tiles"] = start_pos;
    path[1]["last"] = start_pos;

    delete visited;

    while(path[peek_element(h)]["pos"] != end_pos) {
        curr_path = peek_element(h);
        curr_priority = peek_priority(h);
        pop(h);

        curr_pos = path[curr_path]["pos"];
        curr_dir = path[curr_path]["dir"];

        curr_priority -= heuristic(curr_pos, end_pos);

        visited[curr_pos, curr_dir] = curr_priority;

        for (dir in graph[curr_pos]) {
            if (graph[curr_pos][dir]["dest"] SUBSEP graph[curr_pos][dir]["dir"] in visited) {
                if (visited[graph[curr_pos][dir]["dest"], graph[curr_pos][dir]["dir"]] < curr_priority) {
                    continue;
                }
            }

            new_priority = curr_priority;

            if(dir != curr_dir) {
                if (dir == reverse_dir(curr_dir)) {
                    new_priority += 2000;
                } else {
                    new_priority += 1000;
                }
            }
            new_priority += graph[curr_pos][dir]["weight"];
            new_dir = graph[curr_pos][dir]["dir"];
            new_pos = graph[curr_pos][dir]["dest"];

            path[length(path) + 1]["pos"] = new_pos;
            path[length(path)]["dir"] = new_dir;
            path[length(path)]["path"] = path[curr_path]["path"] dir;
            path[length(path)]["tiles"] = path[curr_path]["tiles"] "|" graph[curr_pos][dir]["tiles"] "|" curr_pos "|" new_pos;
            path[length(path)]["last"] = curr_pos;

            new_priority += heuristic(new_pos, end_pos);

            insert(h, length(path), new_priority);
        }
    }
}

END {
    create_graph(start_pos, "E", start_pos, 0, "E", start_pos, start_pos);

    find_best_path(start_pos, "E");
    best_priority = peek_priority(h);

    print best_priority;

    i = 0;
    while (peek_priority(h) == best_priority) {
        i++;
        tiles = path[peek_element(h)]["tiles"];
        split(tiles, a, "|");
        for (j = 1; j <= length(a); j++) {
            spots[a[j]] = 1;
        }

        pop(h);
    }
    print length(spots);
}
