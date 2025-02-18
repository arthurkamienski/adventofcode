#!/usr/bin/awk -f

@include "utils.awk"

BEGIN { FS = ""; }

{
    for (i = 1; i <= NF; i++) {
        if ($i == ".") continue;
        pos_id++;
        map[NR, i] = pos_id;
        positions[pos_id]["height"] = $i;
        positions[pos_id]["x"] = i;
        positions[pos_id]["y"] = NR;

        heights[$i][pos_id] = 1;
    }
}

function build_positions() {
    for (pos_id in positions) {
        x = positions[pos_id]["x"];
        y = positions[pos_id]["y"];

        for (i=-1; i<=1; i++) {
            for (j=-1; j<=1; j++) {
                if ((i + j) % 2 == 0) continue;

                coord = y + i SUBSEP x + j;

                if (coord in map) {
                    positions[pos_id]["neighbors"][map[coord]] = 1;
                }
            }
        }
    }
}

END {
    build_positions();

    for (pos_id in heights[9]) {
        positions[pos_id]["reached"][pos_id] = 1;
        positions[pos_id]["trails"] = 1;
    }

    for (i=9; i>=0; i--) {
        for (pos_id in heights[i]) {
            for (neighbor_id in positions[pos_id]["neighbors"]) {
                neighbor_height = positions[neighbor_id]["height"];
                if (neighbor_height == i-1) {
                    positions[neighbor_id]["trails"] += positions[pos_id]["trails"];
                    for (peak in positions[pos_id]["reached"]) {
                        positions[neighbor_id]["reached"][peak] = 1;
                    }
                }
            }
        }
    }

    for (pos_id in heights[0]) {
        score += length(positions[pos_id]["reached"]);
        ranking += positions[pos_id]["trails"];
    }
    print score;
    print ranking;
}
