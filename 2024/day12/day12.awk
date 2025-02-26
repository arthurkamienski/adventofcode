#!/usr/bin/awk -f

@include "utils.awk"

BEGIN { FS = ""; }

{
    for (i = 1; i <= NF; i++) {
        grid[NR, i] = $i;
    }
}

function union(a, b) {
    a = find(a);
    b = find(b);

    parents[a] = b;
}

function find(a) {
    while (a != parents[a]) {
        b = parents[a];
        parents[a] = parents[b];
        a = b;
    }

    return a;
}

END {
    for (i = 1; i <= NR; i++) {
        for (j = 1; j <= NF; j++) {
            for (x = -1; x <= 1; x++) {
                for (y = -1; y <= 1; y++) {
                    if (abs(x + y) % 2 == 0) continue;

                    if(grid[i + x, j + y] != grid[i, j]) {
                        code = i > i + x ? "U" : i < i + x ? "D" : j > j + y ? "L" : "R";
                        fences[i, j][i+x/2, j+y/2, code] = 1;
                    }
                }
            }
            parents[i, j] = coord(i, j);

            if(grid[i, j-1] == grid[i, j]) union(coord(i, j-1), coord(i, j));
            if(grid[i-1, j] == grid[i, j]) union(coord(i-1, j), coord(i, j));
        }
    }

    for (i = 1; i <= NR; i++) {
        for (j = 1; j <= NF; j++) {
            parent = find(coord(i, j));

            for (fence in fences[i, j]) {
                group[parent]["fences"][fence] = 1;
                parents[fence] = fence;
            }

            group[parent]["area"]++;
            group[parent]["perimeter"] += length(fences[i, j]);
        }
    }

    for (parent in group) {
        for (fence in group[parent]["fences"]) {
            split(fence, coords, SUBSEP);
            x = coords[1];
            y = coords[2];
            dir = coords[3];

            if (x % 1 == 0.5) {
                neighbors[1] = x SUBSEP y+1 SUBSEP dir;
                neighbors[2] = x SUBSEP y-1 SUBSEP dir;
            } else {
                neighbors[1] = x+1 SUBSEP y SUBSEP dir;
                neighbors[2] = x-1 SUBSEP y SUBSEP dir;
            }

            for (i=1; i<=2; i++) {
                if (neighbors[i] in group[parent]["fences"]) {
                    union(fence, neighbors[i]);
                }
            }
        }

        for (fence in group[parent]["fences"]) {
            fence_group = find(fence);
            group[parent]["fence_faces"][fence_group] = 1;
        }

        perimeter_price += group[parent]["area"] * group[parent]["perimeter"];
        fence_price += group[parent]["area"] * length(group[parent]["fence_faces"]);
    }
    print perimeter_price;
    print fence_price;
}
