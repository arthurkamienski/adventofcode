#!/usr/bin/awk -f

@include "utils.awk"

BEGIN {
    FS = ""
}

{
    for (i = 1; i <= NF; i++) {
        fields[NR, i] = $i;
        if ($i == "X") {
            x_coords[NR, i] = 1;
        }
        if ($i == "A") {
            a_coords[NR, i] = 1;
        }
    }
}

END {
    for (x_coord in x_coords) {
        split(x_coord, coords, SUBSEP);
        x = coords[1];
        y = coords[2];

        delete xmass;
        for (i=1; i<=3; i++) {
            xmass[1] = xmass[1] fields[x, y-i];
            xmass[2] = xmass[2] fields[x, y+i];
            xmass[3] = xmass[3] fields[x-i, y];
            xmass[4] = xmass[4] fields[x+i, y];
            xmass[5] = xmass[5] fields[x-i, y-i];
            xmass[6] = xmass[6] fields[x-i, y+i];
            xmass[7] = xmass[7] fields[x+i, y-i];
            xmass[8] = xmass[8] fields[x+i, y+i];
        }

        for (i=1; i<=8; i++) if (xmass[i] == "MAS") count++;
    }

    print count;

    count = 0;
    for (a_coord in a_coords) {
        split(a_coord, coords, SUBSEP);
        x = coords[1];
        y = coords[2];

        corners = fields[x-1, y-1] fields[x+1, y+1] fields[x-1, y+1] fields[x+1, y-1];

        if (corners == "SSMM" || corners == "MMSS") {
            continue;
        }

        if (gsub(/S/, "", corners) == 2 && gsub(/M/, "", corners) == 2) {
            count++;
        }
    }
    print count;
}
