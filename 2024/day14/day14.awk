#!/usr/bin/awk -f

@include "utils.awk"

BEGIN {
    width = 101;
    height = 103;
    SUBSEP = ",";
}

match($0, /p=(\-?[0-9]+),(\-?[0-9]+) v=(\-?[0-9]+),(\-?[0-9]+)/, ns) {
    x = ns[1]+0;
    y = ns[2]+0;
    vx = ns[3]+0;
    vy = ns[4]+0;

    curr_pos = coord(x, y);
    delete visited;

    for(i=0; !(curr_pos in visited); i++) {
        # print i, curr_pos;
        visited[curr_pos] = 1;
        robots[NR][i] = curr_pos;

        x += vx;
        y += vy;

        if (x > 10) x -= width;
        if (x < 0) x += width;
        if (y > 6) y -= height;
        if (y < 0) y += height;

        curr_pos = coord(x, y);
    }

    robot_pos = robots[NR][100 % i];
    robots[NR]["cycle"] = i;

    split(robot_pos, pos, ",");

    x = pos[1];
    y = pos[2];

    if (x != (width - 1) / 2 && y != (height - 1) / 2) {
        quadrant = (x < (width -1) / 2) "," (y < (height - 1) / 2);
        quadrants[quadrant]++;
    }
}

END {
    acc = 1;
    for (quadrant in quadrants) {
        acc *= quadrants[quadrant];
    }
    print acc;

    # for (i=271; ; i+=101) {
    #     print "After " i " seconds:";
    #     delete coords;
    #     for (robot in robots) {
    #         coords[robots[robot][i % robots[robot]["cycle"]]]++;
    #     }
    #     for (j=0; j<height; j++) {
    #         for (k=0; k<width; k++) {
    #             printf "%s", (coord(k, j) in coords) ? coords[coord(k, j)] : ".";
    #         }
    #         print "";
    #     }
    #     system("sleep 0.1");

        # if ((i - 80) % 100 == 0) i = i+=90;
    # }
}
