#!/usr/bin/awk -f

@include "utils.awk"

BEGIN { FS = "" };

{
    for (i = 1; i <= NF; i++) if ($i != ".") antennas[$i][NR][i] = 1;
}

function in_bounds(row, col) {
    return row >= 1 && row <= NR && col >= 1 && col <= NF;
}

function gcd(a, b) {
  while (b != 0) {
    temp = b
    b = a % b
    a = temp
  }
  return a
}

function find_antinodes_simple(row1, col1, row2, col2) {
    dx = row2 - row1;
    dy = col2 - col1;

    if (in_bounds(row2 + dx, col2 + dy)) antinodes[row2 + dx, col2 + dy] = 1;
    if (in_bounds(row1 - dx, col1 - dy)) antinodes[row1 - dx, col1 - dy] = 1;
}

function find_antinodes_multiple(row1, col1, row2, col2) {
    dx = row2 - row1;
    dy = col2 - col1;

    g = gcd(dx, dy);
    dx /= g;
    dy /= g;

    resonant_antinodes[row1, col1] = 1;

    for (i = 1; in_bounds(row1 + i * dx, col1 + i * dy); i++)
        resonant_antinodes[row1 + i * dx, col1 + i * dy] = 1;

    for (i = 1; in_bounds(row1 - i * dx, col1 - i * dy); i++)
        resonant_antinodes[row1 - i * dx, col1 - i * dy] = 1;
}

END {
    for (antenna in antennas) {
        for (row1 in antennas[antenna]) {
            for (col1 in antennas[antenna][row1]) {
                for (row2 in antennas[antenna]) {
                    for (col2 in antennas[antenna][row2]) {
                        if (row1 == row2 && col1 == col2) continue;

                        find_antinodes_simple(row1, col1, row2, col2);
                        find_antinodes_multiple(row1, col1, row2, col2);
                    }
                }
            }
        }
    }

    print length(antinodes);
    print length(resonant_antinodes);
}

