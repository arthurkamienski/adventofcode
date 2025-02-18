#!/usr/bin/awk -f

@include "utils.awk"

function get_stone_count(n, blinks,     count, left, right) {
    if (blinks == 0) {
        count = 1;
    } else if (n SUBSEP blinks in stone_count) {
        count = stone_count[n, blinks];
    } else if (n == 0) {
        count = get_stone_count(1, blinks-1);
    } else if (length(n) % 2 == 0) {
        left = substr(n, 1, length(n)/2) + 0;
        right = substr(n, length(n)/2+1) + 0;

        count = get_stone_count(left, blinks-1) + get_stone_count(right, blinks-1);
    } else {
        count = get_stone_count(n*2024, blinks-1);
    }

    stone_count[n, blinks] = count;

    return count;
}

{
    for (i=1; i<=NF; i++) starting_stones[$i] = 1;

    for (stone in starting_stones) count += get_stone_count(stone, 25);
    print count;

    count = 0;
    for (stone in starting_stones) count += get_stone_count(stone, 75);
    print count;
}
