#!/usr/bin/awk -f

@include "./utils.awk"

{
    left[NR] = $1;
    right[NR] = $2;

    count_left[$1]++;
    count_right[$2]++;
}

END {
    sorted_left = asort(left);
    sorted_right = asort(right);
    distance = 0;
    for (i = 1; i <= NR; i++) {
        distance += abs(left[i] - right[i]);
    }
    print distance;

    similarity = 0;
    for (value in count_left) {
        similarity += count_left[value] * (value * count_right[value]);
    }
    print similarity;
}
