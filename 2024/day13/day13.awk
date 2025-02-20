#!/usr/bin/awk -f

@include "utils.awk"

match($0, /Button A: X\+([0-9]+), Y\+([0-9]+)/, nums) {
    x1 = nums[1];
    x2 = nums[2];
}

match($0, /Button B: X\+([0-9]+), Y\+([0-9]+)/, nums) {
    y1 = nums[1];
    y2 = nums[2];
}

match($0, /Prize: X=([0-9]+), Y=([0-9]+)/, nums) {
    c1 = nums[1];
    c2 = nums[2];

    a = (c2*y1 - c1*y2)/(x2*y1 - x1*y2);
    b = (c1 - x1*a)/y1;

    if (a > 0 && a <= 100 && b > 0 && b <= 100 && a % 1 ==0 && b % 1 == 0) {
        tokens += a * 3 + b * 1;
    }

    c1 += 10000000000000;
    c2 += 10000000000000;

    a = (c2*y1 - c1*y2)/(x2*y1 - x1*y2);
    b = (c1 - x1*a)/y1;

    if (a > 0 && b > 0 && a % 1 ==0 && b % 1 == 0) {
        tokens_large += a * 3 + b * 1;
    }
}

END {
    print tokens;
    print tokens_large;
}

