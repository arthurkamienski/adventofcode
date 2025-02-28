#!/usr/bin/awk -f

@include "utils.awk"

BEGIN {
    FS = ", ";
    pattern = "";
}

/[a-z]+, ([a-z]+(, )?)+/ {
    for (i=1; i <= NF; i++) {
        pattern = pattern "(" $i ")|";
        patterns[$i] = 1;
    }
    pattern = substr(pattern, 1, length(pattern)-1);
    pattern = "^(" pattern ")+$";
    
}

function count_possibilities(str, rec,    n, p, start) {
    if(str in cache) return cache[str];
    if (str == "") return 1;

    n = 0;
    for (p in patterns) {
        start = substr(str, 1, length(p));

        if (start == p) n += count_possibilities(substr(str, length(p)+1), rec "-");
    }

    cache[str] = n;

    return n;
}

$0 ~ pattern {
    count++;

    n += count_possibilities($0, "");
}

END {
    print count;
    print n;
}
