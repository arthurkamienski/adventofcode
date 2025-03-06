#!/usr/bin/awk -f

@include "utils.awk"

BEGIN {
    RS="\n\n";
    FS="\n";
}

{
    is_lock = $1 == "#####";
    start = is_lock ? 2 : 1;

    for (j=1; j<=length($1); j++) {
        if(is_lock) {
            locks[NR][j] = 0;
        } else {
            keys[NR][j] = 0;
        }
    }

    for (i=start; i<=NF; i++) {
        if (!is_lock && i==NF && $i == "#####") continue;
        split($i, chars, "");

        for (j=1; j<=length(chars); j++) {
            if(chars[j] == ".") continue;

            if(is_lock) {
                locks[NR][j]++;
            } else {
                keys[NR][j]++;
            }
        }
    }
}

END {
    for(l in locks) {
        for(k in keys) {
            for(j=1; j<=length(locks[l]); j++) {
                if(locks[l][j] + keys[k][j] > 5) break;
            }
            acc += j == (length(locks[l]) + 1);
        }
    }
    print acc;
}
