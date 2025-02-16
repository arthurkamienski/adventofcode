#!/usr/bin/awk -f

@include "utils.awk"

BEGIN {
    RS=")"
    FS=","
    active=1;
}

/do\($/ {
    active=1;
}

/don't\($/ {
    active=0;
}

/mul\([0-9]+,[0-9]+$/ {
    gsub(/.*mul\(/, "", $0)
    if (active) sum += $1 * $2;
}

END {
    print sum;
}
