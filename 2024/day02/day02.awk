#!/usr/bin/awk -f

@include "utils.awk"

function check_safe(levels) {
    dir = sign(levels[2] - levels[1]);
    safe = 1;

    for (i = 1; i < length(levels) && safe; i++) {
        diff = levels[i + 1] - levels[i];
        safe = (abs(diff) <= 3 && abs(diff) >= 1) && sign(diff) == dir;
    }

    return safe;
}

function find_safe(levels, ignore) {
    if (ignore > length(levels)) {
        return 0;
    }

    delete new_levels;
    i = 1;
    for (level in levels) {
        if (level != ignore) {
            new_levels[i] = levels[level];
            i++;
        }
    }

    safe = check_safe(new_levels);

    if (safe) {
        return 1;
    } else {
        return find_safe(levels, ignore + 1);
    }
}

{
    split($0, levels);

    is_safe = check_safe(levels);

    if (is_safe) {
        n_safe++;
    } else {
        n_safe_dampened += find_safe(levels, 1);
    }
}

END {
    print n_safe;
    print n_safe + n_safe_dampened;
}
