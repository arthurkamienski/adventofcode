#!/usr/bin/awk -f

function sort_with_rules(i1, v1, i2, v2) {
    if (v1 in comparisons && v2 in comparisons[v1]) {
        return comparisons[v1][v2];
    }
    return 0;
}

/^[0-9]+\|[0-9]+$/ {
    split($0, rules[NR], "|");
    comparisons[rules[NR][1]][rules[NR][2]] = -1;
    comparisons[rules[NR][2]][rules[NR][1]] = 1;
}

/^([0-9]+,)+[0-9]+$/ {
    delete update;

    split($0, pages, ",");
    for (i=1; i <= length(pages); i++) update[pages[i]] = i;

    is_correct = 1;
    for (i=1; i <= length(rules) && is_correct; i++) {
        applies = (rules[i][1] in update) && (rules[i][2] in update);

        if (applies) is_correct = update[rules[i][1]] < update[rules[i][2]];
    }
    if (is_correct) sum += pages[(length(update) + 1)/2];

    if (!is_correct) {
        asort(pages, sorted, "sort_with_rules");
        sum_incorrect += sorted[(length(sorted) + 1)/2];
    }
}

END {
    print sum;
    print sum_incorrect;
}
