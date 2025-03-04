#!/usr/bin/awk -f

@include "utils.awk"
@load "time";

BEGIN {
    FS = "-";
}

function intersect(a, b, c,   tmp, i) {
    if(length(a) > length(b)) {
        for (i in b) {
            if (i in a) {
                c[i] = 1;
            }
        }
    } else {
        for (i in a) {
            if (i in b) {
                c[i] = 1;
            }
        }
    }
}


{
    comps[$1][$2] = 1;
    comps[$2][$1] = 1;
}

function add_comp(comp, l,     a) {
    intersect(l, comps[comp], a);

    return length(a) == length(l);
}

END {
    for (comp in comps) {
        lan[0][comp][comp] = 1;
    }

    any_new_lan = 1
    i = 0;

    print "This will take some time because I was too lazy to implement a proper solution. Please wait...";

    while(any_new_lan) {
        print "Iteration", i, "| LANs:", length(lan[i]);
        any_new_lan = 0;
        for (l in lan[i]) {
            for (comp in comps) {
                if (comp in lan[i][l]) continue;

                res = add_comp(comp, lan[i][l]);

                if(res) {
                    any_new_lan = 1;
                    c = l "," comp;
                    split(c, a, ",");
                    asort(a);
                    c = "";
                    for(j=1; j<=length(a); j++) {
                        c = c a[j] (j == length(a) ? "" : ",");
                    }

                    lan[i+1][c][comp] = 1;

                    for(comp in lan[i+1][c]) {
                        lan[i+1][c][comp] = 1;
                    }
                }
            }
        }
        i++;
    }

    acc = 0;
    for (l in lan[2]) {
        has_t = 0;
        for (comp in lan[2][l]) {
            substr(comp, 1, 1) == "t" ? has_t = 1 : 0;
        }
        acc += has_t;
    }
    print acc;

    for (l in lan[i-1]) {
        print l;
    }
}
