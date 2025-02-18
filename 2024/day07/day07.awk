#!/usr/bin/awk -f

@include "utils.awk"

function check_op(curr_res, n, op, three_op) {
    if (curr_res > exp_res) {
        return 0;
    }

    if (n > NF) {
        return curr_res == exp_res;
    }

    if (op == "*") {
        new_res = curr_res * $n;
    } else if (op == "+") {
        new_res = curr_res + $n;
    } else if (op == "||") {
        new_res = (curr_res "" $n)+0;
    }

    if(three_op) {
        return check_three_op(new_res, n+1);
    } else {
        return check(new_res, n+1);
    }

}

function check(curr_res, n) {
    if (check_op(curr_res, n, "+")) {
        return 1;
    }
    if (check_op(curr_res, n, "*")) {
        return 1;
    }
    return 0;
}

function check_three_op(curr_res, n) {
    if (check_op(curr_res, n, "||", 1)) {
        return 1;
    }
    if (check_op(curr_res, n, "+", 1)) {
        return 1;
    }
    if (check_op(curr_res, n, "*", 1)) {
        return 1;
    }
    return 0;
}

{
    gsub(":", "", $1)

    exp_res = $1 + 0;
    curr_res = $2 + 0;

    if(check(curr_res, 3)) count += $1;
    if(check_three_op(curr_res, 3)) count_three_op += $1;
}

END {
    print count;
    print count_three_op;
}
