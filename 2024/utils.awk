function abs(n) {
    return n < 0 ? -n : n;
}

function sign(n) {
    return n < 0 ? -1 : n > 0 ? 1 : 0;
}

function coord(x, y) {
    return x SUBSEP y;
}

function get_neighbors(pos, ns, diagonal) {
    split(pos, a, SUBSEP);
    x = a[1];
    y = a[2];
    for(i=-1; i<=1; i++) {
        for(j=-1; j<=1; j++) {
            if ((abs(i) + abs(j)) % 2 == 0 && !diagonal) continue;

            ns[coord(x+i, y+j)] = 1;
        }
    }
}

function insert(h, element, priority,      last_pos) {
    last_pos = length(h) + 1;
    h[last_pos]["el"] = element;
    h[last_pos]["priority"] = priority;

    fix_up(h, last_pos);
}

function pop(h,     min) {
    h[1]["el"] = h[length(h)]["el"];
    h[1]["priority"] = h[length(h)]["priority"];
    delete h[length(h)];
    fix_down(h, 1);
}

function peek_element(h) {
    return h[1]["el"];
}

function peek_priority(h) {
    return h[1]["priority"];
}

function fix_up(h, pos) {
    parent = (pos - pos % 2) / 2;

    if (pos > 1 && h[parent]["priority"] > h[pos]["priority"]) {
        swap(h, parent, pos);
        fix_up(h, parent);
    }
}

function fix_down(h, pos,      left, right, min) {
    left = 2 * pos;
    right = 2 * pos + 1;
    min = pos;

    if (left <= length(h) && h[left]["priority"] < h[min]["priority"]) {
        min = left;
    }

    if (right <= length(h) && h[right]["priority"] < h[min]["priority"]) {
        min = right;
    }

    if (min != pos) {
        swap(h, pos, min);
        fix_down(h, min);
    }
}

function swap(h ,i, j) {
    temp_el = h[i]["el"];
    temp_priority = h[i]["priority"];
    h[i]["el"] = h[j]["el"];
    h[i]["priority"] = h[j]["priority"];
    h[j]["el"] = temp_el;
    h[j]["priority"] = temp_priority;
}
