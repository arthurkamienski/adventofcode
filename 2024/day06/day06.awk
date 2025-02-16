#!/usr/bin/awk -f

@include "utils.awk"

BEGIN {
    FS = ""
}

{
    for(i = 1; i <= NF; i++) {
        if ($i == "#") {
            map["row"][NR][i] = 1;
            map["col"][i][NR] = 1;
        } else if ($i == "^") {
            positions[1]["x"] = NR;
            positions[1]["y"] = i;
            positions[1]["x_dir"] = 0;
            positions[1]["y_dir"] = -1;
        }
    }
}

function find_next_obs(n, obs,    map_dir, fixed_pos, mov_pos, dir, next_obs_pos) {
    if (positions[n]["x_dir"] == 0) {
        map_dir = "col";
        fixed_pos = positions[n]["y"];
        mov_pos = positions[n]["x"];
        dir = positions[n]["y_dir"];
    } else {
        map_dir = "row";
        fixed_pos = positions[n]["x"];
        mov_pos = positions[n]["y"];
        dir = positions[n]["x_dir"];
    }

    next_obs_pos = -2;
    for (obs_pos in map[map_dir][fixed_pos]) {
        if (obs_pos > mov_pos) {
            if (dir > 0) next_obs_pos = obs_pos+0;
            break;
        }
        next_obs_pos = obs_pos + 0;
    }
    if (next_obs_pos < mov_pos && dir > 0) next_obs_pos = NF+2;

    if (positions[n]["x_dir"] == 0) {
        obs["x"] = next_obs_pos;
        obs["y"] = positions[n]["y"];
    } else {
        obs["x"] = positions[n]["x"];
        obs["y"] = next_obs_pos;
    }
}

function get_position_given_obs(obs, n) {
    positions[n+1]["x"] = obs["x"] - positions[n]["y_dir"];
    positions[n+1]["y"] = obs["y"] - positions[n]["x_dir"];
    positions[n+1]["x_dir"] = -positions[n]["y_dir"];
    positions[n+1]["y_dir"] = positions[n]["x_dir"];
}

function add_visited(n) {
    delta_x = positions[n+1]["x"] - positions[n]["x"];
    delta_y = positions[n+1]["y"] - positions[n]["y"];

    for (i=0; i<(abs(delta_x) + abs(delta_y)); i++) {
        x = positions[n]["x"] + i * positions[n]["y_dir"];
        y = positions[n]["y"] + i * positions[n]["x_dir"];
        visited[x, y] = 1;
    }
}

function pos_in_bounds(n) {
    return positions[n]["x"] > 0 && positions[n]["y"] > 0 && positions[n]["x"] <= NR && positions[n]["y"] <= NF;
}

function log_position(n) {
    history[positions[n]["x"], positions[n]["y"], positions[n]["x_dir"], positions[n]["y_dir"]] = 1;
}

function position_seen_before(n) {
    return history[positions[n]["x"], positions[n]["y"], positions[n]["x_dir"], positions[n]["y_dir"]];
}

function find_path() {
    start_pos_x = positions[1]["x"];
    start_pos_y = positions[1]["y"];

    delete positions;
    delete obs;
    delete history;

    positions[1]["x"] = start_pos_x;
    positions[1]["y"] = start_pos_y;
    positions[1]["x_dir"] = 0;
    positions[1]["y_dir"] = -1;

    n = 1;
    cycle = 0;
    while (pos_in_bounds(n) && !cycle) {
        log_position(n);
        find_next_obs(n, obs);
        get_position_given_obs(obs, n);
        n++;

        cycle = position_seen_before(n);
    }
    return cycle;
}

function add_obs_in_pos(pos_arr) {
    map["row"][pos_arr[1]][pos_arr[2]] = 1;
    map["col"][pos_arr[2]][pos_arr[1]] = 1;
}

function remove_obs_in_pos(pos_arr) {
    delete map["row"][pos_arr[1]][pos_arr[2]];
    delete map["col"][pos_arr[2]][pos_arr[1]];
}

END {
    find_path();
    for (j=1; j<n; j++) add_visited(j);
    print length(visited);

    possible_obs = 0;
    for (pos in visited) {
        if (pos != positions[1]["x"] SUBSEP positions[1]["y"]) {
            split(pos, pos_arr, SUBSEP);
            add_obs_in_pos(pos_arr)
            cycle = find_path();

            possible_obs += cycle;

            remove_obs_in_pos(pos_arr)
        }
    }
    print possible_obs;
}
