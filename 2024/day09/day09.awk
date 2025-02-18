#!/usr/bin/awk -f

@include "utils.awk"

BEGIN {
    RS = ""; FS = "";
}

function move_last_file_blocks_to_space(space_start, space_end) {
    curr_space_len = space_end - space_start + 1;

    last_file_start = fragmented_memory[last_file]["start"];
    last_file_end = fragmented_memory[last_file]["end"];
    last_file_len = last_file_end - last_file_start + 1;

    # print "Last file: " fragmented_memory[last_file]["id"] " (" last_file_start ", " last_file_end ")";

    if (last_file_len == curr_space_len) {
        # print "Last file fits exactly in space";
        fragmented_memory[last_file]["start"] = space_start;
        fragmented_memory[last_file]["end"] = space_end;
        last_file--;
        next_space_start = space_end + 1;
    }  else if (last_file_len > curr_space_len) {
        fragmented_memory[last_file]["end"] -= curr_space_len;
        fragmented_memory[n_files]["start"] = space_start;
        fragmented_memory[n_files]["end"] = space_end;
        fragmented_memory[n_files]["id"] = fragmented_memory[last_file]["id"];
        n_files++;
        next_space_start = space_end + 1;
    } else {
        # print "Last file is smaller than space";
        fragmented_memory[last_file]["start"] = space_start;
        fragmented_memory[last_file]["end"] = space_start + last_file_len - 1;
        last_file--;
        next_space_start = space_start + last_file_len;
    }

    return next_space_start;
}

function add_file(mem, id, start, end) {
    mem[id]["start"] = start;
    mem[id]["end"] = end;
    mem[id]["id"] = id;
}

function init_pointers() {
    for (i=0; i < length(spaces); i++) {
        memory_pointers[i]["next"] = i+1;
        memory_pointers[i]["prev"] = i-1;
    }
    memory_pointers[-1]["next"] = 0;

    for (i=0; i<length(memory); i++) {
        file_pointers[i]["next"] = i;
        file_pointers[i]["prev"] = i-1;
    }
}

function try_to_move_file(    file_moved, pointer) {
    file_moved = 0;
    pointer = memory_pointers[-1]["next"];
    while(!file_moved && (pointer in memory_pointers)) {
        space_start = spaces[pointer]["start"]
        space_end = spaces[pointer]["end"]

        if (space_start > file_end) break;

        space_len = space_end - space_start + 1;

        if(space_len < file_len) {
            pointer = memory_pointers[pointer]["next"];
            continue;
        } else if (space_len == file_len) {
            memory[file]["start"] = space_start;
            memory[file]["end"] = space_end;

            prev_space = memory_pointers[pointer]["prev"];
            next_space = memory_pointers[pointer]["next"];

            memory_pointers[prev_space]["next"] = next_space;
            memory_pointers[next_space]["prev"] = prev_space;
            delete memory_pointers[pointer];

            file_moved = 1;
        } else {
            memory[file]["start"] = space_start;
            memory[file]["end"] = space_start + file_len - 1;
            spaces[pointer]["start"] = space_start + file_len;
            file_moved = 1;
        }
    }

    return file_moved;
}

function create_space_where_file_was() {
    new_space_id = length(spaces)+1;
    spaces[new_space_id]["start"] = file_start;
    spaces[new_space_id]["end"] = file_end;

    space_before_file = file_pointers[file]["prev"];
    space_after_file = file_pointers[file]["next"];

    memory_pointers[new_space_id]["prev"] = space_before_file;
    memory_pointers[new_space_id]["next"] = space_after_file;

    if (space_before_file in memory_pointers) memory_pointers[space_before_file]["next"] = new_space_id;
    if (space_after_file in memory_pointers) memory_pointers[space_after_file]["prev"] = new_space_id;
}

function checksum(mem,    acc) {
    acc = 0;
    for (file in mem) {
        for (i=mem[file]["start"]; i <= mem[file]["end"]; i++) {
            acc += i*mem[file]["id"];
        }
    }
    return acc;
}

{
    shift = 0;
    for (i=0; i < NF; i++) {
        start = shift;
        end = shift + $(i+1) - 1;
        shift += $(i+1);

        if (i % 2 == 1) {
            id = (i-1)/2
            spaces[id]["start"] = start;
            spaces[id]["end"] = end;
        } else {
            id = i/2;
            add_file(fragmented_memory, id, start, end);
            add_file(memory, id, start, end);
            last_file = id;
        }
    }

    n_files = last_file + 1;

    for (space = 0; space < length(spaces); space++) {
        if (spaces[space]["start"] > fragmented_memory[last_file]["end"]) break;

        space_start = spaces[space]["start"];
        space_len = spaces[space]["end"] - space_start + 1;

        while(space_len > 0) {
            space_start = move_last_file_blocks_to_space(space_start, spaces[space]["end"]);
            space_len = spaces[space]["end"] - space_start + 1;
        }
    }

    init_pointers();

    for (file=length(memory)-1; file >= 0; file--) {
        file_start = memory[file]["start"];
        file_end = memory[file]["end"];
        file_len = file_end - file_start + 1;

        file_moved = try_to_move_file();

        if (file_moved) create_space_where_file_was();
    }

    print checksum(fragmented_memory);
    print checksum(memory);
}

