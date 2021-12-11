use std::fs;

fn compare_depths(input: &Vec<i32>) {
    let mut prev: Option<i32> = None;
    let mut count = 0;

    for depth in input {
        match prev {
            Some(prev) => if prev < *depth { 
                count += 1;
            }
            None => ()
        }

        prev = Some(*depth);
    }

    println!("{}", count);
}

fn compare_windows(input: &Vec<i32>) {
    let mut prev: Option<i32> = None;
    let mut i = 0;
    let mut count = 0;

    while i < input.len() - 2 {
        let next3 = &input[i..i+3];
        let depth: i32 = next3.iter().sum();

        match prev {
            Some(prev) => if prev < depth { 
                count += 1;
            }
            None => ()
        }

        prev = Some(depth);
        i += 1;
    }
    println!("{:?}", count);
}


fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let input: Vec<i32> = input.lines().map(|s| s.parse().unwrap()).collect();

    compare_depths(&input);
    compare_windows(&input);
}
