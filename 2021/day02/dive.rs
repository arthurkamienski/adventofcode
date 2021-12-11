use std::fs;

#[derive(Debug)]
enum Move {
    Up(i32),
    Down(i32),
    Forward(i32)
}

impl Move {
    fn parse(line: &str) -> Move {
        let mut line = line.split(" ");

        let dir = line.next().unwrap();
        let n: i32 = line.next().unwrap().parse().unwrap();

        match dir {
            "up" => Move::Up(n),
            "down" => Move::Down(n),
            "forward" => Move::Forward(n),
            _ => panic!("Unexpected invalid token {:?}", dir)
        }
    }
}

#[derive(Debug)]
struct Coord(i32, i32);

impl Coord {
    fn update(&mut self, m: &Move) {
        match m {
            Move::Up(n) => self.1 -= n,
            Move::Down(n) => self.1 += n,
            Move::Forward(n) => self.0 += n
        }
    }

    fn update_multi(&mut self, ms: &Vec<Move>) {
        for m in ms {
            self.update(&m);
        }
    }
}

#[derive(Debug)]
struct Sub {
    hpos: i32,
    depth: i32,
    aim: i32
}

impl Sub {
    fn update(&mut self, m: &Move) {
        match m {
            Move::Up(n) => self.aim -= n,
            Move::Down(n) => self.aim += n,
            Move::Forward(n) => {
                self.hpos += n;
                self.depth += self.aim * n;
            }
        }
    }

    fn update_multi(&mut self, ms: &Vec<Move>) {
        for m in ms {
            self.update(&m);
        }
    }
}


fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let input = input.lines();
    let moves: Vec<Move> = input.map(Move::parse).collect();
    let mut position = Coord(0, 0);

    position.update_multi(&moves);
    println!("Position 1: {:?}", position);
    println!("Position 1 multi: {}", position.0 * position.1);

    let mut sub = Sub {
        hpos: 0,
        depth: 0,
        aim: 0
    };

    sub.update_multi(&moves);
    println!("Position 2: {:?}", sub);
    println!("Position 2 multi: {}", sub.hpos * sub.depth);
}
