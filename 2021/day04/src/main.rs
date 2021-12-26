use std::fs;
use regex::Regex;

fn str_to_int(s: &str) -> u32 { s.parse::<u32>().unwrap() }

struct Board {
    numbers: Vec<u32>,
    nrows: usize,
    ncols: usize,
    row_marks: Vec<u32>,
    col_marks: Vec<u32>
}

impl Board {
    fn from_str(board: &str) -> Board {
        fn split_row(row: &str) -> Vec<&str> { Regex::new(r"\ +").unwrap().split(row).collect() }
        fn parse_row(row: &str) -> Vec<u32> { split_row(row).into_iter().map(str_to_int).collect() }

        let rows: Vec<Vec<u32>> = board.lines().map(parse_row).collect();
        let nrows = rows.len();
        let ncols = rows[0].len();

        Board {
            numbers: rows.into_iter().flatten().collect(),
            nrows: nrows,
            ncols: ncols,
            row_marks: vec![0; nrows],
            col_marks: vec![0; ncols],
        }
    }

    fn vec_pos_to_coord(&self, pos: usize) -> (usize, usize) {
        let row = pos / self.nrows;
        let col = pos % self.ncols;

        (row, col)
    }

    fn get_number_pos(&self, number: u32) -> Option<usize> {
        self.numbers.iter().position(|&n| n == number)
    }

    fn mark_number(&mut self, number: u32) {
        if let Some(pos) = self.get_number_pos(number) {
            let (x, y) = self.vec_pos_to_coord(pos);

            self.row_marks[x] += 1;
            self.col_marks[y] += 1;
        }
    }

    fn is_winner(&self) -> bool {
        self.row_marks.iter().filter(|n| n >= self.nrows).len()
    }
}

struct Bingo {
    numbers: Vec<u32>,
    boards: Vec<Board>
}

impl Bingo {
    fn from_file(path: &str) -> Bingo {
        let content = fs::read_to_string(path).unwrap();
        Bingo::from_str(&content)
    }


    fn from_str(content: &str) -> Bingo {
    
        fn parse_numbers(numbers: &str) -> Vec<u32> {
            numbers.split(",").map(str_to_int).collect() 
        }
    
        let mut chunks = content.split("\n\n");
    
        let numbers = parse_numbers(chunks.next().unwrap());
        let boards = chunks.map(Board::from_str).collect();
    
        Bingo {
            numbers: numbers,
            boards: boards
        }
    }
}

fn main() {}
