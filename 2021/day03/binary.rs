use std::fs;

#[derive(Debug)]
struct Binary {
    bits: Vec<u32>
}

#[derive(Debug)]
struct Report {
    binaries: Vec<Binary>
}

impl Binary {
    fn empty() -> Binary {
        Binary {bits: Vec::new()}
    }

    fn from_string(s: &str) -> Binary {
        fn char_to_int(c: char) -> u32 { c.to_digit(10).unwrap() }

        let bits = s.chars().map(char_to_int).collect();

        Binary {
            bits: bits
        }
    }

    fn has_more_ones(&self) -> bool {
        let len = self.bits.len();
        let ones = self.bits.iter().sum::<u32>() as usize;
        
        ones >= (len - ones)
    }


    fn to_int(&self) -> usize {
        let binary_str: String = self.bits.iter().map(|i| i.to_string()).collect();
    
        usize::from_str_radix(&binary_str, 2).unwrap()
    }

    fn from_indexes(&self, indexes: &Vec<usize>) -> Binary {
        let bits = indexes.iter().map(|i| self.bits[*i]).collect();

        Binary {
            bits: bits
        }
    }
    
    fn indexes_with(&self, bit: u32) -> Vec<usize> {
        let mut indexes = Vec::new();
        for (i, b) in self.bits.iter().enumerate() {
            if *b == bit {
                indexes.push(i);
            }
        }

        indexes
    }
}

impl Report {
    fn from_file(path: &str) -> Report {
        let content = fs::read_to_string(path).unwrap();
        Report::from_string(&content)
    }

    fn from_string(input: &str) -> Report {
        let lines: Vec<&str> = input.lines().collect();
        let binaries: Vec<Binary> = lines.into_iter().map(Binary::from_string).collect();

        Report {
            binaries: binaries
        }
    }

    fn pivot_binaries(&self) -> Report {
        let mut new_binaries: Vec<Binary> = Vec::new();
        let binary_length = self.binaries[0].bits.len();

        for i in 0..binary_length {
            let mut new_binary = Binary::empty();

            for bin in &self.binaries {
                new_binary.bits.push(bin.bits[i]);
            }

            new_binaries.push(new_binary);
        }

        Report { binaries: new_binaries }
    }

    fn gamma_rate(&self) -> usize {
        let mut gamma = Binary::empty();

        for i in 0..(self.binaries.len()) {
            let bit = self.binaries[i].has_more_ones() as u32;
            gamma.bits.push(bit);
        }

        gamma.to_int()
    }

    fn epsilon_rate(&self) -> usize {
        let mut eps = Binary::empty();

        for i in 0..(self.binaries.len()) {
            let bit = !(self.binaries[i].has_more_ones()) as u32;
            eps.bits.push(bit);
        }

        eps.to_int()
    }

    fn power_consumption(&self) -> usize {
        self.epsilon_rate() * self.gamma_rate()
    }

    fn filter_bit_at_pos(&self, bit: u32, pos: usize) -> Report {
        let indexes = self.binaries[pos].indexes_with(bit);

        let mut new_binaries = Vec::new();

        for i in 0..(self.binaries.len()) {
            let new_bin = self.binaries[i].from_indexes(&indexes);
            new_binaries.push(new_bin);
        }

        Report {
            binaries: new_binaries
        }
    }

    fn filter_most_common(&self, pos: usize) -> Report {
        let bit = self.binaries[pos].has_more_ones() as u32;

        self.filter_bit_at_pos(bit, pos)
    }

    fn filter_least_common(&self, pos: usize) -> Report {
        let bit = !(self.binaries[pos].has_more_ones()) as u32;

        self.filter_bit_at_pos(bit, pos)
    }

    fn oxigen_generator_rating_pos(&self, pos: usize) -> usize {
        if self.binaries[0].bits.len() > 1 {
            self.filter_most_common(pos).oxigen_generator_rating_pos(pos+1)
        } else {
            let pivoted = self.pivot_binaries();
            pivoted.binaries[0].to_int()
        }
    }

    fn co2_scrubber_rating_pos(&self, pos: usize) -> usize {
        if self.binaries[0].bits.len() > 1 {
            self.filter_least_common(pos).co2_scrubber_rating_pos(pos+1)
        } else {
            let pivoted = self.pivot_binaries();
            pivoted.binaries[0].to_int()
        }
    }

    fn co2_scrubber_rating(&self) -> usize {
        self.co2_scrubber_rating_pos(0)
    } 

    fn oxigen_generator_rating(&self) -> usize {
        self.oxigen_generator_rating_pos(0)
    } 

    fn life_support_rating(&self) -> usize {
        self.oxigen_generator_rating() * self.co2_scrubber_rating()
    }
}

fn main() {
    let report = Report::from_file("input.txt");
    let report_pivoted = report.pivot_binaries();

    println!("{}", report_pivoted.power_consumption());
    println!("{}", report_pivoted.life_support_rating());
}
