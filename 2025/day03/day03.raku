grammar BatteryBank {
    token TOP { [<bank>\n]+ }
    token bank { <battery>+ }
    token battery { \d }
}

class BatteryBankParser {
    method TOP($/) {
        make [+] $/<bank>>>.made;
    }

    method bank($/) {
        my @batteries-with-index = $/<battery> Z (0...Inf);

        make self.find-largest(@batteries-with-index, -1, 12);
    }

    method find-largest(@batteries-with-index, $start-index, $n-to-choose) {
        my ($joltage, $index) = @batteries-with-index[($start-index+1)..^*-($n-to-choose-1)].max({$_[0], -$_[1]});

        return $n-to-choose == 1 
            ?? $joltage
            !! $joltage ~ self.find-largest(@batteries-with-index, $index, $n-to-choose-1);
    }
}

my $input = slurp "2025/day03/input.txt";

say BatteryBank.parse($input, actions => BatteryBankParser.new).made;
