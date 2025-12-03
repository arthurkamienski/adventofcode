grammar ProductIDRanges {
    token TOP { [<id-range>','?]+ }
    token id-range { <id>'-'<id> }
    token id { \d+ }
}

class RangeParser {
    method TOP ($/) {
        make %(
            twice-repeated => ([+] $/<id-range>>>.made>><twice-repeated>),
            n-repeated => ([+] $/<id-range>>>.made>><n-repeated>)
        );
    }

    method id-range ($/) {
        my $range = +$/<id>[0]..+$/<id>[1];

        my $twice-repeated = [+] $range.race.grep( * ~~ /^ (\d+) $0 $/ );
        my $n-repeated = [+] $range.race.grep( * ~~ /^ (\d+) $0+ $/ );

        make %(
            twice-repeated => $twice-repeated,
            n-repeated => $n-repeated
        );
    }
}

my $input = slurp "2025/day02/input.txt";

say ProductIDRanges.parse($input, actions => RangeParser.new).made;
