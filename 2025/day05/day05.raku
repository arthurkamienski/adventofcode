grammar Database {
    token TOP { [<ingredient-range>\n]+\n[<available-ingredient>\n]+ }
    token ingredient-range { <ingredient-id>'-'<ingredient-id> }
    token available-ingredient { <ingredient-id> }
    token ingredient-id { \d+ }
}

grammar IngredientParser {
    method TOP ($/) {
        my $ranges = $/<ingredient-range>>>.made.any;

        say [+] $/<available-ingredient>.map(* ~~ $ranges);

        my @sorted-ranges = $/<ingredient-range>>>.made.sort( {$_[0], $_[*-1]} );
        my @non-overlapping-ranges = @sorted-ranges[0];

        for @sorted-ranges[1..*] {
            my $last-range = @non-overlapping-ranges[*-1];
            if $last-range[*-1] >= $_[0] {
                @non-overlapping-ranges[*-1] = $last-range[0]..max($_[*-1],$last-range[*-1]);
            } else {
                @non-overlapping-ranges.push($_);
            }
        }

        say [+] @non-overlapping-ranges>>.elems;
    }

    method ingredient-range($/) {
        make +$/<ingredient-id>[0]..+$/<ingredient-id>[1];
    }

    method available-ingredient($/) {
        make ($/ ~~ $<ingredient-range>.made.any);
    }
}
my $input = slurp '2025/day05/input.txt';

Database.parse($input, actions=>IngredientParser.new);
