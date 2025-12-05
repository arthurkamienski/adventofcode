grammar PaperRolls {
    token TOP { <row>+ }
    token row { <position>+\n }
    token position { '@'| '.' };
}

grammar PaperRollMap {
    has $!parsed-rows = 0;
    has $!parsed-cols = 0;
    has %.paper-map;

    method TOP ($/) {
        my %paper-rolls = %.paper-map.grep({ .value ~~ '@' });
        my $can-remove = Inf;
        my $removed = 0;

        while $can-remove > 0 {
            my %removable-rolls = %paper-rolls.race.grep({ self.count-neighbors($_.key) < 4 });
            $can-remove = %removable-rolls.elems;
            if $removed == 0 {
                say "First pass: $can-remove";
            }
            $removed += $can-remove;
            for %removable-rolls.keys { 
                %paper-rolls{$_}:delete;
                %!paper-map{$_} = '.';
            }
        }
        
        say "Total removed: $removed";
    }

    method row ($/) {
        $!parsed-rows++;
        $!parsed-cols=0;
    }

    method position ($/) {
        my $coord = ($!parsed-rows, $!parsed-cols);
        %.paper-map{$coord} = ~$/;
        $!parsed-cols++;
    }

    method count-neighbors ($coord) {
        my ($x, $y) = $coord.split(" ");
        my $rolls = 0;

        for ($x-1)..($x+1) -> $i {
            for ($y-1)..($y+1) -> $j {
                $rolls += %.paper-map{"$i $j"} ~~ '@';
            }
        }

        return $rolls - 1;
    }
}

my $input = slurp '2025/day04/input.txt';

PaperRolls.parse($input, actions => PaperRollMap.new);
