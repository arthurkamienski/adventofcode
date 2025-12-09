grammar JunctionBoxes {
    token TOP { [<box-coord>\n]+ }
    token box-coord { <coord>','<coord>','<coord> }
    token coord { \d+ }
}

class Connector {
    has %circuits;

    method TOP ($/) {
        my %dists;

        for 0..^($/<box-coord>.elems-1) -> $i {
            for ($i+1)..^$/<box-coord>.elems -> $j {
                my $a = $/<box-coord>[$i];
                my $b = $/<box-coord>[$j];

                %dists{$((~$a, ~$b))} = self.dist($a<coord>, $b<coord>);
            }
        }

        my @sorted-connections = (%dists.sort: *.value)>>.keys;

        for 0..^1000 -> $i {
            my ($a, $b) = @sorted-connections[$i].split(" ");

            # say "Connecting ($a) and ($b)";

            self.union($a, $b)
        }


        my %sizes is default(0);
        for %circuits.keys.sort -> $box {
            my $parent = self.find($box);
            %sizes{$parent}++;
        }


        say [*] ((%sizes.sort: -*.value)>>.value).head(3);

        my %parents is SetHash = %sizes.keys;
        my $i = 1000;

        while %parents.elems > 1 {
            my ($a, $b) = @sorted-connections[$i].split(" ");

            my $parent-a = self.find($a);
            my $parent-b = self.find($b);

            if !($parent-a ~~ $parent-b) {
                self.union($parent-a, $parent-b);

                %parents.unset($parent-a);
            }

            $i++;
        }

        my ($a, $b) = @sorted-connections[$i-1].split(" ");
        say $a.split(",")[0] * $b.split(",")[0];
    }

    method dist(@a, @b) {
        return [+] (@a Z @b).map({(+$_[0] - +$_[1]) ** 2});
    }

    method box-coord ($/) {
        %circuits{~$/} = ~$/;
    }

    method union($a, $b) {
        my $parent-a = self.find($a);
        my $parent-b = self.find($b);
        %circuits{$parent-a} = %circuits{$parent-b};
    }

    method find($a) {
        if %circuits{$a} ~~ $a {
            return $a;
        } else {
            %circuits{$a} = self.find(%circuits{$a});

            return %circuits{$a};
        }
    }
}

my $input = slurp "2025/day08/input.txt";

JunctionBoxes.parse($input, actions => Connector.new);
