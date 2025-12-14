grammar RedTiles {
    token TOP { [<coord>\n]+ }
    token coord { <x>','<y> }
    token x { \d+ }
    token y { \d+ }
}

class RectangleFinder {
    method TOP ($/) {
        my $largest-area = 0;
        my $largest-area-inside = 0;

        my $count = 0;

        for 0..^($/<coord>.elems - 1) -> $i {
            for ($i+1)..^$/<coord>.elems -> $j {
                my $coord1 = $/<coord>[$i];
                my $coord2 = $/<coord>[$j];

                my $area = (($coord1<x> - $coord2<x>).abs + 1) * (($coord1<y> - $coord2<y>).abs + 1);

                if $area > $largest-area {
                    $largest-area = $area;
                }

                if $area > $largest-area-inside {
                    if self.is-valid-area($coord1, $coord2, $/<coord>) {
                        $largest-area-inside = $area;
                    }
                }
            }
        }

        say $largest-area;
        say $largest-area-inside;
    }

    method is-valid-area($coord1, $coord2, @red-tiles) {
        my $min-x = (+$coord1<x>, +$coord2<x>).min;
        my $max-x = (+$coord1<x>, +$coord2<x>).max;
        my $min-y = (+$coord1<y>, +$coord2<y>).min;
        my $max-y = (+$coord1<y>, +$coord2<y>).max;

        sub is-tile-inside-x($tile) {
            return +$tile<x> ~~ $min-x^..^$max-x;
        }

        sub is-tile-inside-y($tile) {
            return +$tile<y> ~~ $min-y^..^$max-y;
        }

        sub is-tile-inside($tile) {
            return is-tile-inside-x($tile) && is-tile-inside-y($tile);
        }

        sub crosses-vertically($tile, $next) {
            if is-tile-inside-x($tile) && is-tile-inside-x($next) {
                if (+$tile<y> >= $max-y && +$next<y> <= $min-y) || (+$next<y> >= $max-y && +$tile<y> <= $min-y) {
                    return True;
                }
            }

            return False;
        }

        sub crosses-horizontally($tile, $next) {
            if is-tile-inside-y($tile) && is-tile-inside-y($next) {
                if (+$tile<x> >= $max-x && +$next<x> <= $min-x) || (+$next<x> >= $max-x && +$tile<x> <= $min-x) {
                    return True;
                }
            }

            return False;
        }



        for 0..^@red-tiles.elems -> $i {
            my $tile = @red-tiles[$i];
            my $next = @red-tiles[$i+1 == @red-tiles.elems ?? 0 !! $i+1];

            if is-tile-inside($tile) {
                return False;
            }

            if crosses-horizontally($tile, $next) || crosses-vertically($tile, $next) {
                return False;
            }
        }

        return True;
    }
}

my $input = slurp "2025/day09/input.txt";

RedTiles.parse($input, actions => RectangleFinder.new);
