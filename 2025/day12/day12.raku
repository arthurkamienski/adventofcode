grammar Presents {
    token TOP { <present>+<tree>+ }
    token present { \d':'\n[['#'|'.']+\n]+\n }
    token tree { <size> ':' [' '<present-count>]+\n }
    token present-count {<num>}
    token size { <num>'x'<num> } 
    token num { \d+ }
}

class Parser {
    has $.count = 0;
    method TOP ($/) {
        say [+] $/<tree>>>.made;
    }

    method tree($/) {
        my @present-sizes = (7, 5, 7, 6, 7, 7);
        my $area = [*] $/<size><num>;

        my $present-area = [+] ($/<present-count>>><num> Z @present-sizes).map({$_[0] * $_[1]});

        if $present-area > $area {
        }

        make +($present-area <= $area);
    }
}

my $input = slurp "2025/day12/input.txt";

Presents.parse($input, actions=> Parser.new);
