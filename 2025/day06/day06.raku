grammar MathProblems {
    regex TOP { [<number-list>\n]+<op-list>\n }
    regex number-list { ' '*[<number>' '+]+?<number>' '* }
    token number { \d+ }
    regex op-list { ' '*[<op>' '+]+?<op>' '* }
    proto token op {*}
    token op:sym<*> { <sym> }
    token op:sym<+> { <sym> }
}

class Solver {
    method TOP ($/) {
        my $sum = 0;
        for 0..^$/<number-list>[0]<number>.elems -> $n {
            my @nums = $/<number-list>>><number>>>[$n];
            my $op = $/<op-list><op>[$n];

            if $op ~~ '+' {
                $sum += [+] @nums;
            } else {
                $sum += [*] @nums;
            }
        }
        say $sum;

        my @blocks = ();
        my @curr-block = ();
        for 0..^$/<number-list>[0].chars -> $i {
            my $is-divider = True;
            my $num = "";
            for 0..^$/<number-list>.elems -> $j {
                my $char = (~$/<number-list>[$j]).comb[$i];
                $is-divider = $is-divider && !($char ~~ /\d/);
                $num ~= $char;
            }
            if $is-divider {
                @blocks.push(@curr-block.clone);
                @curr-block = ();
            } else {
                @curr-block.push(+$num);
            }
        }
        @blocks.push(@curr-block.clone);

        $sum = 0;

        for 0..^@blocks.elems -> $n {
            my $op = $/<op-list><op>[$n];
            if $op ~~ '+' {
                $sum += [+] |@blocks[$n];
            } else {
                $sum += [*] |@blocks[$n];
            }
        }
        say $sum;
    }
}

my $input = slurp "2025/day06/input.txt";

MathProblems.parse($input, actions => Solver.new);