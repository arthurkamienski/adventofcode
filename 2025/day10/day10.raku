# Arthur, if you are reading this in the future: this was an absolute NIGHTMARE. This code sucks but I CANT look at it anymore.
# Now I hate simplex more than I hated it in college. Really.
# This takes a long time to run (the first part). The second is fine but the code is shit.

grammar Machines {
    token TOP { [<machine>\n]+ }
    token machine { <lights> ' ' [<button>' ']+ <joltages> }
    token lights { '[' <light>+ ']' }
    token light { '.'|'#' }
    token button { '(' [<light-index>',']*<light-index> ')' }
    token light-index { \d }
    token joltages { '{' [<joltage> ',']* <joltage> '}' }
    token joltage { \d+ }
}

class LightsState {
    has $.lights;
    has $.lights-string;
    has $.steps;

    method press($button) {
        my $lights-remaining-on = $.lights (-) $button;
        my $lights-to-turn-on = $button (-) $.lights;
        my $lights-on = $lights-remaining-on (|) $lights-to-turn-on ;
        my $lights-string = (0..^$.lights-string.chars).map({ $_ (elem) $lights-on ?? '#' !! '.' }).join;

        return LightsState.new(steps=> $.steps+1, lights => $lights-on, lights-string => $lights-string);
    }
}

class Machine {
    has $.lights;
    has $.buttons;
    has @.states;
    has $.found-states;

    method new($lights, $buttons, $n-lights) {
        my $lights-string = '.' x $n-lights;
        my $init-state = LightsState.new(steps => 0, lights=>Set.new, lights-string => $lights-string);
        my @states = ($init-state);
        my $found-states = Set($lights-string);

        self.bless(:$lights, :$buttons, :@states, :$found-states);
    }

    method do-round {
        my @new-states;
        my $set-conflict = 0;

        for self.states.kv -> $key, $state {
            for self.buttons -> $button {
                my $new-state = $state.press($button);

                if $new-state.lights (==) $.lights {
                    return $new-state.steps;
                }

                if !($new-state.lights-string (elem) $.found-states) {
                    @new-states.push($new-state);
                    $.found-states = $.found-states (|) set $new-state.lights-string;
                } else {
                    $set-conflict++;
                }
            }
        }

        # say $set-conflict;
        @.states = @new-states;

        return -1;
    }

    method find-min-steps {
        my $min-steps = -1;

        while $min-steps < 0 {
            $min-steps = self.do-round;
        }

        return $min-steps;
    }
}

class ButtonDecoder {
    method TOP ($/) {
        say "Running step 1 (~60 sec)";
        say [+] $/<machine>>>.made.race>>.find-min-steps;
        say "Running step 2";
        my $joltage-button-push = 0;
        for $/<machine> -> $machine {
            # say "=== Starting ===";
            my @joltages = $machine<joltages><joltage>.map(+*);

            my $n-buttons = $machine<button>.elems;
            my @matrix;

            for 0..^@joltages.elems -> $i {
                @matrix[$i] = [0 xx $n-buttons];

                @matrix[$i].push(@joltages[$i]);

                for 0..^$machine<button>.elems -> $j {
                    my $button = $machine<button>[$j];

                    if $i (elem) set $button<light-index>.map(+*) {
                        @matrix[$i][$j] = 1;
                    }
                }
            }

            my $vars = $machine<button>.elems;
            # say $vars;
            $joltage-button-push += find-csp-solution(@matrix, $vars);
        }

        say $joltage-button-push;
    }

    method machine ($/) {
        my $lights = $/<lights><light>.pairs.grep({ .value ~~ "#" })>>.key.Set;
        my $n-lights = $/<lights><light>.elems;
        my $buttons = $/<button>.Array.map({$_<light-index>.map({ +$_ }).Set}).Array;

        make Machine.new($lights, $buttons, $n-lights);
    }
}

sub find-csp-solution(@matrix, $vars) {
    # say "======= Starting! =======";
    my @tableau;
    my $artificial-vars = @matrix.elems;
    my $M = 10000;

    # say "===============================";
    # say "===============================";
    # say "===============================";
    # say "===============================";
    # say "Initial table:";
    # for @matrix -> @row {
    #     say @row;
    # }

    for 0..^@matrix.elems -> $i {
        @tableau[$i] = [@matrix[$i][0..*-2]];

        for 0..^$artificial-vars -> $j {
            @tableau[$i].push($i == $j ?? 1 !! 0);
        }

        @tableau[$i].push(@matrix[$i][*-1]);
    }

    my @obj-row = |[0 xx (@matrix[0].elems-1)], |[1 xx @matrix.elems], 0;
    @tableau.unshift(@obj-row);

    # for @tableau[1..^@tableau.elems] -> @row {
    #     for 0..^@row.elems -> $i {
    #         @tableau[0][$i] -= @row[$i] * $M;
    #     }
    # }

    for @tableau[1..^@tableau.elems] -> @row {
        for 0..^@row.elems -> $i {
            @tableau[0][$i] -= @row[$i];
        }
    }


    # say "After step 1:";
    # for @tableau -> @row {
    #     say @row;
    # }

    while any(@tableau[0][0..*-2]) < 0 {
        my $pivot-col = @tableau[0][0..*-2].pairs.min(:by{$_.value}).key;
        my $pivot-row = @tableau.pairs[1..*].grep({ $_.value[$pivot-col] > 0}).min(:by{$_.value[*-1] / $_.value[$pivot-col]}).key;

        # say "Pivot col: $pivot-col";
        # say "Pivot row: $pivot-row";

        my @rows-to-eliminate = @tableau.pairs.grep({ $_.value[$pivot-col] != 0}).grep({ $_.key != $pivot-row}).map(*.value);

        my $pivot-value = @tableau[$pivot-row][$pivot-col];

        # say "Row before:";
        # say @tableau[$pivot-row];

        for 0..^@tableau[$pivot-row].elems -> $i {
            @tableau[$pivot-row][$i] /= $pivot-value;
        }

        # say "Normalized row:";
        # say @tableau[$pivot-row];

        for @rows-to-eliminate -> @row {
            my $value-to-eliminate = @row[$pivot-col];

            for 0..^@tableau[$pivot-row].elems -> $i {
                @row[$i] -= $value-to-eliminate * @tableau[$pivot-row][$i];
            }
        }
        # say "Iteration result:";
        # for @tableau -> @row {
        #     say @row;
        # }
    }
    # say "Resulting:";
    # for @tableau -> @row {
    #     say @row;
    # }

    if @tableau[0][*-1] != 0 {
        # say "Infeasible!!!";
        return Nil;
    }

    my $cols-to-keep = @tableau[0].elems - 1 - $artificial-vars;

    for $cols-to-keep..^(@tableau[0].elems-1) -> $i {
        my @rows = @tableau.pairs[0..*-1].grep({$_.value[$i] != 0});
        if @rows.elems == 1 {
            # say "Artificial in basis! $i";
            my $pivot-row = @rows[0].key;
            # say "Row: $pivot-row";
            my $pivot = @tableau[$pivot-row][0..^$cols-to-keep].pairs.grep({$_.value != 0});
            if $pivot.elems > 0 {
                my $pivot-col = $pivot[0].key;
                my $pivot-value = $pivot[0].value;
                # say "Pivot col: $pivot-col ($pivot-value)";

                my @rows-to-eliminate = @tableau.pairs.grep({ $_.value[$pivot-col] != 0}).grep({ $_.key != $pivot-row}).map(*.value);

                # say "Rows to eliminate:";
                # say @rows-to-eliminate;

                for 0..^@tableau[$pivot-row].elems -> $i {
                    @tableau[$pivot-row][$i] /= $pivot-value;
                }
                # say "After norm:";
                # for @tableau -> @row {
                #     say @row;
                # }

                for @rows-to-eliminate -> @row {
                    my $value-to-eliminate = @row[$pivot-col];

                    for 0..^@tableau[$pivot-row].elems -> $i {
                        @row[$i] -= $value-to-eliminate * @tableau[$pivot-row][$i];
                    }
                }
                # say "Final:";
                # for @tableau -> @row {
                #     say @row;
                # }
            } else {
                @tableau.splice($pivot-row, 1);
            }
        }
    }

    my @new-tableau;
    for @tableau -> @row {
        my @new-row = |[@row[0..^$cols-to-keep]], @row[*-1];
        @new-tableau.push(@new-row);
    }

    @tableau = @new-tableau;
    @tableau[0] = [|[1 xx $cols-to-keep], 0];
    for 0..^@tableau[0].elems -> $i {
        my @rows-with-one = @tableau[1..*-1].grep({$_[$i] != 0});
        if @rows-with-one.elems == 1 {
            for 0..^@tableau[0].elems -> $j {
                @tableau[0][$j] -= @rows-with-one[0][$j];
            }
        }
    }

    # say "New tableau after removing artificials:";

    # for @tableau -> @row {
    #     say @row;
    # }

    while any(@tableau[0][0..*-2]) < 0 {
        my $pivot-col = @tableau[0][0..*-2].pairs.min(:by{$_.value}).key;
        my $pivot-row = @tableau.pairs[1..*].grep({ $_.value[$pivot-col] > 0}).min(:by{$_.value[*-1] / $_.value[$pivot-col]}).key;

        # say "Pivot col: $pivot-col";
        # say "Pivot row: $pivot-row";

        my @rows-to-eliminate = @tableau.pairs.grep({ $_.value[$pivot-col] != 0}).grep({ $_.key != $pivot-row}).map(*.value);

        my $pivot-value = @tableau[$pivot-row][$pivot-col];

        # say "Row before:";
        # say @tableau[$pivot-row];

        for 0..^@tableau[$pivot-row].elems -> $i {
            @tableau[$pivot-row][$i] /= $pivot-value;
        }

        # say "Normalized row:";
        # say @tableau[$pivot-row];

        for @rows-to-eliminate -> @row {
            my $value-to-eliminate = @row[$pivot-col];

            for 0..^@tableau[$pivot-row].elems -> $i {
                @row[$i] -= $value-to-eliminate * @tableau[$pivot-row][$i];
            }
        }
        # say "Iteration result:";
        # for @tableau -> @row {
        #     say @row;
        # }
    }


    # for $vars..^@tableau[0].elems -> $i {
    #     my @rows-with-one = @tableau.grep({$_[$i] != 0});
    #     if @rows-with-one.elems == 1 && @rows-with-one[0][*-1] != 0 {
    #         say "Artificial in basis!";
    #         for @tableau -> @row {
    #             say @row;
    #         }
    #         return Nil;
    #     }
    # }

    my %solution;
    for 0..^$vars -> $i {
        my @rows-with-one = @tableau.grep({$_[$i] != 0});
        if @rows-with-one.elems == 1 {
            %solution{$i} = @rows-with-one[0][*-1];
        }
    }

    # say -@tableau[0][*-1];
    # say %solution;

    if any(%solution.values) % 1 > 0 {
        # say "No integer solutions found!";
        my $var-to-split = %solution.grep(*.value % 1 > 0).sort(:by{$_.key}).min(:by{abs(($_.value % 1) - 0.5)});

        # say $var-to-split;

        my @new-matrix;
        for @matrix -> @row {
            @new-matrix.push([|@row[0..*-2], 0, @row[*-1]]);
        }

        my @new-constraint = [0 xx @new-matrix[0].elems];
        @new-constraint[$var-to-split.key] = 1;
        @new-constraint[*-2] = 1;
        @new-constraint[*-1] = truncate($var-to-split.value);
        # say "Adding row";
        # say @new-constraint;

        @new-matrix.push(@new-constraint);

        # say "Branching 1";
        my $res = find-csp-solution(@new-matrix, $vars);

        my @solutions;

        if $res.defined {
            @solutions.push($res);
        } else {
            # say "Branch 1 failed.";
        }

        # say "Branching 2";
        @new-constraint[*-2] = -1;
        @new-constraint[*-1] = truncate($var-to-split.value) + 1;

        $res = find-csp-solution(@new-matrix, $vars);

        if $res.defined {
            @solutions.push($res);
        } else {
            # say "Branch 2 failed.";
        }

        if @solutions.elems == 0 {
            # say "No solution!";
            return Nil;
        } elsif @solutions.elems == 1 {
            # say "One solution found: ", @solutions[0];
            return @solutions[0];
        } else {
            # say "Two solutions found: ", @solutions;
            # say "Returning the lower";
            return @solutions.min;
        }
    } else {
        # say %solution;
        # say -@tableau[0][*-1];
        # say [+] %solution.values;
        return [+] %solution.values;
    }
}

my $input = slurp "2025/day10/input.txt";

Machines.parse($input, actions => ButtonDecoder.new);
