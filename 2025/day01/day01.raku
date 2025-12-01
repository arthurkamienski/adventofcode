grammar Password {
    token TOP { [<rotation>\n]+ }
    token rotation { <direction><clicks> }

    proto token direction {*}
    token direction:sym<L> { <sym> }
    token direction:sym<R> { <sym> }
    token clicks { \d+ }
}

class Decoder {
    constant STARTING-POSITION = 50;

    has Int $!position = STARTING-POSITION;
    has Int $!zero-counter = 0;
    has Int $!any-zero-counter = 0;

    method TOP ($/) {
        make %(
            zero-counter => $!zero-counter,
            any-zero-counter => $!any-zero-counter
        );
    }

    method direction:sym<L> ($/) {
        make -1;
    }

    method direction:sym<R> ($/) {
        make 1;
    }

    method rotation($/) {
        my $rotation = $/<direction>.made * +$/<clicks>;

        my $n-times-crossed-zero = $rotation.abs div 100;
        my $positive-rotation = $rotation % 100;

        my $new-position = $!position + $positive-rotation;

        my $crossed-boundary = do if $!position != 0 {
            $rotation > 0 ?? $new-position >= 100 !! $new-position <= 100
        } else {
            0
        };
        
        $n-times-crossed-zero += $crossed-boundary;

        $!position = $new-position % 100;

        $!zero-counter += $!position == 0;
        $!any-zero-counter += $n-times-crossed-zero;
    }
}

my $input = slurp "2025/day01/input.txt";
say Password.parse($input, actions => Decoder.new).made;