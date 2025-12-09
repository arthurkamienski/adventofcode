grammar Manifold {
    token TOP { <start-row>\n[<row>\n]+ }
    token start-row { '.'+<start>'.'+ }
    token start { 'S' }

    token row { ['.'|<splitter>]+ }
    token splitter { '^' }
}

class BeamSimulator {
    has $beams;
    has $row-length;
    has $n-split = 0;
    has $n-timelines;

    method TOP ($/) {
        say $n-split;
        say $n-timelines;
    }

    method start-row ($/) {
        $beams = Set.new($/<start>.from);
        $row-length = $/.chars;
    }

    method row ($/) {
        my $splitters = $/<splitter>.map({ $_.from - $/.from }).Set;
        my $split-beams = $beams.grep({ .key (elem) $splitters }).cache;
        my $new-beams = $split-beams.map({$_.key - 1 => $_.value}) (+) $split-beams.map({$_.key + 1 => $_.value});
        my $old-beams = $beams.grep({ .key !(elem)  $splitters });

        $n-split += $split-beams.elems;

        $beams = $old-beams (+) $new-beams;
        $n-timelines = [+] $beams.values;
    }
}


my $input = slurp "2025/day07/input.txt";

Manifold.parse($input, actions => BeamSimulator.new);

