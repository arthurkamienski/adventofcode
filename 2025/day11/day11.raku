grammar Connections {
    token TOP { [<connection>\n]+ }
    token connection { <device>': '[<device>' ']*<device> }
    token device { <:alpha>+ }
}

class Device {
    has Set $.inputs is rw;
    has Set $.outputs;
    has Int %.paths is rw = {"" => 0};
}

class Parser {
    has %.devices;

    method TOP ($/) {
        for %.devices.kv -> $k, $v {
            for $v.outputs.keys -> $out {
                if %.devices{$out}:exists {
                    %.devices{$out}.inputs = %.devices{$out}.inputs (|) $k;
                } else {
                    %.devices{$out} = Device.new(inputs => (set ($k)), outputs => Set.new);
                }
            }
        }

        for %.devices<out>.inputs.keys -> $input {
            if $input ~~ "dac" || $input ~~ "fft" {
                %.devices{$input}.paths{$input} = 1;
            } else {
                %.devices{$input}.paths{""} = 1;
            }
        }

        my $visited = %.devices<out>.inputs (|) "out";

        while $visited.elems != %.devices.elems {
            my @new-visited = %.devices.grep({ !($_.key (elem) $visited) }).grep({ ($_.value.outputs (-) $visited).elems == 0 });

            for @new-visited -> $device {
                my @outputs = $device.value.outputs.keys;

                for @outputs -> $output {
                    for %.devices{$output}.paths.kv -> $key, $paths {
                        my $true-key = $key;
                        if $device.key ~~ "dac" || $device.key ~~ "fft" {
                            $true-key ~= $device.key;
                        }

                        $device.value.paths{$true-key} += $paths;
                    }
                }

            }

            $visited = $visited (|) @new-visited.map(*.key);
        }

        say [+] %.devices<you>.paths.values;

        if %.devices<svr>.paths<dacfft>:exists {
            say %.devices<svr>.paths<dacfft>;
        } else {
            say %.devices<svr>.paths<fftdac>;
        }
    }

    method connection($/) {
        my $outputs = set $/<device>[1..*-1].map({~$_});
        %.devices{$/<device>[0]} = Device.new(inputs => Set.new, outputs => $outputs);
    }
}

my $input = slurp '2025/day11/input.txt';

Connections.parse($input, actions=>Parser.new);
