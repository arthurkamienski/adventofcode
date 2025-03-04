#!/usr/bin/awk -f

@include "utils.awk"

{
    secret = $0;
    last = secret % 10;
    secrets[0] = last;
    for(i=0; i<2000; i++) {
        secret = xor(secret * (2 ** 6), secret) % (2 ** 24);
        secret = xor(int(secret / (2 ** 5)), secret) % (2 ** 24);
        secret = xor(secret * (2 ** 11), secret) % (2 ** 24);

        seq[i] = secret % 10 - last;
        secrets[i+1] = secret;
        last = secret % 10;
    }

    acc += secrets[2000];

    delete seen;

    for (i=0; i<2000-4; i++) {
        str = "";
        for(j=0; j<4; j++) {
            str = str "," seq[i+j];
        }

        if(!(str in seen)) vs[str] += secrets[i+4] % 10;

        seen[str] = 1;
    }
}

END {
    print acc;
    max = 0;
    for (v in vs) {
        if (vs[v] > max) {
            max = vs[v];
        }
    }
    print max;
}
