import sys

class Star:
    def __init__(self, cs):
        self.t = tuple(cs)
        self.links = {}
        self.const = None
        self.repr = self
        self.size = 1

    def dist_to(self, other):
        return sum(abs(a-b) for a, b in zip(self.t, other.t))

    def find_repr(self):
        if self.repr != self:
            self.repr = self.repr.find_repr()
        return self.repr

    def __repr__(self):
        return f'Star{self.t}'

def union_find(stars):
    for s1 in stars:
        for s2 in s1.links:
            r1 = s1.find_repr()
            r2 = s2.find_repr()

            if r1 != r2:
                if r1.size > r2.size:
                    r2.repr = r1
                    r2.size = r1.size + r2.size
                else:
                    r1.repr = r2
                    r1.size = r1.size + r2.size
    for s in stars:
        s.find_repr()

def read_input(path):
    def parse_stars(cs):
        cs = cs.strip().split(',')
        return [int(c) for c in cs]
        
    with open(path) as f:
        stars = f.read()

    stars = stars.strip().split('\n')
    stars = [parse_stars(cs) for cs in stars]
    stars = [Star(c) for c in stars]
    return stars

def link_stars(stars):
    for star in stars:
        star.links = {s for s in stars if star.dist_to(s) <= 3}

if __name__ == "__main__":
    path = sys.argv[1]

    stars = read_input(path)

    link_stars(stars)

    union_find(stars)

    print(len({s.repr for s in stars}))
