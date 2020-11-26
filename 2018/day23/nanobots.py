import re
import random
import heapq

class Pos:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z
        self.t = (x, y, z)

    def __add__(self, pos):
        return Pos(self.x + pos.x, self.y + pos.y, self.z + pos.z)

    def __sub__(self, pos):
        return Pos(self.x - pos.x, self.y - pos.y, self.z - pos.z)

    def __truediv__(self, n):
        return Pos(self.x//n, self.y//n, self.z//n)

    def __mul__(self, n):
        return Pos(self.x*n, self.y*n, self.z*n)

    def __lt__(self, other):
        return (self.x+self.y+self.z) < (other.x+other.y+other.z)

    def dist_to(self, pos):
        return sum(abs(a-b) for a, b in zip(self.t, pos.t))
    
    def nanobots_in_range(self, ns):
        return sum(1 for n in ns if self.dist_to(n.pos) <= n.r)

    def is_in_range_of(self, n):
        return self.dist_to(n.pos) <= n.r

    def __repr__(self):
        return f'Pos{self.t}'

    def __str__(self):
        return str(self.t)

class Nanobot:
    def __init__(self, x, y, z, r):
       self.pos = Pos(x, y, z)
       self.r = r
       self.max_reaches = Pos(x+r, y+r, z+r)
       self.min_reaches = Pos(x-r, y-r, z-r)

    def dist_to(self, nanobot):
        return self.pos.dist_to(nanobot.pos)

    def nanobots_in_range(self, ns):
        return sum(1 for n in ns if self.dist_to(n) <= self.r)

    def overlaps_with(self, n):
        return self.r + n.r >= self.dist_to(n)

    def equal_length(self, n):
        return self.r + n.r == self.dist_to(n)

    def __repr__(self):
        return f'Nanobot({self.pos}, {self.r})'

def read_nanobots():
    def parse_line(line):
        line_re = '<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)'
        gs = re.search(line_re, line).groups()    
        gs = [int(i) for i in gs]
        return Nanobot(*gs)
        
    with open('input.txt', 'r') as input_file:
        ls = input_file.readlines()

    return [parse_line(l) for l in ls]

def max_overlaps(overlaps):
    mo = max(overlaps, key=lambda n: len(overlaps & n.overlaps))
    overlaps = mo.overlaps & overlaps
    
    if len(overlaps) > 0:
        return {mo} | max_overlaps(overlaps)
    else:
        return {mo}

def bin_search(bots):
    def min_pos():
        x = min(n.min_reaches.x for n in bots)
        y = min(n.min_reaches.y for n in bots)
        z = min(n.min_reaches.z for n in bots)
        return Pos(x,y,z)

    def max_pos():
        x = max(n.max_reaches.x for n in bots)
        y = max(n.max_reaches.y for n in bots)
        z = max(n.max_reaches.z for n in bots)
        return Pos(x,y,z)

    def sub_boxes(b):
        minp, maxp = b
        d = (maxp-minp) / 2

        mins = set()

        for x in {0, d.x}:
            for y in {0, d.y}:
                for z in {0, d.z}:
                    mins.add(Pos(minp.x+x, minp.y+y, minp.z+z))

        boxes = {(p, Pos(p.x+d.x, p.y+d.y, p.z+d.z)) for p in mins}

        return boxes

    def n_intersects(b):
        def intersects(box, bot):
            minp, maxp = box
        
            d = 0
        
            d += abs(bot.pos.x - minp.x) + abs(bot.pos.x - maxp.x) - (maxp.x-minp.x)
            d += abs(bot.pos.y - minp.y) + abs(bot.pos.y - maxp.y) - (maxp.x-minp.x)
            d += abs(bot.pos.z - minp.z) + abs(bot.pos.z - maxp.z) - (maxp.x-minp.x)
            d //= 2

            return d <= bot.r

        return sum(1 for bot in bots if intersects(b, bot))

    def box_size(b):
        minp, maxp = b
        return minp.dist_to(maxp)
    
    def dist_to_origin(b):
        minp, _ = b
        return minp.dist_to(Pos(0,0,0))

    def heap_entry(b):
        return (-n_intersects(b), -box_size(b), dist_to_origin(b), b)
        
        
    minp = min_pos()
    maxp = max_pos()

    b = (minp, maxp)
    h = [heap_entry(b)]
    v = {(b[0].t, b[1].t)}

    while len(h) != 0 and minp.dist_to(maxp) != 0:
        i,s,d,b = heapq.heappop(h)
        minp, maxp = b
        bs = sub_boxes(b)


        for b in bs:
            t = (b[0].t, b[1].t)
            if t not in v:
                v.add(t)
                heapq.heappush(h, heap_entry(b))

    min_dist = b[0].dist_to(Pos(0,0,0))


    print(min_dist)

if __name__ == '__main__':
    nanobots = read_nanobots()
    max_r = max(nanobots, key=lambda n: n.r)
    print(max_r.nanobots_in_range(nanobots))

    for n in nanobots:
        n.overlaps = {m for m in nanobots if n.overlaps_with(m) and n!=m}
    
    #overlapping = max_overlaps(set(nanobots))
    #print(len(overlapping))
    overlapping = nanobots
    bin_search(overlapping)
