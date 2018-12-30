import time
import re
from heapq import *

def extract(raw_data):
    clay = []
    for line in raw_data:
        match = re.match('([xy])=([0-9]+)\,\ [xy]=([0-9]+)\.\.([0-9]+)', line).groups()
        pos = [int(x) for x in match[1:4]]

        if match[0] == 'x':
            c_x = pos[0]

            for i in range(pos[1], pos[2]+1):
                clay.append(((c_x, i)))
        else:
            c_y = pos[0]

            for i in range(pos[1], pos[2]+1):
                clay.append((i, c_y))

    return clay

class Tile:
    def __init__(self, pos, char):
        self.pos = pos
        self.char = char

    def is_clay(self):
        return self.char == '#'

    def is_free(self):
        return self.char == '.'

    def is_water(self):
        return self.char == '~'

    def __repr__(self):
        return str(self.pos)

    def __str__(self):
        return str(self.pos)

    def __lt__(self, other):
        if self.char == '|' and other.char == '|':
            if self.pos[1] == other.pos[1]:
                return self.pos[0] < other.pos[0]
            else:
                return self.pos[1] < other.pos[1]
        elif self.char == '|':
            return False
        else:
            return self.pos[1] > other.pos[1]

    def flow(self, ground):
        new = []
        tile_below = ground.tile_below(self)


        if tile_below.is_free() or tile_below.char == '|':
            self.char = '|'
            tile_below.char = '|'

            if tile_below.pos[1] < ground.max_y:
                heappush(ground.waters, tile_below)
        else:
            tile_right = ground.tile_right(self)
            tile_left = ground.tile_left(self)

            if self.char == '|':
                if ground.is_recipient(*self.pos):
                    self.char = '~'
                    heappush(ground.waters, self)

            else:
                if ground.tile_above(self).char == '|':
                    heappush(ground.waters, ground.tile_above(self))

            if not tile_right.is_clay() and not tile_right.char == self.char:
                tile_right.char = self.char
                heappush(ground.waters, tile_right)

            if not tile_left.is_clay() and not tile_left.char == self.char:
                tile_left.char = self.char
                heappush(ground.waters, tile_left)

        return new

class Ground:
    def __init__(self, clay):
        self.waters = []
        self.min_x = min([c[0] for c in clay])
        self.max_x = max([c[0] for c in clay])
        self.max_y = max([c[1] for c in clay])
        self.min_y = min([c[1] for c in clay])

        self.create_layout(clay)
        self.tile_at(500, 1).char = '|'
        self.add_water(self.tile_at(500, 1))

    def create_layout(self, clay):
        self.layout = {}
        for c in clay:
            self.layout[c[0]] = self.layout.get(c[0], {})
            self.layout[c[0]][c[1]] = Tile(c, '#')

        self.layout[500] = self.layout.get(500, {})
        self.layout[500][0] = Tile(c, '+')

    def tile_at(self, i, j):
        self.layout[i] = self.layout.get(i, {})
        self.layout[i][j] = self.layout[i].get(j, Tile((i, j), '.'))
        return self.layout[i][j]

    def tile_below(self, tile):
        return self.tile_at(tile.pos[0], tile.pos[1]+1)

    def tile_left(self, tile):
        return self.tile_at(tile.pos[0]-1, tile.pos[1])

    def tile_right(self, tile):
        return self.tile_at(tile.pos[0]+1, tile.pos[1])

    def tile_above(self, tile):
        return self.tile_at(tile.pos[0], tile.pos[1]-1)

    def is_recipient(self, x, j):
        recipient = False
        i = x

        while not self.tile_at(i, j).is_clay() and not self.tile_at(i, j+1).is_free():
            i = i + 1

        if self.tile_at(i, j).is_clay():
            i = x

            while not self.tile_at(i, j).is_clay() and not self.tile_at(i, j+1).is_free():
                i = i - 1

                if self.tile_at(i, j).is_clay():
                    recipient = True

        return recipient


    def print_layout(self):
        for i in range(self.max_y+1):
            for j in range(self.min_x-1, self.max_x+2):
                print(self.tile_at(j, i).char, end='')
            print()
        print()


    def add_water(self, tile):
        self.waters.append(tile)

    def flow(self):
        while self.waters != []:
            w = heappop(self.waters)

            new = w.flow(self)

            # time.sleep(0.1)

            # self.print_layout()

    def count_water(self):
        count = 0
        for i in range(self.min_y, self.max_y+1):
            for j in range(self.min_x-1, self.max_x+2):
                if self.tile_at(j, i).char in '~|':
                    count = count + 1
        return count

    def count_rest(self):
        count = 0
        for i in range(self.min_y, self.max_y+1):
            for j in range(self.min_x-1, self.max_x+2):
                if self.tile_at(j, i).char == '~':
                    count = count + 1
        return count


    def to_file(self):
        with open('out.txt', 'w') as output:
            for i in range(self.max_y+1):
                for j in range(self.min_x-1, self.max_x+2):
                    output.write(self.tile_at(j, i).char)
                output.write('\n')

def main():
    with open('input.txt', 'r') as raw_data:
        clay = extract(raw_data)
        ground = Ground(clay)

        # ground.print_layout()
        
        ground.flow()
        
        print(ground.count_water())
        print(ground.count_rest())
        ground.to_file()

if __name__ == '__main__':
    main()