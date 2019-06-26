from heapq import *

class Cave:
    def __init__(self, start, target, depth):
        self.start = start
        self.target = target
        self.depth = depth
        self.map = {}

    def area(self, x, y):
        if x not in self.map:
            self.map[x] = {y: {'ge': None, 'el': None, 'type': None, 'risk': None}}
        elif y not in self.map[x]:
            self.map[x][y] = {'ge': None, 'el': None, 'type': None, 'risk': None}

        return self.map[x][y]

    def erosion(self, x, y):
        area = self.area(x, y)

        if not area['el']:
            area['el'] = (self.geo_index(x, y) + self.depth) % 20183

        return area['el']

    def risk(self, x, y):
        area = self.area(x, y)

        if not area['risk']:
            area['risk'] = self.erosion(x, y) % 3

        return area['risk']

    def type(self, x, y):
        area = self.area(x, y)

        if not area['type']:
            if (x, y) == self.start:
                area['type'] = 'M'
            elif (x, y) == self.target:
                area['type'] = 'T'
            else:
                if self.risk(x, y) == 0:
                    area['type'] = '.'
                elif self.risk(x, y) == 1:
                    area['type'] = '='
                else:
                    area['type'] = '|'

        return area['type']

    def geo_index(self, x, y):
        area = self.area(x, y)

        if not area['ge']:
            if (x, y) == self.start or (x, y) == self.target:
                area['ge'] = 0
            elif y == 0:
                area['ge'] = x * 16807
            elif x == 0:
                area['ge'] = y * 48271
            else:
                area['ge'] = self.erosion(x, y-1) * self.erosion(x-1, y)

        return area['ge']

    def total_risk(self):
        total = 0
        x, y = self.target

        for i in range(x+1):
            for j in range(y+1):
                total = total + self.risk(i, j)

        return total


    def print_map(self):
        x, y = self.target
        for i in range(x+1):
            for j in range(y+1):
                print(self.type(j, i), end='')
            print()

    def route(self):
        def nbs(x, y):
            if x > 0:
                x


        curr = (0, 0)

        queue = [(0, 0)]

def main():
    depth = 3879
    start = (0, 0)
    target = (8, 713)

    cave = Cave(start, target, depth)

    # cave.print_map()
    # print(cave.erosion(1, 1))

    print(cave.total_risk())


if __name__ == '__main__':
    main()