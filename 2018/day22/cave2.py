from heapq import *

class Area:
    def __init__(self, x, y, top, left):
        self.x = x
        self.y = y
        self.geo_index = None
        self.type = None
        self.erosion = None
        self.risk = None

        self.top = None
        self.left = None
        self.bot = None
        self.right = None

        if top:
            self.top = top
            top.bot = self

        if left:
            self.left = left
            left.right = self

    def neighbors(self):
        return [n for n in [self.top, self.left, self.right, self.bot] if n]

class Cave:
    def __init__(self, start, target, depth):
        self.start = start
        self.target = target
        self.depth = depth
        self.map = {}

        self.make_map()

    def make_map(self):
        x, y = self.target
        for i in range(x+1):
            for j in range(y+1):
                top = self.map[(i, j-1)] if j > 0 else None
                left = self.map[(i-1, j)] if i > 0 else None

                self.map[(i, j)] = Area(i, j, top, left)

    def geo_index(self, x, y):
        area = self.map[(x, y)]

        if not area.geo_index:
            if (x, y) == self.start or (x, y) == self.target:
                area.geo_index = 0
            elif y == 0:
                area.geo_index = x * 16807
            elif x == 0:
                area.geo_index = y * 48271
            else:
                area.geo_index = self.erosion(x, y-1) * self.erosion(x-1, y)

        return area.geo_index

    def erosion(self, x, y):
        area = self.map[(x, y)]

        if not area.erosion:
            area.erosion = (self.geo_index(x, y) + self.depth) % 20183

        return area.erosion


    def risk(self, x, y):
        area = self.map[(x, y)]

        if not area.risk:
            area.risk = self.erosion(x, y) % 3
        return area.risk

    def type(self, x, y):
        area = self.map[(x, y)]

        if not area.type:
            if (x, y) == self.start:
                area.type = 'M'
            elif (x, y) == self.target:
                area.type = 'T'
            else:
                risk = self.risk(x, y)

                if risk == 0:
                    area.type = '.'
                elif risk == 1:
                    area.type = '='
                else:
                    area.type = '|'

        return area.type

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
                print(self.type(i, j), end='')
            print()

    def route(self):
        def dist(area):
            x, y = self.target
            return abs(x - area.x) + abs(y - area.y)

        def switch(curr, n, tool):
            tools = {
                '.': ['torch', 'climbing'],
                '=': ['climbing', 'neither'],
                '|': ['torch', 'neither']
            }

            switch = 0

            if curr.type != n.type:
                if tool not in tools[n.type]:
                    switch = 7
                    tool = tools[curr.type][0] if tool != tools[curr.type][0] else tools[curr.type][1]

            return switch, tool

        INF = 999999999999

        target = self.map[self.target]
        start = self.map[self.start]

        curr = start

        visited = set()        

        source = {start:None}
        score = {start: dist(start)}
        time = {start: 0}
        tool = {start: 'torch'}

        queue = [(score[start], start)]

        while curr != target and queue != []:
            s, curr = heappop(queue)

            if curr != target and curr not in visited:
                visited.add(curr)

                for n in curr.neighbors():

                    s, t = switch(curr, n, tool[curr])
                    new_score = dist(n) + time[curr] + 1 + s

                    if score.get(n, INF) > new_score:
                        tool[n] = t
                        source[n] = curr
                        score[n] = new_score

                        heappush(queue, (new_score, n))

def main():
    start = (0, 0)
    # depth = 510
    # target = (10, 10)
    depth = 3879
    target = (8, 713)

    cave = Cave(start, target, depth)

    # cave.print_map()
    # print(cave.erosion(1, 1))

    # print(cave.total_risk())
    cave.route()

if __name__ == '__main__':
    main()