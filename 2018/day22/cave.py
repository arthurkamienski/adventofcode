from heapq import heappush, heappop, heapify
from math import sqrt

class Branch:
    def __init__(self, pos, tool, time):
        self.pos = pos
        self.tool = tool
        self.time = time

        self.d = 1

    def __lt__(self, other):
        return self.time+self.d < other.time+self.d

    def __repr__(self):
        return f'Branch({self.pos}, {self.tool}, {self.time})'

class Region:
    def __init__(self, x, y, cave):
        self.x = x
        self.y = y
        self.c = (x, y)
        self.cave = cave
        self.calc_index()
        self.calc_erosion()
        self.calc_risk()
        self.calc_type()

    def calc_type(self):
         if self.c == self.cave.start:
             self.type = 'M'
         elif self.c == self.cave.target:
             self.type = 'T'
         else:
             if self.risk == 0:
                 self.type = '.'
             elif self.risk == 1:
                 self.type = '='
             elif self.risk == 2:
                 self.type = '|'
    
    def calc_risk(self):
        self.risk = self.erosion % 3
        
    def calc_erosion(self):
        self.erosion = (self.index + self.cave.depth) % 20183

    def calc_index(self):
        if self.c == self.cave.start or self.c == self.cave.target:
            self.index = 0
        elif self.y == 0:
            self.index = self.x * 16807
        elif self.x == 0:
            self.index = self.y * 48271
        else:
            top = self.cave[self.x, self.y-1]
            bot = self.cave[self.x-1, self.y]
            self.index = top.erosion * bot.erosion

class Cave:
    def __init__(self, start, target, depth):
        self.start = start
        self.target = target
        self.depth = depth
        self.map = {}

    def __getitem__(self, index):
        x, y = index

        if self.map.setdefault(x, {}).get(y) is None:
            self.map[x][y] = Region(x, y, self)

        return self.map[x][y]

    def eval(self, coord):
        x, y = coord

        for i in range(x+1):
            for j in range(y+1):
                self[i, j]

    def total_risk(self):
        total = 0
        x, y = self.target

        for i in range(x+1):
            for j in range(y+1):
                total += self[i, j].risk

        return total

    def print_map(self, end=None):
        if end is None:
            end = self.target
        x, y = end
        for i in range(x+1):
            for j in range(y+1):
                print(self[j, i].type, end='')
            print()

    def dist_target(self, pos):
        return sum([abs(a-b) for a,b in zip(pos, self.target)])

    def route(self):
        queue = []
        #branch = Branch(self.start, 'T', 0)
        branch = [0, self.dist_target((0,0)), (0,0), 'T']
        visited = {((0, 0), 'T'): branch}
        
        tools = {
            '.': {'T', 'C'},
            '=': {'C', 'N'},
            '|': {'N', 'T'},
            'T': {'T', 'C'},
            'M': {'T', 'C'}
        }
           
        import time 
        while branch[2] != self.target or branch[3] != 'T':
            #print('-'*10)
            #print('curr', branch)
            #print()
            #print('queue', queue[:5])
            #print()
            x, y = branch[2]
            curr_type = self[branch[2][0], branch[2][1]].type

            for d in [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]:
                if d[0] >= 0 and d[1] >= 0:
                    dest_type = self[d[0], d[1]].type
                    possible_tools = tools[dest_type] & tools[curr_type]

                    if branch[3] in possible_tools:
                        b = [branch[0]+1, self.dist_target(d), d, branch[3]]
                    else:
                        t = possible_tools.pop()
                        b = [branch[0]+8, self.dist_target(d), d, t]
            
                    key = (b[2], b[3])
                    if key not in visited:
                        #print('add', b)
                        visited[key] = b
                        heappush(queue, b)
                    else:
                        #print('update', b)
                        other = visited[key]
                        if other[0] > b[0]:
                            other[0] = b[0]
                            heapify(queue)
                    
            branch = heappop(queue)
        
        return branch[0]

def main():
    depth = 3879
    target = (8, 713)
    #depth = 510
    #target = (10, 10)
    start = (0, 0)

    cave = Cave(start, target, depth)
    cave.eval((target[0]+50, target[1]+50))

    #cave.print_map()

    print(cave.total_risk())
    print(cave.route())

if __name__ == '__main__':
    main()
