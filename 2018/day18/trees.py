import time

class Acre:
    def __init__(self, pos, char):
        self.pos = pos
        self.char = char

    def __str__(self):
        return str(self.pos)

    def next_state(self, neighbours):
        if self.char == '.':
            tree_num = len([n for n in neighbours if n == '|'])

            if tree_num >= 3:
                self.char = '|'
        elif self.char == '|':
            lumber_num = len([n for n in neighbours if n == '#'])

            if lumber_num >= 3:
                self.char = '#'
        elif self.char == '#':
            lumber_num = len([n for n in neighbours if n == '#'])
            tree_num = len([n for n in neighbours if n == '|'])

            if not (lumber_num >= 1 and tree_num >= 1):
                self.char = '.'

class Map:
    def __init__(self, acres):
        self.snapshot = None
        self.acre_list = []
        self.create_layout(acres)
        self.minutes = 0

    def create_layout(self, acres):
        self.acres = {}

        for i, line in enumerate(acres):
            self.acres[i] = {}
            for j, char in enumerate(line):
                acre = Acre((i, j), char)
                self.acres[i][j] = acre
                self.acre_list.append(acre)
        self.take_snapshot()

    def print_layout(self):
        for row in self.acres.values():
            for acre in row.values():
                print(acre.char, end='')
            print()
        print()

    def neighbours_of(self, acre):
        neighbours = []

        min_x = max(acre.pos[0]-1, 0)
        max_x = min(acre.pos[0]+2, len(self.snapshot))
        min_y = max(acre.pos[1]-1, 0)
        max_y = min(acre.pos[1]+2, len(self.snapshot[0]))

        for i in range(min_x, max_x):
            for j in range(min_y, max_y):
                if (i, j) != acre.pos:
                    try:
                        neighbours.append(self.snapshot[i][j])
                    except TypeError as e:
                        pass

        return neighbours

    def take_snapshot(self):
        self.snapshot = []

        for i, row in enumerate(self.acres.values()):
            self.snapshot.append([])
            for acre in row.values():
                self.snapshot[i].append(acre.char)

    def acre_at(self, i, j):
        return self.acres[i][j]

    def tick(self):
        self.minutes = self.minutes + 1
        self.take_snapshot()
        for acre in self.acre_list:
            acre.next_state(self.neighbours_of(acre))
        # self.print_layout()

    def result(self):
        trees = 0
        lumber = 0

        for acre in self.acre_list:
            if acre.char == '|':
                trees = trees + 1
            elif acre.char == '#':
                lumber = lumber + 1

        return (trees, lumber)

    def ten_min(self):
        for i in range(10):
            time.sleep(0.1)
            self.tick()

        trees, lumber = self.result() 
        return trees*lumber

    def one_billion_minutes(self):
        results = set()
        repeated = False
        rep_start = 0
        rep_seq = []

        r = self.result()

        while not repeated:
            results.add(r)
            # time.sleep(0.1)
            self.tick()
            r = self.result()
            # print(r in results)
            if r in results:
                first = r
                rep_start = self.minutes
                rep_seq = [r]
                
                self.tick()
                r = self.result()

                while r in results:
                    if r == first:
                        repeated = True
                        break

                    rep_seq.append(r)
                    results.add(r)
                    # time.sleep(0.1)
                    self.tick()
                    r = self.result()

        trees, lumber = rep_seq[(1000000000-rep_start)%len(rep_seq)]
        
        return trees*lumber


def main():
    with open('input.txt', 'r') as file:
        acres = [f.strip() for f in file]

        lumber_map = Map(acres)

        print(lumber_map.ten_min())
        print(lumber_map.one_billion_minutes())

        # print(lumber_map.result())

if __name__ == '__main__':
    main()