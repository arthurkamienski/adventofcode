from heapq import *

class Room:
    def __init__(self, pos):
        self.pos = pos
        self.neighbours = {}

    def __lt__(self, other):
        return self.pos < other.pos

    def __str__(self):
        return str(self.pos)

    def __repr__(self):
        return str(self.pos)

def find_branch(instructions):
    stack = 0
    paths = [[]]

    for i in instructions:
        if i == '(':
            stack = stack + 1
            paths.append([])
        elif i == ')':
            stack = stack - 1
            paths[stack].append(paths.pop())
        else:
            paths[stack].append(i)

    return merge(paths[0])[0]

def merge(instructions):
    branches = []
    path = []

    for i in instructions:
        if type(i) is not list:
            if i == '|':
                branches.append(path)
                path = []
            else:
                path.append(i)
        else:
            path.append(merge(i))
    branches.append(path)

    
    return branches

def make_map(instructions, front, rooms):

    for i in instructions:
        if type(i) is list:
            new_front = set()

            for path in i:
                if path == []:
                    new_front = new_front | front
                else:
                    new_front = new_front | make_map(path, front, rooms)

            front = new_front
        else:
            new_front = set()

            for room in front:
                x, y = room.pos

                if i == 'N': 
                    y = y - 1
                    opp = 'S'
                elif i == 'W':
                    x = x - 1
                    opp = 'E'
                elif i == 'S':
                    y = y + 1
                    opp = 'N'
                elif i == 'E':
                    x = x + 1
                    opp = 'W'

                rooms[x] = rooms.get(x, {})
                
                if y not in rooms[x]:
                    rooms[x][y] = Room((x, y))

                room.neighbours[i] = rooms[x][y]
                rooms[x][y].neighbours[opp] = room
                
                new_front.add(rooms[x][y])

            front = new_front

    return front
               
def print_rooms(rooms):

    min_y = min([min(y) for y in rooms.values()])
    max_y = max([max(y) for y in rooms.values()])

    for x in range(min(rooms), max(rooms)+1):
        print('##', end='')
    print('#')

    for y in range(min_y, max_y+1):
        for x in range(min(rooms), max(rooms)+1):
            r = rooms[x].get(y, None)
            if r is not None:
                if 'W' in r.neighbours:
                    print('|', end='')
                else:
                    print('#', end='')

                if x == 0 and y == 0:
                    print('X', end='')
                else:
                    print('.', end='')
            else:
                print('##', end='')

        print('#')

        for x in range(min(rooms), max(rooms)+1):
            print('#', end='')
            r = rooms[x].get(y, None)

            if r is not None:
                if 'S' in r.neighbours:
                    print('-', end='')
                else:
                    print('#', end='')
            else:
                print('#', end='')
        print('#')

def count_doors(start):
    INF = 9999999999
    visited = set()
    
    curr = start

    dist = {start: 0}

    queue = [(dist[start], start)]

    while queue != []:
        s, curr = heappop(queue)

        if curr not in visited:
            # print(curr)
            visited.add(curr)

            neighbours = [t for t in curr.neighbours.values() if t not in visited]
            new_score = dist[curr] + 1

            for n in neighbours:
                if dist.get(n, INF) >= new_score:
                    dist[n] = new_score

                    heappush(queue, (new_score, n))
    # print(dist)
    
    return dist

def main():
    # with open('test.txt', 'r') as file:
    with open('input.txt', 'r') as file:
        instructions = file.readline().strip('^$')

        instructions = find_branch(instructions)

        initial_room = Room((0, 0))

        rooms = {0: {0: initial_room}}

        front = set()
        front.add(initial_room)

        make_map(instructions, front, rooms)

        print_rooms(rooms)
        dists = count_doors(initial_room)

        print(max(dists.values()))
        print(sum([1 for d in dists.values() if d >= 1000]))

if __name__ == '__main__':
    main()