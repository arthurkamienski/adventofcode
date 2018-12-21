# for priority queue
from heapq import *

# constant for distance calculations
INF = 9999999

class Unit:
    """Representation of a unit on the map"""
    def __init__(self, char, tile, damage=3):
        self.char = char
        self.tile = tile
        self.damage = damage

        self.hp = 200
        self.is_elf = char == 'E'

    # comparison between two units gives the greater position
    def __lt__(self, other):
        return self.tile < other.tile

    def dead(self):
        return self.hp <= 0

    # moves unit to given tile and updates the variables
    def move_to(self, tile):
        self.tile.unit = None
        tile.unit = self
        self.tile = tile

    # attack on given unit
    def attack(self, unit):
        unit.hp = unit.hp - self.damage

class MapTile:
    """One square on the grid representing the map"""
    def __init__(self, char, pos):
        self.pos = pos
        # for printing the map
        self.char = char

        self.wall = self.char == '#'

        # unit currently positioned at this tile
        self.unit = None

    # printing the map tile returns the string of the position
    def __repr__(self):
        return str(self.pos)

    def __str__(self):
        return str(self.pos)

    # comparison between two tiles gives the greater position
    def __lt__(self, other):
        return self.pos < other.pos

    def is_free(self):
        return not self.wall and self.unit is None

    # returns the tile char if there is no unit
    # returns the unit char otherwise
    def to_char(self):
        if self.unit is None:
            char = self.char
        else:
            char = self.unit.char

        return char

class Map:
    def __init__(self):
        self.map = {}

    def add_tile(self, tile):
        self.map[tile.pos[0]][tile.pos[1]] = tile

    def new_row(self, i):
        self.map[i] = {}

    # returns the tile in given position
    def tile_at(self, pos):
        return self.map[pos[0]][pos[1]]

    def tiles(self):
        return self.map.values()

    def to_tiles(self, units):
        return [u.tile for u in units]

    def columns(self):
        return self.map[0].keys()

    def rows(self):
        return self.map.values()

    def adjacent_to(self, tiles):
        neighbours = []

        if not type(tiles) is list:
            tiles = [tiles]

        for t in tiles:
            i, j = t.pos
            positions = [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]
            neighbours += [self.tile_at(p) for p in positions]

        return list(set(neighbours))

    def free_adjacent_to(self, tiles):
        return [t for t in self.adjacent_to(tiles) if t.is_free()]

    def path(self, start, finish):
        def dist(tile):
            return abs(tile.pos[0] - finish.pos[0]) + abs(tile.pos[1] - finish.pos[1])

        def retrace(f, source):
            while f != None:
                print(f)
                f = source[f]

        visited = set()
        
        curr = start

        source = {start:None}
        score = {start: dist(start)}
        steps = {start: 0}

        queue = [(score[start], start)]

        while curr != finish and queue != []:
            s, curr = heappop(queue)

            if curr != finish and curr not in visited:
                visited.add(curr)

                neighbours = [t for t in self.free_adjacent_to(curr) if t not in visited]

                for n in neighbours:
                    new_score = dist(n) + steps[curr] + 1

                    if score.get(n, INF) > new_score:
                        source[n] = curr
                        score[n] = new_score
                        steps[n] = steps[curr]+1

                        heappush(queue, (new_score, n))

        # retrace(finish, source)

        return score.get(finish, INF)

class Combat:
    def __init__(self, map_layout, elf_attack=3):
        self.elfs = []
        self.goblins = []

        # elfs+goblins
        self.units = []

        self.rounds = 0

        # attack power of the elves for this battle
        self.elf_attack = elf_attack

        self.load(map_layout)

    # creates a dict of dicts, each representing a line/row of the map
    # fills it with empty/wall MapTile objects for each position
    # and creates units at their given positions
    def load(self, map_layout):
        self.map = Map()

        for i, line in enumerate(map_layout):
            self.map.new_row(i)
            for j, char in enumerate(line.strip()):
                if char == 'E' or char == 'G':
                    # tile underneath the units is empty (not wall)
                    tile = MapTile('.', (i, j))

                    if char == 'E':
                        unit = Unit(char, tile, self.elf_attack)
                        self.elfs.append(unit)
                    else:
                        unit = Unit(char, tile)
                        self.goblins.append(unit)
                        
                    tile.unit = unit
                    self.map.add_tile(tile)
                else:
                    self.map.add_tile(MapTile(char, (i, j)))

        self.units = self.elfs + self.goblins

    def print_map(self):
        # tens digits for columns
        print('  ', end = '')
        for k in self.map.columns():
            print(k//10, end='')
        print('\n  ', end = '')
        # units digits for columns
        for k in self.map.columns():
            print(k%10, end='')
        print()

        for i, row in enumerate(self.map.rows()):
            # prints in "00" format
            print(f'{i:02d}', end='')

            for tile in row.values():
                print(tile.to_char(), end='')
            print()

    # removes a unit from the combats
    def remove_corpse(self, unit):
        self.units.remove(unit)

        if unit.is_elf:
            self.elfs.remove(unit)
        else:
            self.goblins.remove(unit)

        unit.tile.unit = None

    # returns the list of enemies of the unit (elfs if it is a goblin, and vice-versa)
    def enemies_of(self, unit):
        if unit.is_elf:
            enemies = self.goblins
        else:
            enemies = self.elfs

        return enemies

    # calculates the outcome of the fight
    def outcome(self):
        # rounds * total hp left
        return self.rounds * sum([u.hp for u in self.units])

    # checks if all of the units of a team are dead
    def victory(self):
        return self.elfs == [] or self.goblins == []
    
    # returns a list of enemies adjacent to the unit
    def adjacent_enemies(self, unit):
        # list of adjacent units
        adjacent_units = [t.unit for t in self.map.adjacent_to(unit.tile) if t.unit is not None]
        # filters for enemies
        return [u for u in adjacent_units if unit.is_elf != u.is_elf]

    # return the destination tile for a unit
    def destination(self, unit):
        # list of all the enemy tiles
        enemy_tiles = self.map.to_tiles(self.enemies_of(unit))
        in_range  = self.map.free_adjacent_to(enemy_tiles)

        # distances to the tile
        distances = [(self.map.path(unit.tile, t), t) for t in in_range]

        distance, target = min(distances)

        # there are no reachable tiles
        if distance == INF:
            # value error because I am lazy
            raise ValueError

        return target

    # returns the next tile to which the unit has to move based on the target tile
    def next_tile(self, unit):
        dest = self.destination(unit)
        
        # possible moves (tiles adjacent to unit)
        adjacent = self.map.free_adjacent_to(unit.tile)

        distances = [(self.map.path(t, dest), t) for t in adjacent]

        tile = min(distances)[1]

        return tile

    def round(self):
        self.units.sort()

        units_copy = self.units[:]
        i = 0

        for u in units_copy:
            if self.enemies_of(u) != []:
                i = i + 1
                if not u.dead():
                    self.print_map()
                    enemies = self.adjacent_enemies(u)
                    if enemies == []:
                        try:
                            next_tile = self.next_tile(u)
                            u.move_to(next_tile)
                        except ValueError:
                            pass
                        
                        enemies = self.adjacent_enemies(u)

                    if enemies != []:
                        target = min(enemies)
                        u.attack(target)

                        if target.dead():
                            self.remove_corpse(target)
            else:
                break

        if i == len(units_copy):
            self.rounds = self.rounds + 1

    def fight(self):
        while not self.victory():
            self.round()
            print(self.rounds, end='\r')
        print()

        self.summary()

    def summary(self):
        print('Battle Outcome:')
        print('Rounds: ', self.rounds)
        print('Total health: ', sum([u.hp for u in self.units]))
        print('Outcome: ', self.outcome())

    def elf_dies(self):
        elf_num = len(self.elfs)

        while elf_num == len(self.elfs) and not self.victory():
            print(self.rounds, end='\r')
            self.round()
        print()

        return elf_num != len(self.elfs)

def min_attack(raw_data):
    print('Searching minimum attack')
    for attack in range(25, 200):
        print('Attack =', attack, ':')
        combat = Combat(raw_data, attack)
        
        if not combat.elf_dies():
            print('Elves won. No elves dead.')
            combat.summary()
            break
        else:
            print('An elf died.\n')

def main():
    with open('input.txt', 'r') as raw_data:
        map_layout = [l for l in raw_data]
        Combat(map_layout).print_map()
        Combat(map_layout).fight()
        min_attack(map_layout)

if __name__ == '__main__':
    main()