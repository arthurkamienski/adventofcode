# for priority queue
from heapq import *

# constant for distance calculations
INF = 9999999

class TileUnreachable(Exception):
    pass

class RoundFinished(Exception):
    pass

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

    # choose a target out of a list of enemies based on health and position
    def choose_target(self, enemies):
        return min([(e.hp, e) for e in enemies])[1]

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

        # flexibility to use lists of tiles or a single tile as parameter
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

        if in_range == []:
            raise TileUnreachable

        # distances to the tile
        distances = [(self.map.path(unit.tile, t), t) for t in in_range]

        distance, target = min(distances)

        # there are no reachable tiles
        if distance == INF:
            raise TileUnreachable

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
        # sorts units based on position
        self.units.sort()

        # creates a copy of the list to remove the dead units without
        # influencing the iteration
        units_copy = self.units[:]

        for u in units_copy:
            if not self.victory():
                if not u.dead():
                    enemies = self.adjacent_enemies(u)

                    if enemies == []:
                        try:
                            next_tile = self.next_tile(u)
                            u.move_to(next_tile)
                            enemies = self.adjacent_enemies(u)
                        except TileUnreachable:
                            pass
                        
                    if enemies != []:
                        target = u.choose_target(enemies)
                        u.attack(target)

                        if target.dead():
                            self.remove_corpse(target)
            else:
                raise RoundFinished

        self.rounds = self.rounds + 1

    # checks if all of the units of a team are dead
    def victory(self):
        return self.elfs == [] or self.goblins == []

    def fight(self):
        print('Fight broke out...')
        while not self.victory():
            try:
                self.round()
            except RoundFinished:
                pass
            print(f'Round {self.rounds}', end='\r')

        self.summary()

    def elves_live(self):
        elf_num = len(self.elfs)

        while len(self.elfs) == elf_num and not self.victory():
            try:
                print(self.rounds, end='\r')
                self.round()
            except RoundFinished:
                pass
        print()
        return elf_num != len(self.elfs)
        
    def summary(self):
        print('Battle Outcome:')
        print(f'Rounds: {self.rounds}')
        print(f'Total health: {sum([u.hp for u in self.units])}')
        print(f'Outcome: {self.outcome()}\n')
    
    # calculates the outcome of the fight
    def outcome(self):
        # rounds * total hp left
        return self.rounds * sum([u.hp for u in self.units])

# Checks the outcome of each fight until it finds the minimum attack for all of the elves to live
# Brute force: butterfly effect might make binary search imprecise
# Minimum attack = 25
def min_attack(raw_data):
    print('Searching minimum attack')
    for attack in range(4, 200):
        print('Attack =', attack, ':')
        combat = Combat(raw_data, attack)
        
        if not combat.elves_live():
            print('Elves won. No elves dead.')
            combat.summary()
            break
        else:
            print('An elf died.')
            print('-'*30)

def main():
    with open('input.txt', 'r') as raw_data:
        # allows multiple readings
        map_layout = [l for l in raw_data]
        Combat(map_layout).fight()
        print('-'*30)
        min_attack(map_layout)

if __name__ == '__main__':
    main()