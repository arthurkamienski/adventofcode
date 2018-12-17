import re

# extracts the plant layout and the patterns from the input
def extract(raw_data):
    # searches for a sequence of '#' or '.' and transforms into list of char
    plants = list(re.search('.*\ ([\.|#]+)$', raw_data.readline()).group(1))
    # transforms the list of chars into (char, position) list
    plants = [(plant, i) for i, plant in enumerate(plants)]

    # gets rid of the blank line
    raw_data.readline()

    # stores the pattern => result sequence
    patterns = {}

    for line in raw_data:
        # matches a sequence of 5 '#' or '.' and the result afte =>
        pattern, result = re.match('([\.|#]{5})\ =>\ (#|.)', line).groups()
        
        patterns[pattern] = result

    return plants, patterns

# fixes the borders in a way that there are at least 3 '.' on either
# side of the sequence (the plant on the edge might generate others outside the array)
def fix_borders(plants):
    # checks if the first 3 places of the array contain '.'
    # if not, adds '.' until there are
    for i in range(3):
        if plants[i][0] == '#':
            # adds ('.', num-1)
            plants.insert(0, ('.', plants[0][1]-1))

    # checks if the last 3 places of the array contain '.'
    # if not, adds '.' until there are
    for i in reversed(range(len(plants)-3, len(plants))):
        if plants[i][0] == '#':
            # adds ('.', num+1)
            plants.append(('.', plants[len(plants)-1][1]+1))

# removes '.' until there are only '#' at the edges
def clear_borders(plants):
    # removes '.'s from first position
    while plants[0][0] != '#':
        plants.pop(0)

    # removes '.'s from last position
    while plants[len(plants)-1][0] != '#':
        plants.pop()

# calculate the next generation of plants based on the current positions and patterns
def next_gen(plants, patterns):
    # fixes borders so that no new plants exceed the array lenght
    # for example: .#### might generate #.#### (# on position -1)
    fix_borders(plants)

    # new plants list
    new_plants = []

    # for all plants
    for i in range(len(plants)):
        # pattern from 2 positions to the left and to the right of the plant
        pattern = ''
        for j in range(i-2, i+3):
            # considers that plants that are not on the list are '.'
            # only the last 2 plants on either side apply
            if j < 0 or j >= len(plants):
                pattern = pattern + '.'
            else:
                pattern = pattern + plants[j][0]

        # gets the result based on the pattern (considers '.' if pattern does not exist)
        # adds (result, num_plant_i) to the list
        new_plants.append((patterns.get(pattern, '.'), plants[i][1]))

    # removes heading and trailling '.'s
    clear_borders(new_plants)

    return new_plants

# sums the positions of all the plants (to get the puzzle result)
def sum_plants(plants):
    total = 0
    # unpacks (plant, position)
    for plant, i in plants:
        # if there is a plant, sums its position
        if plant == '#':
            total = total + i

    return total

# transforms the list of plants into a string (allows comparison)
def to_string(plants):
    string = ''
    # appends the char representing each plant
    for plant in plants:
        string = string + plant[0]
    return string

# return the repetition parameters
# generation and sum in which repetition begins
# shift between each generation
def get_repetition(plants, patterns):
    last_gen = []
    shift = 0
    gen = 0

    # compares the two strings (because the lists have positions in them)
    while to_string(last_gen) != to_string(plants):

        gen = gen + 1       
        last_gen = plants[:]

        plants = next_gen(plants, patterns)

    shift = sum_plants(plants) - sum_plants(last_gen)

    return sum_plants(plants), shift, gen

# gets the result of the repetition until the given generation
def repeat_until(generation, curr_sum, curr_gen, shift):
    return curr_sum + shift * (generation-curr_gen)

def main():
    with open('input.txt', 'r') as raw_data:
        plants, patterns = extract(raw_data)

        plants_20gen = plants[:]
        for i in range(20):
            plants_20gen = next_gen(plants_20gen, patterns)

        print('Result after 20 generations: ' + str(sum_plants(plants_20gen)))

        gen_sum, shift, gen = get_repetition(plants, patterns)
        result_50b = repeat_until(50000000000, gen_sum, gen, shift)

        print('Result after 50000000000 generations: ' + str(result_50b))

if __name__ == '__main__':
    main()