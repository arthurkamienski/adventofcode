# checks if two units of the polymer react
def reacts(unit1, unit2):
    # the diference between the same upper/lower case letter is 32
    return abs(ord(unit1) - ord(unit2)) == 32

# react the polymer at the given position by deleting the units
def react(polymer, i):
    del polymer[i]
    del polymer[i]

# makes all of the reactions of the polymer and returns its length
def collapse(polymer):
    i = 0
    # starting from the beginning, checks for a reaction between
    # two units next to each other and reacts in case there is one
    # stop when it reaches the end of polymer
    while i < len(polymer)-1:
        if reacts(polymer[i], polymer[i+1]):
            react(polymer, i)
            # if there is a reaction, goes back checks the previous
            # unit for a reaction with the next one
            # max -> stops from looping around
            i = max(i-1, 0)
        else:
            i = i + 1

# creates a new polymer without the given character (by ascii number)
def clean_polymer(polymer, i):
    return [c for c in polymer if (c != chr(i) and c != chr(i-32))]

def main():
    with open('input.txt', 'r') as file:
        # cleans the input and transforms it into a list of chars
        # (to use the delete function)
        initial_polymer = list(file.readline().strip())
        
        # creates a copy of the initial polymer (to be used later)
        polymerA = initial_polymer[:]
        collapse(polymerA)

        print('Length after collapse: ' + str(len(polymerA)))

        results = {}

        # for each letter of the alphabet
        # collapses the clean polymer and stores the resulting length
        for i in range(ord('a'), ord('z')+1):
            # for feedback (may take a couple of second)
            print('Cleaning ' + chr(i))
            clean = clean_polymer(initial_polymer, i)
            collapse(clean)
            results[chr(i)] = len(clean)

        print('Shortest polymer after cleaning: ' + str(min(results.values())))

if __name__ == '__main__':
    main()