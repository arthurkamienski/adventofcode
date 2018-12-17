# calculates cell power according to the instructions
def cell_power(x, y, serial_num):
    rack_id = x+10
    power = (rack_id*y+serial_num)*rack_id

    # gets the hundreds digit and
    power = int(power%1000/100)

    return power-5

# calculates the power of each cell on the grid
def calculate_power(grid, serial_num):
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            grid[i][j] = cell_power(i+1, j+1, serial_num)

# returns the sum of the square with the given size on the grid
def sum_square(x, y, size, grid):
    # check matrix sum algorithms
    # sum = sum (i..n, j..n) (square from i,j to the borders)
    # sum -= sum (i+size..n, j..n) (removes the square from i+size,j to the borders)
    # sum -= sum (i..n, j+size..n) (removes the square from i,j+size to the borders)
    # sum += sum (i+size..n, j+size..n) (adds the square from i+size,j+size to the borders as
    # it was removed twice)

    """
    ===----
    ===----
    ---++++
    ---++++
    """

    total = grid[x][y]

    if x+size < len(grid) and y+size < len(grid[0]):
        total = total + grid[x+size][y+size] - grid[x+size][y] - grid[x][y+size]

    return total

# returns the power and coordinates of the square with the maximum power for the given size
def max_power_coord(grid, size):
    powers = []

    # only calculates full squares (saves time)
    for i in range(len(grid)-size+1):
        for j in range(len(grid[0])-size+1):
            # calculates the sum and appends to list with coordinates
            powers.append((sum_square(i,j, size, grid), (i+1, j+1)))
            
    # put the largest power on the front
    powers.sort(reverse=True)

    # return largest (power, coord)
    return powers[0]

# returns the power, coordinates and size of the square with the maximum power for all sizes
def max_power_size(grid):
    results = []
    # tries every size of square possible
    for size in range(1, len(grid)):
        # gets the maximum power for given square size
        power, coord = max_power_coord(grid, size)

        results.append((power, coord, size))

    # puts max power on the front of the list
    results.sort(reverse=True)

    # returns max (power, coord, size)
    return results[0]

# for each x,y in the grid, sums all of the cells from x,y to n,n (borders) 
def sum_grid(grid):
    # reversed: start from the bottom right corner and go up
    # this way we don't repeat calculations (dinamic programming)
    for i in reversed(range(len(grid))):
        for j in reversed(range(len(grid[0]))):
            # same logic as summing the squares (inversed):
            # sums both sides and removes
            # centes because was added twice
            """
            ===++++
            ===++++
            +++----
            +++----
            """

            if j < len(grid[0])-1 and i < len(grid)-1:
                grid[i][j] = grid[i][j] - grid[i+1][j+1]
            if i < len(grid)-1:
                grid[i][j] = grid[i][j] + grid[i+1][j]
            if j < len(grid[0])-1:
                grid[i][j] = grid[i][j] + grid[i][j+1]

    return grid

def main():
    # input = 4455
    serial_num = 4455

    grid = [[None for i in range(300)] for j in range(300)]

    calculate_power(grid, serial_num)
    sum_grid(grid)
    
    print("Coordinates for max power size = 3: " + str(max_power_coord(grid, 3)[1]))
    print("Coordinates + size for max_power: " + str(max_power_size(grid)[1:3]))

if __name__ == '__main__':
    main()