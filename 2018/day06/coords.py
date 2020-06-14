# converts the input into a list of coordinates
def to_list(raw_data):
    points = []

    for line in raw_data:
        # splits the line in the comma and converts to integer
        coords = map(lambda x: int(x), line.strip().split(', '))
        # converts list to tuple
        points.append(tuple(coords))

    return points

# translates the points so that they all have (0,0) as the origin
def translate(points, new_origin):
    # subtract the new origin for all the points
    return [subtract(point, new_origin) for point in points]

# subtract one coordinate from the other
def subtract(a, b):
    return (a[0]-b[0], a[1]-b[1])

# calculate the manhattan distance between two coordinates
def manhattan(a, b):
    sub = subtract(a, b)
    # sum of the absolute value of the subtraction of the coordinates
    return abs(sub[0])+abs(sub[1])

# calculate which point is closest to each space on the grid
def min_dist(points, grid):
    # for each coordinate on the grid
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            point = (i, j)

            min_dist = 9999999

            # for each point
            for k, p in enumerate(points):
                # calculate distance
                dist = manhattan(point, p)

                # if it is smaller, set grid value to the point number
                if dist < min_dist:
                    min_dist = dist
                    grid[i][j] = k
                # if it is the same, set it as -1 to indicate that
                elif dist == min_dist:
                    grid[i][j] = -1

# count the area closest to each point
def count_areas(grid, points):
    areas = [0 for i in range(len(points))]

    # for each coord in the grid
    for line in grid:
        for column in line:
            # if it is not -1 (equal for 2+ coords)
            if areas[column] >= 0:
                # increase the area for the point that is closest to the coord
                areas[column] = areas[column]+1

    return areas

# checks if it is in the border of the grid (coords equal to 0 or the length)
def is_border(p, grid):
    return p[0] == 0 or p[0]==len(grid)-1 or p[1]==0 or p[1]==len(grid[0])-1

# calculate the sum of all of the distances to each point
def sum_dists(points, grid):
    # for each coord in grid
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            p = (i, j)

            # calculate the sum of the distances
            dist_sum = 0
            for point in points:
                dist_sum = dist_sum + manhattan(point, p)

            grid[i][j] = dist_sum

# calculates the total area in which the distance to all other points is < 10000
def area_less_than_10000(grid):
    area = 0

    for line in grid:
        for column in line:
            if column < 10000:
                area = area + 1

    return area

def main():
    with open('input.txt', 'r') as raw_data:

        points = to_list(raw_data)
        
        # get minimum coordinates to set new origin
        min_x = min([point[0] for point in points])
        min_y = min([point[1] for point in points])

        new_origin = (min_x, min_y)

        points = translate(points, new_origin)

        # get maximum coordinates to create the grid
        max_x = max([point[0] for point in points])
        max_y = max([point[1] for point in points])

        # create the grid based on the maximum values, started with None
        grid = [[None for i in range(max_y+1)] for j in range(max_x+1)]

        min_dist(points, grid)

        areas = count_areas(grid, points)

        # select only the finite points
        # if it is on the border it is infinite
        finites = [k for k, point in enumerate(points) if not is_border(point, grid)]
        finite_areas = [areas[i] for i in finites]
        
        print('Max finite area: ' + str(max(finite_areas)))

        grid = [[None for i in range(max_y+1)] for j in range(max_x+1)]

        sum_dists(points, grid)

        print('Area with distance < 10000: ' + str(area_less_than_10000(grid)))

if __name__ == '__main__':
    main()