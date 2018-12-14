import re

# extracts the position and velocity of the points from the file
def extract(raw_data):
	points = []

	for line in raw_data:
		# matches <XX,XX> pattern twice
		match = re.search('.*<(.*),(.*)>.*<(.*),(.*)>', line)
		# converts to list of integers
		groups = list(map(lambda x: int(x), list(match.groups())))
		# dict for easier reading
		point = {
			'position': {'x': groups[0], 'y': groups[1]},
			'velocity': {'x': groups[2], 'y': groups[3]}
		}
		points.append(point)

	return points
		
# checks if the points are aligned or not
def aligned(points):
	# points are aligned if they span less than 10 positions (letters are 10 positions long)
	# span = max y - min y
	min_y = min([point['position']['y'] for point in points])
	max_y = max([point['position']['y'] for point in points])

	return max_y-min_y < 10

# updates the points' positions according to their velocity
def move(points):
	for point in points:
		point['position']['x'] = point['position']['x'] + point['velocity']['x']
		point['position']['y'] = point['position']['y'] + point['velocity']['y']

# displays the message vertically on the screen
def display(points):
	translate(points)

	# gets the dimensions of the grid
	max_x = max([point['position']['x'] for point in points])
	max_y = max([point['position']['y'] for point in points])

	# grid with points representing empty spaces
	plot = [['.' for i in range(max_y+1)] for j in range(max_x+1)]

	# puts the points on the plot
	for point in points:
		plot[point['position']['x']][point['position']['y']] = '#'

	# prints the plot (reversed so that the letters are on the right direction)
	for line in reversed(plot):
		for cell in line:
			print(cell, end=' ')
		print()

# changes the origin of the points so that all of them are closer to (0,0)
def translate(points):
	# new origin
	min_x = min([point['position']['x'] for point in points])
	min_y = min([point['position']['y'] for point in points])

	# subtracts new origin from every point
	for point in points:
		point['position']['x'] = point['position']['x'] - min_x
		point['position']['y'] = point['position']['y'] - min_y

def main():
	with open('input.txt', 'r') as raw_data:
		points = extract(raw_data)

		seconds = 0
		while not aligned(points):
			seconds = seconds + 1
			move(points)

		display(points)
		print('Seconds to be waited:' + str(seconds))

if __name__ == '__main__':
	main()