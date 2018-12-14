import re

# convert the claim strings to a list of claim dicts
def to_list(raw_claims):
	claims = []

	for info in raw_claims:
		# extracts the required info
		match = re.search('#(.+?) @ (.+?),(.+?): (.+?)x(.+)', info)
		
		# gets the matches and converts to string
		groups = match.group(1,2,3,4,5)
		groups = list(map(lambda x: int(x), groups))

		# makes a claim dict using the previous info
		claim = {
			'id': groups[0],
			'pos': (groups[1], groups[2]),
			'size': (groups[3], groups[4]),
			# position of square sides (readability)
			'square': {
					'left': groups[1],
					'top': groups[2],
					'right': groups[1]+groups[3],
					'bottom': groups[2]+groups[4]
					}
		}

		claims.append(claim)

	return claims

# marks the claim in the fabric
def lay_claim(fabric, square):
	# adds one to each piece of fabric inside the square of the claim
	for i in range(square['top'], square['bottom']):
		for j in range(square['left'], square['right']):
			fabric[i][j] = fabric[i][j] + 1

# counts the overlap area of the fabric
def overlap_area(fabric):
	area = 0
	for i in fabric:
		for j in i:
			# if there are two or more claims on the piece of fabric
			# adds this piece to the overlap area
			if j >= 2:
				area = area + 1

	return area

# checks if the claim overlaps any other claim
def overlaps(fabric, square):
	overlap = False

	# if it overlaps, at least one of the pieces of fabric inside
	# the claim square is bigger than one
	for i in range(square['top'], square['bottom']):
		for j in range(square['left'], square['right']):			
			if fabric[i][j] >= 2:
				overlap = True
				# if it overlaps stops (no need to continue)
				break;

	return overlap

def main():
	with open('input.txt', 'r') as raw_claims:
		claims = to_list(raw_claims)

		# fabric as a 1000x1000 matrix with value 0
		fabric = [[0 for i in range(0, 1000)] for j in range(0, 1000)]

		# lays all of the claims on the fabric
		for claim in claims:
			lay_claim(fabric, claim['square'])

		print('The overlap area is: ' + str(overlap_area(fabric)))

		# checks all claims to find the one that does not overlap
		for claim in claims:
			if not overlaps(fabric, claim['square']):
				print('Claim ID that does not overlap: ' + str(claim['id']))
				break;


if __name__ == '__main__':
	main()