# counts the number of each letter in code
def count_letters(code):
	# dict for O(1) access
	counter = {}

	for letter in code:
		# if exists, returns the value, else returns 0
		counter[letter] = counter.get(letter, 0) + 1

	return counter

def main():
	with open('input.txt', 'r') as codes:
		two_letters = 0
		three_letters = 0

		for code in codes:
			count = count_letters(code.strip())

			# numbers of letters in code
			numbers = set(count.values())

			if 2 in numbers:
				two_letters = two_letters + 1

			if 3 in numbers:
				three_letters = three_letters + 1

		checksum = two_letters * three_letters

		print('The checksum is: ' + str(checksum))

if __name__ == '__main__':
	main()