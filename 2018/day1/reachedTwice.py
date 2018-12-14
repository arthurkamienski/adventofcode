# convert the input file to a list of numbers
# easier to manage
def to_list(input_numbers):
	numbers = []

	for number in input_numbers:
		numbers.append(int(number))

	return numbers

def main():
	with open('input.txt', 'r') as input_numbers:
		numbers = to_list(input_numbers)
		count = 0
		
		# stores the numbers that were already reached
		# set as to check if exists in O(1)
		repeated = set()

		# repeated number may occur after many passes
		while count not in repeated:
			# iterate over input
			for number in numbers:
				repeated.add(count)
				count = count + number

				# if is already in set, stop
				if count in repeated:
					print(count)
					break;


if __name__ == '__main__':
	main()