def main():
	with open('input.txt', 'r') as input_numbers:
		count = 0
		# convert each line (string) to number and add to sum
		for number in input_numbers:
			count = count + int(number)
		# print result
		print('Total sum:' + str(count))

if __name__ == '__main__':
	main()