# convert put codes on list (easier handling)
def to_list(raw_codes):
	codes = []

	for code in raw_codes:
		codes.append(code.strip())

	return codes

# counts how many letters are different between the two
def diff_number(code1, code2):
	diff = 0

	for k in range(0, len(code1)):
		if code1[k] != code2[k]:
			diff = diff + 1

			# if there is more than one character it is not what we are
			# looking for
			if diff > 1:
				break

	return diff

# returns a string with only the letters that are the same between two words
def equal_letters(code1, code2):
	equal = ''

	for i in range(0, len(code1)):
		if code1[i] == code2[i]:
			equal = equal + code1[i]

	return equal

def main():
	with open('input.txt', 'r') as raw_codes:
		codes = to_list(raw_codes)
		# sort codes alphabetically (faster to spot differences)
		codes.sort()

		# compare the code to each other one
		for i in range(0, len(codes)):
			for j in range(i+1, len(codes)):
				if diff_number(codes[i], codes[j]) <= 1:
					print(equal_letters(codes[i], codes[j]))
					break;
			

if __name__ == '__main__':
	main()