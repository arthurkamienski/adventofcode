package day01

import (
	"adventofcode/2023/utils"
	"fmt"
	"regexp"
	"strconv"
)

var today = utils.NewDay("day01")

func PrintResult() {
	input := today.ReadInputLines()
	part1(input)
	part2(input)
}

func part1(input []string) {
	fmt.Println(getNumberSum(input, findNumbers))
}

func part2(input []string) {
	fmt.Println(getNumberSum(input, findSpelledOutNumbers))
}

func getNumberSum(input []string, find func(string) []int) int {
	acc := 0
	for _, line := range input {
		nums := find(line)
		first := nums[0]
		last := nums[len(nums)-1]

		acc += first*10 + last
	}

	return acc
}

func findSpelledOutNumbers(s string) []int {
	matchToInt := func(match string) int {
		if len(match) > 1 {
			switch match {
			case "one":
				match = "1"
			case "two":
				match = "2"
			case "three":
				match = "3"
			case "four":
				match = "4"
			case "five":
				match = "5"
			case "six":
				match = "6"
			case "seven":
				match = "7"
			case "eight":
				match = "8"
			case "nine":
				match = "9"
			}
		}

		n, err := strconv.Atoi(match)

		if err != nil {
			panic(err)
		}

		return n
	}

	possibleMatches := "([0-9]|one|two|three|four|five|six|seven|eight|nine)"

	firstMatch := regexp.MustCompile(possibleMatches).FindString(s)
	lastMatch := regexp.MustCompile(".*" + possibleMatches).FindStringSubmatch(s)[1]

	return []int{matchToInt(firstMatch), matchToInt(lastMatch)}
}

func findNumbers(s string) []int {
	matches := regexp.MustCompile("([0-9])").FindAllString(s, -1)

	numbers := make([]int, len(matches))

	for i, match := range matches {
		n, err := strconv.Atoi(match)

		if err != nil {
			panic(err)
		} else {
			numbers[i] = n
		}
	}

	return numbers
}
