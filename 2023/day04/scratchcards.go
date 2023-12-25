package day04

import (
	"adventofcode/2023/utils"
	"fmt"
	"regexp"
	"strconv"
)

type Card struct {
	Number               int
	WinningNumbers       *utils.Set[int]
	CardNumbers          *utils.Set[int]
	WinningNumbersInCard int
	Point                int
}

func NewCard(str string) *Card {
	numbersSet := func(str string) *utils.Set[int] {
		nums := utils.NewSet[int]()

		for _, n := range regexp.MustCompile(`\ +`).Split(str, -1) {
			num, err := strconv.Atoi(n)
			utils.Check(err)
			nums.Add(num)
		}

		return nums
	}

	splitStr := regexp.MustCompile(`:\ +`).Split(str, -1)
	cardNum, err := strconv.Atoi(regexp.MustCompile(`\ +`).Split(splitStr[0], -1)[1])
	utils.Check(err)

	numbers := regexp.MustCompile(`\ \|\ +`).Split(splitStr[1], -1)

	winningNumbers := numbersSet(numbers[0])
	cardNumbers := numbersSet(numbers[1])

	winningNumbersInCard := winningNumbers.Intersect(cardNumbers).Size()

	points := 0

	if winningNumbersInCard > 0 {
		points = 1 << (winningNumbersInCard - 1)
	}

	return &Card{
		Number:               cardNum,
		WinningNumbers:       winningNumbers,
		CardNumbers:          cardNumbers,
		WinningNumbersInCard: winningNumbersInCard,
		Point:                points,
	}
}

func PrintResult() {
	today := utils.NewDay("day04")
	input := today.ReadInputLines()

	cards := parseInput(input)

	part1(cards)
	part2(cards)
}

func parseInput(input []string) []*Card {
	cards := make([]*Card, len(input))

	for i, line := range input {
		cards[i] = NewCard(line)
	}

	return cards
}

func part1(cards []*Card) {
	acc := 0

	for _, card := range cards {
		acc += card.Point
	}

	fmt.Println(acc)
}

func part2(cards []*Card) {
	cardValues := make(map[int]int)

	for i := len(cards) - 1; i >= 0; i-- {
		card := cards[i]

		cardValue := 1
		for i := card.Number + 1; i <= card.Number+card.WinningNumbersInCard; i++ {
			cardValue += cardValues[i]
		}
		cardValues[card.Number] = cardValue
	}

	acc := 0

	for _, val := range cardValues {
		acc += val
	}

	fmt.Println(acc)
}
