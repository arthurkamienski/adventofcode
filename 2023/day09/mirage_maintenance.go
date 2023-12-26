package day09

import (
	"adventofcode/2023/utils"
	"fmt"
	"strings"
)

type History struct {
	Nums []int
}

func findDifferences(nums []int) []int {
	diffs := make([]int, len(nums)-1)
	for i := 0; i < len(diffs); i++ {
		diffs[i] = nums[i+1] - nums[i]
	}
	return diffs
}

func (h *History) FindDifferences() [][]int {
	isAllZero := func(nums []int) bool {
		for _, num := range nums {
			if num != 0 {
				return false
			}
		}
		return true
	}

	diffs := make([][]int, 0)
	diffs = append(diffs, findDifferences(h.Nums))

	for !isAllZero(diffs[len(diffs)-1]) {
		diffs = append(diffs, findDifferences(diffs[len(diffs)-1]))
	}

	return diffs
}

func (h *History) FindNextValue() int {
	diffs := h.FindDifferences()

	acc := 0
	for i := 0; i < len(diffs); i++ {
		acc += diffs[i][len(diffs[i])-1]
	}

	return h.Nums[len(h.Nums)-1] + acc
}

func (h *History) FindPreviousValue() int {
	diffs := h.FindDifferences()

	acc := 0
	for i := len(diffs) - 1; i >= 0; i-- {
		acc = diffs[i][0] - acc
	}

	return h.Nums[0] - acc
}

func PrintResult() {
	today := utils.NewDay("day09")
	input := today.ReadInputLines()

	histories := parseInput(input)

	part1(histories)
	part2(histories)
}

func parseInput(input []string) []*History {
	histories := make([]*History, len(input))

	for i := 0; i < len(input); i++ {
		nums := make([]int, 0)
		for _, num := range strings.Split(input[i], " ") {
			nums = append(nums, utils.MustAtoi(num))
		}
		histories[i] = &History{Nums: nums}
	}

	return histories
}

func part1(histories []*History) {
	acc := 0

	for _, history := range histories {
		acc += history.FindNextValue()
	}

	fmt.Println(acc)
}

func part2(histories []*History) {
	acc := 0

	for _, history := range histories {
		acc += history.FindPreviousValue()
	}

	fmt.Println(acc)
}
