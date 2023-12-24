package day02

import (
	"adventofcode/2023/utils"
	"fmt"
	"strconv"
	"strings"
)

type Game struct {
	Num        int
	cubeCounts map[string]int
}

func NewGame(definition string) *Game {
	getMax := func(a, b int) int {
		if a > b {
			return a
		}
		return b
	}

	splitDef := strings.Split(definition, ": ")
	idString, cubesString := splitDef[0], splitDef[1]

	gameNum, err := strconv.Atoi(strings.Split(idString, " ")[1])
	utils.Check(err)

	cubeCounts := make(map[string]int)

	for _, cubeString := range strings.Split(cubesString, "; ") {
		cubeColorCounts := strings.Split(cubeString, ", ")

		for _, cubeColorCount := range cubeColorCounts {
			splitCubeColorCount := strings.Split(cubeColorCount, " ")

			count, err := strconv.Atoi(splitCubeColorCount[0])
			utils.Check(err)

			cubeColor := splitCubeColorCount[1]

			cubeCounts[cubeColor] = getMax(cubeCounts[cubeColor], count)
		}
	}

	return &Game{
		Num:        gameNum,
		cubeCounts: cubeCounts,
	}
}

func (g *Game) Power() int {
	return g.cubeCounts["red"] * g.cubeCounts["green"] * g.cubeCounts["blue"]
}

func PrintResult() {
	today := utils.NewDay("day02")
	input := today.ReadInputLines()

	games := make([]*Game, len(input))

	for i, line := range input {
		games[i] = NewGame(line)
	}

	part1(games)
	part2(games)
}

func part1(games []*Game) {
	acc := 0
	for _, game := range games {
		if game.cubeCounts["red"] <= 12 && game.cubeCounts["green"] <= 13 && game.cubeCounts["blue"] <= 14 {
			acc += game.Num
		}
	}

	fmt.Println(acc)
}

func part2(games []*Game) {
	acc := 0
	for _, game := range games {
		acc += game.Power()
	}

	fmt.Println(acc)
}
