package day11

import (
	"adventofcode/2023/utils"
	"fmt"
)

func PrintResult() {
	today := utils.NewDay("day11")
	input := today.ReadInputLines()

	galaxies := parseInput(input)

	maxX := len(input[0])
	maxY := len(input)

	expandingRows, expandingCols := getExpandingCoords(galaxies, maxX, maxY)

	part1(galaxies, expandingRows, expandingCols)
	part2(galaxies, expandingRows, expandingCols)
}

func part1(galaxies *utils.Set[utils.Coord], expandingRows, expandingCols *utils.Set[int]) {
	fmt.Println(sumDistances(galaxies, expandingRows, expandingCols, 2))
}

func part2(galaxies *utils.Set[utils.Coord], expandingRows, expandingCols *utils.Set[int]) {
	fmt.Println(sumDistances(galaxies, expandingRows, expandingCols, 1000000))
}

func sumDistances(galaxies *utils.Set[utils.Coord], expandingRows, expandingCols *utils.Set[int], expansionRate int) int {
	galaxyList := galaxies.Values()
	acc := 0

	for i := 0; i < len(galaxyList); i++ {
		for j := i + 1; j < len(galaxyList); j++ {
			dist := distBetweenGalaxies(galaxyList[i], galaxyList[j], expandingRows, expandingCols, expansionRate)
			acc += dist
		}
	}
	return acc
}

func distBetweenGalaxies(a, b utils.Coord, expandingRows, expandingCols *utils.Set[int], expansionRate int) int {
	expandingRowsBetweenGalaxies := 0
	expandingColsBetweenGalaxies := 0

	minY, maxY := utils.Min(a.Y, b.Y), utils.Max(a.Y, b.Y)
	minX, maxX := utils.Min(a.X, b.X), utils.Max(a.X, b.X)

	for i := minY + 1; i < maxY; i++ {
		if expandingRows.Contains(i) {
			expandingRowsBetweenGalaxies++
		}
	}
	for i := minX + 1; i < maxX; i++ {
		if expandingCols.Contains(i) {
			expandingColsBetweenGalaxies++
		}
	}

	return utils.Abs(a.X-b.X) + utils.Abs(a.Y-b.Y) + (expansionRate-1)*(expandingRowsBetweenGalaxies+expandingColsBetweenGalaxies)
}

func getExpandingCoords(galaxies *utils.Set[utils.Coord], maxX, maxY int) (*utils.Set[int], *utils.Set[int]) {
	expandingRows, expandingCols := utils.NewSet[int](), utils.NewSet[int]()
	for i := 0; i < maxY; i++ {
		expandingRows.Add(i)
	}

	for i := 0; i < maxX; i++ {
		expandingCols.Add(i)
	}

	for _, galaxy := range galaxies.Values() {
		expandingRows.Remove(galaxy.Y)
		expandingCols.Remove(galaxy.X)
	}
	return expandingRows, expandingCols
}

func parseInput(input []string) *utils.Set[utils.Coord] {
	galaxies := utils.NewSet[utils.Coord]()
	for y, line := range input {
		for x, char := range line {
			if char == '#' {
				galaxies.Add(utils.Coord{X: x, Y: y})
			}
		}
	}
	return galaxies
}
