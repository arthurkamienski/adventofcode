package day13

import (
	"adventofcode/2023/utils"
	"fmt"
	"strings"
)

type CoordMap map[int]*utils.Set[utils.Coord]

type Pattern struct {
	MaxX, MaxY               int
	LineCoords, ColumnCoords CoordMap
}

func NewPattern(input string) *Pattern {
	splitInput := strings.Split(input, "\n")

	maxX := len(splitInput[0])
	maxY := len(splitInput)

	pattern := &Pattern{
		MaxX:         maxX,
		MaxY:         maxY,
		LineCoords:   make(CoordMap),
		ColumnCoords: make(CoordMap),
	}

	for y := 0; y < maxY; y++ {
		pattern.LineCoords[y] = utils.NewSet[utils.Coord]()
	}

	for x := 0; x < maxX; x++ {
		pattern.ColumnCoords[x] = utils.NewSet[utils.Coord]()
	}

	for y, line := range splitInput {
		for x, char := range line {
			if char == '#' {
				pattern.LineCoords[y].Add(utils.NewCoord(x, y))
				pattern.ColumnCoords[x].Add(utils.NewCoord(x, y))
			}
		}
	}

	return pattern
}

func (pattern *Pattern) ReflectCoords(reflectIndex int, coords *utils.Set[utils.Coord], reflectHorizontally bool) *utils.Set[utils.Coord] {
	reflected := utils.NewSet[utils.Coord]()
	coordsSlice := coords.Values()

	for _, coord := range coordsSlice {
		if reflectHorizontally {
			reflected.Add(utils.NewCoord(coord.X, reflectIndex+(reflectIndex+1-coord.Y)))
		} else {
			reflected.Add(utils.NewCoord(reflectIndex+(reflectIndex+1-coord.X), coord.Y))
		}
	}

	return reflected
}

func (pattern *Pattern) FindReflectionLine(reflectHorizontally bool) int {
	var coordMap CoordMap
	var maxIndex int

	if reflectHorizontally {
		maxIndex = pattern.MaxY
		coordMap = pattern.LineCoords
	} else {
		maxIndex = pattern.MaxX
		coordMap = pattern.ColumnCoords
	}

	nReflected := 0

	var i int
	for i = 0; i < maxIndex-1 && nReflected == 0; i++ {
		isCurrentReflected := true

		maxPossibleReflaction := i + 1
		if i > maxIndex/2 {
			maxPossibleReflaction = maxIndex - i
		}

		for j := 0; i-j >= 0 && i+j+1 < maxIndex && isCurrentReflected; j++ {
			reflected := pattern.ReflectCoords(i, coordMap[i-j], reflectHorizontally)
			matchingCoords := coordMap[i+1+j]
			isCurrentReflected = reflected.Equals(matchingCoords)

			if isCurrentReflected {
				nReflected++
			} else {
				hasReflected := nReflected > 0
				isPerfectReflection := nReflected == maxPossibleReflaction

				if hasReflected && !isPerfectReflection {
					nReflected = 0
				}
			}
		}
	}
	if nReflected > 0 {
		return i
	} else {
		return 0
	}
}

func (pattern *Pattern) FindReflectionLineWithSmudges(reflectHorizontally bool, previousReflectionIndex int) int {
	var coordMap CoordMap
	var maxIndex int

	if reflectHorizontally {
		maxIndex = pattern.MaxY
		coordMap = pattern.LineCoords
	} else {
		maxIndex = pattern.MaxX
		coordMap = pattern.ColumnCoords
	}

	nReflected := 0
	var i int
	for i = 0; i < maxIndex-1 && nReflected == 0; i++ {
		if i != previousReflectionIndex {
			isCurrentReflected := true
			hasSmudgeYet := false

			maxPossibleReflaction := i + 1
			if i > maxIndex/2 {
				maxPossibleReflaction = maxIndex - i
			}

			for j := 0; (i-j >= 0) && (i+j+1 < maxIndex) && isCurrentReflected; j++ {
				reflected := pattern.ReflectCoords(i, coordMap[i-j], reflectHorizontally)
				matchingCoords := coordMap[i+1+j]

				isCurrentReflected = reflected.Equals(matchingCoords)

				if !isCurrentReflected && !hasSmudgeYet {
					differentCoords := reflected.Union(matchingCoords).Sub(reflected.Intersect(matchingCoords))
					hasOnlyOneDifferentCoord := differentCoords.Size() == 1

					if hasOnlyOneDifferentCoord {
						hasSmudgeYet = true
						isCurrentReflected = true
					}
				}

				if isCurrentReflected {
					nReflected++
				} else {
					hasReflected := nReflected > 0
					isPerfectReflection := nReflected == maxPossibleReflaction

					if hasReflected && !isPerfectReflection {
						nReflected = 0
					}
				}
			}
		}
	}

	if nReflected > 0 {
		return i
	} else {
		return 0
	}
}

func PrintResult() {
	today := utils.NewDay("day13")
	patterns := ParseInput(today.ReadInput())

	colReflectionLines, rowReflectinLines := part1(patterns)
	part2(patterns, colReflectionLines, rowReflectinLines)
}

func part1(patterns []*Pattern) (map[int]int, map[int]int) {
	colReflectionLines := make(map[int]int)
	rowReflectinLines := make(map[int]int)

	cols := 0
	rows := 0
	for i, pattern := range patterns {
		rowIndex := pattern.FindReflectionLine(true)
		colIndex := pattern.FindReflectionLine(false)

		colReflectionLines[i] = colIndex
		rowReflectinLines[i] = rowIndex

		rows += rowIndex
		cols += colIndex
	}

	fmt.Println(rows*100 + cols)

	return colReflectionLines, rowReflectinLines
}

func part2(patterns []*Pattern, colReflectionLines map[int]int, rowReflectinLines map[int]int) {
	cols := 0
	rows := 0
	for i, pattern := range patterns {
		rowIndex := pattern.FindReflectionLineWithSmudges(true, rowReflectinLines[i]-1)
		colIndex := pattern.FindReflectionLineWithSmudges(false, colReflectionLines[i]-1)

		rows += rowIndex
		cols += colIndex
	}
	fmt.Println(rows*100 + cols)
}

func ParseInput(input string) []*Pattern {
	input = strings.TrimSpace(input)
	patterns := make([]*Pattern, 0)

	for _, chunk := range strings.Split(input, "\n\n") {
		pattern := NewPattern(chunk)
		patterns = append(patterns, pattern)
	}

	return patterns
}
