package day14

import (
	"adventofcode/2023/utils"
	"fmt"
	"sort"
)

type MetalPlatform struct {
	MaxX, MaxY         int
	RoundRocks         []utils.Coord
	RoundRocksInRow    map[int][]int
	RoundRocksInColumn map[int][]int
	CubeRocks          []utils.Coord
	CubeRocksInRow     map[int][]int
	CubeRocksInColumn  map[int][]int
}

func (mp *MetalPlatform) GetCubeNextCubeRockInFront(coord utils.Coord, dir rune) int {
	var cubeRockPos int
	switch dir {
	case 'N':
		cubeRocksInColumn := mp.CubeRocksInColumn[coord.X]
		cubeRockPos = -1

		for i := 0; i < len(cubeRocksInColumn); i++ {
			if cubeRocksInColumn[i] < coord.Y {
				cubeRockPos = utils.Max(cubeRockPos, cubeRocksInColumn[i])
			}
		}

	case 'S':
		cubeRocksInColumn := mp.CubeRocksInColumn[coord.X]
		cubeRockPos = mp.MaxY

		for i := 0; i < len(cubeRocksInColumn); i++ {
			if cubeRocksInColumn[i] > coord.Y {
				cubeRockPos = utils.Min(cubeRockPos, cubeRocksInColumn[i])
			}
		}
	case 'E':
		cubeRocksInRow := mp.CubeRocksInRow[coord.Y]
		cubeRockPos = mp.MaxX

		for i := 0; i < len(cubeRocksInRow); i++ {
			if cubeRocksInRow[i] > coord.X {
				cubeRockPos = utils.Min(cubeRockPos, cubeRocksInRow[i])
			}
		}
	case 'W':
		cubeRocksInRow := mp.CubeRocksInRow[coord.Y]
		cubeRockPos = -1

		for i := 0; i < len(cubeRocksInRow); i++ {
			if cubeRocksInRow[i] < coord.X {
				cubeRockPos = utils.Max(cubeRockPos, cubeRocksInRow[i])
			}
		}
	}
	return cubeRockPos
}

func (mp *MetalPlatform) CountRoundRocksBetween(coord utils.Coord, dir rune, pos int) int {
	roundRocks := 0

	switch dir {
	case 'N':
		roundRocksInColumn := mp.RoundRocksInColumn[coord.X]

		for i := 0; i < len(roundRocksInColumn); i++ {
			if roundRocksInColumn[i] < coord.Y && roundRocksInColumn[i] > pos {
				roundRocks++
			}
		}
	case 'S':
		roundRocksInColumn := mp.RoundRocksInColumn[coord.X]

		for i := 0; i < len(roundRocksInColumn); i++ {
			if roundRocksInColumn[i] > coord.Y && roundRocksInColumn[i] < pos {
				roundRocks++
			}
		}
	case 'E':
		roundRocksInRow := mp.RoundRocksInRow[coord.Y]

		for i := 0; i < len(roundRocksInRow); i++ {
			if roundRocksInRow[i] > coord.X && roundRocksInRow[i] < pos {
				roundRocks++
			}
		}
	case 'W':
		roundRocksInRow := mp.RoundRocksInRow[coord.Y]

		for i := 0; i < len(roundRocksInRow); i++ {
			if roundRocksInRow[i] < coord.X && roundRocksInRow[i] > pos {
				roundRocks++
			}
		}
	}
	return roundRocks
}

func (mp *MetalPlatform) Tilt(dir rune) {
	nextRoundRocks := []utils.Coord{}
	switch dir {
	case 'N':
		for _, coord := range mp.RoundRocks {
			nextCubeRock := mp.GetCubeNextCubeRockInFront(coord, dir)
			roundRocksBetween := mp.CountRoundRocksBetween(coord, dir, nextCubeRock)
			nextPos := nextCubeRock + 1 + roundRocksBetween

			nextRoundRocks = append(nextRoundRocks, utils.NewCoord(coord.X, nextPos))
		}
	case 'S':
		for _, coord := range mp.RoundRocks {
			nextCubeRock := mp.GetCubeNextCubeRockInFront(coord, dir)
			roundRocksBetween := mp.CountRoundRocksBetween(coord, dir, nextCubeRock)
			nextPos := nextCubeRock - 1 - roundRocksBetween

			nextRoundRocks = append(nextRoundRocks, utils.NewCoord(coord.X, nextPos))
		}
	case 'E':
		for _, coord := range mp.RoundRocks {
			nextCubeRock := mp.GetCubeNextCubeRockInFront(coord, dir)
			roundRocksBetween := mp.CountRoundRocksBetween(coord, dir, nextCubeRock)
			nextPos := nextCubeRock - 1 - roundRocksBetween

			nextRoundRocks = append(nextRoundRocks, utils.NewCoord(nextPos, coord.Y))
		}
	case 'W':
		for _, coord := range mp.RoundRocks {
			nextCubeRock := mp.GetCubeNextCubeRockInFront(coord, dir)
			roundRocksBetween := mp.CountRoundRocksBetween(coord, dir, nextCubeRock)
			nextPos := nextCubeRock + 1 + roundRocksBetween

			nextRoundRocks = append(nextRoundRocks, utils.NewCoord(nextPos, coord.Y))
		}
	}

	mp.MoveRoundRocks(nextRoundRocks)
}

func (mp *MetalPlatform) MoveRoundRocks(newRocks []utils.Coord) {
	mp.RoundRocks = newRocks
	mp.RoundRocksInRow = make(map[int][]int)
	mp.RoundRocksInColumn = make(map[int][]int)

	for _, coord := range mp.RoundRocks {
		mp.RoundRocksInRow[coord.Y] = append(mp.RoundRocksInRow[coord.Y], coord.X)
		mp.RoundRocksInColumn[coord.X] = append(mp.RoundRocksInColumn[coord.X], coord.Y)
	}
}

func (mp *MetalPlatform) Print() {
	roundRocks := utils.NewSetFromSlice(mp.RoundRocks)
	cubeRocks := utils.NewSetFromSlice(mp.CubeRocks)

	for y := 0; y < mp.MaxY; y++ {
		for x := 0; x < mp.MaxX; x++ {
			coord := utils.NewCoord(x, y)
			if roundRocks.Contains(coord) {
				print("O")
			} else if cubeRocks.Contains(coord) {
				print("#")
			} else {
				print(".")
			}
		}
		println()
	}
}

func (mp *MetalPlatform) Cycle() {
	mp.Tilt('N')
	mp.Tilt('W')
	mp.Tilt('S')
	mp.Tilt('E')
}

func (mp *MetalPlatform) TotalLoad() int {
	load := 0
	for _, coord := range mp.RoundRocks {
		load += mp.MaxY - coord.Y
	}
	return load
}

// We need this function as neither slices nor maps are comparable
func (mp *MetalPlatform) RoundRocksAsString() string {
	sort.Slice(mp.RoundRocks, func(i, j int) bool {
		if mp.RoundRocks[i].X == mp.RoundRocks[j].X {
			return mp.RoundRocks[i].Y < mp.RoundRocks[j].Y
		}
		return mp.RoundRocks[i].X < mp.RoundRocks[j].X
	})

	roundRocksAsString := ""
	for _, coord := range mp.RoundRocks {
		roundRocksAsString += fmt.Sprintf("(%d,%d)", coord.X, coord.Y)
	}
	return roundRocksAsString
}

func (mp *MetalPlatform) FindRepeatingCycle() (int, int) {
	rockPositions := make(map[string]int)
	currRocks := mp.RoundRocksAsString()
	rockPositions[currRocks] = 0
	cycleFound := false

	var i int
	for i = 1; !cycleFound; i++ {
		mp.Cycle()

		currRocks = mp.RoundRocksAsString()

		if _, ok := rockPositions[currRocks]; ok {
			cycleFound = true
		} else {
			rockPositions[currRocks] = i
		}
	}

	return rockPositions[currRocks], i - rockPositions[currRocks] - 1
}

func PrintResult() {
	today := utils.NewDay("day14")
	input := today.ReadInputLines()

	metalPlatform := ParseInput(input)

	// I hate that I am doing this stateful
	part1(metalPlatform)
	part2(metalPlatform)
}

func part1(metalPlatform *MetalPlatform) {
	metalPlatform.Tilt('N')
	fmt.Println(metalPlatform.TotalLoad())
}

func part2(metalPlatform *MetalPlatform) {
	cycleStart, cycleLength := metalPlatform.FindRepeatingCycle()
	missingCycles := (1000000000 - cycleStart) % cycleLength
	for i := 0; i < missingCycles; i++ {
		metalPlatform.Cycle()
	}

	fmt.Println(metalPlatform.TotalLoad())
}

func ParseInput(input []string) *MetalPlatform {
	metalPlatform := &MetalPlatform{
		MaxX:               len(input[0]),
		MaxY:               len(input),
		RoundRocks:         []utils.Coord{},
		RoundRocksInRow:    make(map[int][]int),
		RoundRocksInColumn: make(map[int][]int),
		CubeRocks:          []utils.Coord{},
		CubeRocksInRow:     make(map[int][]int),
		CubeRocksInColumn:  make(map[int][]int),
	}

	for y := 0; y < metalPlatform.MaxX; y++ {
		metalPlatform.RoundRocksInRow[y] = []int{}
		metalPlatform.CubeRocksInRow[y] = []int{}
	}

	for x := 0; x < metalPlatform.MaxX; x++ {
		metalPlatform.RoundRocksInColumn[x] = []int{}
		metalPlatform.CubeRocksInColumn[x] = []int{}
	}

	for y, line := range input {
		for x, char := range line {
			coord := utils.NewCoord(x, y)

			if char == '#' {
				metalPlatform.CubeRocks = append(metalPlatform.CubeRocks, coord)
				metalPlatform.CubeRocksInColumn[x] = append(metalPlatform.CubeRocksInColumn[x], y)
				metalPlatform.CubeRocksInRow[y] = append(metalPlatform.CubeRocksInRow[y], x)
			} else if char == 'O' {
				metalPlatform.RoundRocks = append(metalPlatform.RoundRocks, coord)
				metalPlatform.RoundRocksInColumn[x] = append(metalPlatform.RoundRocksInColumn[x], y)
				metalPlatform.RoundRocksInRow[y] = append(metalPlatform.RoundRocksInRow[y], x)
			}
		}
	}

	return metalPlatform
}
