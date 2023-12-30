package day22

import (
	"adventofcode/2023/utils"
	"fmt"
	"sort"
	"strings"
)

type Ground struct {
	ElevationMap    map[utils.Coord]int
	SlabMap         map[int]map[utils.Coord]SandSlab
	SupportingSlabs map[SandSlab]*utils.Set[SandSlab]
}

func NewGround() Ground {
	return Ground{
		ElevationMap:    make(map[utils.Coord]int),
		SlabMap:         make(map[int]map[utils.Coord]SandSlab),
		SupportingSlabs: make(map[SandSlab]*utils.Set[SandSlab]),
	}
}

func (g *Ground) AddSlab(slab SandSlab) {
	slabArea := slab.GetAreaCoords()

	maxHeight := 0

	for _, coord := range slabArea {
		elevation := g.ElevationMap[coord]
		if elevation > maxHeight {
			maxHeight = elevation
		}
	}

	slabTopHeight := maxHeight + slab.H

	if maxHeight != 0 {
		g.SupportingSlabs[slab] = utils.NewSet[SandSlab]()
	}

	if _, ok := g.SlabMap[slabTopHeight]; !ok {
		g.SlabMap[slabTopHeight] = make(map[utils.Coord]SandSlab)
	}

	for _, coord := range slabArea {
		g.ElevationMap[coord] = slabTopHeight
		g.SlabMap[slabTopHeight][coord] = slab

		if supportingSlab, ok := g.SlabMap[maxHeight][coord]; ok {
			g.SupportingSlabs[slab].Add(supportingSlab)
		}
	}
}

func (g *Ground) PrintElevationMap() {
	maxX := 5
	maxY := 5

	for slab := range g.SupportingSlabs {
		if slab.Pos.X+slab.W-1 > maxX {
			maxX = slab.Pos.X + slab.W
		}
		if slab.Pos.Y+slab.L-1 > maxY {
			maxY = slab.Pos.Y + slab.L
		}
	}

	for y := 0; y <= maxY; y++ {
		for x := 0; x <= maxX; x++ {
			fmt.Print(g.ElevationMap[utils.NewCoord(x, y)], " ")
		}
		fmt.Println()
	}
}

func (g *Ground) IsOnlySupporter(slab SandSlab) bool {
	for _, supportingSlabs := range g.SupportingSlabs {
		if supportingSlabs.Size() == 1 && supportingSlabs.Contains(slab) {
			return true
		}
	}
	return false
}

func (g *Ground) BricksFallingAfterDesintegrationRecur(desintegrated *utils.Set[SandSlab]) int {
	for otherSlab, supportingSlabs := range g.SupportingSlabs {
		if !desintegrated.Contains(otherSlab) && supportingSlabs.Sub(desintegrated).Size() == 0 {
			desintegrated.Add(otherSlab)
			g.BricksFallingAfterDesintegrationRecur(desintegrated)
		}
	}
	return desintegrated.Size() - 1
}

func (g *Ground) BricksFallingAfterDesintegration(desintegrated SandSlab) int {
	set := utils.NewSet[SandSlab](desintegrated)
	return g.BricksFallingAfterDesintegrationRecur(set)
}

type SandSlab struct {
	Pos       utils.Coord
	Elevation int
	W, L, H   int
}

func NewSandSlab(pos utils.Coord, elevation, w, l, h int) SandSlab {
	return SandSlab{Pos: pos, Elevation: elevation, W: w, L: l, H: h}
}

func (s *SandSlab) GetAreaCoords() []utils.Coord {
	coords := make([]utils.Coord, 0)
	for x := s.Pos.X; x < s.Pos.X+s.W; x++ {
		for y := s.Pos.Y; y < s.Pos.Y+s.L; y++ {
			coords = append(coords, utils.NewCoord(x, y))
		}
	}
	return coords
}

func PrintResult() {
	today := utils.NewDay("day22")
	input := today.ReadInputLines()

	sandSlabs := ParseInput(input)
	ground := NewGround()

	for _, slab := range sandSlabs {
		ground.AddSlab(slab)
	}

	part1(sandSlabs, ground)
	part2(sandSlabs, ground)
}

func part1(sandSlabs []SandSlab, ground Ground) {
	acc := 0
	for _, slab := range sandSlabs {
		if !ground.IsOnlySupporter(slab) {
			acc++
		}
	}
	fmt.Println(acc)
}

func part2(sandSlabs []SandSlab, ground Ground) {
	acc := 0
	for _, slab := range sandSlabs {
		acc += ground.BricksFallingAfterDesintegration(slab)
	}
	fmt.Println(acc)
}

func ParseInput(input []string) []SandSlab {
	getCoords := func(s string) (int, int, int) {
		split := strings.Split(s, ",")
		x := utils.MustAtoi(split[0])
		y := utils.MustAtoi(split[1])
		z := utils.MustAtoi(split[2])
		return x, y, z
	}

	sandSlabs := make([]SandSlab, 0)

	for _, line := range input {
		splitLine := strings.Split(line, "~")

		x1, y1, z1 := getCoords(splitLine[0])
		x2, y2, z2 := getCoords(splitLine[1])

		minX := utils.Min(x1, x2)
		minY := utils.Min(y1, y2)

		pos := utils.NewCoord(minX, minY)
		elevation := utils.Max(z1, z2)
		w := utils.Abs(x1-x2) + 1
		l := utils.Abs(y1-y2) + 1
		h := utils.Abs(z1-z2) + 1

		sandSlabs = append(sandSlabs, NewSandSlab(pos, elevation, w, l, h))
	}

	sort.Slice(sandSlabs, func(i, j int) bool {
		return sandSlabs[i].Elevation <= sandSlabs[j].Elevation
	})

	return sandSlabs
}
