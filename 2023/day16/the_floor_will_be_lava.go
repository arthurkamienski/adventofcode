package day16

import (
	"adventofcode/2023/utils"
	"fmt"
)

type Beam struct {
	Dir, Pos utils.Coord
}

func (b Beam) String() string {
	return fmt.Sprintf("Beam{Dir: %v, Pos: %v}", b.Dir, b.Pos)
}

type Mirror struct {
	Type rune
	Pos  utils.Coord
}

func (m *Mirror) String() string {
	return fmt.Sprintf("Mirror{Type: %c, Pos: %v}", m.Type, m.Pos)
}

func (m *Mirror) Reflect(beam Beam) *utils.Set[Beam] {
	newDirs := make([]utils.Coord, 0)
	switch m.Type {
	case '/':
		if beam.Dir.X == 0 {
			if beam.Dir.Y == 1 {
				newDirs = append(newDirs, utils.NewCoord(-1, 0))
			} else {
				newDirs = append(newDirs, utils.NewCoord(1, 0))
			}
		} else {
			if beam.Dir.X == 1 {
				newDirs = append(newDirs, utils.NewCoord(0, -1))
			} else {
				newDirs = append(newDirs, utils.NewCoord(0, 1))
			}
		}
	case '\\':
		if beam.Dir.X == 0 {
			if beam.Dir.Y == 1 {
				newDirs = append(newDirs, utils.NewCoord(1, 0))
			} else {
				newDirs = append(newDirs, utils.NewCoord(-1, 0))
			}
		} else {
			if beam.Dir.X == 1 {
				newDirs = append(newDirs, utils.NewCoord(0, 1))
			} else {
				newDirs = append(newDirs, utils.NewCoord(0, -1))
			}
		}
	case '-':
		newDirs = append(newDirs, utils.NewCoord(1, 0), utils.NewCoord(-1, 0))
	case '|':
		newDirs = append(newDirs, utils.NewCoord(0, 1), utils.NewCoord(0, -1))
	}
	beams := utils.NewSet[Beam]()
	for _, dir := range newDirs {
		beams.Add(Beam{
			Dir: dir,
			Pos: m.Pos,
		})
	}
	return beams
}

type Contraption struct {
	MaxX, MaxY                    int
	MirrorsInRow, MirrorsInColumn map[int][]*Mirror
	EnergizedTiles                *utils.Set[utils.Coord]
	Beams                         *utils.Set[Beam]
	PreviousBeams                 *utils.Set[Beam]
}

func (c *Contraption) Step() {
	newBeams := utils.NewSet[Beam]()
	for _, beam := range c.Beams.Values() {
		hasMirror, mirror := c.GetNextMirror(beam)
		if hasMirror {
			c.Energize(beam.Pos, mirror.Pos)
			reflectedBeams := mirror.Reflect(beam)
			newBeams.InPlaceUnion(reflectedBeams)
			newBeams = newBeams.Sub(c.PreviousBeams)
		} else {
			var finalCoord utils.Coord
			if beam.Dir.X == 0 {
				if beam.Dir.Y == 1 {
					finalCoord = utils.NewCoord(beam.Pos.X, c.MaxY-1)
				} else {
					finalCoord = utils.NewCoord(beam.Pos.X, 0)
				}
			} else {
				if beam.Dir.X == 1 {
					finalCoord = utils.NewCoord(c.MaxX-1, beam.Pos.Y)
				} else {
					finalCoord = utils.NewCoord(0, beam.Pos.Y)
				}
			}
			c.Energize(beam.Pos, finalCoord)
		}
	}
	c.Beams = newBeams
	c.PreviousBeams.InPlaceUnion(newBeams)
}

func (c *Contraption) StepUntilStable() {
	c.PreviousBeams = utils.NewSet[Beam]()
	for c.Beams.Size() != 0 {
		c.Step()
	}
}

func (c *Contraption) GetNextMirror(beam Beam) (bool, *Mirror) {
	hasMirror := false
	mirror := &Mirror{}

	if beam.Dir.Y == 0 {
		if beam.Dir.X == 1 {
			mirrorsInRow := c.MirrorsInRow[beam.Pos.Y]
			firstMirrorPos := c.MaxX

			for i := 0; i < len(mirrorsInRow); i++ {
				if mirrorsInRow[i].Pos.X > beam.Pos.X {
					if mirrorsInRow[i].Pos.X < firstMirrorPos {
						hasMirror = true
						mirror = mirrorsInRow[i]
						firstMirrorPos = mirrorsInRow[i].Pos.X
					}
				}
			}
		} else {
			mirrorsInRow := c.MirrorsInRow[beam.Pos.Y]
			firstMirrorPos := -1

			for i := 0; i < len(mirrorsInRow); i++ {
				if mirrorsInRow[i].Pos.X < beam.Pos.X {
					if mirrorsInRow[i].Pos.X > firstMirrorPos {
						hasMirror = true
						mirror = mirrorsInRow[i]
						firstMirrorPos = mirrorsInRow[i].Pos.X
					}
				}
			}
		}
	} else {
		if beam.Dir.Y == 1 {
			mirrorsInColumn := c.MirrorsInColumn[beam.Pos.X]
			firstMirrorPos := c.MaxY

			for i := 0; i < len(mirrorsInColumn); i++ {
				if mirrorsInColumn[i].Pos.Y > beam.Pos.Y {
					if mirrorsInColumn[i].Pos.Y < firstMirrorPos {
						hasMirror = true
						mirror = mirrorsInColumn[i]
						firstMirrorPos = mirrorsInColumn[i].Pos.Y
					}
				}
			}
		} else {
			mirrorsInColumn := c.MirrorsInColumn[beam.Pos.X]
			firstMirrorPos := -1

			for i := 0; i < len(mirrorsInColumn); i++ {
				if mirrorsInColumn[i].Pos.Y < beam.Pos.Y {
					if mirrorsInColumn[i].Pos.Y > firstMirrorPos {
						hasMirror = true
						mirror = mirrorsInColumn[i]
						firstMirrorPos = mirrorsInColumn[i].Pos.Y
					}
				}
			}
		}
	}

	return hasMirror, mirror
}

func (c *Contraption) Energize(coord1, coord2 utils.Coord) {
	if coord1.X == coord2.X {
		minY := utils.Max(utils.Min(coord1.Y, coord2.Y), 0)
		maxY := utils.Min(utils.Max(coord1.Y, coord2.Y), c.MaxY-1)
		for y := minY; y <= maxY; y++ {
			c.EnergizedTiles.Add(utils.NewCoord(coord1.X, y))
		}
	} else {
		minX := utils.Max(utils.Min(coord1.X, coord2.X), 0)
		maxX := utils.Min(utils.Max(coord1.X, coord2.X), c.MaxX-1)
		for x := minX; x <= maxX; x++ {
			c.EnergizedTiles.Add(utils.NewCoord(x, coord1.Y))
		}
	}
}

func (c *Contraption) PrintEnergized() {
	for y := 0; y < c.MaxY; y++ {
		for x := 0; x < c.MaxX; x++ {
			if c.EnergizedTiles.Contains(utils.NewCoord(x, y)) {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

func (c *Contraption) Reset() {
	c.EnergizedTiles = utils.NewSet[utils.Coord]()
	c.Beams = utils.NewSet[Beam]()
	c.PreviousBeams = utils.NewSet[Beam]()
}

func (c *Contraption) FindMaxEnergization() int {
	c.Reset()

	e := 0

	for y := 0; y < c.MaxY; y++ {
		c.Beams.Add(Beam{Pos: utils.NewCoord(-1, y), Dir: utils.NewCoord(1, 0)})
		c.StepUntilStable()
		e1 := c.EnergizedTiles.Size()
		c.Reset()
		c.Beams.Add(Beam{Pos: utils.NewCoord(c.MaxX, y), Dir: utils.NewCoord(-1, 0)})
		c.StepUntilStable()
		e2 := c.EnergizedTiles.Size()
		c.Reset()

		e = utils.Max(e, utils.Max(e1, e2))
	}

	for x := 0; x < c.MaxX; x++ {
		c.Beams.Add(Beam{Pos: utils.NewCoord(x, -1), Dir: utils.NewCoord(0, 1)})
		c.StepUntilStable()
		e1 := c.EnergizedTiles.Size()
		c.Reset()
		c.Beams.Add(Beam{Pos: utils.NewCoord(x, c.MaxY), Dir: utils.NewCoord(0, -1)})
		c.StepUntilStable()
		e2 := c.EnergizedTiles.Size()
		c.Reset()

		e = utils.Max(e, utils.Max(e1, e2))
	}

	return e
}

func PrintResult() {
	today := utils.NewDay("day16")
	input := today.ReadInputLines()
	contraption := ParseInput(input)
	contraption.Beams.Add(Beam{Pos: utils.NewCoord(-1, 0), Dir: utils.NewCoord(1, 0)})
	contraption.StepUntilStable()
	fmt.Println(contraption.EnergizedTiles.Size())

	fmt.Println(contraption.FindMaxEnergization())
}

func ParseInput(input []string) *Contraption {
	mirrorsInRow := make(map[int][]*Mirror)
	mirrorsInColumn := make(map[int][]*Mirror)
	energizedTiles := utils.NewSet[utils.Coord]()
	for y, line := range input {
		for x, char := range line {
			pos := utils.NewCoord(x, y)
			if char != '.' {
				mirror := &Mirror{
					Type: char,
					Pos:  pos,
				}

				if char != '-' {
					mirrorsInRow[y] = append(mirrorsInRow[y], mirror)
				}
				if char != '|' {
					mirrorsInColumn[x] = append(mirrorsInColumn[x], mirror)
				}
			}
		}
	}
	return &Contraption{
		MaxX:            len(input[0]),
		MaxY:            len(input),
		MirrorsInRow:    mirrorsInRow,
		MirrorsInColumn: mirrorsInColumn,
		EnergizedTiles:  energizedTiles,
		Beams:           utils.NewSet[Beam](),
		PreviousBeams:   utils.NewSet[Beam](),
	}
}
