package utils

import "fmt"

type Coord struct {
	X int
	Y int
}

func NewCoord(x, y int) Coord {
	return Coord{
		X: x,
		Y: y,
	}
}

func (c Coord) Dx(dx int) Coord {
	return Coord{
		X: c.X + dx,
		Y: c.Y,
	}
}

func (c Coord) Dy(dy int) Coord {
	return Coord{
		X: c.X,
		Y: c.Y + dy,
	}
}

func (c Coord) Add(other Coord) Coord {
	return Coord{
		X: c.X + other.X,
		Y: c.Y + other.Y,
	}
}

func (c Coord) Sub(other Coord) Coord {
	return Coord{
		X: c.X - other.X,
		Y: c.Y - other.Y,
	}
}

func (c Coord) Neighbors() *Set[Coord] {
	neighbs := NewSet[Coord]()
	for _, dx := range []int{-1, 0, 1} {
		for _, dy := range []int{-1, 0, 1} {
			if dx != 0 || dy != 0 {
				neighbs.Add(Coord{c.X + dx, c.Y + dy})
			}
		}
	}
	return neighbs
}

func (c Coord) String() string {
	return fmt.Sprintf("(%d,%d)", c.X, c.Y)
}
