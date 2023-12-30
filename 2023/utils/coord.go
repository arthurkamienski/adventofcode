package utils

import "fmt"

type Coord struct {
	X int
	Y int
}

func Directions() []Coord {
	return []Coord{
		UpCoord(),
		DownCoord(),
		LeftCoord(),
		RightCoord(),
	}
}

func UpCoord() Coord {
	return Coord{
		X: 0,
		Y: -1,
	}
}

func DownCoord() Coord {
	return Coord{
		X: 0,
		Y: 1,
	}
}

func LeftCoord() Coord {
	return Coord{
		X: -1,
		Y: 0,
	}
}

func RightCoord() Coord {
	return Coord{
		X: 1,
		Y: 0,
	}
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

func (c Coord) ManhattanDistance(other Coord) int {
	return Abs(c.X-other.X) + Abs(c.Y-other.Y)
}

func (c Coord) Negate() Coord {
	return Coord{
		X: -c.X,
		Y: -c.Y,
	}
}

func (c Coord) XBetween(min, max int) bool {
	return c.X >= min && c.X <= max
}

func (c Coord) YBetween(min, max int) bool {
	return c.Y >= min && c.Y <= max
}

func (c Coord) Times(i int) Coord {
	return Coord{
		X: c.X * i,
		Y: c.Y * i,
	}
}
