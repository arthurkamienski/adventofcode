package day18

import (
	"adventofcode/2023/utils"
)

type Box struct {
	Coord, TopLeft, BottomRight utils.Coord
	Neighbors                   []utils.Coord
}

func NewBox(coord, topLeft, bottomRight utils.Coord, edges *utils.Set[Edge]) Box {
	return Box{
		Coord:       coord,
		TopLeft:     topLeft,
		BottomRight: bottomRight,
		Neighbors:   GetNeighbors(coord, topLeft, bottomRight, edges),
	}
}

func (b Box) AreaWithoutBorder() int {
	return (b.BottomRight.X - b.TopLeft.X - 1) * (b.BottomRight.Y - b.TopLeft.Y - 1)
}

func (b Box) BorderArea(maxX, maxY int) int {
	area := 0
	horizontalBorder := b.BottomRight.X - b.TopLeft.X - 1
	verticalBorder := b.BottomRight.Y - b.TopLeft.Y - 1
	for _, neighbor := range b.Neighbors {
		if neighbor.X == b.Coord.X {
			area += horizontalBorder
			if neighbor.Y >= maxY || neighbor.Y < 0 {
				area += horizontalBorder
			}
		} else if neighbor.Y == b.Coord.Y {
			area += verticalBorder
			if neighbor.X >= maxX || neighbor.X < 0 {
				area += verticalBorder
			}
		}
	}
	return area
}

func (b Box) GetCorners() *utils.Set[utils.Coord] {
	s := utils.NewSet[utils.Coord]()
	topRight := utils.NewCoord(b.BottomRight.X, b.TopLeft.Y)
	bottomLeft := utils.NewCoord(b.TopLeft.X, b.BottomRight.Y)

	s.Add(b.TopLeft)
	s.Add(topRight)
	s.Add(b.BottomRight)
	s.Add(bottomLeft)

	return s
}

func GetNeighbors(coord, topLeft, bottomRight utils.Coord, edges *utils.Set[Edge]) []utils.Coord {
	neighborDirections := utils.NewSetFromSlice[utils.Coord]([]utils.Coord{
		utils.UpCoord(),
		utils.RightCoord(),
		utils.DownCoord(),
		utils.LeftCoord(),
	})

	topRight := utils.NewCoord(bottomRight.X, topLeft.Y)
	bottomLeft := utils.NewCoord(topLeft.X, bottomRight.Y)

	for _, edge := range edges.Values() {

		if edge.ContainsSegment(topLeft, topRight) {
			neighborDirections.Remove(utils.UpCoord())
		} else if edge.ContainsSegment(topRight, bottomRight) {
			neighborDirections.Remove(utils.RightCoord())
		} else if edge.ContainsSegment(bottomRight, bottomLeft) {
			neighborDirections.Remove(utils.DownCoord())
		} else if edge.ContainsSegment(bottomLeft, topLeft) {
			neighborDirections.Remove(utils.LeftCoord())
		}
	}

	neighbors := make([]utils.Coord, neighborDirections.Size())
	for i, dir := range neighborDirections.Values() {
		neighbors[i] = coord.Add(dir)
	}

	return neighbors
}
