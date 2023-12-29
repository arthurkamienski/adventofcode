package day18

import "adventofcode/2023/utils"

type Edge struct {
	From utils.Coord
	To   utils.Coord
}

func NewEdge(from, to utils.Coord) Edge {
	return Edge{
		From: from,
		To:   to,
	}
}

func (e Edge) Contains(c utils.Coord) bool {
	minY := utils.Min(e.From.Y, e.To.Y)
	maxY := utils.Max(e.From.Y, e.To.Y)
	minX := utils.Min(e.From.X, e.To.X)
	maxX := utils.Max(e.From.X, e.To.X)
	return c.XBetween(minX, maxX) && c.YBetween(minY, maxY)
}

func (e Edge) ContainsSegment(coord1, coord2 utils.Coord) bool {
	return e.Contains(coord1) && e.Contains(coord2)
}
