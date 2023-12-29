package day18

import (
	"adventofcode/2023/utils"
	"fmt"
	"slices"
	"strings"
)

const (
	MaxInt = int(^uint(0) >> 1)
	MinInt = -(MaxInt - 1)
)

type Trench struct {
	Vertices *utils.Set[utils.Coord]
	Edges    *utils.Set[Edge]
}

func NewTrench() *Trench {
	return &Trench{
		Vertices: utils.NewSet[utils.Coord](),
		Edges:    utils.NewSet[Edge](),
	}
}

func (t *Trench) Dig(instructions []Instruction) {
	currPos := utils.NewCoord(0, 0)
	t.Vertices.Add(currPos)

	for _, instruction := range instructions {
		nextPos := currPos.Add(instruction.Dir.Times(instruction.Len))
		t.Vertices.Add(nextPos)
		t.Edges.Add(NewEdge(currPos, nextPos))
		currPos = nextPos
	}
}

func (t *Trench) SquareSize() int {
	minX, minY := MaxInt, MaxInt
	maxX, maxY := MinInt, MinInt
	for _, vertex := range t.Vertices.Values() {
		minX = utils.Min(minX, vertex.X)
		minY = utils.Min(minY, vertex.Y)
		maxX = utils.Max(maxX, vertex.X)
		maxY = utils.Max(maxY, vertex.Y)
	}

	return (maxX - minX + 1) * (maxY - minY + 1)
}

func (t *Trench) GetVertexGrid() [][]utils.Coord {
	rowsWithVertices := utils.NewSet[int]()
	colsWithVertices := utils.NewSet[int]()

	for _, vertex := range t.Vertices.Values() {
		rowsWithVertices.Add(vertex.Y)
		colsWithVertices.Add(vertex.X)
	}
	rowsWithVerticesValues := rowsWithVertices.Values()
	colsWithVerticesValues := colsWithVertices.Values()

	slices.Sort(rowsWithVerticesValues)
	slices.Sort(colsWithVerticesValues)

	grid := make([][]utils.Coord, len(rowsWithVerticesValues))

	for x, row := range rowsWithVerticesValues {
		grid[x] = make([]utils.Coord, len(colsWithVerticesValues))
		for y, col := range colsWithVerticesValues {
			grid[x][y] = utils.NewCoord(col, row)
		}
	}

	return grid
}

func (t *Trench) GetBoxes(grid [][]utils.Coord) map[utils.Coord]Box {
	boxes := make(map[utils.Coord]Box)
	for i := 0; i < len(grid)-1; i++ {
		row := grid[i]
		for j := 0; j < len(row)-1; j++ {
			topLeft := grid[i][j]
			bottomRight := grid[i+1][j+1]
			coord := utils.NewCoord(j, i)

			boxes[coord] = NewBox(coord, topLeft, bottomRight, t.Edges)
		}
	}
	return boxes
}

func (t *Trench) FindExternalEdgeBoxes(boxes map[utils.Coord]Box, maxX, maxY int) *utils.Set[utils.Coord] {
	externalBoxes := utils.NewSet[utils.Coord]()
	for _, box := range boxes {
		for i := 0; i < len(box.Neighbors) && !externalBoxes.Contains(box.Coord); i++ {
			neighbor := box.Neighbors[i]
			if !neighbor.XBetween(0, maxX-1) || !neighbor.YBetween(0, maxY-1) {
				externalBoxes.Add(box.Coord)
			}
		}
	}
	return externalBoxes
}

func (t *Trench) FindExternalBoxes(boxes map[utils.Coord]Box, maxX, maxY int) *utils.Set[utils.Coord] {
	initExternalBoxes := t.FindExternalEdgeBoxes(boxes, maxX, maxY)
	allExternalBoxes := utils.NewSet[utils.Coord]()
	allExternalBoxes.InPlaceUnion(initExternalBoxes)
	newExternalBoxes := initExternalBoxes

	for newExternalBoxes.Size() != 0 {
		prevExternalBoxesValues := newExternalBoxes.Values()
		newExternalBoxes = utils.NewSet[utils.Coord]()

		for _, externalBox := range prevExternalBoxesValues {
			box := boxes[externalBox]
			for _, neighbor := range box.Neighbors {
				if _, ok := boxes[neighbor]; ok && !allExternalBoxes.Contains(neighbor) {
					newExternalBoxes.Add(neighbor)
				}
			}
		}
		allExternalBoxes.InPlaceUnion(newExternalBoxes)
	}

	return allExternalBoxes
}

func (t *Trench) FindInternalArea() int {
	grid := t.GetVertexGrid()
	maxX, maxY := len(grid[0])-1, len(grid)-1
	boxes := t.GetBoxes(grid)
	externalBoxes := t.FindExternalBoxes(boxes, maxX, maxY)

	externalInternalArea := 0
	externalBorderArea := 0
	externalCorners := utils.NewSet[utils.Coord]()

	for _, externalBox := range externalBoxes.Values() {
		externalInternalArea += boxes[externalBox].AreaWithoutBorder()
		externalBorderArea += boxes[externalBox].BorderArea(maxX, maxY)
		externalCorners.InPlaceUnion(boxes[externalBox].GetCorners())
	}

	for _, corner := range externalCorners.Values() {
		for _, edge := range t.Edges.Values() {
			if edge.Contains(corner) {
				externalCorners.Remove(corner)
			}
		}
	}

	totalArea := t.SquareSize()
	externalArea := externalInternalArea + (externalBorderArea / 2) + externalCorners.Size()

	return totalArea - externalArea
}

func PrintResult() {
	today := utils.NewDay("day18")
	input := today.ReadInputLines()
	instructions := ParseInput(input)

	part1(instructions)
	part2(instructions)
}

func part1(instructions []Instruction) {
	t := NewTrench()
	t.Dig(instructions)

	internalArea := t.FindInternalArea()
	fmt.Println(internalArea)
}

func part2(instructions []Instruction) {
	t := NewTrench()
	for i, instruction := range instructions {
		instructions[i] = instruction.ColorToInstruction()
	}
	t.Dig(instructions)

	internalArea := t.FindInternalArea()
	fmt.Println(internalArea)
}

func ParseInput(input []string) []Instruction {
	var instructions []Instruction
	for _, line := range input {
		splitLine := strings.Split(line, " ")
		dist := utils.MustAtoi(splitLine[1])
		color := strings.Trim(splitLine[2], "()")

		var dir utils.Coord
		switch splitLine[0] {
		case "R":
			dir = utils.RightCoord()
		case "L":
			dir = utils.LeftCoord()
		case "U":
			dir = utils.UpCoord()
		case "D":
			dir = utils.DownCoord()
		}

		instructions = append(instructions, Instruction{
			Dir:   dir,
			Len:   dist,
			Color: color,
		})
	}

	return instructions
}
