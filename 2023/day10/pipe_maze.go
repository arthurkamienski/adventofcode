package day10

import (
	"adventofcode/2023/utils"
	"fmt"
)

type PipeMap map[utils.Coord]*Pipe

type Pipe struct {
	Char      rune
	Pos       utils.Coord
	Neighbors *utils.Set[utils.Coord]
}

func NewPipe(pos utils.Coord, char rune) *Pipe {
	var neighbors []utils.Coord
	switch char {
	case '|':
		neighbors = []utils.Coord{
			pos.Dy(-1),
			pos.Dy(1),
		}
	case '-':
		neighbors = []utils.Coord{
			pos.Dx(-1),
			pos.Dx(1),
		}
	case 'L':
		neighbors = []utils.Coord{
			pos.Dx(1),
			pos.Dy(-1),
		}
	case 'F':
		neighbors = []utils.Coord{
			pos.Dx(1),
			pos.Dy(1),
		}
	case '7':
		neighbors = []utils.Coord{
			pos.Dx(-1),
			pos.Dy(1),
		}
	case 'J':
		neighbors = []utils.Coord{
			pos.Dx(-1),
			pos.Dy(-1),
		}
	default:
		panic(fmt.Sprintf("Invalid pipe char: %c", char))
	}

	return &Pipe{
		Pos:       pos,
		Char:      char,
		Neighbors: utils.NewSetFromSlice[utils.Coord](neighbors),
	}
}

func (p *Pipe) LeftAndRightNeighbors(dir utils.Coord) (*utils.Set[utils.Coord], *utils.Set[utils.Coord]) {
	leftNeighbors := utils.NewSet[utils.Coord]()  //convention -> left
	rightNeighbors := utils.NewSet[utils.Coord]() // convention -> right

	left := p.Pos.Dx(-1)
	right := p.Pos.Dx(1)
	up := p.Pos.Dy(-1)
	down := p.Pos.Dy(1)

	char := p.Char
	if char == 'S' {
		if p.Neighbors.Contains(left) {
			if p.Neighbors.Contains(up) {
				char = 'J'
			} else if p.Neighbors.Contains(down) {
				char = '7'
			} else {
				char = '-'
			}
		} else if p.Neighbors.Contains(right) {
			if p.Neighbors.Contains(up) {
				char = 'L'
			} else {
				char = 'F'
			}
		} else {
			char = '|'
		}
	}

	switch p.Char {
	case '|':
		if dir.Y == -1 { // going up
			leftNeighbors.Add(left)
			rightNeighbors.Add(right)
		} else {
			leftNeighbors.Add(right)
			rightNeighbors.Add(left)
		}
	case '-':
		if dir.X == 1 { // going right
			leftNeighbors.Add(up)
			rightNeighbors.Add(down)
		} else {
			leftNeighbors.Add(down)
			rightNeighbors.Add(up)
		}
	case 'L':
		if dir.Y == 1 { // going down
			rightNeighbors.Add(down)
			rightNeighbors.Add(left)
		} else { // going left
			leftNeighbors.Add(down)
			leftNeighbors.Add(left)
		}
	case 'F':
		if dir.Y == -1 { // going up
			leftNeighbors.Add(up)
			leftNeighbors.Add(left)
		} else { // going left
			rightNeighbors.Add(up)
			rightNeighbors.Add(left)
		}
	case '7':
		if dir.Y == -1 { // going up
			rightNeighbors.Add(up)
			rightNeighbors.Add(right)
		} else { // going right
			leftNeighbors.Add(up)
			leftNeighbors.Add(right)
		}
	case 'J':
		if dir.Y == 1 { // going down
			leftNeighbors.Add(down)
			leftNeighbors.Add(right)
		} else { // going right
			rightNeighbors.Add(down)
			rightNeighbors.Add(right)
		}
	}

	return leftNeighbors, rightNeighbors
}

func (p *Pipe) String() string {
	return fmt.Sprintf("Pipe %c at %s", p.Char, p.Pos)
}

func (p *Pipe) GetNext(pipeMap PipeMap, previous *Pipe) *Pipe {
	var next *Pipe
	for _, neighbor := range p.Neighbors.Values() {
		if neighbor != previous.Pos {
			next = pipeMap[neighbor]
		}
	}

	return next
}

func PrintResult() {
	today := utils.NewDay("day10")
	input := today.ReadInputLines()

	pipes, startPipe := parseInput(input)

	part1(pipes, startPipe)
	part2(pipes, startPipe)
}

func parseInput(input []string) (PipeMap, *Pipe) {
	pipeMap := make(PipeMap)
	var startPos utils.Coord

	for y, line := range input {
		for x, char := range line {
			if char != '.' {
				pos := utils.Coord{X: x, Y: y}
				if char == 'S' {
					startPos = pos
				} else {
					pipe := NewPipe(pos, char)
					pipeMap[pos] = pipe
				}
			}
		}
	}

	startNeighbors := utils.NewSet[utils.Coord]()
	for _, pipe := range pipeMap {
		if pipe.Neighbors.Contains(startPos) {
			startNeighbors.Add(pipe.Pos)
		}
	}

	startPipe := &Pipe{
		Char:      'S',
		Pos:       startPos,
		Neighbors: startNeighbors,
	}
	pipeMap[startPos] = startPipe

	return pipeMap, startPipe
}

func part1(pipeMap PipeMap, startPipe *Pipe) {
	loop := findLoop(pipeMap, startPipe)

	fmt.Println(loop.Size() / 2)
}

func findLoop(pipeMap PipeMap, startPipe *Pipe) *utils.Set[utils.Coord] {
	curr := pipeMap[startPipe.Neighbors.Values()[0]]
	previous := startPipe

	path := utils.NewSet[utils.Coord]()
	path.Add(startPipe.Pos)

	for curr != startPipe {
		path.Add(curr.Pos)
		aux := curr
		curr = curr.GetNext(pipeMap, previous)
		previous = aux
	}

	return path
}

func part2(pipeMap PipeMap, startPipe *Pipe) {
	path := findLoop(pipeMap, startPipe)
	leftNeighbors, rightNeighbors := findLeftAndRightNeighbors(pipeMap, startPipe)

	leftNeighbors = leftNeighbors.Sub(path)
	rightNeighbors = rightNeighbors.Sub(path)

	maxX, maxY := getUpperBounds(pipeMap)

	rightNeighbors = GrowCoordsUntilStable(path, rightNeighbors, maxX, maxY)
	leftNeighbors = GrowCoordsUntilStable(path, leftNeighbors, maxX, maxY)

	inside := leftNeighbors

	if isOutside(leftNeighbors, maxX, maxY) {
		inside = rightNeighbors
	}

	fmt.Println(inside.Size())
}

func findLeftAndRightNeighbors(pipeMap PipeMap, startPipe *Pipe) (*utils.Set[utils.Coord], *utils.Set[utils.Coord]) {
	curr := pipeMap[startPipe.Neighbors.Values()[0]]
	previous := startPipe

	dir := curr.Pos.Sub(previous.Pos)

	leftNeighbors, rightNeighbors := curr.LeftAndRightNeighbors(dir)

	for curr != startPipe {
		aux := curr
		curr = curr.GetNext(pipeMap, previous)
		previous = aux

		dir = curr.Pos.Sub(previous.Pos)

		newLeftNeighbors, newRightNeighbors := curr.LeftAndRightNeighbors(dir)

		leftNeighbors.InPlaceUnion(newLeftNeighbors)
		rightNeighbors.InPlaceUnion(newRightNeighbors)
	}

	return leftNeighbors, rightNeighbors
}

func GrowCoordsUntilStable(path *utils.Set[utils.Coord], coords *utils.Set[utils.Coord], maxX, maxY int) *utils.Set[utils.Coord] {
	currCoords := coords

	for currCoords.Size() != 0 {
		newCoords := utils.NewSet[utils.Coord]()
		for _, coord := range coords.Values() {
			for _, neighbor := range coord.Neighbors().Values() {
				isInBounds := neighbor.X >= -1 && neighbor.Y >= -1 && neighbor.X <= maxX+1 && neighbor.Y <= maxY+1
				if isInBounds && !path.Contains(neighbor) && !coords.Contains(neighbor) {
					newCoords.Add(neighbor)
				}
			}
		}

		coords = coords.Union(newCoords)
		currCoords = newCoords
	}

	return coords
}

func getUpperBounds(pipeMap PipeMap) (int, int) {
	maxX := 0
	maxY := 0
	for coord := range pipeMap {
		if coord.X > maxX {
			maxX = coord.X
		}
		if coord.Y > maxY {
			maxY = coord.Y
		}
	}

	return maxX, maxY
}

func isOutside(coords *utils.Set[utils.Coord], maxX, maxY int) bool {
	for _, coord := range coords.Values() {
		if coord.X == -1 || coord.Y == -1 || coord.X == maxX+1 || coord.Y == maxY+1 {
			return true
		}
	}

	return false
}
