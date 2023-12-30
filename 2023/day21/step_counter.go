package day21

import (
	"adventofcode/2023/utils"
	"fmt"
)

type State struct {
	Pos   utils.Coord
	Steps int
}

func NewState(pos utils.Coord, steps int) State {
	return State{Pos: pos, Steps: steps}
}

type Garden struct {
	Rocks      *utils.Set[utils.Coord]
	StartPos   utils.Coord
	MaxX, MaxY int
}

func (g *Garden) GetStartState() State {
	return NewState(g.StartPos, 0)
}

func (g *Garden) NextStatesFrom(state State, isEven bool, reachableCoords *utils.Set[utils.Coord]) []State {
	nextStates := make([]State, 0)
	for _, dir := range utils.Directions() {
		nextPos := state.Pos.Add(dir)

		isInBounds := nextPos.XBetween(0, g.MaxX-1) && nextPos.YBetween(0, g.MaxY-1)

		if g.Rocks.Contains(nextPos) || reachableCoords.Contains(nextPos) || !isInBounds {
			continue
		}

		isReachable := isEven && (state.Steps+1)%2 == 0 || !isEven && (state.Steps+1)%2 == 1

		if isReachable {
			reachableCoords.Add(nextPos)
		}

		nextStates = append(nextStates, NewState(nextPos, state.Steps+1))
	}
	return nextStates
}

func (g *Garden) GetNextStates(
	states *utils.Set[State],
	isEven bool,
	reachableCoords *utils.Set[utils.Coord]) *utils.Set[State] {
	nextStates := make([]State, 0)
	for _, state := range states.Values() {
		nextStates = append(nextStates, g.NextStatesFrom(state, isEven, reachableCoords)...)
	}
	return utils.NewSetFromSlice[State](nextStates)
}

func (g *Garden) GetReachableCoords(steps int, isEven bool, startCoord utils.Coord) int {
	reachableCoords := utils.NewSet[utils.Coord]()
	states := utils.NewSet[State]()
	states.Add(NewState(startCoord, 0))

	for step := 0; step < steps; step++ {
		states = g.GetNextStates(states, isEven, reachableCoords)
	}

	return reachableCoords.Size()
}

func PrintResult() {
	today := utils.NewDay("day21")
	input := today.ReadInputLines()

	garden := ParseInput(input)

	part1(garden)
	part2(garden, 26501365)
}

func part1(garden *Garden) {
	fmt.Println(garden.GetReachableCoords(64, true, garden.StartPos))
}

// Considering the number of total steps is ODD (26501365)
// and that the map has a direct straight path from the center to each edge,
// we can think of the problem as a grid of maps, where traversing
// one map is equivalent to walking mapSize+1 steps in the original map.
// Conveniently, we start in the center of the map and the number os steps minus (mapSize/2)
// is divisible by mapSize, meaning that we will always traverse an integer number of maps.
//
// We will thus traverse R = ((steps - mapSize/2) / mapSize) maps, which forms a circle of radius R.
// The area of this circle is (R+1)^2 + Rˆ2, but that is considering that we will always reach all of the
// squares in a map (which is not the case for maps in the edges of the circle). Doing the math, we will have
// (R^2 - (R-1)^2) full filled maps, R maps that are only filled in the corners (mapSize/2-1 steps from the corner),
// R-1 maps that are not filled in the corners (mapSize/2+mapSize-1 steps from the corner), and 4 points, which are
// filled from the middle edges of the map walking mapSize-1 steps.
// On top of that, we also have to consider that some maps are filled in the "even" coordinates, while others are
// filled in the odd coordinates
func part2(garden *Garden, totalSteps int) {
	oddSteps := totalSteps%2 == 1
	radius := (totalSteps - garden.MaxX/2) / garden.MaxX

	totalReachableEven := garden.GetReachableCoords(500, true, garden.StartPos)
	totalReachableOdd := garden.GetReachableCoords(500, false, garden.StartPos)

	cornerCoords := []utils.Coord{
		utils.NewCoord(0, 0),
		utils.NewCoord(0, garden.MaxY-1),
		utils.NewCoord(garden.MaxX-1, 0),
		utils.NewCoord(garden.MaxX-1, garden.MaxY-1),
	}
	middleCoords := []utils.Coord{
		utils.NewCoord(0, garden.MaxY/2),
		utils.NewCoord(garden.MaxX/2, 0),
		utils.NewCoord(garden.MaxX-1, garden.MaxY/2),
		utils.NewCoord(garden.MaxX/2, garden.MaxY-1),
	}

	cornerReachable := 0
	for _, corner := range cornerCoords {
		cornerReachable += garden.GetReachableCoords(garden.MaxX/2-1, oddSteps, corner)
	}

	missingCornerReachable := 0
	for _, corner := range cornerCoords {
		missingCornerReachable += garden.GetReachableCoords(garden.MaxX+garden.MaxX/2-1, !oddSteps, corner)
	}

	endCaps := 0
	for _, middle := range middleCoords {
		endCaps += garden.GetReachableCoords(garden.MaxX-1, oddSteps, middle)
	}

	fullSquares := 0
	if oddSteps {
		fullEvenSquares := radius * radius * totalReachableEven
		fullOddSquares := (radius - 1) * (radius - 1) * totalReachableOdd
		fullSquares = fullEvenSquares + fullOddSquares
	} else {
		fullEvenSquares := (radius - 1) * (radius - 1) * totalReachableEven
		fullOddSquares := radius * radius * totalReachableOdd
		fullSquares = fullEvenSquares + fullOddSquares
	}

	evenCorners := radius * cornerReachable
	oddMissingCorners := (radius - 1) * missingCornerReachable

	fmt.Println(fullSquares + evenCorners + oddMissingCorners + endCaps)

}

func ParseInput(input []string) *Garden {
	rocks := utils.NewSet[utils.Coord]()
	startPos := utils.NewCoord(0, 0)
	for y, line := range input {
		for x, char := range line {
			if char == '#' {
				rocks.Add(utils.NewCoord(x, y))
			} else if char == 'S' {
				startPos = utils.NewCoord(x, y)
			}
		}
	}

	return &Garden{
		Rocks:    rocks,
		StartPos: startPos,
		MaxX:     len(input[0]),
		MaxY:     len(input),
	}
}
