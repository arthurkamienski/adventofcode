package day23

import (
	"adventofcode/2023/utils"
	"fmt"
)

type State struct {
	Pos               utils.Coord
	DistanceTravelled int
	VisitedCode       int
}

func (s State) String() string {
	return fmt.Sprintf("%v-%d-%v", s.Pos, s.VisitedCode, s.DistanceTravelled)
}

func NewState(pos utils.Coord, code int) State {
	return State{
		Pos:               pos,
		DistanceTravelled: 0,
		VisitedCode:       code,
	}
}

func (s State) Follow(trail Trail, code int) State {
	return State{
		Pos:               trail.To,
		DistanceTravelled: s.DistanceTravelled + trail.Steps,
		VisitedCode:       s.VisitedCode + code,
	}
}

func (s State) HasVisited(code int) bool {
	return s.VisitedCode&code > 0
}

type Trail struct {
	From, To utils.Coord
	Steps    int
}

func (t Trail) String() string {
	return fmt.Sprintf("%v -> %v (%v)", t.From, t.To, t.Steps)
}

func NewTrail(coord utils.Coord) Trail {
	return Trail{
		From:  coord,
		To:    coord,
		Steps: 0,
	}
}

func (t Trail) GoTo(coord utils.Coord) Trail {
	return Trail{
		From:  t.From,
		To:    coord,
		Steps: t.Steps + 1,
	}
}

type TrailMap struct {
	MaxX, MaxY int

	Forests   *utils.Set[utils.Coord]
	Slopes    map[utils.Coord]utils.Coord
	Crossings []utils.Coord

	Trails        map[utils.Coord][]Trail
	CrossingCodes map[utils.Coord]int
}

func NewTrailMap(
	maxX, maxY int,
	forests *utils.Set[utils.Coord],
	slopes map[utils.Coord]utils.Coord,
) *TrailMap {
	t := &TrailMap{
		Forests:       forests,
		Slopes:        slopes,
		Crossings:     []utils.Coord{},
		Trails:        make(map[utils.Coord][]Trail),
		MaxX:          maxX,
		MaxY:          maxY,
		CrossingCodes: make(map[utils.Coord]int),
	}

	t.FindCrossings()

	return t
}

func (t *TrailMap) PossibleNeighbors(pos utils.Coord, considerSlopes bool) []utils.Coord {
	neighbors := make([]utils.Coord, 0)

	for _, dir := range utils.Directions() {
		nextPos := pos.Add(dir)

		isInBounds := nextPos.XBetween(0, t.MaxX-1) && nextPos.YBetween(0, t.MaxY-1)

		if !isInBounds {
			continue
		}

		if t.Forests.Contains(nextPos) {
			continue
		}

		if slope, ok := t.Slopes[nextPos]; considerSlopes && ok && slope == dir.Negate() {
			continue
		}

		neighbors = append(neighbors, nextPos)
	}

	return neighbors
}

func (t *TrailMap) FindCrossings() {
	crossings := make([]utils.Coord, 0)

	for x := 0; x < t.MaxX; x++ {
		for y := 0; y < t.MaxY; y++ {
			pos := utils.NewCoord(x, y)
			if t.Forests.Contains(pos) {
				continue
			}
			if len(t.PossibleNeighbors(pos, false)) > 2 {
				crossings = append(crossings, pos)
			}
		}
	}

	t.Crossings = append(crossings, utils.NewCoord(1, 0), utils.NewCoord(t.MaxX-2, t.MaxY-1))

	for i, crossing := range t.Crossings {
		t.CrossingCodes[crossing] = 1 << i
	}

}

func (t *TrailMap) AddCrossingTrails(crossing utils.Coord, hasSlopes bool) {
	neighbors := make([]Trail, 0)
	crossingsSet := utils.NewSet[utils.Coord](t.Crossings...)

	queue := make([]Trail, 0)
	queue = append(queue, NewTrail(crossing))

	visited := utils.NewSet[utils.Coord]()

	for len(queue) > 0 {
		trail := queue[0]
		queue = queue[1:]

		if visited.Contains(trail.To) {
			continue
		}

		visited.Add(trail.To)

		if trail.To != crossing && crossingsSet.Contains(trail.To) {
			neighbors = append(neighbors, trail)
			continue
		}

		for _, neighbor := range t.PossibleNeighbors(trail.To, hasSlopes) {
			queue = append(queue, trail.GoTo(neighbor))
		}
	}

	t.Trails[crossing] = neighbors
}

func (t *TrailMap) FindTrails(hasSlopes bool) {
	t.Trails = make(map[utils.Coord][]Trail)

	for _, crossing := range t.Crossings {
		t.AddCrossingTrails(crossing, hasSlopes)
	}
}

func (t *TrailMap) FindLongestPath(hasSlopes bool) int {
	t.FindTrails(hasSlopes)

	startPos := utils.NewCoord(1, 0)
	endPos := utils.NewCoord(t.MaxX-2, t.MaxY-1)
	finalStates := make([]State, 0)

	visitedStates := make(map[string]int)

	queue := make([]State, 0)
	queue = append(queue, NewState(startPos, t.CrossingCodes[startPos]))

	for len(queue) > 0 {
		state := queue[0]
		queue = queue[1:]

		if path, ok := visitedStates[state.String()]; ok && path >= state.DistanceTravelled {
			continue
		} else {
			visitedStates[state.String()] = state.DistanceTravelled
		}

		if state.Pos == endPos {
			finalStates = append(finalStates, state)
			continue
		}

		for _, trail := range t.Trails[state.Pos] {
			crossingCode := t.CrossingCodes[trail.To]
			if state.HasVisited(crossingCode) {
				continue
			}
			newState := state.Follow(trail, crossingCode)
			queue = append(queue, newState)
		}
	}

	maxPath := 0
	for _, state := range finalStates {
		if state.DistanceTravelled > maxPath {
			maxPath = state.DistanceTravelled
		}
	}

	return maxPath
}

func PrintResult() {
	today := utils.NewDay("day23")
	input := today.ReadInputLines()

	trailMap := ParseInput(input)

	fmt.Println(trailMap.FindLongestPath(true))
	fmt.Println("Part 2 takes some time...")
	fmt.Println(trailMap.FindLongestPath(false))
}

func ParseInput(input []string) *TrailMap {
	forests := utils.NewSet[utils.Coord]()
	slopes := make(map[utils.Coord]utils.Coord)

	for y, line := range input {
		for x, char := range line {
			pos := utils.NewCoord(x, y)

			switch char {
			case '#':
				forests.Add(pos)
			case '^':
				dir := utils.NewCoord(0, -1)
				slopes[pos] = dir
			case 'v':
				dir := utils.NewCoord(0, 1)
				slopes[pos] = dir
			case '<':
				dir := utils.NewCoord(-1, 0)
				slopes[pos] = dir
			case '>':
				dir := utils.NewCoord(1, 0)
				slopes[pos] = dir
			}
		}
	}

	return NewTrailMap(len(input[0]), len(input), forests, slopes)
}
