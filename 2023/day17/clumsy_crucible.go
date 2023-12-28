package day17

import (
	"adventofcode/2023/utils"
	"container/heap"
	"fmt"
)

type HeatLossMap struct {
	HeatLossMap map[utils.Coord]int
	MaxX, MaxY  int
	Dest        utils.Coord
	MinHeatLoss map[string]int
}

func (hm *HeatLossMap) Reset() {
	hm.MinHeatLoss = make(map[string]int)
}

func (hm *HeatLossMap) FindBestRoute(maxContinuousStreak, minContinuousStreak int) int {
	startState := NewStartState(utils.Coord{X: 0, Y: 0}, utils.RightCoord(), hm.Dest)
	pq := make(PriorityQueue, 0)

	heap.Init(&pq)

	heap.Push(&pq, startState)

	for pq[0].Pos != hm.Dest || pq[0].ContinuousStreak < minContinuousStreak {
		hm.Iterate(&pq, maxContinuousStreak, minContinuousStreak)
	}

	return pq[0].HeatLoss
}

func (hm *HeatLossMap) Iterate(pq *PriorityQueue, maxContinuousStreak, minContinuousStreak int) {
	nextState := heap.Pop(pq).(*State)
	nextStates := nextState.NextStates(hm, maxContinuousStreak, minContinuousStreak)
	for _, state := range nextStates {
		stateHash := state.Hash()
		minHeatLoss := hm.MinHeatLoss[stateHash]
		if minHeatLoss == 0 || state.HeatLoss < minHeatLoss {
			hm.MinHeatLoss[stateHash] = state.HeatLoss
			heap.Push(pq, state)
		}
	}
}

type State struct {
	Pos utils.Coord
	Dir utils.Coord
	ContinuousStreak,
	HeatLoss,
	Priority,
	DistToDest,
	Index int
}

func (s *State) String() string {
	var dir string
	switch s.Dir {
	case utils.Coord{X: 0, Y: -1}:
		dir = "up"
	case utils.Coord{X: 0, Y: 1}:
		dir = "down"
	case utils.Coord{X: -1, Y: 0}:
		dir = "left"
	case utils.Coord{X: 1, Y: 0}:
		dir = "right"
	}

	return fmt.Sprintf("State{Pos: %s (%d), Dir: %s, Streak: %d, HeatLoss: %d, Priority: %d}", s.Pos, s.DistToDest, dir, s.ContinuousStreak, s.HeatLoss, s.Priority)
}

func (s *State) Hash() string {
	return fmt.Sprintf("%s,%s,%d", s.Pos, s.Dir, s.ContinuousStreak)
}

func NewStartState(pos utils.Coord, dir utils.Coord, dest utils.Coord) *State {
	return NewState(pos, dir, pos.ManhattanDistance(dest), 0, 0)
}

func NewState(pos utils.Coord, dir utils.Coord, distToDest int, continuousStreak int, heatLoss int) *State {
	s := &State{
		Pos:              pos,
		Dir:              dir,
		ContinuousStreak: continuousStreak,
		HeatLoss:         heatLoss,
		DistToDest:       distToDest,
		Priority:         -distToDest - heatLoss,
		Index:            -1,
	}
	return s
}

func (s *State) NextStates(heatLossMap *HeatLossMap, maxContinuousStreak, minContinuousStreak int) []*State {
	var nextStates []*State
	for _, dir := range []utils.Coord{
		utils.DownCoord(),
		utils.UpCoord(),
		utils.LeftCoord(),
		utils.RightCoord(),
	} {
		canContinueForward := dir == s.Dir && s.ContinuousStreak+1 <= maxContinuousStreak
		canTurn := dir != s.Dir && s.ContinuousStreak >= minContinuousStreak
		isNotBackwards := dir != s.Dir.Negate()

		isValidDir := (canContinueForward || canTurn) && isNotBackwards

		if isValidDir {
			nextPos := s.Pos.Add(dir)

			continuousStreak := s.ContinuousStreak + 1

			if dir != s.Dir {
				continuousStreak = 1
			}

			isInBounds := nextPos.XBetween(0, heatLossMap.MaxX-1) && nextPos.YBetween(0, heatLossMap.MaxY-1)

			if isInBounds {
				newState := NewState(nextPos, dir, nextPos.ManhattanDistance(heatLossMap.Dest), continuousStreak, s.HeatLoss+heatLossMap.HeatLossMap[nextPos])
				nextStates = append(nextStates, newState)
			}
		}
	}
	return nextStates
}

type PriorityQueue []*State

func (pq PriorityQueue) Len() int { return len(pq) }
func (pq PriorityQueue) Less(i, j int) bool {
	// We want Pop to give us the highest, not lowest, priority so we use greater than here.
	return pq[i].Priority > pq[j].Priority
}
func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].Index = i
	pq[j].Index = j
}
func (pq *PriorityQueue) Push(x any) {
	n := len(*pq)
	item := x.(*State)
	item.Index = n
	*pq = append(*pq, item)
}
func (pq *PriorityQueue) Pop() any {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil  // avoid memory leak
	item.Index = -1 // for safety
	*pq = old[0 : n-1]
	return item
}

func PrintResult() {
	today := utils.NewDay("day17")
	input := today.ReadInputLines()
	heatLossMap := ParseInput(input)

	fmt.Println(heatLossMap.FindBestRoute(3, 0))
	heatLossMap.Reset()
	fmt.Println(heatLossMap.FindBestRoute(10, 4))
}

func ParseInput(input []string) *HeatLossMap {
	heatLossMap := make(map[utils.Coord]int)
	for y, line := range input {
		for x, char := range line {
			heatLossMap[utils.Coord{X: x, Y: y}] = int(char - '0')
		}
	}

	maxX := len(input[0])
	maxY := len(input)

	return &HeatLossMap{
		HeatLossMap: heatLossMap,
		MaxX:        maxX,
		MaxY:        maxY,
		Dest:        utils.Coord{X: maxX - 1, Y: maxY - 1},
		MinHeatLoss: make(map[string]int),
	}
}
