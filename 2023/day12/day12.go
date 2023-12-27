package day12

import (
	"adventofcode/2023/utils"
	"fmt"
	"strings"
)

type SpringRow struct {
	def              string
	length           int
	knownSprings     map[int]bool
	contiguousGroups []int
}

func NewSpringRow(def string, contiguousGroups []int) *SpringRow {
	knownSprings := make(map[int]bool)
	for i, char := range def {
		if char != '?' {
			knownSprings[i] = char == '.'
		}
	}

	return &SpringRow{
		def:              def,
		length:           len(def),
		knownSprings:     knownSprings,
		contiguousGroups: contiguousGroups,
	}
}

func (springRow *SpringRow) FindPossibleArrangements() int {
	nextStates := NewStartState(springRow).NextStates()
	stateCount := make(map[State]int)

	for _, state := range nextStates.Values() {
		stateCount[state] = 1
	}

	for i := 1; i < springRow.length; i++ {
		newNextStates := utils.NewSet[State]()
		for _, state := range nextStates.Values() {
			newStates := state.NextStates()
			for _, newState := range newStates.Values() {
				stateCount[newState] += stateCount[state]
				newNextStates.Add(newState)
			}
		}
		nextStates = newNextStates
	}

	acc := 0
	for _, state := range nextStates.Values() {
		if state.IsValid() {
			acc += stateCount[state]
		}
	}

	return acc
}

func (s *SpringRow) Unfold() *SpringRow {
	def := s.def
	contiguousGroups := append(make([]int, 0), s.contiguousGroups...)

	for i := 0; i < 4; i++ {
		def += "?" + s.def
		contiguousGroups = append(contiguousGroups, s.contiguousGroups...)
	}

	return NewSpringRow(def, contiguousGroups)
}

func (s *SpringRow) String() string {
	return fmt.Sprintf("SpringRow{def: %s, contiguousGroups: %v}", s.def, s.contiguousGroups)
}

type State struct {
	pos                  int
	currContinuousLength int
	continuousGroupIndex int
	springRow            *SpringRow
}

func NewStartState(springRow *SpringRow) State {
	return State{
		pos:                  0,
		currContinuousLength: 0,
		continuousGroupIndex: 0,
		springRow:            springRow,
	}
}

func (s State) PossibleValues() *utils.Set[bool] {
	possibilities := utils.NewSet[bool]()

	if s.continuousGroupIndex < len(s.springRow.contiguousGroups) && s.pos < s.springRow.length {
		currContinuousGroup := s.springRow.contiguousGroups[s.continuousGroupIndex]

		canAddWorkingSpring := s.currContinuousLength == 0 || s.currContinuousLength == currContinuousGroup
		canAddBrokenSpring := s.currContinuousLength+1 <= currContinuousGroup

		if canAddWorkingSpring {
			possibilities.Add(true)
		}
		if canAddBrokenSpring {
			possibilities.Add(false)
		}
	} else {
		possibilities.Add(true)
	}

	return possibilities
}

func (s State) NextStates() *utils.Set[State] {
	possibilities := s.PossibleValues()

	if isWorking, ok := s.springRow.knownSprings[s.pos]; ok {
		newPoss := utils.NewSet[bool]()
		newPoss.Add(isWorking)
		possibilities = possibilities.Intersect(newPoss)
	}

	nextStates := utils.NewSet[State]()

	for _, isWorking := range possibilities.Values() {
		if !isWorking {
			nextStates.Add(State{
				pos:                  s.pos + 1,
				currContinuousLength: s.currContinuousLength + 1,
				continuousGroupIndex: s.continuousGroupIndex,
				springRow:            s.springRow,
			})
		} else {
			continuousGroupIndex := s.continuousGroupIndex

			if s.currContinuousLength != 0 {
				continuousGroupIndex++
			}

			nextStates.Add(State{
				pos:                  s.pos + 1,
				currContinuousLength: 0,
				continuousGroupIndex: continuousGroupIndex,
				springRow:            s.springRow,
			})
		}
	}

	return nextStates
}

func (s State) IsValid() bool {
	allGroupsCounted := s.continuousGroupIndex == len(s.springRow.contiguousGroups) && s.currContinuousLength == 0
	currGroupCounted := s.continuousGroupIndex == len(s.springRow.contiguousGroups)-1 && s.currContinuousLength == s.springRow.contiguousGroups[s.continuousGroupIndex]
	return allGroupsCounted || currGroupCounted
}

func (s State) String() string {
	return fmt.Sprintf("State{pos: %d, currContinuousLength: %d, continuousGroupIndex: %d}", s.pos, s.currContinuousLength, s.continuousGroupIndex)
}

func PrintResult() {
	today := utils.NewDay("day12")
	input := today.ReadInputLines()
	springRows := parseInput(input)

	part1(springRows)
	part2(springRows)
}

func part1(springRows []*SpringRow) {
	acc := 0
	for _, springRow := range springRows {
		acc += springRow.FindPossibleArrangements()
	}

	fmt.Println(acc)
}

func part2(springRows []*SpringRow) {
	unfoldedSpringRows := make([]*SpringRow, 0)
	for _, springRow := range springRows {
		unfoldedSpringRows = append(unfoldedSpringRows, springRow.Unfold())
	}

	acc := 0
	for _, springRow := range unfoldedSpringRows {
		acc += springRow.FindPossibleArrangements()
	}

	fmt.Println(acc)
}

func parseInput(input []string) []*SpringRow {
	springRows := make([]*SpringRow, 0)
	for _, line := range input {
		splitLine := strings.Split(line, " ")

		contiguousGroups := make([]int, 0)
		for _, group := range strings.Split(splitLine[1], ",") {
			contiguousGroups = append(contiguousGroups, utils.MustAtoi(group))
		}

		springRows = append(springRows, NewSpringRow(splitLine[0], contiguousGroups))
	}

	return springRows
}
