package day03

import (
	"adventofcode/2023/utils"
	"fmt"
	"regexp"
	"strconv"
)

type Symbol struct {
	Char      rune
	Pos       utils.Coord
	Neighbors *utils.Set[utils.Coord]
}

func NewSymbol(char rune, pos utils.Coord) *Symbol {
	return &Symbol{
		Char:      char,
		Pos:       pos,
		Neighbors: pos.Neighbors(),
	}
}

func (s *Symbol) String() string {
	return fmt.Sprintf("%c at %s", s.Char, s.Pos)
}

type Part struct {
	Num int
	Pos *utils.Set[utils.Coord]
}

func NewPart(num string, firstX int, y int) *Part {
	coords := utils.NewSet[utils.Coord]()
	for i := range num {
		coords.Add(utils.Coord{X: firstX + i, Y: y})
	}
	numAsInt, err := strconv.Atoi(num)
	utils.Check(err)

	return &Part{
		Num: numAsInt,
		Pos: coords,
	}
}

func (p *Part) String() string {
	return fmt.Sprintf("%d at %s", p.Num, p.Pos)
}

func PrintResult() {
	today := utils.NewDay("day03")
	input := today.ReadInputLines()
	symbols, parts := parseInput(input)

	part1(symbols, parts)
	part2(symbols, parts)
}

func part1(symbols []*Symbol, parts []*Part) {
	symbolNeighbors := utils.NewSet[utils.Coord]()

	for _, symbol := range symbols {
		symbolNeighbors = symbolNeighbors.Union(symbol.Neighbors)
	}

	acc := 0

	for _, part := range parts {
		if part.Pos.Intersect(symbolNeighbors).IsNotEmpty() {
			acc += part.Num
		}
	}

	fmt.Println(acc)
}

func part2(symbols []*Symbol, parts []*Part) {
	possibleGears := make([]*Symbol, 0)

	for _, symbol := range symbols {
		if symbol.Char == '*' {
			possibleGears = append(possibleGears, symbol)
		}
	}

	gearNeighbors := make(map[*Symbol][]int)

	for _, gear := range possibleGears {
		for _, part := range parts {
			if part.Pos.Intersect(gear.Neighbors).IsNotEmpty() {
				gearNeighbors[gear] = append(gearNeighbors[gear], part.Num)
			}
		}
	}

	acc := 0

	for _, neighbs := range gearNeighbors {
		if len(neighbs) == 2 {
			acc += neighbs[0] * neighbs[1]
		}
	}

	fmt.Println(acc)
}

func parseInput(input []string) ([]*Symbol, []*Part) {
	symbols := make([]*Symbol, 0)
	parts := make([]*Part, 0)

	for y, line := range input {
		partsMatches := regexp.MustCompile("([0-9]+)").FindAllStringIndex(line, -1)

		for _, partMatch := range partsMatches {
			part := NewPart(line[partMatch[0]:partMatch[1]], partMatch[0], y)
			parts = append(parts, part)
		}

		symbolMatches := regexp.MustCompile(`([^0-9\.])`).FindAllStringIndex(line, -1)

		for _, symbolMatch := range symbolMatches {
			x := symbolMatch[0]
			symbol := line[x]

			symbols = append(symbols, NewSymbol(rune(symbol), utils.Coord{X: x, Y: y}))
		}
	}

	return symbols, parts
}
