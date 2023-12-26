package day05

import (
	"adventofcode/2023/utils"
	"fmt"
	"slices"
	"strconv"
	"strings"
)

const MaxInt = int(^uint(0) >> 1)

type ConversionRule struct {
	Start int
	End   int
	Shift int
}

func NewConversionRule(dest int, source int, length int) *ConversionRule {
	return &ConversionRule{
		Start: source,
		End:   source + length - 1,
		Shift: dest - source,
	}
}

func (r *ConversionRule) String() string {
	return fmt.Sprintf("%d-%d -> %d", r.Start, r.End, r.Shift)
}

func (r *ConversionRule) Contains(num int) bool {
	return num >= r.Start && num <= r.End
}

func (r *ConversionRule) GetShiftFor(num int) int {
	if r.Contains(num) {
		return r.Shift
	}

	return 0
}

func (r *ConversionRule) Apply(num int) int {
	return num + r.GetShiftFor(num)
}

type ConversionMap struct {
	Rules []*ConversionRule
}

func (m *ConversionMap) String() string {
	s := "ConversionMap[\n"
	for _, rule := range m.Rules {
		s += fmt.Sprintf("\t%s\n", rule)
	}
	return s + "]"
}

func (m *ConversionMap) Contains(num int) bool {
	for _, rule := range m.Rules {
		if rule.Contains(num) {
			return true
		}
	}

	return false
}

func (m *ConversionMap) Apply(num int) int {
	convertedNum := num
	for i := 0; i < len(m.Rules) && convertedNum == num; i++ {
		convertedNum = m.Rules[i].Apply(num)
	}

	return convertedNum
}

func PrintResult() {
	today := utils.NewDay("day05")
	input := today.ReadInput()
	seeds, maps := parseInput(strings.TrimSpace(input))

	finalMap := combineMaps(maps[0], maps[1])

	for i := 2; i < len(maps); i++ {
		finalMap = combineMaps(finalMap, maps[i])
	}

	part1(seeds, finalMap)
	part2(seeds, finalMap)
}

func parseInput(input string) ([]int, []*ConversionMap) {
	chunks := strings.Split(input, "\n\n")
	seedsStr := strings.Split(chunks[0], ": ")[1]

	seeds := make([]int, 0)
	for _, chunk := range strings.Split(seedsStr, " ") {
		num, err := strconv.Atoi(chunk)
		utils.Check(err)
		seeds = append(seeds, num)
	}

	conversionMaps := make([]*ConversionMap, len(chunks[1:]))

	for i, chunk := range chunks[1:] {
		lines := strings.Split(chunk, "\n")[1:]
		rules := make([]*ConversionRule, len(lines))

		for j, line := range lines {
			splitLine := strings.Split(line, " ")
			dest, err := strconv.Atoi(splitLine[0])
			utils.Check(err)
			source, err := strconv.Atoi(splitLine[1])
			utils.Check(err)
			length, err := strconv.Atoi(splitLine[2])
			utils.Check(err)

			rules[j] = NewConversionRule(dest, source, length)
		}

		conversionMaps[i] = &ConversionMap{
			Rules: rules,
		}
	}

	return seeds, conversionMaps
}

func part1(seeds []int, conversionMap *ConversionMap) {
	minLocation := MaxInt
	for _, seed := range seeds {
		seed = conversionMap.Apply(seed)

		if seed < minLocation {
			minLocation = seed
		}
	}

	fmt.Println(minLocation)
}

func combineMaps(preMap, postMap *ConversionMap) *ConversionMap {
	adjustedPreRules := make([]*ConversionRule, 0)

	for _, rule := range preMap.Rules {
		adjustedPreRules = append(adjustedPreRules, &ConversionRule{
			Start: rule.Start + rule.Shift,
			End:   rule.End + rule.Shift,
			Shift: rule.Shift,
		})
	}

	allRules := append(adjustedPreRules, postMap.Rules...)

	boundaries := make(map[int]int)

	for _, rule := range allRules {
		boundaries[rule.Start] = 0
		boundaries[rule.End+1] = 0
	}

	for boundary := range boundaries {
		for _, rule := range allRules {
			boundaries[boundary] += rule.GetShiftFor(boundary)
		}
	}

	finalRuleBoundaries := make(map[int]int)

	for boundary, shift := range boundaries {
		convertedNum := boundary
		for i := 0; i < len(adjustedPreRules) && convertedNum == boundary; i++ {
			convertedNum = boundary - adjustedPreRules[i].GetShiftFor(boundary)
		}
		finalRuleBoundaries[convertedNum] = shift
	}

	keys := make([]int, 0)
	for key := range finalRuleBoundaries {
		keys = append(keys, key)
	}
	slices.Sort(keys)

	finalRules := make([]*ConversionRule, 0)

	for i := 0; i < len(keys)-1; i++ {
		finalRules = append(finalRules, &ConversionRule{
			Start: keys[i],
			End:   keys[i+1] - 1,
			Shift: finalRuleBoundaries[keys[i]],
		})
	}

	return &ConversionMap{Rules: finalRules}
}

func part2(seeds []int, conversionMap *ConversionMap) {
	seedRanges := make([]*ConversionRule, len(seeds)/2)

	for i := 0; i < len(seeds)-1; i += 2 {
		seedRanges[i/2] = &ConversionRule{
			Start: seeds[i],
			End:   seeds[i] + seeds[i+1] - 1,
			Shift: 0,
		}
	}

	seedMap := &ConversionMap{Rules: seedRanges}

	finalMap := combineMaps(seedMap, conversionMap)
	finalRules := make([]*ConversionRule, 0)

	for _, rule := range finalMap.Rules {
		if seedMap.Contains(rule.Start) && seedMap.Contains(rule.End) {
			finalRules = append(finalRules, rule)
		}
	}

	minLocation := MaxInt

	for _, rule := range finalRules {
		if rule.Start+rule.Shift < minLocation {
			minLocation = rule.Start + rule.Shift
		}
	}

	fmt.Println(minLocation)

}
