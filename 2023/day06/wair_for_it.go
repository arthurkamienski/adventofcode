package day06

import (
	"adventofcode/2023/utils"
	"fmt"
	"math"
	"regexp"
	"strings"
)

type RaceDefinition struct {
	Time   int
	Record int
}

func (r *RaceDefinition) FindRoots() []int {
	// eq = t^2 - t*raceTime + record = 0
	// t = (raceTime +- sqrt(raceTime^2 - 4*record)) / 2

	discriminant := r.Time*r.Time - 4*r.Record

	if discriminant < 0 {
		return []int{}
	}

	r0 := (float64(r.Time) - math.Sqrt(float64(discriminant))) / 2
	r1 := (float64(r.Time) + math.Sqrt(float64(discriminant))) / 2

	return []int{int(math.Ceil(r0)), int(math.Floor(r1))}
}

func (r *RaceDefinition) WaysToWin() int {
	roots := r.FindRoots()

	if len(roots) == 0 {
		return 0
	}

	return roots[1] - roots[0] + 1
}

func PrintResult() {
	today := utils.NewDay("day06")
	input := today.ReadInputLines()

	part1(parseManyRaces(input))
	part2(parseOneRace(input))
}

func parseManyRaces(input []string) []*RaceDefinition {
	raceTimes := regexp.MustCompile(`\ +`).Split(input[0], -1)
	records := regexp.MustCompile(`\ +`).Split(input[1], -1)

	raceDefinitions := make([]*RaceDefinition, len(raceTimes)-1)

	for i := 1; i < len(raceTimes); i++ {
		raceDefinitions[i-1] = &RaceDefinition{
			Time:   utils.MustAtoi(raceTimes[i]),
			Record: utils.MustAtoi(records[i]),
		}
	}

	return raceDefinitions
}

func parseOneRace(input []string) *RaceDefinition {
	raceTime := strings.Split(regexp.MustCompile(`\ +`).ReplaceAllString(input[0], ""), ":")[1]
	record := strings.Split(regexp.MustCompile(`\ +`).ReplaceAllString(input[1], ""), ":")[1]

	return &RaceDefinition{
		Time:   utils.MustAtoi(raceTime),
		Record: utils.MustAtoi(record),
	}
}

func part1(races []*RaceDefinition) {
	waysToWin := 1

	for _, race := range races {
		waysToWin *= race.WaysToWin()
	}

	fmt.Println(waysToWin)
}

func part2(race *RaceDefinition) {
	fmt.Println(race.WaysToWin())
}
