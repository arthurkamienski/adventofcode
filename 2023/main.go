package main

import (
	"adventofcode/2023/day01"
	"adventofcode/2023/day02"
	"adventofcode/2023/day03"
	"adventofcode/2023/day04"
	"adventofcode/2023/day05"
	"adventofcode/2023/day06"
	"adventofcode/2023/day07"
	"adventofcode/2023/day08"
	"adventofcode/2023/day09"
	"adventofcode/2023/day10"
	"adventofcode/2023/day11"
	"adventofcode/2023/day12"
	"adventofcode/2023/day13"
	"adventofcode/2023/day14"
	"adventofcode/2023/day15"
	"adventofcode/2023/day16"
	"adventofcode/2023/day17"
	"adventofcode/2023/day18"
	"adventofcode/2023/day19"
	"adventofcode/2023/day20"
	"flag"
)

func main() {
	day := flag.String("d", "", "the day to run")
	flag.Parse()

	switch *day {
	case "day01":
		day01.PrintResult()
	case "day02":
		day02.PrintResult()
	case "day03":
		day03.PrintResult()
	case "day04":
		day04.PrintResult()
	case "day05":
		day05.PrintResult()
	case "day06":
		day06.PrintResult()
	case "day07":
		day07.PrintResult()
	case "day08":
		day08.PrintResult()
	case "day09":
		day09.PrintResult()
	case "day10":
		day10.PrintResult()
	case "day11":
		day11.PrintResult()
	case "day12":
		day12.PrintResult()
	case "day13":
		day13.PrintResult()
	case "day14":
		day14.PrintResult()
	case "day15":
		day15.PrintResult()
	case "day16":
		day16.PrintResult()
	case "day17":
		day17.PrintResult()
	case "day18":
		day18.PrintResult()
	case "day19":
		day19.PrintResult()
	case "day20":
		day20.PrintResult()
	// case "day21":
	// 	day21.PrintResult()
	// case "day22":
	// 	day22.PrintResult()
	// case "day23":
	// 	day23.PrintResult()
	// case "day24":
	// 	day24.PrintResult()
	// case "day25":
	// 	day25.PrintResult()
	default:
		panic("no valid day specified")
	}
}
