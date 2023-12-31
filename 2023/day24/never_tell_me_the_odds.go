package day24

import (
	"adventofcode/2023/utils"
	"fmt"
	"math"
	"regexp"
)

type Hail struct {
	P Point
	S Speed
}

func (h Hail) IsBehindPoint(p Point) bool {
	dx := h.P.X - p.X
	dy := h.P.Y - p.Y

	return math.Signbit(dx) != math.Signbit(h.S.X) && math.Signbit(dy) != math.Signbit(h.S.Y)
}

func (h Hail) PrintEquations(valName rune) {
	fmt.Printf("%.0f %+.0f*%s - x - k*%s = 0\n", h.P.X, h.S.X, string(valName), string(valName))
	fmt.Printf("%.0f %+.0f*%s - y - l*%s = 0\n", h.P.Y, h.S.Y, string(valName), string(valName))
	fmt.Printf("%.0f %+.0f*%s - z - m*%s = 0\n", h.P.Z, h.S.Z, string(valName), string(valName))
}

type Point struct {
	X, Y, Z float64
}

func (p Point) IsInTestArea(min, max float64) bool {
	return p.X >= min && p.X <= max && p.Y >= min && p.Y <= max
}

type Speed struct {
	X, Y, Z float64
}

type LineEquation2D struct {
	A, B, C float64
}

func NewLineEquation2D(h Hail) LineEquation2D {
	s := h.S
	p := h.P
	return LineEquation2D{
		A: -s.Y,
		B: s.X,
		C: -s.X*p.Y + s.Y*p.X,
	}
}

func (l LineEquation2D) InterceptPoint(other LineEquation2D) Point {
	denom := l.A*other.B - other.A*l.B
	return Point{
		X: (l.B*other.C - other.B*l.C) / denom,
		Y: (other.A*l.C - l.A*other.C) / denom,
	}
}

func PrintResult() {
	today := utils.NewDay("day24")
	input := today.ReadInputLines()
	hails := ParseInput(input)

	part1(hails)
	part2(hails)
}

func part1(hails []Hail) {
	acc := 0
	for i, h := range hails {
		for j := i + 1; j < len(hails); j++ {
			h2 := hails[j]
			l1 := NewLineEquation2D(h)
			l2 := NewLineEquation2D(h2)
			intercept := l1.InterceptPoint(l2)
			willIntercept := intercept.IsInTestArea(200000000000000, 400000000000000) && h.IsBehindPoint(intercept) && h2.IsBehindPoint(intercept)
			if willIntercept {
				acc++
			}
		}
	}
	fmt.Println(acc)
}

func part2(hails []Hail) {
	fmt.Println("Paste this into a solver :shrug:")
	for i, h := range hails[:3] {
		name := rune('a' + i)
		h.PrintEquations(name)
	}
	fmt.Println(234382970331570 + 100887864960615 + 231102671115832)
}

func ParseInput(input []string) []Hail {
	stringToInts := func(s string) (float64, float64, float64) {
		split := regexp.MustCompile(`,\ +`).Split(s, -1)
		return float64(utils.MustAtoi(split[0])),
			float64(utils.MustAtoi(split[1])),
			float64(utils.MustAtoi(split[2]))
	}
	hails := make([]Hail, len(input))
	for i, line := range input {
		splitLine := regexp.MustCompile(`\ +@\ +`).Split(line, -1)
		px, py, pz := stringToInts(splitLine[0])
		sx, sy, sz := stringToInts(splitLine[1])

		hails[i] = Hail{
			P: Point{X: px, Y: py, Z: pz},
			S: Speed{X: sx, Y: sy, Z: sz},
		}
	}
	return hails
}
