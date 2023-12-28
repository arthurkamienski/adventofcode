package day15

import (
	"adventofcode/2023/utils"
	"fmt"
	"sort"
	"strings"
)

type Lens struct {
	Label       string
	FocalLength int
	Position    int
}

type Box struct {
	Lenses  map[string]*Lens
	NLenses int
}

func (b *Box) AddLens(l *Lens) {
	if lens, ok := b.Lenses[l.Label]; ok {
		lens.FocalLength = l.FocalLength
	} else {
		l.Position = b.NLenses
		b.Lenses[l.Label] = l
		b.NLenses++
	}
}

func (b *Box) RemoveLens(label string) {
	delete(b.Lenses, label)
}

func HASH(s string) int {
	currValue := 0
	for _, c := range s {
		currValue = (currValue + int(c)) * 17 % 256
	}
	return currValue
}

func PrintResult() {
	today := utils.NewDay("day15")
	input := strings.Split(strings.TrimSpace(today.ReadInput()), ",")

	part1(input)
	part2(input)

}

func part2(steps []string) {
	boxes := make(map[int]*Box)

	for i := 0; i < 256; i++ {
		boxes[i] = &Box{
			Lenses:  make(map[string]*Lens),
			NLenses: 0,
		}
	}

	for _, step := range steps {
		splitStep := strings.Split(step, "=")

		if len(splitStep) == 2 {
			label := splitStep[0]
			focalLength := utils.MustAtoi(splitStep[1])

			lens := &Lens{
				Label:       label,
				FocalLength: focalLength,
			}

			boxes[HASH(label)].AddLens(lens)
		} else {
			label := step[:len(step)-1]
			boxes[HASH(label)].RemoveLens(label)
		}
	}

	acc := 0
	for i, box := range boxes {
		lenses := make([]*Lens, 0)

		for _, lens := range box.Lenses {
			lenses = append(lenses, lens)
		}

		sort.Slice(lenses, func(i, j int) bool {
			return lenses[i].Position < lenses[j].Position
		})

		for j, lens := range lenses {
			acc += (i + 1) * (j + 1) * lens.FocalLength
		}
	}

	fmt.Println(acc)
}

func part1(steps []string) {
	acc := 0

	for _, steps := range steps {
		acc += HASH(steps)
	}

	fmt.Println(acc)
}
