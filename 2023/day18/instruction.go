package day18

import (
	"adventofcode/2023/utils"
	"fmt"
)

type Instruction struct {
	Dir   utils.Coord
	Len   int
	Color string
}

func (i Instruction) ColorToInstruction() Instruction {
	hexToInt := func(hex string) int {
		var result int
		fmt.Sscanf(hex, "%x", &result)
		return result
	}

	runeToDir := func(r rune) utils.Coord {
		switch r {
		case '0':
			return utils.RightCoord()
		case '1':
			return utils.DownCoord()
		case '2':
			return utils.LeftCoord()
		case '3':
			return utils.UpCoord()
		}
		panic(fmt.Sprintf("Invalid rune: %c", r))
	}

	return Instruction{
		Dir:   runeToDir(rune(i.Color[len(i.Color)-1])),
		Len:   hexToInt(i.Color[1 : len(i.Color)-1]),
		Color: i.Color,
	}
}
