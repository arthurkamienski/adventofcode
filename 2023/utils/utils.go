package utils

import (
	"os"
	"strings"
)

type day struct {
	Name string
	Dir  string
}

func NewDay(name string) *day {
	return &day{
		Name: name,
		Dir:  "/Users/arthur.kamienski/Personal/adventofcode/2023/",
	}
}

func (d *day) ReadFile(fileName string) string {
	fullFileName := d.Dir + d.Name + "/" + fileName
	dat, err := os.ReadFile(fullFileName)

	if err != nil {
		panic(err)
	}

	return string(dat)
}

func (d *day) toLines(input string) []string {
	return strings.Split(strings.TrimSpace(input), "\n")
}

func (d *day) ReadFileLines(fileName string) []string {
	return d.toLines(d.ReadFile(fileName))
}

func (d *day) ReadInput() string {
	return d.ReadFile("input.txt")
}

func (d *day) ReadInputLines() []string {
	return d.toLines(d.ReadInput())
}

func (d *day) ReadTestInput() string {
	return d.ReadFile("test_input.txt")
}

func (d *day) ReadTestInputLines() []string {
	return d.toLines(d.ReadTestInput())
}

func Check(err error) {
	if err != nil {
		panic(err)
	}
}
