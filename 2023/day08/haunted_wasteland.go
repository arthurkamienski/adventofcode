package day08

import (
	"adventofcode/2023/utils"
	"fmt"
	"strings"
)

type NodeMap map[string]map[string]string

func PrintResult() {
	today := utils.NewDay("day08")
	input := today.ReadInput()

	directions, nodeMap := parseInput(strings.TrimSpace(input))

	// fmt.Println(directions)
	// fmt.Println(nodeMap)
	part1(directions, nodeMap)
	part2(directions, nodeMap)
}

func parseInput(input string) (string, NodeMap) {
	splitInput := strings.Split(strings.TrimSpace(input), "\n\n")
	directions := splitInput[0]

	nodeMap := make(NodeMap)

	for _, node := range strings.Split(splitInput[1], "\n") {
		splitNode := strings.Split(node, " = ")
		nodeName := splitNode[0]

		paths := strings.Split(strings.Trim(splitNode[1], "()"), ", ")
		nodeMap[nodeName] = map[string]string{
			"L": paths[0],
			"R": paths[1],
		}
	}
	return directions, nodeMap
}

func part1(directions string, nodeMap NodeMap) {
	currNode := "AAA"
	steps := 0

	for currNode != "ZZZ" {
		for i := 0; i < len(directions) && currNode != "ZZZ"; i++ {
			currNode = nodeMap[currNode][string(directions[i])]
			steps++
		}
	}

	fmt.Println(steps)
}

func part2(directions string, nodeMap NodeMap) {
	startNodes := make([]string, 0)

	for node := range nodeMap {
		if node[len(node)-1] == 'A' {
			startNodes = append(startNodes, node)
		}
	}

	loopLengths := make([]int, 0)
	for _, startNode := range startNodes {
		loopLengths = append(loopLengths, stepsToFirstEndNode(nodeMap, startNode, directions))
		findStepsToLoop(nodeMap, startNode, directions)
	}

	fmt.Println(findLeastCommonMultiple(loopLengths))
}

func findLeastCommonMultiple(nums []int) int {
	lcm := nums[0]

	for _, num := range nums[1:] {
		lcm = findLeastCommonMultipleOfTwo(lcm, num)
	}

	return lcm
}

func findLeastCommonMultipleOfTwo(a, b int) int {
	return a * b / findGreatestCommonDivisor(a, b)
}

func findGreatestCommonDivisor(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}

	return a
}

func stepsToFirstEndNode(nodeMap NodeMap, startNode string, directions string) int {
	currNode := startNode
	steps := 0

	for currNode[len(currNode)-1] != 'Z' {
		for i := 0; i < len(directions) && currNode[len(currNode)-1] != 'Z'; i++ {
			currNode = nodeMap[currNode][string(directions[i])]
			steps++
		}
	}

	return steps
}

// This is a more general solution that finds the loop length and the first loop visit
// for part 2. After a few tinkering I found that every start node only visits one end node
// and that happens regularly after N steps, where N is the loop length.
// So instead of finding a general solution I just find the number of steps to the first end node
// for each start node and then find the least common multiple of those numbers.
func findStepsToLoop(nodeMap NodeMap, startNode string, directions string) {
	type VisitedNodeMap map[string]map[int]int

	visitedNodes := make(VisitedNodeMap)

	wasVisited := func(node string, steps int) bool {
		return visitedNodes[node][steps] != 0
	}

	addVisit := func(node string, steps int, visit int) {
		if visitedNodes[node] == nil {
			visitedNodes[node] = make(map[int]int)
		}
		visitedNodes[node][steps] = visit
	}

	endNodes := make([]string, 0)
	for node := range nodeMap {
		if node[len(node)-1] == 'Z' {
			endNodes = append(endNodes, node)
		}
	}

	currNode := startNode
	steps := 0

	i := 0

	for !wasVisited(currNode, i) || steps == 0 {
		for i = 0; i < len(directions) && !wasVisited(currNode, i) || steps == 0; i++ {
			addVisit(currNode, i, steps)
			currNode = nodeMap[currNode][string(directions[i])]
			steps++
		}
	}

	firstLoopVisit := steps
	for _, visit := range visitedNodes[currNode] {
		if visit < firstLoopVisit {
			firstLoopVisit = visit
		}
	}

	visitedEndNodes := make([]int, 0)

	for _, endNode := range endNodes {
		visits := visitedNodes[endNode]

		for _, steps := range visits {
			visitedEndNodes = append(visitedEndNodes, steps)
		}
	}

	_ = visitedEndNodes

	// fmt.Println("Loop found starting in node", startNode, "after", steps, "steps")
	// fmt.Println("First loop visit was at", firstLoopVisit, "steps in node", currNode)
	// fmt.Println("Loop length is", steps-firstLoopVisit, "steps")
	// fmt.Println("Visited end nodes at", visitedEndNodes, "steps")
	// fmt.Println("Meaning that we will revisit the end nodes after", steps-firstLoopVisit+visitedEndNodes[0], "steps")
	// fmt.Println()
}
