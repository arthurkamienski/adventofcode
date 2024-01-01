package day25

import (
	"adventofcode/2023/utils"
	"fmt"
	"strings"
)

type Graph struct {
	Nodes *utils.Set[string]
	Edges map[string]*utils.Set[string]
}

func (g *Graph) ContractEdge(n1, n2 string, edgeCount map[string]map[string]int) map[string]map[string]int {
	newNode := n1 + ";" + n2
	g.Edges[newNode] = utils.NewSet[string]()
	edgeCount[newNode] = make(map[string]int)

	for _, n := range g.Edges[n2].Values() {
		g.Edges[n].Remove(n2)
		g.Edges[n].Remove(n1)
		g.Edges[n].Add(newNode)
		g.Edges[newNode].Add(n)

		edgeCount[newNode][n] = edgeCount[n][n2] + edgeCount[n][n1]
		edgeCount[n][newNode] = edgeCount[newNode][n]
	}

	for _, n := range g.Edges[n1].Values() {
		g.Edges[n].Remove(n1)
		g.Edges[n].Remove(n2)
		g.Edges[n].Add(newNode)

		g.Edges[newNode].Add(n)
		edgeCount[newNode][n] = edgeCount[n][n2] + edgeCount[n][n1]
		edgeCount[n][newNode] = edgeCount[newNode][n]
	}

	g.Edges[newNode].Remove(n1)
	g.Edges[newNode].Remove(n2)
	g.Edges[newNode].Remove(newNode)

	delete(g.Edges, n1)
	delete(g.Edges, n2)

	g.Nodes.Remove(n1)
	g.Nodes.Remove(n2)
	g.Nodes.Add(newNode)

	return edgeCount
}

func (g *Graph) Contract() map[string]map[string]int {
	edgeCount := make(map[string]map[string]int)

	for _, node := range g.Nodes.Values() {
		edgeCount[node] = make(map[string]int)
		for _, edge := range g.Edges[node].Values() {
			edgeCount[node][edge] = 1
		}
	}

	for g.Nodes.Size() > 2 {
		n1 := g.Nodes.Take(1)[0]

		var n2 string
		for _, n := range g.Edges[n1].Values() {
			if edgeCount[n1][n2] != 3 {
				n2 = n
				break
			}
		}

		edgeCount = g.ContractEdge(n1, n2, edgeCount)
	}
	return edgeCount
}

func FindMinCut(input []string) int {
	for {
		graph := ParseInput(input)

		edgeCount := graph.Contract()
		nodes := graph.Nodes.Values()

		if edgeCount[nodes[0]][nodes[1]] == 3 {
			node1 := strings.Split(nodes[0], ";")
			node2 := strings.Split(nodes[1], ";")
			return len(node1) * len(node2)
		}
	}
}

func PrintResult() {
	today := utils.NewDay("day25")
	input := today.ReadInputLines()

	fmt.Println(FindMinCut(input))
}

func ParseInput(input []string) *Graph {
	nodes := utils.NewSet[string]()
	edges := make(map[string]*utils.Set[string])
	for _, line := range input {
		splitLine := strings.Split(line, ": ")

		nodes.Add(splitLine[0])

		for _, edge := range strings.Split(splitLine[1], " ") {
			if _, ok := edges[splitLine[0]]; !ok {
				edges[splitLine[0]] = utils.NewSet[string]()
			}
			if _, ok := edges[edge]; !ok {
				edges[edge] = utils.NewSet[string]()
			}
			nodes.Add(edge)
			edges[splitLine[0]].Add(edge)
			edges[edge].Add(splitLine[0])
		}
	}

	return &Graph{
		Nodes: nodes,
		Edges: edges,
	}
}
