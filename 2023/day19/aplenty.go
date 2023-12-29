package day19

import (
	"adventofcode/2023/utils"
	"fmt"
	"regexp"
	"strings"
)

type Part struct {
	Index  int
	Fields map[string]int
}

func (p Part) Value() int {
	acc := 0
	for _, field := range p.Fields {
		acc += field
	}
	return acc
}

type PartSorter interface {
	GetResult(Part) string
	Result() string
}

type ComparisonRule struct {
	PartField string
	CompStr   string
	Value     int
	result    string
}

func (c ComparisonRule) GetResult(p Part) string {
	switch c.CompStr {
	case ">":
		if p.Fields[c.PartField] > c.Value {
			return c.result
		}
	case "<":
		if p.Fields[c.PartField] < c.Value {
			return c.result
		}
	}
	return ""
}

func (c ComparisonRule) Negate() ComparisonRule {
	var compStr string
	value := c.Value
	switch c.CompStr {
	case ">":
		compStr = "<"
		value++
	case "<":
		compStr = ">"
		value--
	}

	return ComparisonRule{c.PartField, compStr, value, c.result}
}

func (c ComparisonRule) Result() string {
	return c.result
}

func (c ComparisonRule) String() string {
	return fmt.Sprintf("Comparison(%s %s %d -> %s)", c.PartField, c.CompStr, c.Value, c.result)
}

type ConstantRule struct {
	result string
}

func (c ConstantRule) String() string {
	return fmt.Sprintf("Constant(%s)", c.result)
}

func (c ConstantRule) GetResult(p Part) string {
	return c.result
}

func (c ConstantRule) Result() string {
	return c.result
}

func NewComparisonRule(field string, compStr string, value int, result string) ComparisonRule {
	return ComparisonRule{field, compStr, value, result}
}

func NewConstantRule(result string) ConstantRule {
	return ConstantRule{result}
}

type Workflow struct {
	Name            string
	Rules           []PartSorter
	PossibleResults *utils.Set[string]
}

func NewWorkflow(name string, rules []PartSorter) Workflow {
	possibleResults := utils.NewSet[string]()
	for _, rule := range rules {
		possibleResults.Add(rule.Result())
	}

	return Workflow{name, rules, possibleResults}
}

func (w Workflow) GetResult(p Part) string {
	for _, rule := range w.Rules {
		if result := rule.GetResult(p); result != "" {
			return result
		}
	}
	panic("No result found")
}

func (w Workflow) RulesToGetResult(r string) []*utils.Set[ComparisonRule] {
	allRules := make([]*utils.Set[ComparisonRule], 0)
	if w.PossibleResults.Contains(r) {
		rules := utils.NewSet[ComparisonRule]()
		for i := 0; i < len(w.Rules); i++ {
			rule := w.Rules[i]
			switch rule := rule.(type) {
			case ComparisonRule:
				if rule.Result() == r {
					oldRules := rules
					rules = rules.Copy()
					oldRules.Add(rule)
					allRules = append(allRules, oldRules)
				}
				rules.Add(rule.Negate())

			case ConstantRule:
				if rule.Result() == r {
					allRules = append(allRules, rules)
				}
			}
		}
	}
	return allRules
}

type RuleBranch struct {
	CurrentWorkflow string
	PreviousRules   *utils.Set[ComparisonRule]
}

func (r RuleBranch) Collapse() map[string]map[string]int {
	bounds := map[string]map[string]int{
		"x": {"min": 1, "max": 4000},
		"m": {"min": 1, "max": 4000},
		"a": {"min": 1, "max": 4000},
		"s": {"min": 1, "max": 4000},
	}

	for _, rule := range r.PreviousRules.Values() {
		switch rule.CompStr {
		case ">":
			bounds[rule.PartField]["min"] = utils.Max(bounds[rule.PartField]["min"], rule.Value+1)
		case "<":
			bounds[rule.PartField]["max"] = utils.Min(bounds[rule.PartField]["max"], rule.Value-1)
		}
	}

	return bounds
}

func (r RuleBranch) GetNextBranches(workflows map[string]Workflow) []RuleBranch {
	branches := make([]RuleBranch, 0)
	currWorkflow := workflows[r.CurrentWorkflow]

	for _, result := range currWorkflow.PossibleResults.Values() {
		if result != "R" {
			rules := currWorkflow.RulesToGetResult(result)

			for _, rule := range rules {
				branches = append(branches, RuleBranch{
					PreviousRules:   r.PreviousRules.Union(rule),
					CurrentWorkflow: result,
				})
			}
		}
	}

	return branches
}

func PrintResult() {
	today := utils.NewDay("day19")
	input := strings.TrimSpace(today.ReadInput())
	workflows, parts := ParseInput(input)

	part1(parts, workflows)
	part2(workflows)
}

func ParseInput(input string) (map[string]Workflow, []Part) {
	splitInput := strings.Split(input, "\n\n")
	workflowsInput := splitInput[0]
	partsInput := splitInput[1]

	workflows := make(map[string]Workflow)
	for _, line := range strings.Split(workflowsInput, "\n") {
		splitWorkflow := regexp.MustCompile(`(.+)\{(.+?)\}`).FindStringSubmatch(line)
		name := splitWorkflow[1]
		rules := splitWorkflow[2]

		workflowRules := make([]PartSorter, 0)
		for _, rule := range strings.Split(rules, ",") {
			if !strings.Contains(rule, ":") {
				workflowRules = append(workflowRules, NewConstantRule(rule))
			} else {
				fields := regexp.MustCompile(`(.+)([><])(\d+):(.+)`).FindStringSubmatch(rule)
				field := fields[1]
				comp := fields[2]
				value := utils.MustAtoi(fields[3])
				result := fields[4]
				workflowRules = append(workflowRules, NewComparisonRule(field, comp, value, result))
			}
		}

		workflows[name] = NewWorkflow(name, workflowRules)
	}

	parts := make([]Part, 0)
	for i, line := range strings.Split(partsInput, "\n") {
		fields := regexp.MustCompile(`\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}`).FindStringSubmatch(line)
		parts = append(parts, Part{
			Index: i,
			Fields: map[string]int{
				"x": utils.MustAtoi(fields[1]),
				"m": utils.MustAtoi(fields[2]),
				"a": utils.MustAtoi(fields[3]),
				"s": utils.MustAtoi(fields[4]),
			},
		})
	}

	return workflows, parts
}
func part1(parts []Part, workflows map[string]Workflow) {
	newPartWorkflows := make(map[int]string)
	for _, part := range parts {
		newPartWorkflows[part.Index] = "in"
	}

	sortedParts := make(map[int]string)
	for len(newPartWorkflows) > 0 {
		partWorkflows := newPartWorkflows
		newPartWorkflows = make(map[int]string)
		for partIdx, workflow := range partWorkflows {
			result := workflows[workflow].GetResult(parts[partIdx])

			if result == "A" || result == "R" {
				sortedParts[partIdx] = result
			} else if result != "" {
				newPartWorkflows[partIdx] = result
			}
		}
	}

	acc := 0
	for partIdx, result := range sortedParts {
		if result == "A" {
			acc += parts[partIdx].Value()
		}
	}
	fmt.Println(acc)
}

func part2(workflows map[string]Workflow) {
	branch := RuleBranch{
		PreviousRules:   utils.NewSet[ComparisonRule](),
		CurrentWorkflow: "in",
	}

	newBranches := []RuleBranch{branch}
	finalBranches := []RuleBranch{}

	for len(newBranches) > 0 {
		prevBranches := newBranches
		newBranches = []RuleBranch{}
		for _, branch := range prevBranches {
			branches := branch.GetNextBranches(workflows)
			for _, newBranch := range branches {
				if newBranch.CurrentWorkflow == "A" {
					finalBranches = append(finalBranches, newBranch)
				} else {
					newBranches = append(newBranches, newBranch)
				}
			}
		}
	}

	sum := 0
	for _, branch := range finalBranches {
		acc := 1
		for _, result := range branch.Collapse() {
			acc *= result["max"] - result["min"] + 1
		}
		sum += acc
	}

	fmt.Println(sum)
}
