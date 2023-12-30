package day20

import (
	"adventofcode/2023/utils"
	"fmt"
	"strings"
)

type Pulse int

const (
	LowPulse  Pulse = iota
	HighPulse Pulse = iota
	NoPulse   Pulse = iota
)

type Packet struct {
	Signal      Pulse
	Source      string
	Destination string
}

func NewPacket(signal Pulse, source string, destination string) Packet {
	return Packet{Signal: signal, Source: source, Destination: destination}
}

func (p Packet) String() string {
	if p.Signal == LowPulse {
		return fmt.Sprintf("%s -low-> %s", p.Source, p.Destination)
	} else {
		return fmt.Sprintf("%s -high-> %s", p.Source, p.Destination)
	}
}

type Module interface {
	GetName() string
	ReceivePacket(Packet) Pulse
	GetDestinations() []string
	ProcessPacket(Packet) []Packet
}

func NewModule(moduleType string, name string, destinations []string) Module {
	var module Module
	switch moduleType {
	case "%":
		module = NewFlipFlop(name, destinations)
	case "&":
		module = NewConjunction(name, destinations)
	case "broadcaster":
		module = NewBroadcaster(destinations)
	}
	return module
}

type FlipFlop struct {
	Name         string
	IsOn         bool
	Destinations []string
}

func NewFlipFlop(name string, destinations []string) *FlipFlop {
	return &FlipFlop{Name: name, IsOn: false, Destinations: destinations}
}

func (f *FlipFlop) String() string {
	return fmt.Sprintf("FlipFlop{%s; IsOn: %t; Destinations: %v}", f.Name, f.IsOn, f.Destinations)
}

func (f *FlipFlop) GetName() string {
	return f.Name
}

func (f *FlipFlop) GetDestinations() []string {
	return f.Destinations
}

func (f *FlipFlop) ProcessPacket(packet Packet) []Packet {
	return ProcessPacket(f, packet)
}

func (f *FlipFlop) ReceivePacket(packet Packet) Pulse {
	result := NoPulse
	if packet.Signal == LowPulse {
		f.IsOn = !f.IsOn
		if f.IsOn {
			result = HighPulse
		} else {
			result = LowPulse
		}
	}
	return result
}

type Conjunction struct {
	Name         string
	Memory       map[string]Pulse
	Destinations []string
}

func NewConjunction(name string, destinations []string) *Conjunction {
	return &Conjunction{Name: name, Memory: make(map[string]Pulse), Destinations: destinations}
}

func (c *Conjunction) String() string {
	return fmt.Sprintf("Conjunction{%s; Memory: %v; Destinations: %v}", c.Name, c.Memory, c.Destinations)
}

func (c *Conjunction) AddSources(sources []string) {
	for _, source := range sources {
		c.Memory[source] = LowPulse
	}
}

func (c *Conjunction) GetName() string {
	return c.Name
}

func (c *Conjunction) GetDestinations() []string {
	return c.Destinations
}

func (c *Conjunction) ProcessPacket(packet Packet) []Packet {
	return ProcessPacket(c, packet)
}

func (c *Conjunction) ReceivePacket(packet Packet) Pulse {
	c.Memory[packet.Source] = packet.Signal

	for _, signal := range c.Memory {
		if signal == LowPulse {
			return HighPulse
		}
	}

	return LowPulse
}

type Broadcaster struct {
	Destinations []string
}

func (b *Broadcaster) String() string {
	return fmt.Sprintf("Broadcaster{Destinations: %v}", b.Destinations)
}

func NewBroadcaster(destinations []string) *Broadcaster {
	return &Broadcaster{Destinations: destinations}
}

func (c *Broadcaster) GetName() string {
	return "broadcaster"
}

func (c *Broadcaster) GetDestinations() []string {
	return c.Destinations
}

func (c *Broadcaster) ProcessPacket(packet Packet) []Packet {
	return ProcessPacket(c, packet)
}

func (c *Broadcaster) ReceivePacket(packet Packet) Pulse {
	return packet.Signal
}

type Circuit struct {
	Modules       map[string]Module
	PacketQueue   []Packet
	PacketCount   map[Pulse]int
	Presses       int
	SentHighPulse map[string]bool
}

func NewCircuit(modules []Module) *Circuit {
	c := &Circuit{
		Modules:       make(map[string]Module),
		PacketQueue:   make([]Packet, 0),
		PacketCount:   make(map[Pulse]int),
		Presses:       0,
		SentHighPulse: make(map[string]bool),
	}

	for _, module := range modules {
		c.Modules[module.GetName()] = module
	}
	return c
}

func (c *Circuit) PressButton() {
	c.Presses++
	c.AddPacketToQueue(NewPacket(LowPulse, "button", "broadcaster"))

	for len(c.PacketQueue) > 0 {
		packet := c.GetNextPacket()
		if module, ok := c.Modules[packet.Destination]; ok {
			packets := module.ProcessPacket(packet)
			for _, packet := range packets {
				if packet.Signal == HighPulse {
					c.SentHighPulse[packet.Source] = true
				}
				c.AddPacketToQueue(packet)
			}
		}
	}
}

func (c *Circuit) AddPacketToQueue(packet Packet) {
	c.PacketQueue = append(c.PacketQueue, packet)
	c.PacketCount[packet.Signal]++
}

func (c *Circuit) GetNextPacket() Packet {
	packet := c.PacketQueue[0]
	c.PacketQueue = c.PacketQueue[1:]
	return packet
}

func PrintResult() {
	today := utils.NewDay("day20")
	input := today.ReadInputLines()
	circuit := ParseInput(input)

	part1(circuit)
	part2(circuit)
}

func part1(circuit *Circuit) {
	for i := 0; i < 1000; i++ {
		circuit.PressButton()
	}

	fmt.Println(circuit.PacketCount[HighPulse] * circuit.PacketCount[LowPulse])
}

func part2(circuit *Circuit) {
	rxInput := ""
	for _, module := range circuit.Modules {
		for _, destination := range module.GetDestinations() {
			if destination == "rx" {
				rxInput = module.GetName()
				break
			}
		}
	}

	rxInputInputs := circuit.Modules[rxInput].(*Conjunction).Memory
	pressesToFirstLowPulse := make(map[string]int)

	for len(pressesToFirstLowPulse) < len(rxInputInputs) && circuit.Presses < 4000 {
		circuit.PressButton()
		for input := range rxInputInputs {
			if circuit.SentHighPulse[input] {
				if _, ok := pressesToFirstLowPulse[input]; !ok {
					pressesToFirstLowPulse[input] = circuit.Presses
				}
			}
		}
	}

	lcm := 1
	for _, input := range pressesToFirstLowPulse {
		lcm = LCM(lcm, input)
	}

	fmt.Println(lcm)
}

func LCM(i1, i2 int) int {
	return i1 * i2 / GCD(i1, i2)
}

func GCD(i1, i2 int) int {
	for i2 != 0 {
		i1, i2 = i2, i1%i2
	}
	return i1
}

func ProcessPacket(m Module, packet Packet) []Packet {
	packets := make([]Packet, 0)
	result := m.ReceivePacket(packet)
	if result != NoPulse {
		for _, destination := range m.GetDestinations() {
			packets = append(packets, Packet{Signal: result, Source: m.GetName(), Destination: destination})
		}
	}
	return packets
}

func ParseInput(input []string) *Circuit {
	modules := make([]Module, 0)
	sources := make(map[string][]string)

	for _, line := range input {
		splitLine := strings.Split(line, " -> ")
		moduleType := ""
		name := ""

		if splitLine[0] == "broadcaster" {
			moduleType = splitLine[0]
		} else {
			moduleType = splitLine[0][:1]
			name = splitLine[0][1:]
		}

		destinations := strings.Split(splitLine[1], ", ")
		for _, destination := range destinations {
			if s, ok := sources[destination]; ok {
				sources[destination] = append(s, name)
			} else {
				sources[destination] = []string{name}
			}
		}

		modules = append(modules, NewModule(moduleType, name, destinations))
	}

	for _, module := range modules {
		if c, ok := module.(*Conjunction); ok {
			c.AddSources(sources[c.GetName()])
		}
	}

	return NewCircuit(modules)
}
