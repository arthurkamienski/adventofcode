package day07

import (
	"adventofcode/2023/utils"
	"fmt"
	"sort"
	"strings"
)

type HandType int

const (
	FiveOfAKind  HandType = 6
	FourOfAKind  HandType = 5
	FullHouse    HandType = 4
	ThreeOfAKind HandType = 3
	TwoPair      HandType = 2
	OnePair      HandType = 1
	HighCard     HandType = 0
)

func (h HandType) String() string {
	return [...]string{"HighCard", "OnePair", "TwoPair", "ThreeOfAKind", "FullHouse", "FourOfAKind", "FiveOfAKind"}[h]
}

type Hand struct {
	Cards            string
	Bid              int
	EncodedCards     string
	EncodedWithJoker string
	Type             HandType
	TypeWithJoker    HandType
}

func NewHand(input string) *Hand {
	splitInput := strings.Split(input, " ")
	bid := utils.MustAtoi(splitInput[1])
	cards := splitInput[0]

	return &Hand{
		Cards:            cards,
		Bid:              bid,
		EncodedCards:     encodeCards(cards),
		EncodedWithJoker: encodeCardsWithJoker(cards),
		Type:             getHandType(cards),
		TypeWithJoker:    getHandTypeWithJoker(cards),
	}
}

func (h *Hand) String() string {
	return fmt.Sprintf(
		"Hand: %s (%s):\n\tType: %s\n\tBid: %d\n\tEncodedWithJoker: %s\n\tTypeWithJoker: %s\n",
		h.Cards, h.EncodedCards, h.Type, h.Bid, h.EncodedWithJoker, h.TypeWithJoker)
}

func encode(cards string, cardToChar map[string]rune, firstNumCardChar int) string {
	encodedCards := ""

	for _, card := range cards {
		if char, ok := cardToChar[string(card)]; ok {
			encodedCards += string(char)
		} else {
			cardNum := utils.MustAtoi(string(card))
			char := string(firstNumCardChar - 9 + cardNum)
			encodedCards += char
		}
	}

	return encodedCards
}

func encodeCards(cards string) string {
	cardToChar := map[string]rune{
		"A": 'm',
		"K": 'l',
		"Q": 'k',
		"J": 'j',
		"T": 'i',
	}

	return encode(cards, cardToChar, 'h')
}

func encodeCardsWithJoker(cards string) string {
	cardToChar := map[string]rune{
		"A": 'm',
		"K": 'l',
		"Q": 'k',
		"T": 'j',
		// other nums here
		"J": 'a',
	}

	return encode(cards, cardToChar, 'i')
}

func cardCountsToHandType(cardCounts map[string]int) HandType {
	max, secondMax := 0, 0
	for _, count := range cardCounts {
		if count > max {
			secondMax = max
			max = count
		} else if count > secondMax {
			secondMax = count
		}
	}

	if max == 5 {
		return FiveOfAKind
	} else if max == 4 {
		return FourOfAKind
	} else if max == 3 && secondMax == 2 {
		return FullHouse
	} else if max == 3 {
		return ThreeOfAKind
	} else if max == 2 && secondMax == 2 {
		return TwoPair
	} else if max == 2 {
		return OnePair
	} else if max == 1 {
		return HighCard
	} else {
		panic(fmt.Sprintf("invalid card counts - max: %d, snd: %d, counts: %v", max, secondMax, cardCounts))
	}
}

func getHandType(cards string) HandType {
	cardCounts := make(map[string]int)

	for _, card := range cards {
		cardCounts[string(card)]++
	}

	return cardCountsToHandType(cardCounts)
}

func getHandTypeWithJoker(cards string) HandType {
	cardCounts := make(map[string]int)

	for _, card := range cards {
		cardCounts[string(card)]++
	}

	maxCard := ""
	for card, count := range cardCounts {
		if card != "J" && count > cardCounts[maxCard] {
			maxCard = card
		}
	}

	if jokerCount, ok := cardCounts["J"]; ok && maxCard != "" {
		cardCounts[maxCard] += jokerCount
		cardCounts["J"] = 0
	}

	return cardCountsToHandType(cardCounts)
}

func PrintResult() {
	today := utils.NewDay("day07")
	input := today.ReadInputLines()

	hands := make([]*Hand, len(input))
	for i, line := range input {
		hands[i] = NewHand(line)
	}

	part1(hands)
	part2(hands)
}

func part1(hands []*Hand) {
	sort.Slice(hands, func(i, j int) bool {
		if hands[i].Type != hands[j].Type {
			return hands[i].Type < hands[j].Type
		}
		return hands[i].EncodedCards < hands[j].EncodedCards
	})

	acc := 0
	for i := 0; i < len(hands); i++ {
		acc += hands[i].Bid * (i + 1)
	}
	fmt.Println(acc)
}

func part2(hands []*Hand) {
	sort.Slice(hands, func(i, j int) bool {
		if hands[i].TypeWithJoker != hands[j].TypeWithJoker {
			return hands[i].TypeWithJoker < hands[j].TypeWithJoker
		}
		return hands[i].EncodedWithJoker < hands[j].EncodedWithJoker
	})

	acc := 0
	for i := 0; i < len(hands); i++ {
		acc += hands[i].Bid * (i + 1)
	}

	fmt.Println(acc)
}
