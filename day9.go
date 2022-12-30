// go build day9.go && ./day9
package main

import (
	"fmt"
	"strings"
	"strconv"
	"io/ioutil"
)

// Little functional helper, probably not very idiomatic Go
func reduce[T, M any](s []T, f func(M, T) M, initValue M) M {
	acc := initValue

	for _, v := range s {
		acc = f(acc, v)
	}

	return acc
}

// Implementation

const sampleInput = `
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
`

const sampleInput2 = `
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
`

type Position struct {
	x int
	y int
}

type Direction int

const (
	Up Direction = iota
	Down
	Left
	Right
)

func parseDirection(candidate string) Direction {
	switch candidate {
	case "U": return Up
	case "D": return Down
	case "L": return Left
	case "R": return Right
	default: return Up
	}
}

type Instruction struct {
	direction Direction
	steps int
}

func readInput(useFile bool) string {
	if useFile == true {
		content, err := ioutil.ReadFile("day9-input.txt")
		if err != nil {
			fmt.Println("Failed to read file!")
			return ""
		}

		return string(content)
	} else {
		return sampleInput
	}
}

func parseInstructions(rawInput string) []Instruction {
	lines := strings.Split(strings.TrimSpace(rawInput), "\n")

	initAcc := []Instruction{}
	return reduce(lines, func(acc []Instruction, s string) []Instruction {
		instructionParts := strings.Split(s, " ")
		direction := parseDirection(instructionParts[0])
		steps, _ := strconv.Atoi(instructionParts[1])
		instruction := Instruction{ direction, steps }

		return append(acc, instruction)
	}, initAcc)
}

func moveTail(tail Position, head Position) Position {
	xDiff := head.x - tail.x
	yDiff := head.y - tail.y

	switch {
	case (xDiff > 1 && yDiff >= 1) || (xDiff >= 1 && yDiff > 1):	// Upper right
		return Position{tail.x + 1, tail.y + 1}

	case (xDiff > 1 && yDiff <= -1) || (xDiff >= 1 && yDiff < -1): // Lower right
		return Position{tail.x + 1, tail.y - 1}

  case (xDiff > 1 && yDiff == 0): // Right
		return Position{tail.x + 1, tail.y}

  case (xDiff < -1 && yDiff >= 1) || (xDiff <= -1 && yDiff > 1): // Upper left
		return Position{tail.x - 1, tail.y + 1}

  case (xDiff < -1 && yDiff <= -1) || (xDiff <= -1 && yDiff < -1): // Lower left
		return Position{tail.x - 1, tail.y - 1}

	case (xDiff < -1 && yDiff == 0): // Left
		return Position{tail.x - 1, tail.y}

  case (yDiff > 1 && xDiff == 0): // Top
		return Position{tail.x, tail.y + 1}

	case (yDiff < -1 && xDiff == 0): // Bottom
		return Position{tail.x, tail.y - 1}

	default: // Still within range
		return tail
	}
}

func moveHead(head Position, direction Direction) Position {
  switch direction {
	case Up:
		return Position{head.x, head.y + 1}
	case Down:
		return Position{head.x, head.y - 1}
	case Left:
		return Position{head.x - 1, head.y}
	case Right:
		return Position{head.x + 1, head.y}
	default:
		return head
	}
}

type State struct {
	tail []Position
	head Position
	positions []Position // Positions the tail has been at
}

func runInstruction(state State, inst Instruction) State {

	for i := 0; i < inst.steps; i++ {
		state = runSingleStep(state, inst.direction)
	}

	return state
}

type StepState struct {
	head Position
	tail []Position
}

func runSingleStep(state State, direction Direction) State {
	newHead := moveHead(state.head, direction)

	finalState := reduce(state.tail, func(stepState StepState, knot Position) StepState {
		knotAhead := stepState.head
		newKnot := moveTail(knot, knotAhead)
		return StepState{newKnot, append(stepState.tail, newKnot)}
  }, StepState{newHead, []Position{}})

	newPositions := append(state.positions, finalState.head)

	return State{finalState.tail, newHead, newPositions}
}

func collectUnique(positions []Position) []Position {
	allKeys := make(map[Position]bool)
	list := []Position{}

	for _, item := range positions {
		if _, value := allKeys[item]; !value {
			allKeys[item] = true
			list = append(list, item)
		}
	}
	return list
}

func main() {
	useFile := true
	rawInput := readInput(useFile)
	instructions := parseInstructions(rawInput)

	fmt.Println("Part 1")
	tailPart1 := []Position{Position{0, 0}}

	finalStatePart1 := reduce(instructions, func(acc State, inst Instruction) State {
		return runInstruction(acc, inst)
  }, State{tailPart1, Position{0,0}, []Position{{0,0}}})
	uniquePart1Positions := collectUnique(finalStatePart1.positions)
	fmt.Println(len(uniquePart1Positions))

	fmt.Println("Part 2")
	tailPart2 := []Position{
		Position{0, 0},
		Position{0, 0},
		Position{0, 0},
		Position{0, 0},
		Position{0, 0},
		Position{0, 0},
		Position{0, 0},
		Position{0, 0},
		Position{0, 0},
	}

	finalStatePart2 := reduce(instructions, func(acc State, inst Instruction) State {
		return runInstruction(acc, inst)
  }, State{tailPart2, Position{0,0}, []Position{{0,0}}})
	uniquePart2Positions := collectUnique(finalStatePart2.positions)
	fmt.Println(len(uniquePart2Positions))

}
