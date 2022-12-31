#!/usr/bin/env swift

import Foundation

let sampleInput = """
  addx 15
  addx -11
  addx 6
  addx -3
  addx 5
  addx -1
  addx -8
  addx 13
  addx 4
  noop
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx 5
  addx -1
  addx -35
  addx 1
  addx 24
  addx -19
  addx 1
  addx 16
  addx -11
  noop
  noop
  addx 21
  addx -15
  noop
  noop
  addx -3
  addx 9
  addx 1
  addx -3
  addx 8
  addx 1
  addx 5
  noop
  noop
  noop
  noop
  noop
  addx -36
  noop
  addx 1
  addx 7
  noop
  noop
  noop
  addx 2
  addx 6
  noop
  noop
  noop
  noop
  noop
  addx 1
  noop
  noop
  addx 7
  addx 1
  noop
  addx -13
  addx 13
  addx 7
  noop
  addx 1
  addx -33
  noop
  noop
  noop
  addx 2
  noop
  noop
  noop
  addx 8
  noop
  addx -1
  addx 2
  addx 1
  noop
  addx 17
  addx -9
  addx 1
  addx 1
  addx -3
  addx 11
  noop
  noop
  addx 1
  noop
  addx 1
  noop
  noop
  addx -13
  addx -19
  addx 1
  addx 3
  addx 26
  addx -30
  addx 12
  addx -1
  addx 3
  addx 1
  noop
  noop
  noop
  addx -9
  addx 18
  addx 1
  addx 2
  noop
  noop
  addx 9
  noop
  noop
  noop
  addx -1
  addx 2
  addx -37
  addx 1
  addx 3
  noop
  addx 15
  addx -21
  addx 22
  addx -6
  addx 1
  noop
  addx 2
  addx 1
  noop
  addx -10
  noop
  noop
  addx 20
  addx 1
  addx 2
  addx 2
  addx -6
  addx -11
  noop
  noop
  noop
  """

enum Instruction {
    case noop
    case addx(Int)
}

struct State {
    var cycle: Int = 1
    var register: Int = 1
    var signalStrengths: [Int] = []
    var pixels: String = ""
}

func parseInput(_ useInputFile: Bool) -> [Instruction] {
    let input: String = {
        if useInputFile {
            let fileUrl = URL(fileURLWithPath: "day10-input.txt")
            let rawInput = try! Data(contentsOf: fileUrl)
            guard let inputString = String(data: rawInput, encoding: .utf8) else {
                print("Failed to parse input")
                return ""
            }
            return inputString
        } else {
            return sampleInput
        }
    }()

    let parsedInput = input
      .trimmingCharacters(in: .whitespacesAndNewlines)
      .split(separator: "\n")
      .map { (s: Substring) -> Instruction in
          let parts: [Substring] = s.split(separator: " ")
          if parts.count <= 1 {
              return Instruction.noop
          } else {
              let value = Int(parts[1])
              return Instruction.addx(value!)
          }
      }

    return parsedInput
}

func rem(_ a: Int, _ b: Int) -> Int {
    return Int(Float(a).truncatingRemainder(dividingBy: Float(b)))
}

func determinePixel(cycle: Int, register: Int) -> String {
    let position = rem(cycle - 1, 40)

    switch (position - register) {
    case 0, 1, -1:
        return "#"
    default:
        return "."
    }
}

func doNoop(_ state: State) -> State {
    var newSignalStrengths = state.signalStrengths

    if state.cycle == 20 {
        newSignalStrengths.append(state.register * 20)
    } else if rem(state.cycle - 20, 40) == 0 {
        newSignalStrengths.append(state.register * state.cycle)
    }

    let newPixel = determinePixel(cycle: state.cycle, register: state.register)

    return State(cycle: state.cycle + 1,
                 register: state.register,
                 signalStrengths: newSignalStrengths,
                 pixels: state.pixels + newPixel)
}

func doAddx(_ state: State, _ n: Int) -> State {
    var newSignalStrengths = state.signalStrengths

    if state.cycle == 20 || state.cycle + 1 == 20 {
        newSignalStrengths.append(state.register * 20)
    } else if rem(state.cycle - 20, 40) == 0 {
        newSignalStrengths.append(state.register * state.cycle)
    } else if rem(state.cycle + 1 - 20, 40) == 0 {
        newSignalStrengths.append(state.register * (1 + state.cycle))
    }

    let newPixel1 = determinePixel(cycle: state.cycle, register: state.register)
    let newPixel2 = determinePixel(cycle: state.cycle + 1, register: state.register)

    return State(cycle: state.cycle + 2,
                 register: state.register + n,
                 signalStrengths: newSignalStrengths,
                 pixels: state.pixels + newPixel1 + newPixel2)
}

func chunk(_ array: [Character], _ size: Int) -> [[Character]] {
    return stride(from: 0, to: array.count, by: size).map {
        Array(array[$0 ..< Swift.min($0 + size, array.count)])
    }
}

func run(_ useInputFile: Bool) {
    let instructions: [Instruction] = parseInput(useInputFile)

    let finalState = instructions.reduce(
      State(),
      { accState, inst in
          switch inst {
          case Instruction.noop:
              return doNoop(accState)

          case Instruction.addx(let n):
              return doAddx(accState, n)
          }
      })

    let totalSignalStrength =
      finalState
      .signalStrengths
      .reduce(0, { accSum, signal in accSum + signal })

    print("Answer 1")
    print(totalSignalStrength)
    // 16060

    let screen = chunk(Array(finalState.pixels), 40)
    print("Answer 2")
    for lines in screen {
        print(String(lines))
    }
    // BACEKLHF
}

run(true)
