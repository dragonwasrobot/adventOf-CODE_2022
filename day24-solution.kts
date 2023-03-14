#!/usr/bin/env kotlin

import java.io.File

fun main() {

    val useFile = true
    val rawInput = readInput(useFile)
    val configurationOne = parseInput(rawInput)

    val partOne = solvePartOne(configurationOne)
    println("Part one: ${partOne}") // 274

    val configurationTwo = parseInput(rawInput)
    val partTwo = solvePartTwo(configurationTwo)
    println("Part two: ${partTwo}") // 839
}

fun readInput(useFile: Boolean): String {
    if (useFile) {
        return File("day24-input.txt").readText()
    } else {
        return """
#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
"""
    }
}

data class Position(val row: Int, val col: Int)

data class Dimensions(val rows: Int, val cols: Int)

enum class Direction {
    North, South, West, East;

    override fun toString(): String {
           return when (this) {
               North -> "^"
               South -> "v"
               East -> ">"
               West -> "<"
           }
    }

    companion object {
        fun parse(candidate: String): Direction? {
            return when (candidate) {
                "^" -> North
                "v" -> South
                ">" -> East
                "<" -> West
                else -> null
            }
        }
    }
}

data class Blizzard(var position: Position,
                    val direction: Direction)

data class Configuration(var minute: Int,
                         val dimensions: Dimensions,
                         var positions: MutableSet<Position>,
                         val blizzards: MutableList<Blizzard>)

fun printMap(configuration: Configuration) {
    val dimensions = configuration.dimensions

    val rows: MutableList<String> = mutableListOf()
    for (i in 0..dimensions.rows - 1) {
        rows.add(".".repeat(dimensions.cols))
    }

    for (i in 0..dimensions.cols - 1) {
        if (i != 1) {
            rows[0] = rows[0].replaceRange(i, i+1, "#")
        }

        if (i != dimensions.cols - 2) {
            rows[rows.size - 1] = rows[rows.size - 1].replaceRange(i, i+1, "#")
        }
    }

    for (i in 0..rows.size - 1) {
        rows[i] = rows[i].replaceRange(0, 1, "#")
            .replaceRange(dimensions.cols - 1, dimensions.cols, "#")
    }

    for (blizzard in configuration.blizzards) {
        val pos = blizzard.position
        val direction = blizzard.direction.toString()
        rows[pos.row] = rows[pos.row].replaceRange(pos.col, pos.col + 1, direction)
    }

    for (pos in configuration.positions) {
        rows[pos.row] = rows[pos.row].replaceRange(pos.col, pos.col + 1, "E")
    }

    for (row in rows) {
        println(row)
    }
}

fun parseInput(rawInput: String): Configuration {

    val map: List<List<String>> =
        rawInput
        .trim()
        .split("\n")
        .map { rowStr: String ->
            val row = rowStr.trim().split("")
            row.drop(1).dropLast(1)
        }

    val blizzards: MutableList<Blizzard> = mutableListOf()
    for (i in 0..map.size - 1) {
        val row = map[i]
        for (j in 0..row.size - 1) {
            val direction  = Direction.parse(map[i][j])
            if (direction != null) {
                val blizzard = Blizzard(Position(i, j), direction)
                blizzards.add(blizzard)
            }
        }
    }

    val dimensions = Dimensions(map.size, map[0].size)
    val startPositions: MutableSet<Position> = mutableSetOf()

    return Configuration(0, dimensions, startPositions, blizzards)
}

fun solvePartOne(configuration: Configuration): Int {

    configuration.positions = mutableSetOf(Position(0, 1))
    var done = false
    while (!done) {
        done = playNextRound(configuration, false)
    }

    return configuration.minute
}

fun solvePartTwo(configuration: Configuration): Int {

    val dimensions = configuration.dimensions

    configuration.positions = mutableSetOf(Position(0, 1))
    var done = false
    while (!done) {
        done = playNextRound(configuration, false)
    }

    configuration.positions = mutableSetOf(Position(dimensions.rows - 1, dimensions.cols - 2))
    done = false
    while (!done) {
        done = playNextRound(configuration, true)
    }

    configuration.positions = mutableSetOf(Position(0, 1))
    done = false
    while (!done) {
        done = playNextRound(configuration, false)
    }

    return configuration.minute
}

fun computeNewBlizzardPosition(dimensions: Dimensions,
                               blizzard: Blizzard): Position {

    val position = blizzard.position

    return when (blizzard.direction) {
        Direction.North -> {
            if (position.row - 1 == 0) {
                Position(dimensions.rows - 2, position.col)
            } else {
                Position(position.row - 1, position.col)
            }
        }

        Direction.South -> {
            if (position.row + 1 == dimensions.rows - 1) {
                Position(1, position.col)
            } else {
                Position(position.row + 1, position.col)
            }
        }

        Direction.East -> {
            if (position.col + 1 == dimensions.cols - 1) {
                Position(position.row, 1)
            } else {
                Position(position.row, position.col + 1)
            }
        }

        Direction.West -> {
            if (position.col - 1 == 0) {
                Position(position.row, dimensions.cols - 2)
            } else {
                Position(position.row, position.col - 1)
            }
        }
    }
}

fun computeNewUserPositions(dimensions: Dimensions,
                            grid: Grid,
                            positions: MutableSet<Position>,
                            backAgain: Boolean): MutableSet<Position> {

    val rows = dimensions.rows
    val cols = dimensions.cols

    val newPositions: MutableSet<Position> = mutableSetOf()

    for (p in positions) {
        // Wait
        if (grid[p.row][p.col] == false) {
            newPositions.add(Position(p.row, p.col))
        }

        // North
        if (backAgain && p.row - 1 == 0 && p.col == 1) {
            newPositions.add(Position(p.row - 1, p.col))
        }

        if (p.row - 1 > 0 && grid[p.row - 1][p.col] == false) {
            newPositions.add(Position(p.row - 1, p.col))
        }

        if (p.row - 1 > 0 && grid[p.row - 1][p.col] == false) {
            newPositions.add(Position(p.row - 1, p.col))
        }

        // South
        if (!backAgain && p.row + 1 == rows - 1 && p.col == cols - 2) {
            newPositions.add(Position(p.row + 1, p.col))
        }

        if (p.row + 1 < rows - 1 && grid[p.row + 1][p.col] == false) {
            newPositions.add(Position(p.row + 1, p.col))
        }

        // West
        if (p.row > 0 && p.row < rows - 1 &&
                p.col - 1 > 0 && grid[p.row][p.col - 1] == false) {
            newPositions.add(Position(p.row, p.col - 1))
        }

        // East
        if (p.row > 0 && p.row < rows - 1 &&
                p.col + 1 < cols - 1 && grid[p.row][p.col + 1] == false) {
            newPositions.add(Position(p.row, p.col + 1))
        }
    }

    return newPositions
}

typealias Grid = Array<Array<Boolean>>

fun playNextRound(configuration: Configuration,
                  backAgain: Boolean): Boolean {
    val dimensions = configuration.dimensions
    val blizzards = configuration.blizzards

    val rows = dimensions.rows
    val cols = dimensions.cols
    val grid: Grid = Array(rows) { _ -> Array(cols) { _ -> false } }

    for (blizzard in blizzards) {
        blizzard.position = computeNewBlizzardPosition(dimensions, blizzard)
        val position = blizzard.position
        grid[position.row][position.col] = true
    }

    val positions = configuration.positions
    val newPositions = computeNewUserPositions(dimensions, grid, positions, backAgain)

    if (!backAgain && newPositions.any { it.row == rows - 1 && it.col == cols - 2 }) {
        configuration.minute += 1
        return true
    }

    if (backAgain && newPositions.any { it.row == 0 && it.col == 1 }) {
        configuration.minute += 1
        return true
    }

    configuration.positions = newPositions
    configuration.minute += 1
    return false
}

main()
