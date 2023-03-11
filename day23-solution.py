# Day 23

# '#' = elf
# '.' = ground

sample_input = """
....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
"""

def read_input(use_file):
    if use_file:
        with open('day23-input.txt', 'r', encoding="utf-8") as f:
            return f.read().strip()
    else:
        return sample_input.strip()

def parse_data(raw_data):
    return [list(s) for s in raw_data.split('\n')]

def pad_data(parsed_data):
   inner_length = len(parsed_data[0])
   inner_padding = ["." for _ in range(inner_length)]
   inner_padded = [["." for _ in range(inner_length)] + \
                   row + \
                   ["." for _ in range(inner_length)] for row in parsed_data]

   grid = [3 * inner_padding for _ in range(inner_length)] + \
          inner_padded + \
          [3 * inner_padding for _ in range(inner_length)]

   elves = []
   next_elf_id = 0

   for row in range(len(grid)):
       for col in range(len(grid[row])):
           if grid[row][col] == "#":
               elves.append((next_elf_id, (row, col)))
               next_elf_id = next_elf_id + 1

   return {"grid": grid, "elves": elves}

def check_north(grid, position):
    row, col = position
    return grid[row - 1][col - 1] == "." \
        and grid[row - 1][col] == "." \
        and grid[row - 1][col + 1] == "."

def check_south(grid, position):
    row, col = position
    return grid[row + 1][col - 1] == "." \
        and grid[row + 1][col] == "." \
        and grid[row + 1][col + 1] == "."

def check_west(grid, position):
    row, col = position
    return grid[row - 1][col - 1] == "." \
        and grid[row][col - 1] == "." \
        and grid[row + 1][col - 1] == "."

def check_east(grid, position):
    row, col = position
    return grid[row - 1][col + 1] == "." \
        and grid[row][col + 1] == "." \
        and grid[row + 1][col + 1] == "."

def first_half(directions, grid, old_elves):
    new_elves = []

    for elf in old_elves:

        elf_id = elf[0]
        elf_position = elf[1]
        north_clear = check_north(grid, elf_position)
        south_clear = check_south(grid, elf_position)
        west_clear = check_west(grid, elf_position)
        east_clear = check_east(grid, elf_position)

        if north_clear and south_clear and west_clear and east_clear:
            new_elves.append((elf_id, elf_position))

        elif not north_clear and not south_clear and not west_clear and not east_clear:
            new_elves.append((elf_id, elf_position))

        else:
            row, col = elf_position
            for direction in directions:
                if direction == "N":
                    if north_clear:
                        new_position = (row - 1, col)
                        new_elves.append((elf_id, new_position))
                        break

                elif direction == "S":
                    if south_clear:
                        new_position = (row + 1, col)
                        new_elves.append((elf_id, new_position))
                        break

                elif direction == "W":
                    if west_clear:
                        new_position = (row, col - 1)
                        new_elves.append((elf_id, new_position))
                        break

                elif direction == "E":
                    if east_clear:
                        new_position = (row, col + 1)
                        new_elves.append((elf_id, new_position))
                        break

                else:
                    print(f'Unexpected direction {direction}')

    return new_elves

def second_half(grid, old_elves, new_elves):
    for i in range(len(new_elves)):
        elf = new_elves[i]
        elf_id = elf[0]
        elf_position = elf[1]

        unique_position = True
        for j in range(len(new_elves)):
            other_elf = new_elves[j]
            other_elf_id = other_elf[0]
            other_elf_position = other_elf[1]
            if elf_position == other_elf_position and elf_id != other_elf_id:
                unique_position = False
                old_elf = old_elves[j]
                new_elves[j] = (old_elf[0], old_elf[1])

        if unique_position == False:
            old_elf = old_elves[i]
            new_elves[i] = (old_elf[0], old_elf[1])

    # Update grid
    for i in range(len(old_elves)):
        old_elf = old_elves[i]
        old_row, old_col = old_elf[1]
        grid[old_row][old_col] = "."

    for i in range(len(new_elves)):
        new_elf = new_elves[i]
        new_row, new_col = new_elf[1]
        grid[new_row][new_col] = "#"

    return new_elves

def play_round(configuration, directions):
    grid = configuration["grid"]
    old_elves = configuration["elves"]
    new_elves = first_half(directions, grid, old_elves)
    configuration["elves"] = second_half(grid, old_elves, new_elves)

def rotate(directions):
    direction = directions.pop(0)
    directions.append(direction)

def play_rounds(configuration, rounds):
    directions = ["N", "S", "W", "E"]

    for _ in range(rounds):
        play_round(configuration, directions)
        rotate(directions)

def print_configuration(configuration):
    for i in range(len(configuration["grid"])):
        row = configuration["grid"][i]
        pretty_row = "".join(row)
        print(pretty_row)
    print(configuration["elves"])

def compute_empty_tiles(configuration):
    grid = configuration["grid"]
    elves = configuration["elves"]

    min_row = min(elf[1][0] for elf in elves)
    max_row = max(elf[1][0] for elf in elves)
    min_col = min(elf[1][1] for elf in elves)
    max_col = max(elf[1][1] for elf in elves)

    empty_tiles = 0
    for r in range(min_row, max_row + 1):
        for c in range(min_col, max_col + 1):
            if grid[r][c] == ".":
                empty_tiles += 1

    return empty_tiles

def play_until_done(configuration):
    directions = ["N", "S", "W", "E"]

    for i in range(100000):
        old_elves = configuration["elves"]
        play_round(configuration, directions)
        rotate(directions)

        new_elves = configuration["elves"]
        if old_elves == new_elves:
            return i + 1

def solve_part1(use_file):
    raw_data = read_input(use_file)
    parsed_data = parse_data(raw_data)
    configuration = pad_data(parsed_data)
    play_rounds(configuration, 10)
    empty_tiles = compute_empty_tiles(configuration)
    return empty_tiles

def solve_part2(use_file):
    raw_data = read_input(use_file)
    parsed_data = parse_data(raw_data)
    configuration = pad_data(parsed_data)
    rounds_until_done = play_until_done(configuration)
    return rounds_until_done

use_file = True

part1 = solve_part1(use_file)
print(f'Part 1: {part1}') # 3923

part2 = solve_part2(use_file)
print(f'Part2 2: {part2}') # 1019
