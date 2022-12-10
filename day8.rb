# day8.rb
require "ostruct"

def raw_input(use_input_file)

  raw_input =
    if use_input_file
      File.open("day8-input.txt").read
    else
      """
30373
25512
65332
33549
35390
"""
    end

  raw_input.strip.split("\n")
end

def parse_forest(tree_lists)
  trees = []

  tree_lists.each_with_index { |raw_tree_line, i |
    trees[i] = []
    tree_line = raw_tree_line.split("")

    tree_line.each_with_index { | str_height, j |
      height = str_height.to_i()
      visible = (i == 0 || i == tree_lists.length - 1 || j == 0 || j == tree_line.length - 1)
      tree = OpenStruct.new(height: height, visible: visible, score: 1)
      trees[i][j] = tree
    }
  }
  trees
end

# Part 1

def calculate_visibility(tree_line)
  tree_line.inject(0) { | tallest_neighbour, tree |
    if tallest_neighbour < tree.height then
      tree.visible = true
      tree.height
    else
      tallest_neighbour
    end
  }
  tree_line
end

def swap_rows_and_columns(trees)
  swapped = []

  n = trees.length
  m = trees[0].length

  (0...n).step(1) { | i |
    swapped[i] = []

    (0...m).step(1) { | j |
      swapped[i][j] = trees[j][i]
      }
    }

  swapped
end

def count_visibles(trees)
  trees
    .flatten()
    .map { | tree | tree.visible }
    .filter { | visible | visible }
    .count()
end

# Part 2

def calculate_scenic_score(tree_line)
  tree_line.each_with_index { | tree, i |
    if i == 0 then
      tree.score = tree.score * 0
    else
      line_of_sight = 0
      previous_height = 0
      j = i - 1

      begin
        previous_height = tree_line[j].height
        line_of_sight = line_of_sight + 1
        j = j - 1
      end until (previous_height >= tree.height or j == -1)

      new_score = tree.score * line_of_sight
      tree.score = new_score
    end
  }
  tree_line
end

def find_best_view(trees)
  trees
  .flatten()
  .map { | tree | tree.score }
  .max
end

def day8(use_file_input)

  # Initialize trees
  trees1 = parse_forest(raw_input(use_file_input))

  trees1 =
    trees1
      # reduce: if tallest_west_neighbour < me -> mark as visible
      .map { | tree_line | calculate_visibility(tree_line) }
  # reduce: if tallest_east_neighbour < me
     .map { | tree_line | calculate_visibility(tree_line.reverse()).reverse() }

   trees1 =
     swap_rows_and_columns(trees1)
       .map { | tree_line | calculate_visibility(tree_line) }
  # reduce: if tallest_south_neighbour < me
       .map { | tree_line | calculate_visibility(tree_line.reverse()).reverse() }

   trees1 =
     swap_rows_and_columns(trees1)

    answer1 =
      count_visibles(trees1)

  # Count: 1703
  puts("Part 1:")
  puts(answer1)

  # Initialize trees
  trees2 = parse_forest(raw_input(use_file_input))

  trees2 =
    trees2
      # West
      .map { | tree_line | calculate_scenic_score(tree_line) }
      # East
      .map { | tree_line | calculate_scenic_score(tree_line.reverse()).reverse() }

  trees2 =
    swap_rows_and_columns(trees2)
      # North
      .map { | tree_line | calculate_scenic_score(tree_line) }
      # # South
      .map { | tree_line | calculate_scenic_score(tree_line.reverse()).reverse() }

  trees2 =
    swap_rows_and_columns(trees2)

  answer2 =
    find_best_view(trees2)

  puts("Part 2:")
  puts(answer2)
end

day8(true)
