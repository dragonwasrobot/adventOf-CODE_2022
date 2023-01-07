alias RockPath = Array(Position)
alias Position = Tuple(Int32, Int32)

alias Configuration = Tuple(Canvas, SandPoint, Dimensions)
alias Canvas = Array(Array(Char))
alias SandPoint = Tuple(Int32, Int32)
alias Dimensions = Tuple(Int32, Int32)

def parse_input(use_file : Bool) : Array(RockPath)

  sample_input = "
  498,4 -> 498,6 -> 496,6
  503,4 -> 502,4 -> 502,9 -> 494,9
  "

  raw_input =
    if use_file
      File.read("day14-input.txt")
    else
      sample_input
    end

  raw_input
    .strip(' ')
    .split('\n', remove_empty: true)
    .map { |path_str|
        path_str
        .strip(' ')
        .split(" -> ")
        .map { |segment_str|
          Tuple(Int32, Int32).from(
            segment_str
              .split(',')
              .map { |point_str| point_str.to_i }
          )
        }
    }
end

def compute_rock_positions(paths : Array(RockPath),
                           x_min : Int32,
                           x_max : Int32,
                           y_max : Int32,
                           part2 : Bool) : Array(Position)

  rocks_positions =
    paths.map { |path|
      path
        .reduce([] of Position) { | acc, segment |
          if acc.empty?
            acc << segment
          else
            x = segment[0]
            y = segment[1]

            prev = acc.last()
            prev_x = prev[0]
            prev_y = prev[1]

              if prev_x == x
                if y < prev_y
                  segments = (y..prev_y).map { |some_y| {x, some_y} }
                  acc.concat(segments.reverse())
                else
                  segments = (prev_y..y).map { |some_y| {x, some_y} }
                  acc.concat(segments)
                end
              else
                if x < prev_x
                  segments = (x..prev_x).map { |some_x| {some_x, y} }
                  acc.concat(segments.reverse())
                else
                  segments = (prev_x..x).map { |some_x| {some_x, y} }
                  acc.concat(segments)
                end
              end

            acc
          end
      }
        .uniq()
    }
    .flatten()

  if part2
    ground_positions = (0..(x_max + x_min - 1)).map { |x| {x, y_max} }
    rocks_positions.concat(ground_positions)
  end

  rocks_positions
end

def init_configuration(paths : Array(RockPath), part2 : Bool) : Configuration

  x_min = (paths.flatten().min_by { |x, _| x })[0]
  x_max = (paths.flatten().max_by { |x, _| x })[0]
  y_max = (paths.flatten().max_by { |_, y| y })[1]

  if part2
    y_max = y_max + 2
  end

  canvas = Array(Array(Char)).new
  (0..(x_max + x_min)).each { |i| canvas << Array.new(y_max + 1, '.') }

  sand_point = {500, 0}
  canvas[500][0] = '+'

  rocks_positions = compute_rock_positions(paths, x_min, x_max, y_max, part2)
  rocks_positions.each { | x, y | canvas[x][y] = '#'}

  {canvas, sand_point, {x_max, y_max}}
end

def move_sand(canvas : Canvas,
              sand_point : Position,
              dimensions : Dimensions,
              counter : Int32,
              part2 : Bool): Tuple(Bool, Int32)

  x = sand_point[0]
  y = sand_point[1]
  x_max = dimensions[0]
  y_max = dimensions[1]

  case
  when (y+1 > y_max) && !part2
    {true, counter}

  when canvas[x][y+1] == '.'
    move_sand(canvas, {x, y+1}, {x_max, y_max}, counter, part2)

  when (x-1 < 0 || y+1 > y_max) && !part2
    {true, counter}

  when canvas[x-1][y+1] == '.'
    move_sand(canvas, {x-1, y+1}, {x_max, y_max}, counter, part2)

  when (x+1 < 0 || y+1 > y_max) && !part2
    {true, counter}

  when canvas[x+1][y+1] == '.'
    move_sand(canvas, {x+1, y+1}, {x_max, y_max}, counter, part2)

  when x == 500 && y == 0 && part2
    canvas[x][y] = 'o'
    {true, counter+1}

  else
    canvas[x][y] = 'o'
    {false, counter+1}
  end
end

def run_simulation(configuration : Configuration, part2 : Bool): Int32

  canvas = configuration[0]
  sand_point = configuration[1]
  dimensions = configuration[2]

  counter = 0
  loop do
    result = move_sand(canvas, sand_point, dimensions, counter, part2)
    stopped = result[0]
    counter = result[1]
    break if stopped
  end

  counter
end

def run(use_input_file : Bool, part2 : Bool) : Int32
  input = parse_input(use_input_file)
  configuration = init_configuration(input, part2)
  run_simulation(configuration, part2)
end

answer1 = run(true, false)
puts "Part 1: #{answer1}" # 838

answer2 = run(true, true)
puts "Part 2: #{answer2}" # 27539
