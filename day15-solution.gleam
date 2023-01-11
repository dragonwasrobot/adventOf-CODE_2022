import gleam/erlang/file
import gleam/int
import gleam/io
import gleam/list
import gleam/map.{Map}
import gleam/regex.{Match}
import gleam/string

const sample_input = "
  Sensor at x=2, y=18: closest beacon is at x=-2, y=15
  Sensor at x=9, y=16: closest beacon is at x=10, y=16
  Sensor at x=13, y=2: closest beacon is at x=15, y=3
  Sensor at x=12, y=14: closest beacon is at x=10, y=16
  Sensor at x=10, y=20: closest beacon is at x=10, y=16
  Sensor at x=14, y=17: closest beacon is at x=10, y=16
  Sensor at x=8, y=7: closest beacon is at x=2, y=10
  Sensor at x=2, y=0: closest beacon is at x=2, y=10
  Sensor at x=0, y=11: closest beacon is at x=2, y=10
  Sensor at x=20, y=14: closest beacon is at x=25, y=17
  Sensor at x=17, y=20: closest beacon is at x=21, y=22
  Sensor at x=16, y=7: closest beacon is at x=15, y=3
  Sensor at x=14, y=3: closest beacon is at x=15, y=3
  Sensor at x=20, y=1: closest beacon is at x=15, y=3
"

type Position {
  Position(x: Int, y: Int)
}

type Sensor {
  Sensor(sensor: Position, beacon: Position, distance: Int)
}

type Configuration {
  Configuration(target_y: Int, search_space: Int, sensors: List(Sensor))
}

fn parse_input(use_file: Bool) -> Configuration {
  let #(target_y, search_space, raw_input) =
      case use_file {
        True -> {
          assert Ok(file_input) = file.read("day15-input.txt")
          #(2_000_000, 4_000_000, file_input)
        }
        False -> #(10, 20, sample_input)
      }

  assert Ok(sensor_regex) = regex.from_string("(-?\\d+)")

  let to_position = fn(left: Match, right: Match) -> Position {
      assert Ok(x) = int.parse(left.content)
      assert Ok(y) = int.parse(right.content)
      Position(x, y)
  }

  let parsed_input =
        raw_input
        |> string.trim()
        |> string.split(on: "\n")
        |> list.map(fn(sensor_str) {
          let [sensor_position, beacon_position] =
            regex.scan(with: sensor_regex, content: sensor_str)
              |> list.sized_chunk(2)
              |> list.map(fn(match_pair) {
                case match_pair {
                  [left_match, right_match] -> to_position(left_match, right_match)
                }
              })

          Sensor(sensor: sensor_position,
                 beacon: beacon_position,
                 distance: distance(sensor_position, beacon_position))
        })

  Configuration(target_y, search_space, parsed_input)
}

fn distance(p1: Position, p2: Position) -> Int {
  int.absolute_value(p2.x - p1.x) + int.absolute_value(p2.y - p1.y)
}

fn within_row(sensor: Sensor, target_y: Int) -> Bool {
  let Sensor(Position(x: _, y: sensor_y), _, distance: distance) = sensor
  sensor_y - distance <= target_y &&  target_y <= sensor_y + distance
}

fn within_search_space(pos: Position, search_space: Int) -> Bool {
  //             (0,0) ...            (0, search_space)
  //                ...                              ...
  // (search_space, 0) ... (search_space, search_space)
  0 <= pos.x && pos.x <= search_space && 0 <= pos.y && pos.y <= search_space
}

fn overlap(sensor: Sensor, target_y: Int) -> List(Position) {
  let Sensor(Position(x: x, y: y), beacon: _, distance: distance) = sensor
  let fanout = int.absolute_value(target_y - y) - distance

  let fanout_range =
    case x - fanout < x + fanout {
      True -> list.range(x - fanout, x + fanout)
      False -> list.range(x + fanout, x - fanout)
    }

  fanout_range
  |> list.map(fn(pos_x) { Position(pos_x, target_y) })
}

fn circumference(sensor: Sensor) -> List(Position) {
  let Sensor(Position(x: x, y: y), _, distance: distance) = sensor
  let circum_dist = distance + 1
  // (2, 2) dist = 2
  // 4 larger each time

  // (2 + 0, 2 - 2) = (2, 0)
  // (2 - 0, 2 + 2) = (2, 4)
  // (2 + 1, 2 - 1) = (3, 1)
  // (2 + 1, 2 + 1) = (3, 3)
  // (2 - 1, 2 + 1) = (1, 3)
  // (2 - 1, 2 - 1) = (1, 1)
  // (2 + 2, 2 - 0) = (4, 2)
  // (2 - 2, 2 + 0) = (0, 2)
  list.range(0, circum_dist)
  |> list.fold([], fn(acc: List(Position), idx: Int) {
    case idx {
      0 -> {
        let y_offset = circum_dist - idx
        // change to cons
        list.append([Position(x, y - y_offset), Position(x, y + y_offset)], acc)
      }

      _ -> {
        let y_offset = circum_dist - idx
        let pos_list = [Position(x + idx, y - y_offset), Position(x + idx, y + y_offset)]
        let neg_list = [Position(x - idx, y - y_offset), Position(x - idx, y + y_offset)]
        list.append(pos_list, list.append(neg_list, acc))
      }
    }
  })
}

pub fn main() {
  let use_file = True

  let Configuration(target_y, search_space, sensors) = parse_input(use_file)
  let beacon_positions = sensors |> list.map(fn(s: Sensor) { s.beacon })

  let init_map: Map(String, Position) = map.new()
  let answer1: Int =
      sensors
      |> list.filter(fn(sensor: Sensor) { within_row(sensor, target_y) })
      |> list.fold(init_map, fn(outer_acc: Map(String, Position), sensor: Sensor) {
        overlap(sensor, target_y)
        |> list.fold(outer_acc, fn(inner_acc: Map(String, Position), pos: Position) {
           let key: String = int.to_string(pos.x) <> "," <> int.to_string(pos.y)

           inner_acc |> map.insert(key, pos)
        })
      })
      |> map.values()
      |> list.filter(fn(pos: Position) { list.contains(beacon_positions, any: pos) == False })
      |> list.length()

  io.print("Answer 1: " <> int.to_string(answer1) <> "\n")

  let reduced_search_space =
    sensors
      |> list.map(fn(sensor) { circumference(sensor) })
      |> list.flatten()
      |> list.filter(fn(point) { within_search_space(point, search_space) })

  assert Ok(Position(x: distress_x, y: distress_y)) =
    reduced_search_space
      |> list.find(fn(pos: Position) {
         sensors
         |> list.all(fn(sensor: Sensor) { distance(sensor.sensor, pos) > sensor.distance })
      })

  let answer2: Int = distress_x * 4_000_000 + distress_y
  io.print("Answer 2: " <> int.to_string(answer2) <> "\n")

  io.print("Done!\n")
  0
}
