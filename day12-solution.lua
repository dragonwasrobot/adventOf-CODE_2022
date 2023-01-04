local sample_input = [[
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
]]

function read_input(use_file)
  if use_file then
    local fh = assert(io.open("day12-input.txt", "rb"))
    local contents = fh:read("a")
    fh:close()
    return contents
  else
    return sample_input
  end
end

function parse_input(use_file)
  local raw_input = read_input(use_file)
  local trimmed_input = string.match(raw_input, "^%s*(.-)%s*$")

  local result = {};
  for line in string.gmatch(trimmed_input, "[^\n]+") do
    table.insert(result, line)
  end
  return result
end

function to_char_table(str)
  t = {}
  str:gsub(".", function(c) table.insert(t, c) end)
  return t
end

function create_nodes(input)
  local nodes = {}

  for row, line in ipairs(input) do
    local chars = to_char_table(line)

    for col, char in ipairs(chars) do
      table.insert(nodes, to_node(char, row, col))
    end
  end

  return nodes
end

function to_node(c, row, col)
  local elevation_map = {
    S = 0,
    a = 0,
    b = 1,
    c = 2,
    d = 3,
    e = 4,
    f = 5,
    g = 6,
    h = 7,
    i = 8,
    j = 9,
    k = 10,
    l = 11,
    m = 12,
    n = 13,
    o = 14,
    p = 15,
    q = 16,
    r = 17,
    s = 18,
    t = 19,
    u = 20,
    v = 21,
    w = 22,
    x = 23,
    y = 24,
    z = 25,
    E = 25
  }

  return {
      position = {row = row, col = col},
      id = c,
      elevation = elevation_map[c],
      dist = (c == "E" and 0 or 1000000),
      prev = nil,
      visited = false
    }
end

function all_visited(nodes)
  local visited = true
  for idx, node in ipairs(nodes) do
    if node.visited == false then
      visited = false
      break
    end
  end
  return visited
end

function find_min(nodes)
  local min = nil

  for idx, node in ipairs(nodes) do
    if node.visited == false then
      if min == nil or node.dist < min.dist then
        min = node
      end
    end
  end

  return min
end

function find_edges(node, nodes)
  local edges = {}

  local north = nil
  for idx, candidate in ipairs(nodes) do
    if candidate.visited == false and
      candidate.position.row == node.position.row - 1 and
      candidate.position.col == node.position.col and
      candidate.elevation >= node.elevation - 1 then
      north = candidate
      break
    end
  end
  if north ~= nil then
    table.insert(edges, north)
  end

  local east = nil
  for idx, candidate in ipairs(nodes) do
    if candidate.visited == false and
      candidate.position.row == node.position.row and
      candidate.position.col == node.position.col + 1 and
      candidate.elevation >= node.elevation - 1 then
      east = candidate
      break
    end
  end
  if east ~= nil then
    table.insert(edges, east)
  end

  local west = nil
  for idx, candidate in ipairs(nodes) do
    if candidate.visited == false and
      candidate.position.row == node.position.row + 1 and
      candidate.position.col == node.position.col and
      candidate.elevation >= node.elevation - 1 then
      west = candidate
      break
    end
  end
  if west ~= nil then
    table.insert(edges, west)
  end

  local south = nil
  for idx, candidate in ipairs(nodes) do
    if candidate.visited == false and
      candidate.position.row == node.position.row and
      candidate.position.col == node.position.col - 1 and
      candidate.elevation >= node.elevation - 1 then
      south = candidate
      break
    end
  end
  if south ~= nil then
    table.insert(edges, south)
  end

  return edges
end

function dijkstra(nodes)
  if all_visited(nodes) then
   return nodes
 else
   local u = find_min(nodes)
   local neighbour_nodes = find_edges(u, nodes)

   for idx, v in ipairs(neighbour_nodes) do
     local alt = u.dist + 1

     if alt < v.dist then
       v.dist = alt
       v.prev = u.position
     end
   end
   u.visited = true

   return dijkstra(nodes)
 end
end

function find_answer1(graph)
  for idx, node in ipairs(graph) do
    if node.id == "S" then
      return node.dist
    end
  end
end

function find_answer2(graph)
  local min = nil

  for idx, node in ipairs(graph) do
    if node.elevation == 0 then
      if min == nil or node.dist < min.dist then
        min = node
      end
    end
  end

  return min.dist
end

function run(use_input_file)
  local text_input = parse_input(use_input_file)
  local init_graph = create_nodes(text_input)
  local final_graph = dijkstra(init_graph)

  print("Answer 1")
  local answer1 = find_answer1(final_graph)
  print(answer1)

  print("Answer 2")
  local answer2 = find_answer2(final_graph)
  print(answer2)

end

run(true)
-- Sample input answer 1: 31
-- Real input answer 1: 490
-- Sample input answer 2: 29
-- Real input answer 2: 488
