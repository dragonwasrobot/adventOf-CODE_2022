#load "str.cma"

let sample_input = "
  Monkey 0:
    Starting items: 79, 98
    Operation: new = old * 19
    Test: divisible by 23
      If true: throw to monkey 2
      If false: throw to monkey 3

  Monkey 1:
    Starting items: 54, 65, 75, 74
    Operation: new = old + 6
    Test: divisible by 19
      If true: throw to monkey 2
      If false: throw to monkey 0

  Monkey 2:
    Starting items: 79, 60, 97
    Operation: new = old * old
    Test: divisible by 13
      If true: throw to monkey 1
      If false: throw to monkey 3

  Monkey 3:
    Starting items: 74
    Operation: new = old + 3
    Test: divisible by 17
      If true: throw to monkey 0
      If false: throw to monkey 1
"

(* Parsing *)

module MonkeyMap = Map.Make (Int)

type operation = Plus of int | Mult of int | Square

type monkey = {
  id : int;
  items : int list;
  inspected : int;
  operation : operation;
  test : int;
  true_clause : int;
  false_clause : int;
}

let find_match rx (idx : int) (candidate : string) =
  let optIdx =
    try Some (Str.search_forward rx candidate idx) with Not_found -> None
  in
  Option.map (fun newIdx -> (newIdx, Str.matched_string candidate)) optIdx

let rec find_all_matches rx (idx : int) (candidate : string) =
  match find_match rx idx candidate with
  | None -> []
  | Some (newIdx, newMatch) ->
      newMatch
      :: find_all_matches rx (newIdx + String.length newMatch) candidate

let parse_id (line : string) =
  let digits = Str.regexp "[0-9]+" in
  match find_match digits 0 line with
  | None -> -1
  | Some (_, idStr) -> int_of_string idStr

let parse_items line =
  let digits = Str.regexp "[0-9]+" in
  line |> find_all_matches digits 0 |> List.map (fun str -> int_of_string str)

let last (lst : 'a list) = lst |> List.rev |> List.hd

let parse_operation (line : string) =
  let raw_parts =
    line |> String.split_on_char '=' |> last |> String.trim
    |> String.split_on_char ' '
  in
  match raw_parts with
  | [ _; "*"; "old" ] -> Square
  | [ _; "*"; raw_int ] -> Mult (int_of_string raw_int)
  | [ _; "+"; raw_int ] -> Plus (int_of_string raw_int)
  | _ -> Plus 0

let parse_test (line : string) =
  line |> String.trim |> String.split_on_char ' ' |> last |> int_of_string

let parse_monkeys monkey_lists =
  let initial_monkey =
    {
      id = -1;
      items = [];
      inspected = 0;
      operation = Square;
      test = -1;
      true_clause = -1;
      false_clause = -1;
    }
  in

  monkey_lists
  |> List.fold_left
       (fun (acc_map : monkey MonkeyMap.t) (monkey_str : string) ->
         let monkey =
           monkey_str |> String.trim |> String.split_on_char '\n'
           |> List.map String.trim
           |> List.fold_left

                (fun (acc_monkey : monkey) (line : string) ->
                  if String.starts_with ~prefix:"Monkey" line then
                    { acc_monkey with id = parse_id line }
                  else if String.starts_with ~prefix:"Starting items:" line then
                    { acc_monkey with items = parse_items line }
                  else if String.starts_with ~prefix:"Operation:" line then
                    { acc_monkey with operation = parse_operation line }
                  else if String.starts_with ~prefix:"Test:" line then
                    { acc_monkey with test = parse_test line }
                  else if String.starts_with ~prefix:"If true:" line then
                    { acc_monkey with true_clause = parse_test line }
                  else if String.starts_with ~prefix:"If false:" line then
                    { acc_monkey with false_clause = parse_test line }
                  else acc_monkey)
                initial_monkey
         in
         acc_map |> MonkeyMap.add monkey.id monkey)
       MonkeyMap.empty

(* Computing *)

let compute_part1 monkey item =
  let new_item_value =
    match monkey.operation with
    | Plus value -> item + value
    | Mult value -> item * value
    | Square -> item * item
  in
  let new_value = new_item_value / 3 in
  if new_value mod monkey.test == 0 then (new_value, monkey.true_clause)
  else (new_value, monkey.false_clause)

let compute_part2 monkey item divisor_product =
  let new_value =
    match monkey.operation with
    | Plus value -> item + value
    | Mult value -> item * value
    | Square -> item * item
  in
  if new_value mod monkey.test == 0 then
    (new_value mod divisor_product, monkey.true_clause)
  else (new_value mod divisor_product, monkey.false_clause)

let compute_divisor_product monkey_map =
  monkey_map |> MonkeyMap.bindings
  |> List.fold_left (fun acc (_, m) -> acc * m.test) 1

let do_round monkey_map is_part2 =
  let monkey_range = monkey_map |> MonkeyMap.bindings |> List.length in
  let range = List.init monkey_range (fun i -> i) in
  range
  |> List.fold_left
       (fun (acc_outer_map : monkey MonkeyMap.t) (outer_monkey_id : int) ->
         let monkey = MonkeyMap.find outer_monkey_id acc_outer_map in

         monkey.items
         |> List.map (fun (item : int) ->
                if is_part2 then
                  let divisor_product = compute_divisor_product monkey_map in
                  compute_part2 monkey item divisor_product
                else compute_part1 monkey item)

         |> List.fold_left
              (fun (acc_inner_map : monkey MonkeyMap.t) (item, inner_monkey_id) ->
                acc_inner_map
                |> MonkeyMap.update inner_monkey_id (fun optMonkey ->
                       optMonkey
                       |> Option.map (fun m ->
                              { m with items = m.items @ [ item ] })))
              acc_outer_map

         |> MonkeyMap.update outer_monkey_id (fun optMonkey ->
                optMonkey
                |> Option.map (fun m ->
                       {
                         m with
                         items = [];
                         inspected = m.inspected + List.length m.items;
                       })))
       monkey_map

(* Init *)

let read_input (use_file : bool) =
  if use_file then (
    let channel = open_in "day11-input.txt" in
    let content = really_input_string channel (in_channel_length channel) in
    close_in channel;
    Str.split (Str.regexp "\n\n") content)
  else Str.split (Str.regexp "\n\n") sample_input

let run (use_file : bool) (is_part2 : bool) =
  let rounds = if is_part2 then 10000 else 20 in
  let init_monkey_map = read_input use_file |> parse_monkeys in

  let final_monkey_map =
    List.init rounds (fun i -> i)
    |> List.fold_left
         (fun acc_map _ -> do_round acc_map is_part2)
         init_monkey_map
  in

  let sorted_by_items =
    final_monkey_map |> MonkeyMap.bindings
    |> List.map (fun (_, m) -> m)
    |> List.sort (fun m1 m2 -> compare m2.inspected m1.inspected)
    |> List.map (fun m -> m.inspected)
  in
  let max = List.nth sorted_by_items 0 * List.nth sorted_by_items 1 in
  Printf.printf "Answer 1: %i\n" max

let () = run true true
(* sample part 1 = 10605 *)
(* input part 1 = 151312 *)
(* sample part 2 = 2713310158 *)
(* input part 2 = 51382025916 *)
