use std::fs;
use std::cmp;

const SAMPLE_INPUT: &str = "
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
";

#[derive(Debug)]
struct Blueprint {
    id: i32,
    ore_robot_cost: i32, // ore
    clay_robot_cost: i32, // ore
    obsidian_robot_cost: ObsidianRobotCost,
    geode_robot_cost: GeodeRobotCost,
    max_ore_cost: i32,
    max_clay_cost: i32,
    max_obsidian_cost: i32
}

#[derive(Debug)]
struct ObsidianRobotCost(i32, i32); // (ore, clay)

#[derive(Debug)]
struct GeodeRobotCost(i32, i32); // (ore, obsidian)

fn main() {
    let use_file = true;
    let blueprints: Vec<Blueprint> = parse_input(use_file);

    // Part 1
    let quality_levels: Vec<i32> =
        blueprints
        .iter()
        .map(| blueprint| blueprint.id * calculate_max_geodes(&blueprint, 24))
        .collect();

    let answer1: i32 = quality_levels.into_iter().sum();

    println!("Answer 1: {answer1}"); // 1264

    // Part 1
    let max_geodes_first_three: Vec<i32> =
        blueprints.split_at(3).0
        .iter()
        .map(| blueprint| calculate_max_geodes(&blueprint, 32))
        .collect();

    let answer2: i32 =
        max_geodes_first_three
        .into_iter()
        .product();

    println!("Answer 2: {answer2}"); // 13475

    println!("Done!");
}

fn  parse_input(use_file: bool) -> Vec<Blueprint> {
    let file_path = "../day19-input.txt";

    let raw_input: String = if use_file {
        fs::read_to_string(file_path).expect("Could not read input file")
    } else {
        SAMPLE_INPUT.to_string()
    };

    return raw_input.trim().split("\n").map(|line| {
        let parts: Vec<i32> =
            line
            .trim()
            .replace(":", " ")
            .split(" ")
            .map(|candidate| candidate.to_string().parse::<i32>())
            .filter(|candidate| candidate.is_ok())
            .map(|result| result.unwrap())
            .collect();

        return Blueprint {
            id: parts[0],
            ore_robot_cost: parts[1],
            clay_robot_cost: parts[2],
            obsidian_robot_cost: ObsidianRobotCost(parts[3], parts[4]),
            geode_robot_cost: GeodeRobotCost(parts[5], parts[6]),
            max_ore_cost: *[parts[1], parts[2], parts[3], parts[5]].iter().max().unwrap(),
            max_clay_cost: parts[4],
            max_obsidian_cost: parts[6]
        };
    }).collect();
}

#[derive(Debug, Clone)]
struct State {
    minutes: i32,
    ore_count: i32,
    clay_count: i32,
    obsidian_count: i32,
    geode_count: i32,
    ore_robots: i32,
    clay_robots: i32,
    obsidian_robots: i32,
    geode_robots: i32
}

#[derive(Debug, PartialEq)]
enum BuildAction {
    BuildOreRobot,
    BuildClayRobot,
    BuildObsidianRobot,
    BuildGeodeRobot
}

fn calculate_max_geodes(blueprint: &Blueprint, minutes: i32) -> i32 {
    let mut max_geodes = 0;
    let mut candidates: Vec<State> = vec![init_state(minutes)];

    while !candidates.is_empty() {
        let state = candidates.remove(0);
        if max_attainable_geodes(&state) >= max_geodes {
            candidates.append(&mut determine_possible_states(&blueprint, &state));
        }

        let max_geodes_for_state = state.geode_count + state.geode_robots * (state.minutes - 1);
        if max_geodes_for_state > max_geodes {
            max_geodes = max_geodes_for_state;
        }
    }

    return max_geodes;
}

fn init_state(minutes: i32) -> State {
    return State {
        minutes,
        ore_count: 1,
        clay_count: 0,
        obsidian_count: 0,
        geode_count: 0,
        ore_robots: 1,
        clay_robots: 0,
        obsidian_robots: 0,
        geode_robots:0
    };
}

fn max_attainable_geodes(state: &State) -> i32 {
    return state.geode_count +
        ((state.minutes - 1) * (state.minutes - 1) / 2) +
        (state.minutes - 1) * state.geode_robots;
}

fn determine_possible_states(blueprint: &Blueprint, state: &State) -> Vec<State> {
    let mut state_list: Vec<State> = vec![];

    if  blueprint.max_ore_cost > state.ore_robots {
        state_list.push(build_robot(state, blueprint, BuildAction::BuildOreRobot));
    }

    if blueprint.max_clay_cost > state.clay_robots {
        state_list.push(build_robot(state, blueprint, BuildAction::BuildClayRobot));
    }

    if blueprint.max_obsidian_cost > state.obsidian_robots && state.clay_robots > 0 {
        state_list.push(build_robot(state, blueprint, BuildAction::BuildObsidianRobot));
    }

    if state.obsidian_robots > 0 {
        state_list.push(build_robot(state, blueprint, BuildAction::BuildGeodeRobot));
    }

    return state_list
        .into_iter()
        .filter(|s| s.minutes > 0)
        .collect();
}


fn build_robot(state: &State, blueprint: &Blueprint, action: BuildAction) -> State {
    match action {

        BuildAction::BuildOreRobot => {
            let additional_ore_needed = cmp::max(0, blueprint.ore_robot_cost - state.ore_count);
            let rounds_until_enough_ore = (additional_ore_needed as f32 / state.ore_robots as f32).ceil() as i32;

            let build_time = 1 + rounds_until_enough_ore;

            return State {
                minutes: state.minutes - build_time,
                ore_count: state.ore_count + state.ore_robots * build_time - blueprint.ore_robot_cost,
                clay_count: state.clay_count + state.clay_robots * build_time,
                obsidian_count: state.obsidian_count + state.obsidian_robots * build_time,
                geode_count: state.geode_count + state.geode_robots * build_time,
                ore_robots: state.ore_robots + 1,
                ..state.clone()
            }
        },

        BuildAction::BuildClayRobot => {
            let additional_ore_needed = cmp::max(0, blueprint.clay_robot_cost - state.ore_count);
            let rounds_until_enough_ore = (additional_ore_needed as f32 / state.ore_robots as f32).ceil() as i32;

            let build_time = 1 + rounds_until_enough_ore;

            return State {
                minutes: state.minutes - build_time,
                ore_count: state.ore_count + state.ore_robots * build_time - blueprint.clay_robot_cost,
                clay_count: state.clay_count + state.clay_robots * build_time,
                obsidian_count: state.obsidian_count + state.obsidian_robots * build_time,
                geode_count: state.geode_count + state.geode_robots * build_time,
                clay_robots: state.clay_robots + 1,
                ..state.clone()
            }
        },

        BuildAction::BuildObsidianRobot => {
            let additional_ore_needed = cmp::max(0, blueprint.obsidian_robot_cost.0 - state.ore_count);
            let rounds_until_enough_ore = (additional_ore_needed as f32 / state.ore_robots as f32).ceil() as i32;

            let additional_clay_needed = cmp::max(0, blueprint.obsidian_robot_cost.1 - state.clay_count);
            let rounds_until_enough_clay = (additional_clay_needed as f32 / state.clay_robots as f32).ceil() as i32;

            let build_time = 1 + cmp::max(rounds_until_enough_ore, rounds_until_enough_clay);

            return State {
                minutes: state.minutes - build_time,
                ore_count: state.ore_count + state.ore_robots * build_time - blueprint.obsidian_robot_cost.0,
                clay_count: state.clay_count + state.clay_robots * build_time - blueprint.obsidian_robot_cost.1,
                obsidian_count: state.obsidian_count + state.obsidian_robots * build_time,
                geode_count: state.geode_count + state.geode_robots * build_time,
                obsidian_robots: state.obsidian_robots + 1,
                ..state.clone()
            }
        }

        BuildAction::BuildGeodeRobot => {
            let additional_ore_needed = cmp::max(0, blueprint.geode_robot_cost.0 - state.ore_count);
            let rounds_until_enough_ore = (additional_ore_needed as f32 / state.ore_robots as f32).ceil() as i32;

            let additional_obsidian_needed = cmp::max(0, blueprint.geode_robot_cost.1 - state.obsidian_count);
            let rounds_until_enough_obsidian = (additional_obsidian_needed as f32 / state.obsidian_robots as f32).ceil() as i32;

            let build_time = 1 + cmp::max(rounds_until_enough_ore, rounds_until_enough_obsidian);

            return State {
                minutes: state.minutes - build_time,
                ore_count: state.ore_count + state.ore_robots * build_time - blueprint.geode_robot_cost.0,
                clay_count: state.clay_count + state.clay_robots * build_time,
                obsidian_count: state.obsidian_count + state.obsidian_robots * build_time - blueprint.geode_robot_cost.1,
                geode_count: state.geode_count + state.geode_robots * build_time,
                geode_robots: state.geode_robots + 1,
                ..state.clone()
            }
        }
    };
}
