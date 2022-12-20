use regex::{CaptureMatches, Regex};
use std::fs;

type Blueprint = [[usize; 4]; 4];

type State = (usize, [usize; 4], [usize; 4]);

fn parse_input() -> Vec<Blueprint> {
    // let input: String = fs::read_to_string("day19.sample").unwrap();
    let input: String = fs::read_to_string("day19.input").unwrap();

    let pattern =
        Regex::new(r"(?m)Blueprint (\d+):\s+Each ore robot costs (\d+) ore.\s+Each clay robot costs (\d+) ore.\s+Each obsidian robot costs (\d+) ore and (\d+) clay.\s+Each geode robot costs (\d+) ore and (\d+) obsidian.")
            .unwrap();

    //  (..) has flow rate=(\\d+); tunnels? lead? to valves? (.*)$
    let mod_input = input.replace('\n', " ").replace(r"\\S+", " ");
    println!("{:?}", mod_input);
    let captures: CaptureMatches = pattern.captures_iter(&mod_input);

    let mut blueprints: Vec<Blueprint> = Vec::new();

    for capture in captures {
        let ore: usize = capture.get(2).unwrap().as_str().parse::<usize>().unwrap();
        let clay: usize = capture.get(3).unwrap().as_str().parse::<usize>().unwrap();
        let obsidian1: usize = capture.get(4).unwrap().as_str().parse::<usize>().unwrap();
        let obsidian2: usize = capture.get(5).unwrap().as_str().parse::<usize>().unwrap();
        let geode1: usize = capture.get(6).unwrap().as_str().parse::<usize>().unwrap();
        let geode2: usize = capture.get(7).unwrap().as_str().parse::<usize>().unwrap();
        blueprints.push([
            [ore, 0, 0, 0],
            [clay, 0, 0, 0],
            [obsidian1, obsidian2, 0, 0],
            [geode1, 0, geode2, 0],
        ]);
    }
    return blueprints;
}

fn state_is_complete(state: &State, limit: usize) -> bool {
    let (time, _robots, _supplies) = state;
    return *time >= limit;
}

fn get_possible_n(blueprint: Blueprint, state: State) -> Vec<usize> {
    let (_time, robots, _supplies) = state;
    let mut possibles = Vec::new();

    if robots[0] < blueprint[0][0]
        || robots[0] < blueprint[1][0]
        || robots[0] < blueprint[2][0]
        || robots[0] < blueprint[3][0]
    {
        possibles.push(0);
    }
    if robots[1] < blueprint[2][1] || robots[1] < blueprint[3][1] {
        possibles.push(1);
    }
    if robots[1] > 0 && robots[2] < blueprint[3][2] {
        possibles.push(2)
    }
    if robots[2] > 0 {
        possibles.push(3)
    }

    return possibles;
}

fn advance_state(blueprint: Blueprint, state: State, n: usize, limit: usize) -> State {
    let (time, robots, supplies) = state;

    let blueprint_n = blueprint[n];
    let mut steps_needed = 1;
    for t in 0..4 {
        let needed = blueprint_n[t];
        let have = supplies[t];
        let delta = robots[t];

        let mut steps_t = 1;
        if have < needed {
            steps_t = 1 + ((needed - have) + delta - 1) / delta;
        }
        if (steps_t > steps_needed) {
            steps_needed = steps_t;
        }
    }
    let mut do_purchase = true;
    if steps_needed > limit - time {
        do_purchase = false;
        steps_needed = limit - time;
    }

    let mut new_supplies = supplies.clone();
    let mut new_robots = robots.clone();

    for t in 0..4 {
        new_supplies[t] += steps_needed * robots[t];
    }

    if do_purchase {
        for t in 0..4 {
            new_supplies[t] -= blueprint_n[t];
        }
        new_robots[n] += 1;
    }
    let new_state = (time + steps_needed, new_robots, new_supplies);
    // println!("{:?} {:?} {}", state, new_state, n);
    return new_state;
}

fn iterate_state(blueprint: Blueprint, state: State, limit: usize) -> Vec<State> {
    let mut new_states: Vec<State> = Vec::new();
    let possible_n = get_possible_n(blueprint, state);

    for n in possible_n.iter() {
        new_states.push(advance_state(blueprint, state, *n, limit))
    }
    return new_states;
}

fn better_than(new_state: State, old_state: Option<State>) -> bool {
    match old_state {
        Some(s) => {
            return new_state.2[3] > s.2[3];
        }
        None => return true,
    }
}

fn main() {
    let blueprints: Vec<Blueprint> = parse_input();
    let b1 = blueprints[0];

    let mut score = 0;
    for (i, b) in blueprints.iter().enumerate() {
        let foo = find_best_for_blueprint(*b, 24);
        println!("{} {:?} {}", i, b, foo);
        score += (i + 1) * foo;
    }
    println!("{}", score);

    let mut score2 = 1;
    for (i, b) in blueprints.iter().take(3).enumerate() {
        let foo = find_best_for_blueprint(*b, 32);
        println!("{} {:?} {}", i, b, foo);
        score2 *= foo;
    }
    println!("{}", score2);
}

fn find_best_for_blueprint(b1: Blueprint, limit: usize) -> usize {
    let mut states: Vec<State> = vec![(0, [1, 0, 0, 0], [0, 0, 0, 0])];
    let mut best_state: Option<State> = None;
    while states.len() > 0 {
        match states.pop() {
            Some(state) => {
                // println!("consider {:?}", state);
                if state_is_complete(&state, limit) {
                    if better_than(state, best_state) {
                        best_state = Some(state);
                    }
                } else {
                    let mut new_states = iterate_state(b1, state, limit);
                    states.append(&mut new_states);
                    // println!("appended {:?}", states);
                }
            }
            None => {}
        }
    }
    println!("{:?} {:?}", b1, best_state);
    return best_state.unwrap().2[3];
}
