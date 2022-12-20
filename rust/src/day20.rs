use std::{fs, process};

// const ITERATIONS: usize = 1;
// const KEY: i64 = 1;

const ITERATIONS: usize = 10;
const KEY: i64 = 811589153;

// const PATH: &str = "day20.sample";
const PATH: &str = "day20.input";

#[derive(Debug)]
struct Node {
    value: i64,
    next_node: usize,
    previous_node: usize,
}

fn parse_input() -> Vec<i64> {
    let input: String = fs::read_to_string(PATH).unwrap();

    let parsed = input.lines().map(|x| x.parse::<i64>().unwrap()).collect();
    return parsed;
}

fn find_locations(nodes: &Vec<Node>, index: usize) -> (usize, usize) {
    let cycle_size: i64 = nodes.len() as i64 - 1;
    let mut location = index;
    let mut rotation = nodes.get(index).unwrap().value;
    rotation = rotation % cycle_size;

    if rotation < 0 {
        rotation += cycle_size;
    }

    for _ in 0..rotation {
        location = nodes.get(location).unwrap().next_node;
    }
    return (index, location);
}

fn update_locations(nodes: &mut Vec<Node>, source: usize, target: usize) {
    if source == target {
        return;
    }

    let old_previous = nodes.get(source).unwrap().previous_node;
    let old_next = nodes.get(source).unwrap().next_node;

    let new_previous = target;
    let new_next = nodes.get(target).unwrap().next_node;

    nodes.get_mut(old_previous).unwrap().next_node = old_next;
    nodes.get_mut(old_next).unwrap().previous_node = old_previous;

    nodes.get_mut(new_previous).unwrap().next_node = source;
    nodes.get_mut(new_next).unwrap().previous_node = source;

    nodes.get_mut(source).unwrap().previous_node = new_previous;
    nodes.get_mut(source).unwrap().next_node = new_next;
}

fn print_nodes(nodes: &Vec<Node>, from: usize, limit: usize) {
    let mut location = from;
    for (i, _) in (0..nodes.len()).take(limit).enumerate() {
        let node = nodes.get(location).unwrap();
        println!(
            "{} [{} {}] ->      ({} {})",
            i, location, node.value, node.previous_node, node.next_node
        );
        location = node.next_node;
    }
    println!();
}

fn iterate_from(nodes: &Vec<Node>, source: usize, index: usize) -> i64 {
    let smaller_index = index % nodes.len();
    let mut location = source;
    for _ in 0..smaller_index {
        location = nodes.get(location).unwrap().next_node;
    }

    return nodes.get(location).unwrap().value;
}

fn main() {
    let input = parse_input();

    let mut nodes: Vec<Node> = input
        .iter()
        .enumerate()
        .map(|(original_index, value)| Node {
            value: *value * KEY,
            next_node: (original_index + 1) % input.len(),
            previous_node: (original_index + input.len() - 1) % input.len(),
        })
        .collect();

    for _ in 0..ITERATIONS {
        for i in 0..nodes.len() {
            let (a, b) = find_locations(&nodes, i);
            update_locations(&mut nodes, a, b);
        }
    }
    let zero = nodes
        .iter()
        .enumerate()
        .find(|(_i, x)| x.value == 0)
        .unwrap();

    // print_nodes(&nodes, zero.0, usize::MAX);

    let k1 = iterate_from(&nodes, zero.0, 1000);
    let k2 = iterate_from(&nodes, zero.0, 2000);
    let k3 = iterate_from(&nodes, zero.0, 3000);
    println!("{} {} {}", k1, k2, k3);
    println!("{}", k1 + k2 + k3);

    // println!("{:?}", nodes)
}
