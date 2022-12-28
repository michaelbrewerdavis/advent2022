use memoize::memoize;
use regex::{CaptureMatches, Captures, Regex};
use std::{
    collections::{HashMap, LinkedList},
    fs,
};

#[derive(Debug)]
struct Valve {
    id: String,
    flow_rate: u32,
    neighbors: Vec<String>,
}

fn parse_input() -> Vec<Valve> {
    // let input: String = fs::read_to_string("day16.sample").unwrap();
    let input: String = fs::read_to_string("day16.input").unwrap();

    let pattern =
        Regex::new(r"(?m)Valve (..) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)")
            .unwrap();

    //  (..) has flow rate=(\\d+); tunnels? lead? to valves? (.*)$
    let captures: CaptureMatches = pattern.captures_iter(&input);

    let mut values: Vec<Valve> = Vec::new();

    for capture in captures {
        let id = capture.get(1).unwrap().as_str().to_string();
        let flow_rate: u32 = capture.get(2).unwrap().as_str().parse::<u32>().unwrap();
        let neighbors = capture
            .get(3)
            .unwrap()
            .as_str()
            .split(", ")
            .map(|x| x.to_string())
            .collect();
        values.push(Valve {
            id,
            flow_rate,
            neighbors,
        });
    }
    return values;
}

fn distance(
    valves: &Vec<Valve>,
    from: &String,
    to: &String,
    depth: usize,
) -> Option<LinkedList<String>> {
    if from == to {
        return Some(LinkedList::from([]));
    } else if depth == valves.len() {
        return None;
    } else {
        let valve = valves.iter().find(|x| x.id == *from).unwrap();
        let neighbor_paths =
            valve
                .neighbors
                .iter()
                .map(|x| match distance(valves, x, to, depth + 1) {
                    Some(mut list) => {
                        list.push_front(x.to_string());
                        return list;
                    }
                    None => LinkedList::from([]),
                });
        return neighbor_paths.min_by(|left, right| left.len().cmp(&right.len()));

        // let neighbors = valve.neighbors.iter().map(|x| distance(x, to, depth + 1));
    }
}

fn main() {
    let VALVES: Vec<Valve> = parse_input();

    println!("{:?}", VALVES);

    let interesting_ids: Vec<&String> = VALVES
        .iter()
        .filter(|x| x.flow_rate > 0 || x.id == "AA")
        .map(|x| &x.id)
        .collect();
    println!("{:?}", interesting_ids);

    let mut paths: HashMap<&String, HashMap<&String, LinkedList<String>>> = HashMap::new();
    let mut distances: HashMap<&String, HashMap<&String, usize>> = HashMap::new();

    for id1 in interesting_ids.iter() {
        let mut inner_paths: HashMap<&String, LinkedList<String>> = HashMap::new();
        let mut inner_distances: HashMap<&String, usize> = HashMap::new();
        for id2 in interesting_ids.iter() {
            let path = distance(&VALVES, id1, id2, 0).unwrap();
            inner_distances.insert(id2, path.len());
            inner_paths.insert(id2, path);
        }
        paths.insert(id1, inner_paths);
        distances.insert(id1, inner_distances);
    }
    println!("{:?}", distances);
}
