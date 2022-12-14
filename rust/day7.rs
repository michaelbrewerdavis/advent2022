use std::{collections::HashMap, iter::Peekable, str::Lines};

#[derive(Debug)]
struct Command<'a> {
    command: &'a str,
    argument: Option<&'a str>,
    output: Vec<&'a str>,
}

struct CommandStruct<'a> {
    lines: Peekable<Lines<'a>>,
}

impl<'a> CommandStruct<'a> {
    fn new(lines: Lines<'a>) -> Self {
        Self {
            lines: lines.peekable(),
        }
    }

    fn read_output(&mut self) -> Vec<&'a str> {
        let mut args = [].to_vec();
        while self.lines.peek() != None && !is_command(self.lines.peek().unwrap()) {
            args.push(self.lines.next().unwrap())
        }
        args
    }
}

fn is_command(line: &str) -> bool {
    line.starts_with("$")
}

impl<'a> Iterator for CommandStruct<'a> {
    type Item = Command<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.lines.next();

        match value {
            None => None,
            Some(line) => {
                let fields: Vec<&str> = line.split(" ").collect();
                Some(Command {
                    command: fields.get(1).unwrap(),
                    argument: fields.get(2).copied(),
                    output: self.read_output(),
                })
            }
        }
    }
}

#[derive(Debug)]
struct Env<'a> {
    root: usize,
    nodes: Vec<Node<'a>>,
}

impl<'a> Env<'a> {
    fn new() -> Self {
        let root_node = Node::new(0);
        let mut nodes = Vec::new();
        nodes.push(root_node);

        Env { root: 0, nodes }
    }
}

#[derive(Debug)]
struct Node<'a> {
    parent: usize,
    subdirs: HashMap<&'a str, usize>,
    files: HashMap<&'a str, u32>,
}

impl<'a> Node<'a> {
    fn new(parent: usize) -> Self {
        Node {
            parent,
            subdirs: HashMap::new(),
            files: HashMap::new(),
        }
    }
}

type Tracker<'a, 'b> = (&'a mut Env<'b>, usize);

fn change_directory<'b, 'a: 'b>(tracker: Tracker<'a, 'b>, dir: &'a str) -> Tracker<'a, 'b> {
    let (env, index) = tracker;
    let mut new_index;

    if dir == ".." {
        let node = env.nodes.get(index).unwrap();
        new_index = node.parent;
    } else {
        let nodes = &mut env.nodes;
        new_index = nodes.len();

        let new_node = Node::new(index);

        let child_index = nodes
            .get_mut(index)
            .unwrap()
            .subdirs
            .entry(dir)
            .or_insert(new_index);

        if *child_index == new_index {
            nodes.push(new_node)
        }
        new_index = *child_index;
    }
    println!("adding {}, {:?} {:?}", dir, env, new_index);
    return (env, new_index);
}

fn main() {
    let input = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    let lines = input.lines();

    let commands = CommandStruct::new(lines);
    let mut env = Env::new();

    {
        {
            let mut current = (&mut env, 0);
            for command in commands {
                println!("a line {:?} {:?}", command, current);

                if command.command == "cd" {
                    current = change_directory(current, command.argument.unwrap());
                }
                if command.command == "ls" {
                    println!("saw ls");
                }

                println!("b line {:?} {:?}", command, current);
            }
        }
    }

    println!("{:?}", env)
}
