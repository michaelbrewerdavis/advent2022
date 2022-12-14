use std::{
    cmp::{max, min},
    fmt, fs,
};

#[derive(Debug)]
struct Space {
    space: Vec<Vec<char>>,
}

impl Space {
    pub fn new(height: usize) -> Self {
        let space = vec![vec![' '; 1]; height];
        Space { space }
    }

    pub fn set(&mut self, pt: (usize, usize), c: char) {
        if self.space[pt.1].len() < pt.0 + 1 {
            self.space[pt.1].resize(2 * pt.0 + 1, ' ');
        }
        self.space[pt.1][pt.0] = c;
    }

    pub fn get(&self, pt: (usize, usize)) -> char {
        match self.space.get(pt.1) {
            None => 'X',
            Some(row) => match row.get(pt.0) {
                None => ' ',
                Some(value) => *value,
            },
        }
    }
}

impl fmt::Display for Space {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Use `self.number` to refer to each positional data point.
        for line in &self.space {
            write!(f, "|").unwrap();
            for c in line {
                write!(f, "{}", c).unwrap();
            }
            writeln!(f, "|").unwrap();
        }
        writeln!(f, "")
    }
}

fn main() {
    let _input: &str = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";
    let input: String = fs::read_to_string("./day14.input").unwrap();

    let walls: Vec<Vec<Vec<usize>>> = input
        .lines()
        .map(|l| {
            l.split(" -> ")
                .map(|x| x.split(",").map(|n| n.parse::<usize>().unwrap()).collect())
                .collect()
        })
        .collect();

    let max_y: usize = max(
        0,
        walls
            .iter()
            .map(|w| w.iter().map(|s| s[1]).max().unwrap())
            .max()
            .unwrap(),
    );

    let mut space = Space::new(max_y + 2);

    for wall in walls {
        let mut last = &wall[0];
        for segment in &wall[1..] {
            for x in min(last[0], segment[0])..=max(last[0], segment[0]) {
                for y in min(last[1], segment[1])..=max(last[1], segment[1]) {
                    space.set((x, y), '=');
                }
            }
            last = segment;
        }
    }

    let mut i = 0;

    'outer: loop {
        let mut snow = (500, 0);
        loop {
            if space.get(snow) == 'o' {
                break 'outer;
            }
            space.set(snow, 'o');

            let down = (snow.0, snow.1 + 1);
            let down_left = (snow.0 - 1, snow.1 + 1);
            let down_right = (snow.0 + 1, snow.1 + 1);

            if space.get(down) == ' ' {
                space.set(snow, ' ');
                snow = down;
            } else if space.get(down_left) == ' ' {
                space.set(snow, ' ');
                snow = down_left;
            } else if space.get(down_right) == ' ' {
                space.set(snow, ' ');
                snow = down_right;
            } else {
                break;
            }
        }
        i += 1;
    }
    println!("{}", i);
    // println!("{}", space);
}
