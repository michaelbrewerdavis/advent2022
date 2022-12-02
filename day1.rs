use std::io;

fn main() -> io::Result<()> {
    let lines = io::stdin().lines();
    let mut current_total = 0;

    let mut totals = vec![];

    for rawline in lines {
        let line = rawline.unwrap();
        if line == "" {
            totals.push(current_total);
            current_total = 0;
        } else {
            let value = line.parse::<i32>().unwrap();
            current_total = current_total + value;
        }
    }
    totals.push(current_total);
    totals.sort();

    let biggest_total = totals[totals.len() - 1];
    println!("{}", biggest_total);

    let top3 = &totals[totals.len() - 3..totals.len()];
    let sum: i32 = top3.iter().sum();
    println!("{:?} {}", top3, sum);
    Ok(())
}
