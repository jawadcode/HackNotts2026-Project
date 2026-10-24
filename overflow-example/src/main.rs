fn main() {
    let x = i64::MAX;
    println!("{}", add_one(x));
}

fn add_one(x: i64) -> i64 {
    x + 1
}
