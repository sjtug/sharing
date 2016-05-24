enum Op {
    Add(i32),
    Sub(i32),
    Mul(i32),
    Div(i32),
}

fn main() {
    let x = 1;
    // x = 2;
    let mut y: i32 = 3;
    y = 4;
    let x = "Shadowed!"; // Bindings may be shadowed
    println!("{} {}", x, y);

    // you can use pattern matching in let
    let (a, b) = ("hello", 42);
    println!("{} {}", a, b);

    // Array
    let arr: [i32; 10] = [0; 10];
    println!("{:?}", arr);

    // no implicit conversion
    // println!("{}", arr[y]);
    println!("{}", arr[y as usize]);

    // everything is an expression: something which returns a value.
    3;

    // Control Flow
    if y > 0 {
        println!("positive");
    } else {
        println!("not positive");
    }

    while y > 0 {
        y -= 1;
        println!("y: {}", y);
    }

    /*
     * loop {
     *     println!("looping");
     * }
     */

    for elem in &arr {
        println!("elem: {}", elem);
    }

    // Pattern matching
    match Op::Add(10) {
        Op::Add(n) => println!("add: {}", n),
        Op::Sub(n) => println!("sub: {}", n),
        Op::Mul(n) => println!("mul: {}", n),
        Op::Div(n) => println!("div: {}", n),
    }
    
}
