fn main() {
    // Ownership & move semantics
    let x = vec![1, 2, 3];
    let mut y = x;
    // println!("{:?}", x);
    println!("{:?}", y);

    let a = 2;
    let b = a;
    println!("{} {}", a, b);

    {
        let y1 = &y;
        let y2 = &y;
    }

    {
        let y1 = &mut y;
        let y2 = &y;
    }
}
