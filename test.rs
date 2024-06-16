// GLOBAL
static s: i32 = 100;
const c: f64 = 100.1;

fn main(){

    //BOOL
    let b1 = true;
    let mut b2 = false;
    let b3: bool = true;
    let mut b4: bool = false;
    b4 = false;

    //INT
    let i1 = 1;
    let mut i2 = 2;
    let i3: i32 = 3;
    let mut i4: i32 = 4;
    i4 = 0;

    //DOUBLE
    let d1 = 1.1;
    let mut d2 = 2.2;
    let d3: f32 = 3.3;
    let mut d4: f32 = 4.4;
    d4 = 0.0;

    //STRING
    let s1 = "1";
    let mut s2 = "2";
    let s3: String = "3";
    let mut s4: String = "4";
    s4 = "";

    //ARRAY
    let a1 = [1];
    let mut a2: [i32; 1] = [0];
    let a3 = [2, 3];
    let mut a4 = [3; 5];
    a4 = 0;

    //BUILT-IN FUNCTION
    println!("BUILT-IN FUNCTION \n");
    println!("1: {}\n", 1);
    println!("i5: {}\n", d1);
    println!("foo: {}\n", foo(1, 2));    
    let r = read!("1");
    println("Read: {}\n", r);
    let f = format!("{} {}", 1, 2.2);
    println("Format: {}\n", f);

    //OPERATION
    println!("\nOperation \n");
    println!("Mul: {}\n", 1 * 1);
    println!("Sum: {}\n", 1 + 1);
    println!("Div: {}\n", 1 / 1);
    println!("Minus: {}\n", 1 - 1);

    let a = 5;
    let b = 10;
    println!("Less Than: {}\n", a < b);
    println!("Greater Than: {}\n", a > b);
    println!("And: {}\n", 1 && 1);
    println!("Or: {}\n", 1 || 1);

    //Control Statements
    println!("\nControl Statements \n");

    let mut n = 0;
    while n < 3 {
        println!("{}\n", n);
        n = n + 1;
    }

    println!("\n");
    for n in 0..=3 {
        println!("{}\n", n);
    }

    println!("\n");
    for n in 0..=3 {
        println!("{}\n", n);
    }    

    println!("\n");
    if n < 0 {
        println!("{} is negative\n", n);
    } else if n > 0 {
        println!("{} is positive\n", n);
    } else {
        println!("{} is zero\n", n);
    }

    //Error Checking
    // z = 1; // Undeclare
    // i1 = 1; // Mutability
    //let i1; // No Type
    //let i1 = 1; //Redelcartion
    // i2 = 1.1; // Type
    //i1(); // Varible use as function

    
    //read!("1", "2"); // Number of parameters
    // foo(1.1, 2.2); // Paramter Type

    //println[0]; // Function use as array

    //let mut a: [i32; 1.1]; // Size is not int
    //let mut a = [3; 5.5]; //Size is not int
    //let mut a: [i32; 1] = [1.1]; // Mismatch Type

    //a2[10]; // Array Index 
    //a2(); // Array as function
}

fn foo(x: i32, y: i32) -> i32 {
    return x + y;
}