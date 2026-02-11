/// Calculate the nth Fibonacci number (0-indexed).
///
/// fib(0) = 0, fib(1) = 1, fib(2) = 1, fib(3) = 2, ...
fn fibonacci(n: u64) -> u64 {
    if n <= 1 {
        return n;
    }
    let mut a: u64 = 0;
    let mut b: u64 = 1;
    for _ in 2..=n {
        let temp = b;
        b = a + b;
        a = temp;
    }
    b
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <n>", args[0]);
        std::process::exit(1);
    }
    let n: u64 = match args[1].parse() {
        Ok(v) => v,
        Err(_) => {
            eprintln!("Error: argument must be a non-negative integer");
            std::process::exit(1);
        }
    };
    println!("fib({}) = {}", n, fibonacci(n));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base_cases() {
        assert_eq!(fibonacci(0), 0);
        assert_eq!(fibonacci(1), 1);
    }

    #[test]
    fn test_small_values() {
        assert_eq!(fibonacci(2), 1);
        assert_eq!(fibonacci(3), 2);
        assert_eq!(fibonacci(4), 3);
        assert_eq!(fibonacci(5), 5);
        assert_eq!(fibonacci(6), 8);
    }

    #[test]
    fn test_larger_values() {
        assert_eq!(fibonacci(10), 55);
        assert_eq!(fibonacci(20), 6765);
        assert_eq!(fibonacci(30), 832040);
    }
}
