"""Fibonacci number calculator."""

import sys


def fibonacci(n):
    """Return the nth Fibonacci number (0-indexed).

    fib(0) = 0, fib(1) = 1, fib(2) = 1, fib(3) = 2, ...
    """
    if n < 0:
        raise ValueError("n must be non-negative")
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b


def main():
    if len(sys.argv) != 2:
        print(f"Usage: python {sys.argv[0]} <n>")
        sys.exit(1)
    try:
        n = int(sys.argv[1])
    except ValueError:
        print("Error: argument must be an integer")
        sys.exit(1)
    print(f"fib({n}) = {fibonacci(n)}")


if __name__ == "__main__":
    main()
