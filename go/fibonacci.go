// Package main provides a Fibonacci number calculator.
package main

import (
	"fmt"
	"os"
	"strconv"
)

// Fibonacci returns the nth Fibonacci number (0-indexed).
// fib(0) = 0, fib(1) = 1, fib(2) = 1, fib(3) = 2, ...
func Fibonacci(n int) (int, error) {
	if n < 0 {
		return 0, fmt.Errorf("n must be non-negative")
	}
	if n <= 1 {
		return n, nil
	}
	a, b := 0, 1
	for i := 2; i <= n; i++ {
		a, b = b, a+b
	}
	return b, nil
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s <n>\n", os.Args[0])
		os.Exit(1)
	}
	n, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error: argument must be an integer")
		os.Exit(1)
	}
	result, err := Fibonacci(n)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("fib(%d) = %d\n", n, result)
}
