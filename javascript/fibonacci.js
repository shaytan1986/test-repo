/**
 * Fibonacci number calculator.
 */

function fibonacci(n) {
  if (n < 0) {
    throw new RangeError("n must be non-negative");
  }
  if (n <= 1) return n;
  let a = 0, b = 1;
  for (let i = 2; i <= n; i++) {
    [a, b] = [b, a + b];
  }
  return b;
}

// CLI entry point
if (require.main === module) {
  const args = process.argv.slice(2);
  if (args.length !== 1) {
    console.error(`Usage: node ${process.argv[1]} <n>`);
    process.exit(1);
  }
  const n = parseInt(args[0], 10);
  if (isNaN(n)) {
    console.error("Error: argument must be an integer");
    process.exit(1);
  }
  console.log(`fib(${n}) = ${fibonacci(n)}`);
}

module.exports = { fibonacci };
