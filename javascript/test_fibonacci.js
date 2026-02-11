/**
 * Tests for fibonacci module.
 */

const { fibonacci } = require("./fibonacci");

let passed = 0;
let failed = 0;

function assertEqual(expected, actual, description) {
  if (expected === actual) {
    console.log(`  PASS: ${description}`);
    passed++;
  } else {
    console.log(`  FAIL: ${description} (expected ${expected}, got ${actual})`);
    failed++;
  }
}

function assertThrows(fn, description) {
  try {
    fn();
    console.log(`  FAIL: ${description} (expected an error)`);
    failed++;
  } catch (e) {
    console.log(`  PASS: ${description}`);
    passed++;
  }
}

console.log("Running Fibonacci tests...\n");

console.log("Base cases:");
assertEqual(0, fibonacci(0), "fib(0) = 0");
assertEqual(1, fibonacci(1), "fib(1) = 1");

console.log("\nSmall values:");
assertEqual(1, fibonacci(2), "fib(2) = 1");
assertEqual(2, fibonacci(3), "fib(3) = 2");
assertEqual(3, fibonacci(4), "fib(4) = 3");
assertEqual(5, fibonacci(5), "fib(5) = 5");
assertEqual(8, fibonacci(6), "fib(6) = 8");

console.log("\nLarger values:");
assertEqual(55, fibonacci(10), "fib(10) = 55");
assertEqual(6765, fibonacci(20), "fib(20) = 6765");
assertEqual(832040, fibonacci(30), "fib(30) = 832040");

console.log("\nError handling:");
assertThrows(() => fibonacci(-1), "fib(-1) throws error");

console.log(`\nResults: ${passed} passed, ${failed} failed`);
process.exit(failed === 0 ? 0 : 1);
