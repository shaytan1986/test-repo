# Fibonacci Calculator - 5 Languages

Fibonacci number calculators implemented in Python, Common Lisp, JavaScript, Go, and Rust. Each implementation computes the nth Fibonacci number (0-indexed) using an iterative approach, and includes tests.

## Project Structure

```
python/        - Python implementation
lisp/          - Common Lisp implementation (SBCL)
javascript/    - JavaScript implementation (Node.js)
go/            - Go implementation
rust/          - Rust implementation (Cargo)
```

---

## Python

**Prerequisites:** Python 3.x

**Run:**
```bash
cd python
python fibonacci.py 10
# Output: fib(10) = 55
```

**Test:**
```bash
cd python
python -m unittest test_fibonacci -v
```

---

## Common Lisp

**Prerequisites:** [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)

Install SBCL:
- **macOS:** `brew install sbcl`
- **Ubuntu/Debian:** `sudo apt install sbcl`
- **Arch:** `sudo pacman -S sbcl`

**Run:**
```bash
cd lisp
sbcl --script fibonacci.lisp 10
# Output: fib(10) = 55
```

**Test:**
```bash
cd lisp
sbcl --script test_fibonacci.lisp
```

---

## JavaScript

**Prerequisites:** [Node.js](https://nodejs.org/) (v12+)

**Run:**
```bash
cd javascript
node fibonacci.js 10
# Output: fib(10) = 55
```

**Test:**
```bash
cd javascript
node test_fibonacci.js
```

---

## Go

**Prerequisites:** [Go](https://go.dev/) (1.16+)

**Run:**
```bash
cd go
go run fibonacci.go 10
# Output: fib(10) = 55
```

**Test:**
```bash
cd go
go test -v
```

---

## Rust

**Prerequisites:** [Rust](https://www.rust-lang.org/tools/install) (install via `rustup`)

Install Rust:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

**Run:**
```bash
cd rust
cargo run -- 10
# Output: fib(10) = 55
```

**Test:**
```bash
cd rust
cargo test
```
