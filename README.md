# The Fibonacci Ranch - Five Ways to Strike Gold

Well, howdy partner! Welcome to the biggest dang Fibonacci operation this side of the Pecos River. Now I know what you're thinkin' — "Why in tarnation would a man need FIVE different ways to calculate Fibonacci numbers?" And to that I say: same reason I got five oil derricks on my south forty. Because I CAN, and because DIVERSIFICATION is the name of the game, son.

Each one of these here implementations computes the nth Fibonacci number faster than a jackrabbit on a hot skillet, and every last one of 'em comes with tests — because a man who don't test his code is like a man who don't check his wells for pressure. Reckless.

## The Spread

```
python/        - The dependable workhorse. Like a good quarter horse — gets the job done.
lisp/          - Old money. Been around since before your granddaddy struck his first well.
javascript/    - Flashy, like a brand new Cadillac. Everybody's got one.
go/            - Built Ford tough. Fast, no-nonsense, runs like a diesel engine.
rust/          - The new steel on the ranch. Ain't nothin' gettin' past this one's safety checks.
```

---

## Python

Now Python here is my old reliable. Like a seasoned ranch hand — you tell it what to do, and it does it without no fuss.

**What you'll need:** Python 3.x (and if you ain't got Python on your machine, well, bless your heart)

**Fire it up:**
```bash
cd python
python fibonacci.py 10
# Output: fib(10) = 55   ...that's 55 barrels of pure Fibonacci crude, baby
```

**Make sure she runs true:**
```bash
cd python
python -m unittest test_fibonacci -v
```

---

## Common Lisp

Now I'll be honest with ya, pardner — Lisp is like fine bourbon. It's been around since 1958, most folks don't understand it, and the ones who do won't shut up about it. But I'll be damned if it don't get the job done with a certain... *elegance*.

**What you'll need:** [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp — and no, that ain't one of my banks)

Saddle up SBCL:
- **macOS:** `brew install sbcl`
- **Ubuntu/Debian:** `sudo apt install sbcl`
- **Arch:** `sudo pacman -S sbcl`

**Fire it up:**
```bash
cd lisp
sbcl --script fibonacci.lisp 10
# Output: fib(10) = 55   ...all them parentheses finally paid off
```

**Make sure she runs true:**
```bash
cd lisp
sbcl --script test_fibonacci.lisp
```

---

## JavaScript

JavaScript is like that nephew of mine who showed up to the family barbecue in a Tesla. Sure, it's everywhere and everybody's got an opinion on it, but you can't deny — it WORKS, and it works FAST.

**What you'll need:** [Node.js](https://nodejs.org/) (v12 or newer — we ain't runnin' no antiques here)

**Fire it up:**
```bash
cd javascript
node fibonacci.js 10
# Output: fib(10) = 55   ...that's liquid gold right there
```

**Make sure she runs true:**
```bash
cd javascript
node test_fibonacci.js
```

---

## Go

Now Go is a language after my own heart. No frills, no fancy decorations, just pure American efficiency. Like a good pickup truck — starts every time, hauls whatever you need, and don't complain about it neither.

**What you'll need:** [Go](https://go.dev/) (1.16 or newer)

**Fire it up:**
```bash
cd go
go run fibonacci.go 10
# Output: fib(10) = 55   ...steady as a pumpjack at sunrise
```

**Make sure she runs true:**
```bash
cd go
go test -v
```

---

## Rust

Rust is the newest addition to the ranch, and let me tell you — this thing is TIGHT. Memory safe, fast as all get-out, and it won't let you shoot yourself in the foot even if you TRY. It's like havin' a foreman who won't let nobody on the rig without their hard hat.

**What you'll need:** [Rust](https://www.rust-lang.org/tools/install) (install via `rustup`)

If you ain't got Rust yet, run this here incantation:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

**Fire it up:**
```bash
cd rust
cargo run -- 10
# Output: fib(10) = 55   ...tighter than a new pair of boots
```

**Make sure she runs true:**
```bash
cd rust
cargo test
```

---

*Now git on out there and calculate some Fibonacci numbers. And remember — in Texas, we don't just count to infinity. We count to infinity FIVE DIFFERENT WAYS.*
