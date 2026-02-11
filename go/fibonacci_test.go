package main

import "testing"

func TestBaseCase(t *testing.T) {
	tests := []struct {
		n, want int
	}{
		{0, 0},
		{1, 1},
	}
	for _, tc := range tests {
		got, err := Fibonacci(tc.n)
		if err != nil {
			t.Errorf("Fibonacci(%d) returned error: %v", tc.n, err)
		}
		if got != tc.want {
			t.Errorf("Fibonacci(%d) = %d, want %d", tc.n, got, tc.want)
		}
	}
}

func TestSmallValues(t *testing.T) {
	tests := []struct {
		n, want int
	}{
		{2, 1},
		{3, 2},
		{4, 3},
		{5, 5},
		{6, 8},
	}
	for _, tc := range tests {
		got, err := Fibonacci(tc.n)
		if err != nil {
			t.Errorf("Fibonacci(%d) returned error: %v", tc.n, err)
		}
		if got != tc.want {
			t.Errorf("Fibonacci(%d) = %d, want %d", tc.n, got, tc.want)
		}
	}
}

func TestLargerValues(t *testing.T) {
	tests := []struct {
		n, want int
	}{
		{10, 55},
		{20, 6765},
		{30, 832040},
	}
	for _, tc := range tests {
		got, err := Fibonacci(tc.n)
		if err != nil {
			t.Errorf("Fibonacci(%d) returned error: %v", tc.n, err)
		}
		if got != tc.want {
			t.Errorf("Fibonacci(%d) = %d, want %d", tc.n, got, tc.want)
		}
	}
}

func TestNegativeReturnsError(t *testing.T) {
	_, err := Fibonacci(-1)
	if err == nil {
		t.Error("Fibonacci(-1) should return an error")
	}
}
