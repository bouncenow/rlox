# rlox
Tree walk interpreter of lox language described in the first part of Crafting Interpreters book (http://www.craftinginterpreters.com/a-tree-walk-interpreter.html)
Example program:
```
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

var before = clock();
print fib(20);
var after = clock();
print after - before;
```
Original implementation uses Java GC as a GC for the interpreter. Rust doesn't have a GC, and this interpreter has memory leaks:
`Rc`s and `RefCell`s created during program interpretation can create a cycle.
