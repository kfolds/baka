# baka: a programming language uwu

## example code

```
include std;

foo :: (bar: int) -> int {
    x := std.random.uniform();
    if (x > 0.5) {
        bar = bar + bar;
    }
    bar *= bar;
    return bar;
}

main :: (argc: int, argv: const u8[]) -> void {
    
}
```

## roadmap

- [ ] *lexing and parsing basic language*
- [ ] semantic analysis
- [ ] codegen with LLVM
- [ ] error unions/error sets
- [ ] compile-time code execution
- [ ] standard library
