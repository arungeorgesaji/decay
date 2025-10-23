# decay

A self-mutating programming language where code decays over time, challenging you to write resilient programs.

## Concept

**decay** is an experimental programming language that introduces the concept of *code decay* - your programs gradually deteriorate and require active maintenance to remain functional. This forces developers to write more robust, self-healing code and think about long-term program stability.

## Language Features

### Memory Types
- **`stable`** - Persistent variables that resist decay
- **`volatile`** - Temporary variables that decay rapidly

### Function Speeds
- **`slow func`** - Reliable but slower execution
- **`fast func`** - Fast but more prone to decay

### Maintenance Operations
- **`repair(target)`** - Fix decayed code sections
- **`reinforce(target)`** - Strengthen against future decay  
- **`accelerate(target)`** - Optimize performance (with risks)

## Language Syntax

### Variables

```decay
stable permanentValue = 42      # Resists decay
volatile temporaryData = "temp" # Decays quickly
```

### Functions

```decay
slow func reliableFunction(x, y) {
    # Slow but stable execution
    return x + y
}

fast func quickFunction(data) {
    # Fast but prone to errors
    volatile processed = data * 1.5
    return processed
}
```

### Control Flow

```decay
if condition {
    # code block
} else {
    # alternative
}

while running {
    # loop body
    repair(criticalVariable)
}
```

## Decay Mechanics

- Code gradually loses functionality over time
- Variables may corrupt or return unexpected values
- Functions can slow down or produce incorrect results
- Use maintenance operations to combat decay
- Stable constructs decay slower than volatile ones

**Challenge yourself to write code that survives its own decay.**
