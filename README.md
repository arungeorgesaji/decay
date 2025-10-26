# decay
decay is an experimental, self-mutating programming language where code gradually deteriorates over time. This decay challenges developers to write resilient, self-healing programs and think critically about long-term program stability.

---

## Complete Language Features

### 1. Memory Types
Variables can be declared with different decay characteristics:

- **`stable`** - Persistent variables that resist decay
- **`volatile`** - Temporary variables that decay rapidly

```decay
stable permanentValue = 42      # Resists decay
volatile temporaryData = "temp" # Decays quickly
```

### 2. Function Declarations
Functions have speed modifiers that affect performance and decay:

- **`slow func`** - Reliable but slower execution, resists decay
- **`fast func`** - Fast execution but more prone to decay

```decay
slow func reliableFunction(x, y) {
    return x + y
}

fast func quickFunction(data) {
    volatile processed = data * 1.5
    return processed
}
```

### 3. Data Types

**Primitive Types:**
- **Integers**: `42`, `-17`, `0`
- **Floats**: `3.14`, `-0.5`, `2.0`
- **Strings**: `"hello"`, `"decay language"`
- **Booleans**: `true`, `false`

**Complex Types:**
- **Arrays**: `[1, 2, 3]`, `["a", "b", "c"]`

### 4. Operators

**Arithmetic Operators:**
```decay
+ - * /        # Addition, subtraction, multiplication, division
```

**Comparison Operators:**
```decay
== !=          # Equal, not equal
< > <= >=      # Less than, greater than, less/greater or equal
```

**Assignment:**
```decay
=              # Variable assignment
```

### 5. Control Flow

**Conditional Statements:**
```decay
if condition {
    # code block
} else {
    # alternative
}
```

**Loops:**
```decay
while running {
    # loop body
}
```

**Function Returns:**
```decay
return value
```

### 6. Comments
```decay
# This is a single-line comment
```

### 7. Syntax Elements

**Statement Terminators:**
- Semicolons `;` can be used to separate statements
- Newlines also separate statements

**Delimiters:**
- `()` - Function calls and parameter lists
- `{}` - Code blocks
- `[]` - Array literals and indexing
- `,` - Separator for parameters and array elements
- `.` - Member access (for future object support)

### 8. Maintenance Operations

- **`repair(target)`** - Fix decayed code sections
- **`reinforce(target)`** - Strengthen against future decay  
- **`accelerate(target)`** - Optimize performance (increases decay risk)

```decay
stable criticalData = 100

while true {
    repair(criticalData)      # Restore corrupted values
    reinforce(criticalData)   # Increase decay resistance
}
```

---

## Examples

### Variable Declaration and Usage
```decay
stable counter = 0
volatile temp = "processing"

counter = counter + 1
```

### Function Definition and Calls
```decay
slow func calculateSum(a, b) {
    stable result = a + b
    return result
}

volatile answer = calculateSum(10, 20)
```

### Control Flow
```decay
stable health = 100
volatile damage = 0

if health > 50 {
    volatile status = "healthy"
} else {
    volatile status = "critical"
    repair(health)
}

while health > 0 {
    health = health - damage
    if health < 20 {
        reinforce(health)
    }
}
```
---

## Decay Mechanics

### How Decay Works
1. **Time-based degradation**: Code quality decreases over execution time
2. **Variable corruption**: Values may become incorrect or unpredictable
3. **Function slowdown**: Performance degrades, execution becomes unreliable
4. **Memory leaks**: Volatile variables may persist unexpectedly
5. **Logic errors**: Control flow may behave unpredictably

### Decay Rates
- **`volatile`** constructs: Fast decay (seconds to minutes)
- **`fast func`**: Medium decay (minutes to hours)
- **Regular code**: Normal decay (hours)
- **`slow func`**: Slow decay (hours to days)
- **`stable`** constructs: Very slow decay (days to weeks)

### Maintenance Strategies
- Use `stable` for critical data that must persist
- Use `volatile` for temporary calculations only
- Mix `slow func` and `fast func` based on needs
- Regularly apply `repair()` to critical components
- Use `reinforce()` before long-running operations
- Avoid `accelerate()` unless performance is critical

**Challenge yourself to write code that survives its own decay.**
