# Compiler Stages and Module System Design

## Module System Approaches

### 1. Ciao's use_package/use_module distinction
- **Pros:**
  - Clean separation of compile-time extensions from runtime imports
  - Proven self-hosting capability
  - Explicit control over what loads when
  - Avoids circular dependency issues
- **Cons:**
  - More complex API - users must understand the distinction
  - Two concepts to learn instead of one
  - Less portable - other Prologs don't have use_package

### 2. SWI/Quintus traditional approach  
- **Pros:**
  - Familiar and portable across Prolog systems
  - Simple conceptual model
  - Well-documented and understood
- **Cons:**
  - Term expansion is an afterthought (module_transparent hacks)
  - Assumes dynamic clause database
  - No clean compile/runtime separation
  - Circular dependencies are problematic

### 3. ECLiPSe's progressive visibility resolution
- **Pros:**
  - Handles incremental compilation elegantly
  - Supports interactive development/REPL naturally
  - Allows partial module knowledge during compilation
  - Forward-only state transitions are predictable
- **Cons:**
  - Complex implementation
  - Still requires syntax decisions on top
  - More of an implementation strategy than user-facing design

### 4. Mercury's pure static approach
- **Pros:**
  - Clean, predictable semantics
  - Everything resolved at compile time
  - Maps perfectly to static module systems (ESM)
  - Efficient runtime
- **Cons:**
  - No term expansion/macros
  - Can't implement call/N with dynamic terms
  - Requires complete dependency graph upfront
  - Less flexible for interactive development

### 5. Unified use_module (load everything automatically)
- **Pros:**
  - Simplest possible user interface
  - No artificial distinctions to learn
  - Automatically handles term expansion
  - More "Prolog-like" - declarative what, not imperative how
- **Cons:**
  - Loads every dependency at compile-time (performance)
  - Can't resolve circular dependencies with expansions
  - Semantic confusion about when expansions apply
  - No precedent in successful languages

### 6. ESM + global DB hybrid (original rework proposal)
- **Pros:**
  - Solves all technical requirements
  - Enables proper call/N with term expansion
  - Supports dynamic and multifile predicates
  - Good for self-hosting
- **Cons:**
  - Conceptually complex - two resolution mechanisms
  - Novel and unproven
  - Hard to explain and document
  - May surprise users familiar with other systems

## Key Trade-offs

### Compile-time vs Runtime Loading
The fundamental question is whether modules that provide term expansions must be loaded during compilation. This creates a tension:
- Loading at compile-time enables term expansion but slows compilation
- Separating compile/runtime loading adds complexity but improves performance
- Automatic loading is simpler but can create dependency cycles

### Static vs Dynamic Resolution
- Static resolution (Mercury-style) is predictable and efficient but inflexible
- Dynamic resolution enables call/N and runtime metaprogramming but complicates compilation
- Progressive resolution (ECLiPSe) offers a middle ground

### Familiarity vs Innovation
- Using familiar syntax (SWI-style) aids adoption and porting
- Novel approaches may better fit modern compilation targets (JS, WASM)
- The best solution might combine familiar syntax with innovative implementation

## Implementation Considerations

### Self-hosting Requirements
For a self-hosted compiler, you need:
- Ability to load modules during compilation (for term_expansion)
- Clear phase separation between loading and compiling
- Bootstrap path from existing Prolog

### Target Language Constraints
Different backends impose different requirements:
- JavaScript/ESM wants static imports
- WASM might want even more static resolution
- Native targets might support more dynamic loading

### Term Expansion Semantics
Key questions that any design must answer:
- When do expansions from module A apply to module B?
- Can expanded code itself be expanded?
- How do you handle expansion conflicts?
- What about recursive expansions?

## Recommendation

**Use a two-phase approach with familiar syntax:**

1. Keep SWI-style `use_module/1` and `use_module/2` as the primary interface
2. Add `use_package/1` only for explicit compile-time extensions
3. Make `use_module` automatically load at compile-time IF the module exports term expansions (detected by the compiler)
4. Use ECLiPSe-style progressive visibility internally
5. Generate static ESM modules as output

This gives you:
- Familiar syntax for the common case
- Explicit control when needed (use_package)
- Automatic handling of term expansion
- Clean compilation model
- Flexibility for multiple backends

The key insight is that the compiler can detect which modules provide expansions and handle them specially, removing the burden from users in most cases while still allowing explicit control when needed.