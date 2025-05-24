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

## Possible Directive Interfaces

### Option 1: Qualifier syntax
```prolog
:- use_module(library(lists)).          % Both phases (default)
:- use_module(library(dcg), [runtime]). % Runtime only
:- use_module(library(macro), [compile]). % Compile only
```

### Option 2: Separate directives
```prolog
:- use_module(library(lists)).          % Both (compat)
:- compile_with(library(macro)).        % Compile-time only
:- import_module(library(pure_data)).   % Runtime only
```

### Option 3: Property syntax
```prolog
:- use_module(library(lists)).                    % Both
:- use_module(library(data)) as runtime_only.     % Runtime only  
:- use_module(library(macro)) as compile_only.    % Compile only
```

### Option 4: Nested directive
```prolog
:- use_module(library(lists)).          % Both
:- runtime_only use_module(library(x)). % Runtime only
:- compile_only use_module(library(y)). % Compile only
```

### Option 5: JavaScript-inspired names
```prolog
:- use_module(library(lists)).          % Both phases (backwards compatible)
:- require(library(macros)).            % Compile-time only (like Node.js require)
:- import(library(runtime)).            % Runtime only (like ES6 import)
```

## Recommendation

**Use Option 5 (JavaScript-inspired names) with a two-phase approach:**

1. `use_module/1,2` - Loads at both compile-time and runtime (backwards compatible)
2. `require/1,2` - Compile-time only (for macros/term expansion)
3. `import/1,2` - Runtime only (for pure runtime dependencies)

This gives you:
- Backwards compatibility by default
- Clear, distinct names that map to JS concepts
- No syntactic surprises
- Each directive does one thing

Example usage:
```prolog
:- require(library(macros)).    % "I require this for compilation"
:- import(library(runtime)).    % "Import this at runtime"
:- use_module(library(both)).   % "Use this module" (both phases)
```

Combined with ECLiPSe-style progressive visibility internally and static ESM output, this provides a clean, understandable model that scales from simple to complex use cases.
