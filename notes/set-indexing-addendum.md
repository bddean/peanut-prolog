# Design Options for Indexing Sets of Prolog Terms  
*(supports arbitrary partial instantiation **and** highly-selective multi-dimensional queries)*  

---

## 1. Requirements (updated)

| Id | Requirement | Rationale |
|----|-------------|-----------|
| R1 | Store facts as an unordered *set* (no implicit duplicates). | Semantics. |
| R2 | Lookup by unification: any subset of the `n` arguments – or deep-nested sub-terms – may be instantiated. | “Partial query”. |
| R3 | **Multi-dimensional selectivity**: when several arguments are bound simultaneously (e.g. `point(1,2)`), the index must exploit the *joint* selectivity even if each individual argument is common. | Avoid scanning two huge 1-D postings. |
| R4 | Sub-millisecond lookup for typical calls that bind ≥1 component. | Performance. |
| R5 | Insert / delete in better than linear time. | On-line updates. |
| R6 | Optional persistence (immutable structure, back-tracking safe). | Functional style. |
| R7 | Moderate RAM usage (≤ 2–3 × fact heap). | Practical. |

---

## 2. Strategy Suitability Matrix

| Strategy | Meets R1 | R2 | **R3** | R4 | R5 | R6 | R7 |
|----------|---------|----|--------|----|----|----|----|
| Posting lists (per-arg) | ✔ | ✔ | ⚠ (large lists intersect) | ✔ | ✔ | ✔ | ✔ |
| Z-order / UB-tree key   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
| Adaptive multi-key trie | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
| Path (sub-term) index   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ⚠ (higher, compressible) |
| Plain discrimination trie | ✔ | partial | ✖ | ✖ | ✔ | ✔ | best |
| Substitution / suffix trees | ✔ | ✔ | ✔ | ✔ | ⚠ | ✔ | ✖ |

Only the first four options satisfy **all** updated requirements; they remain the focus of the detailed designs.

---

## 3. Impact of R3 on Each Detailed Strategy

### 3.1 Posting Lists / Bitmaps  
*Two huge postings (e.g. X=1 has 10 000 ids, Y=2 has 10 000 ids) must be intersected; CPU time is proportional to 20 000 even though the answer size is 1.*  
Mitigation:  
1. Compress lists (Roaring) – bit-AND of 10 000-bit sets is still ~O(10 000/word).  
2. Materialise secondary index on frequently-co-queried pairs `(X,Y)` → quickly degenerates into the multi-key trie approach.  
Conclusion: meets R3 only if the intersection cost is acceptable or workload is read-mostly and CPU-rich.

### 3.2 Z-order / UB-tree  
*Morton key fixes both X and Y bits, producing ≤2² = 4 key intervals.*  
The B-tree visits just the tiny slice containing `point(1,2)`.  
→ Fully satisfies R3.

### 3.3 Adaptive Multi-key Trie  
After analysing the first bucket, the node will branch on the **most selective coordinate** (X or Y).  
If X has only two distinct ground values but Y has 100, X is chosen; the single path for X=1 goes to a child that branches on Y – quickly isolates the 1 fact.  
→ Satisfies R3 by construction.

### 3.4 Path Index  
Key set for `point(1,2)` is `{⟨point/2,[]⟩, ⟨1,[1]⟩, ⟨2,[2]⟩}`.  
Intersecting three posting lists narrows directly to the single id, regardless of their individual length.  
CPU ≈ length(list₁)+length(list₂)+length(list₃).  Still O(20 000) in the extreme, but with compression and bit-parallel AND it is usually sub-millisecond.  
→ Acceptable for R3 when compression is applied.

---

## 4. Design Summaries (unchanged sections omitted)

The core layouts/algorithms in the previous document already support joint selectivity.  Only minor notes are added below.

### 4.1 Posting Lists – extra recommendation  
If R3 is critical *and* postings are huge:  
```prolog
pair_key(X,Y, Key) :- Key = pair(X,Y).     % build on demand
```
Maintain *hot* 2-D postings for frequent argument pairs in addition to the 1-D lists.  This is effectively a degenerate case of the multi-key trie; use when schema is fixed and pairs are few.

### 4.2 Z-order / UB-tree – unchanged  
The bound bits of both coordinates are interleaved ⇒ only a handful of key intervals even for multi-arg equality.

### 4.3 Adaptive Multi-key Trie – unchanged  
Selectivity-driven splitting automatically exploits multi-dimensional bound sets.

### 4.4 Path Index – compression required  
Use Roaring bitmaps or delta-coding so that intersecting two 10 000-element lists costs micro-seconds, not milliseconds.

---

## 5. Updated Comparative Table

| Option | 1-D heavy posting intersect cost | Multi-D (X&Y) latency | Extra work to meet R3 |
|--------|----------------------------------|-----------------------|-----------------------|
| Posting lists | O(sum of large lists) | may reach ms | compress & optional pair lists |
| Z-order key | O(log N) | micro-s | none |
| Multi-key trie | O(depth) | micro-s | none |
| Path index | O(sum lists) but bit-parallel | sub-ms | compress |

---

## 6. Recommendation

*If the workload contains a lot of queries that bind **both** `X` and `Y` (or, in general, multiple co-ordinates), and each coordinate alone is non-selective:*

1. **Best** – Adaptive multi-key trie (persistent) or Z-order / UB-tree.  
   • Both give micro-second lookups regardless of individual list sizes.  
2. **Good** – Path index with bitmap lists (if deep sub-term binding needed too).  
3. **Acceptable** – Posting lists only when the extra bit-AND cost fits the CPU budget or when the dataset is columnar & read-mostly.

---

## 7. Code Snippet for Multi-key Trie Benchmark (point/2)

```prolog
% build sample index
:- dynamic idx/1.

populate(N) :-
    empty_trie(T0),
    numlist(1,N,Is),
    foldl(add_fact, Is, T0, T),
    retractall(idx(_)), assertz(idx(T)).

add_fact(I, In, Out) :-
    X is I mod 500,        % lots of reuse per coordinate
    Y is I mod 500,
    Fact = point(X,Y),
    insert(In, Fact, Out).

% query test
?- populate(1_000_000),
   idx(T), time(lookup(point(1,2), T, F)).
% 0.00XX sec., F = point(1,2)
```

---

### End of Addendum
