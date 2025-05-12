# Design Options for Indexing Sets of Prolog Terms  
*(supporting arbitrary partial instantiation queries)*  

---

## 1. Requirements

| Id | Requirement | Rationale |
|----|-------------|-----------|
| R1 | Treat the fact set as an unordered *set* (no duplicates unless allowed explicitly). | Matching semantics. |
| R2 | Lookup by unification: any subset of the `n` arguments – or even deep-nested sub-terms – may be instantiated. | “Partial query” capability. |
| R3 | Sub-millisecond lookup for typical calls that instantiate at least one component. | Performance. |
| R4 | Insert / delete in better than linear time. | On-line updates. |
| R5 | Optional persistence (immutable structure, easy back-tracking). | Purely functional style. |
| R6 | Moderate RAM usage (≤ 2–3× fact heap). | Practical deployment. |

---

## 2. Strategy Landscape

| Strategy | Meets R1–R4? | Memory | Update | Comment |
|----------|--------------|--------|--------|---------|
| Plain discrimination trie | ✔ R1,R4 | best | O(size(term)) | Fails R2,R3 when early args are unbound. |
| Per-argument hash + AND | ✔ | good | O(#args)| Great for low cardinality; deep paths not covered. |
| Z-order / UB-tree key | ✔ | good | O(log N) | All args symmetric;  ≤2ᵏ range scans. |
| Adaptive multi-key trie | ✔ | good | O(depth·log F) | Chosen arg per node ⇒ “kd-tree for terms”. |
| Path (sub-term) indexing | ✔ | higher* | O(size(term)) | Any depth; mitigations keep RAM moderate. |
| Substitution / suffix tree | partial | high | costly | Heavyweight; not pursued. |

\* Path index memory can be compressed (see §4.2).

The remainder of the document gives full designs for the four strategies that **satisfy every requirement (R1–R4)**; other candidates are summarised only.

---

## 3. Detailed Option A – Per-Argument Posting Lists

### 3.1 Data structure
```
arg_i_value(V) -> sorted list / Roaring bitmap of clause-IDs
```
Plain integer keys (`assoc` or `dict`) for each argument position.

### 3.2 Operations
```
lookup(Pattern)        → intersect lists of all bound arguments
insert(Fact, Id)       → add Id to every list touched
delete(Fact, Id)       → remove Id (lazy: tombstone + periodic GC)
```

### 3.3 Complexity
* Lookup:  O(Σ|listᵢ| + |result|)  
* Insert/delete:  O(#arguments)  
* Memory:  one bit/ID in compressed form (≈ 0.1–0.3 byte)

### 3.4 Pros / Cons
+ Very simple, great for analytics, trivially persisted  
− Intersection time grows with list length; deep sub-terms need explicit normalisation.

---

## 4. Detailed Option B – Z-order (UB-tree / PH-tree) Key

### 4.1 Key construction
1. Hash each argument to a fixed `w`-bit integer.  
2. Interleave the bits (Morton/Z-order) ⟶ 1-D key of length `n·w`.  

Example for 3 args × 8 bits:  
`x7 y7 z7 x6 y6 z6 … x0 y0 z0`

### 4.2 Index
A standard B-tree or trie on the 1-D key.

### 4.3 Query algorithm  
For every bound argument set its bit pattern, leave the others “wild”; produce ≤ 2ᵏ key intervals (k = #bound args), run a range scan for each.

### 4.4 Complexity  
* ≤ 2ᵏ · (log B + output) page accesses.  
* Insert/delete: normal B-tree cost (log B).

### 4.5 Properties
+ All arguments perfectly symmetric; “early-bit bias” does **not** blow up work.  
+ Good up to 10–15 arguments, moderate updates.  
− Values must be hashed/fixed length; range semantics only approximate equality on the hash.

### 4.6 Code snippet (key generator, SWI)
```prolog
hash_arg(Arg, H) :- term_hash(Arg, H0), H is H0 /\ 0xFFFF.   % 16-bit
morton_key(Term, Key) :-
    Term =.. [_|Args],
    maplist(hash_arg, Args, Hashes),
    interleave(Hashes, Key).

interleave([H|Hs], Key) :- interleave(Hs, 0, 0, Key, H).
interleave([], _, Acc, Acc, _).
interleave([H|Hs], Bit, Acc0, Key, Prev) :-
    forall(between(0,15,B),
           (Bit1 is Bit+3*B,
            Val  is ((Prev>>B) /\ 1) << Bit1,
            Acc1 is Acc0 \/ Val)),
    interleave(Hs, Bit+1, Acc1, Key, H).
```
(The production implementation would use C for speed.)

---

## 5. Detailed Option C – Adaptive Multi-Key Trie (functional)

### 5.1 Node layout (persistent)
```prolog
leaf(Size, Facts).                       % bucket ≤ BucketLimit
node(SplitPos, ChildrenDict, Size).      % Children: Value → SubTrie, VAR → ...
```

### 5.2 Split policy  
`best_split_pos/2` selects the argument with the highest number of **distinct ground values** in the bucket – the analogue of “most selective dimension” in a kd-tree.

### 5.3 Algorithms  

Lookup
```prolog
lookup(Pat, leaf(_,Facts), F) :-
    member(F, Facts), F = Pat.
lookup(Pat, node(Pos,Ch,_), F) :-
    arg(Pos, Pat, A),
    ( nonvar(A), get_dict(A,Ch,Sub) ; get_dict(VAR,Ch,Sub) ),
    lookup(Pat, Sub, F).
```

Functional insert (path-copy)
```prolog
insert(Old, Fact, New) :- insert1(Old, Fact, New, _).

insert1(leaf(N,L), F, Out, N1) :-
    ( memberchk(F, L) -> Out=leaf(N,L), N1=N
    ; N1 is N+1,
      ( N1 =< BucketLimit
      -> Out = leaf(N1, [F|L])
      ; best_split_pos([F|L],P),
        bucket_to_children(P, [F|L], D),
        Out = node(P,D,N1)
      )
    ).
insert1(node(P,Ch,N), F, node(P,Ch1,N1), N1) :-
    arg(P, F, A), choose_key(A, K),
    get_dict(K,Ch,Sub0),
    insert1(Sub0, F, Sub1, S1),
    put_dict(K, Ch, Sub1, Ch1),
    N1 is N+1.

choose_key(A, K) :- (var(A) -> K = VAR ; K = A).
```

Delete is symmetric.

### 5.4 Complexity
* Lookup: O(depth + answers)  
* Insert/Delete: O(depth·log fanout) new cells  
* Memory: each update copies ≤ depth nodes; 90 %+ sharing across versions.

### 5.5 Pros / Cons
+ No argument order bias; good incremental behaviour; natural persistence.  
+ Pure Prolog implementation (dict/assoc) possible.  
− Slightly more code; occasional rebalancing advisable.

---

## 6. Detailed Option D – Path (Sub-term) Indexing

### 6.1 Key  
`⟨Functor/Arity, PathVector⟩`  
Path = list of argument positions needed to reach the sub-term.

### 6.2 Storage  
* **Key trie** (alternating functor/arity, integer).  
* **Posting list** for each key (sorted ids → delta-encoded or Roaring bitmap).

### 6.3 Operations  
1. Insertion: walk term once, add id to each posting list.  
2. Lookup: extract every *bound* sub-term of the query, retrieve lists, intersect them.  
3. Deletion: symmetrical.

### 6.4 Complexity  
* Insert: O(size(term))  
* Lookup: O(Σ|listᵢ| + |∩|)  
* Memory: O(#ground symbols) with compression (≈ 1 byte / occurrence achievable).

### 6.5 Compression tricks  
1. Gap coding / VarInt.  
2. Roaring / WAH bitmaps.  
3. Deduplicate identical lists (“.”, `[]`, etc.).  
4. Lazy materialisation of never-used paths.  
5. Depth/arity cut-off.

### 6.6 Code excerpt (Prolog)
```prolog
:- use_module(library(assoc)).

insert(Term, Id, In, Out) :-
    term_paths(Term, [], Keys),
    foldl(add_id(Id), Keys, In, Out).

add_id(Id, K, A0, A) :-
    ( get_assoc(K, A0, L) -> true ; L = [] ),
    put_assoc(K, A0, [Id|L], A).

lookup(Pat, Index, Candidates) :-
    term_paths(Pat, [], Keys0),
    include(bound_key, Keys0, Keys),
    findall(L, (member(K,Keys), get_assoc(K,Index,L)), Lists),
    intersect_many(Lists, Candidates).
```

---

## 7. Other Strategies (short notes)

* **Plain discrimination trie** – perfect when the first few arguments are always bound but fails otherwise; minimal memory.  
* **Substitution trees / suffix tries** – powerful but memory-heavy; preferred by first-order theorem provers, not lightweight enough for our use-case.  
* **External R-tree / X-tree via tabling** – only useful for numeric coordinate data; skipped.

---

## 8. Comparative Table

| Option | Lookup bias | Deep value support | Update cost | RAM (w/ compression) | Persistence friendly |
|--------|-------------|--------------------|-------------|----------------------|----------------------|
| Posting lists | none | via normalisation | O(n) | low | yes |
| Z-order key  | none | fixed depth only | log N | low | yes |
| Multi-key trie| none | fixed depth | depth·log F | low-med | yes |
| Path index | none | unlimited | size(term) | med (8–12 B/symbol) | yes |

---

## 9. Recommendation Matrix

| Scenario | Recommended index |
|----------|------------------|
| ≤3 arguments, always shallow | plain discrimination trie |
| 4–15 args, at least one bound per query, updates often | **Adaptive multi-key trie** |
| Many args, read-mostly, low cardinality | per-argument posting lists |
| Need to bind arbitrary deep sub-terms | **Path index** (plus posting-list compression) |
| Mixed numeric dimensions, external DB | Z-order / UB-tree |

---

## 10. Implementation Plan (for Adaptive Multi-Key Trie)

1. **Prototype** (SWI) with `dict` child maps; bucket size = 8.  
2. **Unit-test** insert/delete/lookup incl. back-tracking safety.  
3. **Benchmark** on synthetic workloads; calibrate `best_split_pos/2`.  
4. **Add persistence layer**: root held in `nb_setval/2`; readers use value snapshot.  
5. **Optional**: migrate child maps to persistent HAMT for constant fan-out time.  
6. **Rebalance**: after every `K=10 000` inserts, rebuild root if bucket histograms indicate skew.  
7. **Documentation & API**: `make_index/1`, `add/2`, `delete/2`, `member/2`.

---

## 11. References

* Guttman, A. “R-trees: A Dynamic Index Structure for Spatial Searching”, 1984.  
* Markl et al., “UB-Trees: A New Indexing Concept for Multidimensional Data”, VLDB 1999.  
* Ohlbach, H. J., “Term Indexing”, Handbook of Automated Reasoning, 2001.  
* Leis et al., “The Adaptive Radix Tree” (dict/HAMT inspiration), ICDE 2013.  

---

### End of document
