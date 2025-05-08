Source-maps – design v2 (no IR bloat)
====================================

Requirement recap
-----------------
• JS stack-traces must reference *.pl* lines.
• **Do not** add a span argument to every IR term.

Chosen strategy
---------------
Keep the existing IR untouched.  Carry position data *out-of-band* all the way
to the backend via an *extra argument of the JS code-gen DCG*.

Key pieces
----------
1. Front-end (comp.pl)
   a. read_term/3  with  subterm_positions(PosTree).
   b. Walk Term × PosTree once; emit

        node(Id, IR)                % current payload, unchanged
        span(Id, Line0,Col0, Line1,Col1).

   Id = integer, assigned by a fast gensym.

   Resulting top-level structure:

       program(Nodes, Spans).

   Nodes is the old IR list; Spans is an assoc from Id → Span.

2. Optimisation passes (peephole, DCE)
   • When they *replace* a node keep the Id — spans stay valid.
   • When they *insert* a new node generate a fresh Id and (optionally)
     copy the parent’s span so traces still point somewhere sensible.

3. JS backend (backend/js.pl)
   • dcg entrypoint becomes

         js(+NodeId, +SpanMap)//

     or, equivalently, we thread SpanMap as an additional DCG state with

         --> { lookup_span(NodeId, SpanMap, Span) }, emit(Node).

   • After every emit action call   record_mapping(Span).

     record_mapping/1 uses the *current* generated line/col counters that are
     already maintained by the pretty-printer.

4. Map encoder
   • Gather all recorded tuples → sort by generated position → encode VLQ.
   • Append  //# sourceMappingURL=…  footer.

5. CLI flag  --source-maps  to opt-in (off by default for speed).

Why it satisfies the constraint
-------------------------------
• **IR nodes stay bit-identical** — no extra arguments.
• At most two new predicates touch code-gen sites (lookup_span/3 and
  record_mapping/1).  No ripple effect through the rest of the compiler.

Implementation to-dos
---------------------
[] Add Id + span facts in comp.pl.
[] Update optimisation passes to preserve/clone Ids.
[] Thread SpanMap arg through js//2 DCG entry.
[] Emit .map file & CLI flag.

Nice-to-have
------------
• Fallback that emits only *predicate-level* spans when inner goals lack Ids
  (simpler front-end wiring, still useful for most traces).
• Pretty printer  @bundle --debug   that prints generated line ←→ .pl line.
