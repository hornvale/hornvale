# The Route Cache

The Fetch left current-relative water searches uncached because the old
`RouteMemo` was home-keyed. This follow-up measured whether a moving-position
cache had a bounded working set.

The answer was no on the available workload. On seed 17 over twelve waits,
the cumulative `(current position, remembered water)` population rose from
14 keys after wait 1 to 649 after wait 12. The companion sweep asked 485
routes over 201 distinct home-key pairs. The current-relative curve was still
growing, and the longer Fetch diagnostic had already shown that a plateau can
resume later.

No persistent position-keyed cache was introduced. Reusing `RouteMemo` would
hide a different key and ownership contract behind a home-based type. A
per-decision one-to-many distance field remains a possible future optimization,
because its lifetime is naturally bounded by one decision and it answers the
actual nearest-from-here question.
