# 0012. Config files are JSON, not YAML

**Status:** Accepted (2026-07-06) · **Decider:** Nathan

In the context of choosing a file format for Lab study files (and future
config), facing a genuine ergonomic preference for YAML, we decided to **use
JSON**, accepting the less comfortable authoring experience — solely because
JSON rides the existing `serde_json` dependency and YAML would require a new
crate ([0004](0004-no-new-dependencies.md)).

**Context.** This decision is recorded precisely because it goes *against* the
decider's stated preference; without the record, "why not YAML?" would be
relitigated. The no-new-dependencies rule is the whole reason.

**Consequence.** Study files are `studies/*.study.json`, parsed with the same
`parse_pin` the CLI uses. Revisit **only** if the dependency budget ever
loosens; if it does, this is the first candidate to reopen.

**See also.** Campaign L0 spec §2.4, §4; [0004](0004-no-new-dependencies.md).
