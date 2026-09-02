# The Brattice — campaign ledger

Campaign: gates for the underworld (The Circuit, campaign 2). Metaplan:
`docs/superpowers/specs/2026-09-01-the-circuit-metaplan.md` §3. Predecessor:
The Crosscut (`docs/superpowers/ledgers/2026-09-01-the-crosscut.md`,
decisions 0566–0568, merged as `daa94492c`). Autopilot engaged from the first
message; Nathan reviews at G3 and G6. Decision block 0616–0625 reserved
(`scripts/decision-block-request.sh the-brattice`, 2026-09-02); 0569–0575
belong to the-crosscut's reservation and are not minted into.

Entries are committed as each ruling occurs (decision 0486), never batched.

#1 [Q] — Visual companion? · **Started at the first message, before any
question** · Why: CLAUDE.md's standing preference ("always use the visual
companion during brainstorming — don't ask, just set it up"); the Crosscut
skipped it because no human was present and flagged the deviation at G3
(its entry #6), and this campaign's brief asks for the companion explicitly
· Discarded: text-only with fenced diagrams (the Crosscut's substitute) —
the brief names the deviation as the thing to correct · Ideonomy: 1 pass
(inversion: what does a companion cost when nobody is watching? — the
screens persist under `.superpowers/brainstorm/` and read as a record of
what was shown, so an absent reader loses nothing and a present one gains
the diagrams; no overturn) · Capture: server started with `--project-dir`
on this worktree; screens are numbered `NN-<topic>.html`.
