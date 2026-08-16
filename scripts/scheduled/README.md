# Scheduled jobs

Unattended checks on `lefford`, restoring the **scheduling** function decision
0125 deleted along with CI. Compute was never the scarce resource — lefford ran
22.9 h of measured work in a 744-hour window, 3.1% utilisation — but nothing
remembered to run things, and CLAUDE.md accumulated six separate paragraphs
asking humans to remember instead.

## The two rules

1. **A scheduled job never commits and never touches `main`'s working tree.**
   It reports to the board. Precedent: decision 0129's lane rule. The hazard is
   a nightly job committing while a merge is mid-landing. Since decision 0139
   `main` advances only through the merge queue's serial claim on this box, and
   a scheduled job takes no claim — so it is this rule, not the substrate, that
   keeps a nightly run out of a landing merge's way.
2. **Jobs run in a LINKED WORKTREE of lefford's main checkout, never a separate
   clone.** `git reset --hard` in a worktree touches only that worktree's own
   branch, so the "owns its checkout" property holds — while a *clone* would
   break the board.

   **Why a clone breaks it, verified rather than assumed.** The board is an
   orphan ref (`refs/hornvale/board`) and `git clone` does not fetch
   `refs/hornvale/*`, so a fresh clone starts with an EMPTY board. Its
   `board-sync` would then push a divergent log to
   `refs/hornvale/hosts/lefford`, which already exists. Two consequences, and
   the second is the bad one:
   - It cannot clobber: `tools/board/src/sync.rs` asserts a no-force invariant
     on the push argv directly (B3), precisely because "the consequence of a
     force-push here is there is nothing left to compare against". The push is
     **rejected**.
   - `board-sync` is best-effort and **never fails its caller**, so the
     rejection is silent and every nightly post would be published nowhere.
     And the rejection text names a *hostname collision between two machines*
     — a correct message for its designed case and a misleading one here,
     where it is two checkouts on one box.

   A linked worktree shares the object store and the ref namespace, so there is
   exactly one board log per host and posts publish normally.

## Install (on lefford, once)

```bash
# A WORKTREE, not a clone — see rule 2 above; a clone silently breaks the board.
cd ~/Projects/hornvale
git worktree add ~/Projects/hornvale-scheduled -b scheduled origin/main
mkdir -p ~/.config/systemd/user
cp ~/Projects/hornvale-scheduled/scripts/scheduled/systemd/* ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now hornvale-nightly.timer
loginctl enable-linger "$USER"   # so user timers run without an active session
```

## Read it

```bash
systemctl --user list-timers hornvale-nightly.timer
journalctl --user -u hornvale-nightly.service -n 200
systemctl --user start hornvale-nightly.service   # run it now, out of band
```
