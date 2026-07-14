# MANDATORY PREAMBLE — Hornvale subagent

Working directory: `<WORKTREE>`
Expected branch: `<BRANCH>`

1. Your FIRST action, before reading any file or planning anything:

   ```bash
   cd <WORKTREE> && pwd && git branch --show-current
   ```

   If the printed branch is not `<BRANCH>`: STOP. Reply exactly
   `BLOCKED: wrong branch <actual-branch>` and end your turn.
   Do not fix the branch, do not proceed on the wrong one.

2. Run every long command (cargo test, artifact regeneration)
   **in the foreground** and wait for it to finish. Pass an explicit long
   timeout on those Bash calls (`timeout: 3600000`) — repo settings raise
   the ceiling to 60 minutes; the unstated default is 20 and a cold gate
   can exceed it. Do not start watchers. NEVER regenerate censuses
   locally: `bash scripts/regenerate-artifacts.sh` skips them by default
   (they are opt-in via HV_CENSUS=1, which only the AWS regen path sets —
   never set it yourself; the once-per-campaign census refresh is the
   controller's job).
   If a job ends up in the background anyway, your next action is a bounded
   foreground poll — never ending your turn to "wait":

   ```bash
   until grep -q '<done-marker>' <output-file>; do sleep 20; done
   ```

   then continue with the task.

3. Before every `git commit`: re-run `pwd && git branch --show-current`
   and confirm both match the values above.

4. Your final reply MUST begin with a verdict line — `DONE: <summary>` or
   `BLOCKED: <reason>` — followed by the pwd/branch evidence and pasted
   command output (gate results, commit hash). Paste what happened, never
   what you intend to wait for.

# END PREAMBLE
