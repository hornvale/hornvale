# Full12 replay comparison

The frozen `run.summarize` check accepted both retained dossiers independently:
each has `completed=true` and all twelve declared arms. The primary and
independent source identities are equal for all twelve arms, and all four
question outcomes are equal for all twelve arms. Candidate outputs are kept
separately under each replay directory and compare equal arm by arm; they are
not used as checker authority.

The two runs report the same Linux host string and the same committed panel,
checker, rules, owner, roster and implementation identities. The primary arm
wall total is 127.765 seconds and the independent total is 127.794 seconds,
a 0.0287795914 second difference. Observation subprocess totals are 0.144 and
0.143 seconds, a -0.0008323798 second difference. These are run timing
differences on a shared canonical registry cache, not behavioral differences;
queue and author costs remain null in both dossiers.

The machine-readable per-arm comparison is
`full12-replay-comparison.json`. The raw dossiers are archived separately in
`primary-full12-replay/dossier.tar.gz` and
`independent-full12-replay/dossier.tar.gz`; neither archive contains a
checkout or target directory. The canonical stage log is retained beside the
primary archive, and the independent invocation provenance is retained beside
the independent archive.

## Transfer evidence

The primary streamed transfer completed with 12,125,738 bytes and SHA256
`5b6af132ae2ad7dccc683234cc891befe984203563fa052731cb13cf9c697046`.
The first independent transfer attempt failed locally because its output
directory did not yet exist; it did not reach or modify the remote parent.
The sequential retry completed with 12,162,572 bytes and SHA256
`2a76b5410d659ccd2c2f205cb487f865b931c502e3efed993b328995f2c9c3d9`.
Archive member checks found 358 and 359 members respectively and no
`checkout` or `target` member.

Exact successful commands and their machine-readable receipts are in the two
`archive-receipt.json` files. The failed local staging command was:

```text
mkdir -p /tmp/counterpart-task7-transfer && ssh lefford 'set -eu; d=/home/nathan/.local/state/hornvale/counterpart-independent-full12-m1o2h8zh; tar -C "$d" --exclude=checkout --exclude=target -czf - dossier dependency-fetch.json invocation-provenance.json invocation-source.sha preparation-before.json preparation-after.json' > /tmp/counterpart-task7-transfer/independent.tar.gz
```

It returned `/bin/bash: /tmp/counterpart-task7-transfer/independent.tar.gz: No such file or directory`; the retry was run after `mkdir -p` completed.
