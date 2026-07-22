# The Standing Offer

*Two sessions writing the same true thing at different times is not a
disagreement.*

This project commits pages that no one authors — a stream manifest, a
type-audit report, the reference dumps a build produces from its own
current state. They are committed so a drift check can catch the day
they stop matching the code, which means they are also committed where
two campaigns working at once can both, honestly, touch them. Neither
session is wrong. Each one simply asked the same question — *what does
the code say right now?* — at a moment slightly different from the
other's, and got a slightly different, equally true answer. Git, shown
two true answers to the same question, has always called that a
conflict and asked a person to pick.

There was never a choice to pick between. The right answer to "what does
this page say" is never "session A's version" or "session B's version" —
it is *ask again*. A merge driver can ask again as easily as a person
can, and rather more quickly: given a conflict on one of these pages, it
throws both versions away and reruns the one command that produces the
page, fresh, against the merged code both sessions were actually
building toward. Two different true answers become one, not by choosing,
but by asking the question a third time, now that both sessions'
changes are finally in the same room together.

Not every committed page answers to this. A chronicle's table of
contents and an idea registry's rows are not a question with one right
answer — they are a running record of everything anyone has said,
and two additions are both keepers; git's own older, simpler trick of
keeping every line either side wrote handles that case without asking
anything at all. And some committed pages are not supposed to answer
freshly, ever — a world frozen at a specific commit exists precisely so
that a later disagreement about what it should look like is caught, not
smoothed over. Those stay conflicts. A conflict on one of them is not
noise to clear; it is the entire reason the page was frozen in the first
place, and the offer to regenerate is deliberately never extended to it.
