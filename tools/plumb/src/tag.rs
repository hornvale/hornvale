//! Parsing the `plumb:` verdict line inside an item's doc comment.
//!
//! The grammar is `type-audit`'s, deliberately: one tag line in the item's own
//! doc comment, a single verdict keyword, and one parenthesised argument. The
//! two tools are read by the same people in the same files, so a second syntax
//! would be a second thing to remember for no gain.
//!
//! Three rules the parser enforces that are easy to miss when writing a tag:
//!
//! - **Exactly one verdict per constant** — a second one anywhere in the doc
//!   comment is refused, on a new line *or* on the same line.
//! - **The argument may not contain parentheses.** Write "the day length is
//!   drawn per seed", not "the day length (drawn per seed)". This is what lets
//!   the parser insist the first `(` and the final `)` are the only ones,
//!   instead of silently absorbing trailing junk into a reason.
//! - **A `pending` wave is digits only, with no leading zero**, so the label
//!   the tool stores is always the label that was authored.

/// Which axis an authored constant varies along.
///
/// The rungs are `MAP-one-kind-model`'s three additions seen from the numeric
/// side, so a verdict here is also a registration: a constant tagged
/// `per-people` names a consumer for the kind-edge campaign that does not
/// exist yet.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rung {
    /// Varies by nothing — a physical or lattice constant.
    Universal,
    /// Varies by seed and pins — a property of the world, not of the code.
    PerWorld,
    /// Varies by `KindId` — belongs in a species component table.
    PerSpecies,
    /// Varies by what a people does — needs a kind-to-kind edge.
    PerPeople,
    /// Varies by the individual — derived from `Lineage`, never stored.
    PerIndividual,
    /// Not yet judged. Carries a wave, exactly as type-audit's does.
    Pending(String),
}

/// The five **judged** rung keywords, in ladder order.
///
/// `pending` is deliberately absent: it is not a rung on the ladder but the
/// declaration that no rung has been chosen yet, and it takes a wave where the
/// five take a reason. Keeping the two lists apart is what lets
/// [`parse_tag_full`] give a reasonless `universal` a different error from an
/// unknown keyword.
pub const JUDGED_RUNGS: &[&str] = &[
    "universal",
    "per-world",
    "per-species",
    "per-people",
    "per-individual",
];

impl Rung {
    /// The tag keyword this rung is written as.
    pub fn keyword(&self) -> &'static str {
        match self {
            Rung::Universal => "universal",
            Rung::PerWorld => "per-world",
            Rung::PerSpecies => "per-species",
            Rung::PerPeople => "per-people",
            Rung::PerIndividual => "per-individual",
            Rung::Pending(_) => "pending",
        }
    }

    /// A stable label for grouping in the report; a `pending` keeps its wave,
    /// because a backlog with every wave collapsed into one row is not a
    /// backlog.
    pub fn label(&self) -> String {
        match self {
            Rung::Pending(wave) => format!("pending({wave})"),
            other => other.keyword().to_string(),
        }
    }
}

/// Why a tag line could not be turned into a [`Rung`].
///
/// The kinds are distinguished because they mean different things to a
/// ratchet: [`TagErrorKind::Missing`] is *backlog* (a constant nobody has
/// judged yet), while every other kind is a *defect* (someone wrote a tag and
/// wrote it wrong).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TagErrorKind {
    /// No `plumb:` line in the doc comment at all.
    Missing,
    /// More than one `plumb:` line on the same item.
    Duplicate,
    /// A tag line that is not `<keyword>(<argument>)`.
    Malformed,
    /// A keyword that is neither a judged rung nor `pending`.
    UnknownRung,
    /// A judged rung with no stated reason.
    MissingReason,
    /// A `pending` whose argument is not `wave-<number>`.
    BadWave,
}

/// A malformed, absent, or ambiguous tag.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TagError {
    /// Which failure this is.
    pub kind: TagErrorKind,
    /// Human-readable reason, for a diagnostic line.
    pub message: String,
}

impl TagError {
    fn new(kind: TagErrorKind, message: impl Into<String>) -> Self {
        TagError {
            kind,
            message: message.into(),
        }
    }
}

/// One fully parsed tag: the rung, plus the reason that justified it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tag {
    /// The declared rung.
    pub rung: Rung,
    /// The stated reason, for the five judged rungs.
    ///
    /// `None` for `pending`, whose parenthesised argument is a **wave** rather
    /// than a reason — the wave lives inside [`Rung::Pending`]. This is the one
    /// place the grammar is not uniform, and it is not uniform because
    /// `pending` is not a judgement: there is nothing yet to justify.
    pub reason: Option<String>,
}

/// Concatenate the text of an item's `#[doc = "…"]` attributes into one
/// string — the haystack [`find_tag_line`] searches.
///
/// A doc comment is an attribute on the item, which is why this tool parses
/// Rust: a line scanner has to guess which comment belongs to which constant,
/// and guesses wrong at the top of a file, after a `#[allow]`, and anywhere a
/// blank line separates the two.
pub fn doc_text_of(attrs: &[syn::Attribute]) -> String {
    let mut out = String::new();
    for attr in attrs {
        if attr.path().is_ident("doc")
            && let syn::Meta::NameValue(nv) = &attr.meta
            && let syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(s),
                ..
            }) = &nv.value
        {
            out.push_str(&s.value());
            out.push('\n');
        }
    }
    out
}

/// Return the single `plumb:` payload (text after the colon), or `None`.
///
/// Errors if more than one verdict is present: two verdicts on one constant is
/// an ambiguity, and silently taking the first would let a stale one hide
/// behind a fresh one.
///
/// **A second verdict on the SAME line counts.** The first draft iterated
/// `doc.lines()` and checked only for a second *line*, so
/// `plumb: universal(a) plumb: per-world(b)` parsed as `Universal` with the
/// reason `"a) plumb: per-world(b"` and produced no finding at all — the exact
/// hiding this function's own doc claimed to prevent. The payload is now
/// searched for a further `plumb:` as well.
pub fn find_tag_line(doc: &str) -> Result<Option<&str>, TagError> {
    let dup = || TagError::new(TagErrorKind::Duplicate, "more than one plumb: verdict");
    let mut found: Option<&str> = None;
    for line in doc.lines() {
        if let Some(rest) = line.trim().strip_prefix("plumb:") {
            if found.is_some() {
                return Err(dup());
            }
            let rest = rest.trim();
            if rest.contains("plumb:") {
                return Err(dup());
            }
            found = Some(rest);
        }
    }
    Ok(found)
}

/// Parse the item's tag line into a [`Rung`].
///
/// This is the signature the ratchet consumes. An absent tag is an `Err`
/// carrying [`TagErrorKind::Missing`] rather than an `Ok(None)`, so that a
/// caller cannot silently treat "nobody judged this" as a passing verdict —
/// which is the whole property the audit exists to hold.
pub fn parse_tag(doc: &str) -> Result<Rung, TagError> {
    parse_tag_full(doc).map(|t| t.rung)
}

/// Parse the item's tag line into a [`Tag`], keeping the stated reason.
pub fn parse_tag_full(doc: &str) -> Result<Tag, TagError> {
    let Some(payload) = find_tag_line(doc)? else {
        return Err(TagError::new(
            TagErrorKind::Missing,
            "no plumb: verdict on this constant",
        ));
    };

    let Some(open) = payload.find('(') else {
        // Classify by the bare keyword, so the diagnostic says which of the
        // two very different mistakes this is.
        let head = payload.trim();
        return Err(if JUDGED_RUNGS.contains(&head) {
            TagError::new(
                TagErrorKind::MissingReason,
                format!("{head} needs a reason in parentheses"),
            )
        } else if head == "pending" {
            TagError::new(
                TagErrorKind::BadWave,
                "pending needs a wave: pending(wave-N)",
            )
        } else {
            TagError::new(TagErrorKind::UnknownRung, format!("unknown rung: {head:?}"))
        });
    };
    if !payload.ends_with(')') {
        return Err(TagError::new(
            TagErrorKind::Malformed,
            format!("malformed verdict: {payload:?}"),
        ));
    }
    let head = payload[..open].trim();
    let arg = payload[open + 1..payload.len() - 1].trim();
    // The first `(` and the final `)` must be the only ones. Without this,
    // `universal(a)b)` parsed happily with the reason `"a)b"`, and any trailing
    // junk after a closing paren was silently absorbed into the reason. The
    // cost is that a reason may not itself contain parentheses; that is a
    // stated rule of the grammar, not an accident.
    if arg.contains('(') || arg.contains(')') {
        return Err(TagError::new(
            TagErrorKind::Malformed,
            format!("a verdict argument may not contain parentheses: {arg:?}"),
        ));
    }

    if head == "pending" {
        // Digits only, and no leading zero: `u32::from_str` accepts a leading
        // `+`, so a bare `.parse()` admitted `wave-+1` and then NORMALISED it
        // to `wave-1` — a stored label that no longer matched the authored
        // text. The invariant asserted below is that it always does.
        let digits = arg
            .strip_prefix("wave-")
            .filter(|d| !d.is_empty() && d.bytes().all(|b| b.is_ascii_digit()))
            .filter(|d| d.len() == 1 || !d.starts_with('0'))
            .ok_or_else(|| TagError::new(TagErrorKind::BadWave, format!("bad wave: {arg:?}")))?;
        let n: u32 = digits
            .parse()
            .map_err(|_| TagError::new(TagErrorKind::BadWave, format!("bad wave: {arg:?}")))?;
        // The label the tool stores is always the label that was authored; the
        // filters above are what make that true, and
        // `a_wave_is_digits_only_and_its_label_round_trips` is what holds it. A
        // `debug_assert` here would be a second, weaker copy of that assertion
        // — and it INTERCEPTED a mutation aimed at the filters, turning a kill
        // by the test into a kill by the guard.
        return Ok(Tag {
            rung: Rung::Pending(format!("wave-{n}")),
            reason: None,
        });
    }

    let rung = match head {
        "universal" => Rung::Universal,
        "per-world" => Rung::PerWorld,
        "per-species" => Rung::PerSpecies,
        "per-people" => Rung::PerPeople,
        "per-individual" => Rung::PerIndividual,
        other => {
            return Err(TagError::new(
                TagErrorKind::UnknownRung,
                format!("unknown rung: {other:?}"),
            ));
        }
    };
    if arg.is_empty() {
        return Err(TagError::new(
            TagErrorKind::MissingReason,
            format!("{head} needs a reason in parentheses"),
        ));
    }
    Ok(Tag {
        rung,
        reason: Some(arg.to_string()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_every_judged_rung() {
        let cases = [
            ("universal", Rung::Universal),
            ("per-world", Rung::PerWorld),
            ("per-species", Rung::PerSpecies),
            ("per-people", Rung::PerPeople),
            ("per-individual", Rung::PerIndividual),
        ];
        for (keyword, rung) in &cases {
            let doc = format!("Some prose.\nplumb: {keyword}(a stated reason)");
            assert_eq!(parse_tag(&doc), Ok(rung.clone()), "keyword {keyword}");
            assert_eq!(
                parse_tag_full(&doc).unwrap().reason.as_deref(),
                Some("a stated reason")
            );
        }
        // The keyword list the parser accepts and the list it advertises must
        // be the same list — a rung nobody can spell is not a rung.
        assert_eq!(JUDGED_RUNGS.len(), cases.len());
        for (keyword, _) in cases {
            assert!(JUDGED_RUNGS.contains(&keyword));
        }
    }

    /// A verdict with no reason is a parse error, not a lenient default.
    /// `seam-guard` made the same choice for `expect(survives: …)` and its own
    /// doc explains why: a reasonless declaration is one nobody can review.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): make the reason
    /// optional — insert `return Ok(Tag { rung: Rung::Universal, reason: None });`
    /// as the first statement of `parse_tag_full`'s no-parenthesis branch.
    ///
    /// ```text
    /// assertion failed: parse_tag("plumb: universal").is_err()
    /// ```
    #[test]
    fn a_verdict_without_a_reason_is_refused() {
        assert!(parse_tag("plumb: universal").is_err());
        assert!(parse_tag("plumb: universal(the tick lattice)").is_ok());
    }

    /// The reasonless refusal has two shapes, and only testing the bare
    /// keyword would leave the second one unheld: `universal()` parses as a
    /// well-formed verdict whose argument happens to be empty, and takes an
    /// entirely different code path from `universal`.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): neutralise the
    /// refusal — `if arg.is_empty() {` -> `if false {`.
    ///
    /// ```text
    /// called `Result::unwrap_err()` on an `Ok` value: Universal
    /// ```
    #[test]
    fn an_empty_reason_is_refused_the_same_as_an_absent_one() {
        assert_eq!(
            parse_tag("plumb: universal()").unwrap_err().kind,
            TagErrorKind::MissingReason
        );
        assert_eq!(
            parse_tag("plumb: universal(   )").unwrap_err().kind,
            TagErrorKind::MissingReason
        );
        assert_eq!(
            parse_tag("plumb: per-species").unwrap_err().kind,
            TagErrorKind::MissingReason
        );
    }

    /// `pending` takes a WAVE where the judged rungs take a reason, so it needs
    /// its own well-formedness rule: an unparseable wave is a backlog entry
    /// nobody can schedule.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3, fix round 1): silently accept
    /// a malformed wave as wave-0 — replace the
    /// `.ok_or_else(|| TagError::new(TagErrorKind::BadWave, …))?` on the
    /// `digits` binding with `.unwrap_or("0")`.
    ///
    /// ```text
    /// called `Result::unwrap_err()` on an `Ok` value: Pending("wave-0")
    /// ```
    #[test]
    fn pending_requires_a_well_formed_wave() {
        assert_eq!(
            parse_tag("plumb: pending(wave-1)").unwrap(),
            Rung::Pending("wave-1".into())
        );
        assert_eq!(
            parse_tag("plumb: pending(wave-x)").unwrap_err().kind,
            TagErrorKind::BadWave
        );
        assert_eq!(
            parse_tag("plumb: pending(later)").unwrap_err().kind,
            TagErrorKind::BadWave
        );
        assert_eq!(
            parse_tag("plumb: pending").unwrap_err().kind,
            TagErrorKind::BadWave
        );
        // A `pending` carries no reason: its argument is the wave.
        assert_eq!(
            parse_tag_full("plumb: pending(wave-2)").unwrap().reason,
            None
        );
    }

    /// An ABSENT tag is an `Err(Missing)`, never a quiet success. This is the
    /// distinction Task 4's ratchet is built on, and the reason `parse_tag`
    /// returns `Result<Rung, _>` rather than `Result<Option<Rung>, _>`.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): replace the
    /// `TagErrorKind::Missing` return with
    /// `return Ok(Tag { rung: Rung::Universal, reason: None });`.
    ///
    /// ```text
    /// called `Result::unwrap_err()` on an `Ok` value: Universal
    /// ```
    #[test]
    fn an_absent_tag_is_missing_not_a_default_verdict() {
        assert_eq!(
            parse_tag("Just prose about a constant.").unwrap_err().kind,
            TagErrorKind::Missing
        );
        assert_eq!(parse_tag("").unwrap_err().kind, TagErrorKind::Missing);
    }

    #[test]
    fn rejects_unknown_rungs_duplicates_and_malformed_lines() {
        assert_eq!(
            parse_tag("plumb: frobnicate(x)").unwrap_err().kind,
            TagErrorKind::UnknownRung
        );
        assert_eq!(
            parse_tag("plumb: frobnicate").unwrap_err().kind,
            TagErrorKind::UnknownRung
        );
        assert_eq!(
            parse_tag("plumb: universal(unterminated").unwrap_err().kind,
            TagErrorKind::Malformed
        );
        assert_eq!(
            parse_tag("plumb: universal(a)\nplumb: per-world(b)")
                .unwrap_err()
                .kind,
            TagErrorKind::Duplicate
        );
    }

    /// The tag is found on the LAST line of a doc comment the same way
    /// `type-audit`'s is, but nothing about the parser depends on position —
    /// it scans every line and refuses a second one. Both facts are asserted
    /// because the placement rule is a convention and the refusal is a
    /// mechanism, and only the second one is enforced.
    #[test]
    fn finds_the_tag_anywhere_in_the_doc_comment() {
        let doc = " Ticks in one standard day. The lattice's own unit.\n plumb: universal(a kernel constant)\n";
        assert_eq!(parse_tag(doc), Ok(Rung::Universal));
        assert_eq!(
            find_tag_line(doc).unwrap(),
            Some("universal(a kernel constant)")
        );
        assert_eq!(find_tag_line("no tag").unwrap(), None);
    }

    /// **Two verdicts on ONE line used to evade the duplicate check.**
    /// `find_tag_line` iterated `doc.lines()`, so this parsed as `Universal`
    /// with the reason `"a) plumb: per-world(b"` and produced no finding —
    /// precisely the "stale one hiding behind a fresh one" that function's own
    /// doc claims to prevent.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3,
    /// fix round 1): delete the `if rest.contains("plumb:")` refusal from
    /// `find_tag_line`.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: Malformed
    ///  right: Duplicate
    /// ```
    #[test]
    fn a_second_verdict_on_the_same_line_is_a_duplicate() {
        assert_eq!(
            parse_tag("plumb: universal(a) plumb: per-world(b)")
                .unwrap_err()
                .kind,
            TagErrorKind::Duplicate
        );
        assert_eq!(
            find_tag_line("plumb: universal(a) plumb: per-world(b)")
                .unwrap_err()
                .kind,
            TagErrorKind::Duplicate
        );
    }

    /// The first `(` and the final `)` must be the only ones. Without this,
    /// `universal(a)b)` parsed with the reason `"a)b"`, silently absorbing
    /// whatever followed the close.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3,
    /// fix round 1): delete the `arg.contains('(') || arg.contains(')')`
    /// refusal.
    ///
    /// ```text
    /// called `Result::unwrap_err()` on an `Ok` value: Universal
    /// ```
    #[test]
    fn an_argument_may_not_contain_parentheses() {
        for bad in [
            "plumb: universal(a)b)",
            "plumb: universal(a(b))",
            "plumb: per-world(the day length (drawn per seed))",
        ] {
            assert_eq!(
                parse_tag(bad).unwrap_err().kind,
                TagErrorKind::Malformed,
                "{bad}"
            );
        }
        // The rule the refusal costs, stated as a passing case.
        assert!(parse_tag("plumb: per-world(the day length is drawn per seed)").is_ok());
    }

    /// `u32::from_str` accepts a leading `+`, so a bare `.parse()` admitted
    /// `wave-+1` and then stored it as `wave-1` — a label that no longer
    /// matched the authored text. Digits only, no leading zero, so the stored
    /// label always round-trips.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3,
    /// fix round 1): drop the `is_ascii_digit` filter and parse directly.
    ///
    /// ```text
    /// called `Result::unwrap_err()` on an `Ok` value: Pending("wave-1")
    /// ```
    #[test]
    fn a_wave_is_digits_only_and_its_label_round_trips() {
        for bad in [
            "plumb: pending(wave-+1)",
            "plumb: pending(wave- 1)",
            "plumb: pending(wave-01)",
            "plumb: pending(wave-1x)",
            "plumb: pending(wave-)",
        ] {
            assert_eq!(
                parse_tag(bad).unwrap_err().kind,
                TagErrorKind::BadWave,
                "{bad}"
            );
        }
        for (text, label) in [
            ("wave-0", "wave-0"),
            ("wave-1", "wave-1"),
            ("wave-12", "wave-12"),
        ] {
            assert_eq!(
                parse_tag(&format!("plumb: pending({text})")).unwrap(),
                Rung::Pending(label.to_string())
            );
        }
    }

    #[test]
    fn labels_keep_the_wave_but_collapse_the_judged_rungs() {
        assert_eq!(Rung::PerPeople.label(), "per-people");
        assert_eq!(Rung::Pending("wave-3".into()).label(), "pending(wave-3)");
        assert_eq!(Rung::Pending("wave-3".into()).keyword(), "pending");
    }
}
