// The book-native rendering rule (spec: mockup B): the sim's prose reads
// in the book's own serif voice; only the mechanical lines — the room
// header and the exit list — drop to muted monospace.

/** One rendered line of a session response. */
export interface Line {
  cls: "casement-meta" | "casement-prose" | "casement-map";
  text: string;
}

/** Split a session response into classed lines for the transcript. The
 * chart is a grid: proportional type would shear it, so map lines take
 * their own monospace class rather than the meta one.
 *
 * Map mode is entered by the `"[lens: "` header line (a stateful marker,
 * not an indentation heuristic — real tapered grid rows can start with
 * 3+ spaces, which would misfire a leading-whitespace check). It is left
 * either by a blank line, or by the chart's own closing `"  legend: "`
 * line (`render_surrounds_ascii` in `windows/scene/src/surrounds_ascii.rs`
 * always closes a chart with a legend line — the legend is never empty,
 * since it always carries at least the observer's own regime descriptor —
 * so a chart never actually ends on a blank line in practice; relying on
 * the blank line alone left map mode never exiting, so prose appended
 * after a chart in the same response was misclassified as chart text). */
export function splitResponse(text: string): Line[] {
  let inMap = false;
  return text.split("\n").map((line) => {
    if (line.startsWith("[lens: ")) inMap = true;
    const cls = inMap
      ? "casement-map" as const
      : line.startsWith("[room ") || line.startsWith("Ways on:")
      ? "casement-meta" as const
      : "casement-prose" as const;
    if (inMap && (line.trim() === "" || line.startsWith("  legend: "))) {
      inMap = false;
    }
    return { cls, text: line };
  });
}
