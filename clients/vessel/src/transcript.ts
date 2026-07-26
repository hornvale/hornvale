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
 * their own monospace class rather than the meta one. */
export function splitResponse(text: string): Line[] {
  let inMap = false;
  return text.split("\n").map((line) => {
    if (line.startsWith("[lens: ")) inMap = true;
    else if (inMap && line.trim() === "") inMap = false;
    const cls = inMap
      ? "casement-map" as const
      : line.startsWith("[room ") || line.startsWith("Ways on:")
      ? "casement-meta" as const
      : "casement-prose" as const;
    return { cls, text: line };
  });
}
