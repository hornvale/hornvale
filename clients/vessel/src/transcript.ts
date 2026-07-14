// The book-native rendering rule (spec: mockup B): the sim's prose reads
// in the book's own serif voice; only the mechanical lines — the room
// header and the exit list — drop to muted monospace.

/** One rendered line of a session response. */
export interface Line {
  cls: "casement-meta" | "casement-prose";
  text: string;
}

/** Split a session response into classed lines for the transcript. */
export function splitResponse(text: string): Line[] {
  return text.split("\n").map((line) => ({
    cls: line.startsWith("[room ") || line.startsWith("Ways on:")
      ? "casement-meta" as const
      : "casement-prose" as const,
    text: line,
  }));
}
