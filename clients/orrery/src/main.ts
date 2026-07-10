/// <reference lib="dom" />
/** Entry point for the live orrery client (filled in by later tasks). */
function boot(): void {
  const el = document.getElementById("orrery");
  if (el) el.textContent = "orrery loading…";
}
if (typeof document !== "undefined") boot();
