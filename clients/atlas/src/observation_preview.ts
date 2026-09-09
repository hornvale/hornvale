import { parseObservationFramePacket, renderObservationPreview } from "./observation.ts";

const PHONE = { width: 390, height: 844 };
const LAPTOP = { width: 1440, height: 900 };

/** Mount two real browser frames, each backed by the same validated packet. */
export function mountObservationPreview(root: HTMLElement, packetText: string): void {
  const packet = parseObservationFramePacket(packetText);
  const preview = renderObservationPreview(packet);
  root.replaceChildren(
    frame("Phone", PHONE.width, PHONE.height, preview.phone.html),
    frame("Laptop", LAPTOP.width, LAPTOP.height, preview.laptop.html),
  );
}

function frame(title: string, width: number, height: number, source: string): HTMLElement {
  const section = document.createElement("section");
  section.dataset.viewport = title.toLowerCase();
  const heading = document.createElement("h2");
  heading.textContent = `${title} preview`;
  const iframe = document.createElement("iframe");
  iframe.title = `${title} observation frame`;
  iframe.width = String(width);
  iframe.height = String(height);
  iframe.srcdoc = source;
  section.append(heading, iframe);
  return section;
}
