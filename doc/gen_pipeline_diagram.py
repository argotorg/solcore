"""
Generate an SVG flowchart for the Hull compilation pipeline section.
Output: doc/src/hull/diagrams/pipeline.svg
"""
import os

OUT = os.path.join(os.path.dirname(__file__), "src", "hull", "diagrams")
os.makedirs(OUT, exist_ok=True)

W        = 560      # total SVG width
BOX_W    = 220      # box width
BOX_H    = 48       # box height
BOX_RX   = 8        # corner radius
BOX_X    = (W - BOX_W) // 2   # left edge of all boxes

GAP      = 28       # vertical gap between box bottom and next box top
ARROW_H  = GAP      # arrow spans the full gap


FONT     = "monospace"
FONT_SZ  = 14
LABEL_SZ = 11

FILL_HIGHLIGHT = "#fff9db"   # yellow tint for Hull box
FILL_NORMAL    = "#ffffff"
STROKE         = "#444444"
ARROW_COLOR    = "#444444"
LABEL_COLOR    = "#666666"
TEXT_COLOR     = "#222222"

steps = [
    ("Core Solidity (.solc)",      None,                             False),
    ("Hull",                        None,                             True),
    ("Yul",                         None,                             False),
    ("EVM bytecode",                None,                             False),
]

arrows = [
    "type-check · specialize · match-compile",
    "Yul translation  (yule binary)",
    "solc",
]

def box_top(i):
    return 20 + i * (BOX_H + GAP)

total_h = box_top(len(steps) - 1) + BOX_H + 20

lines = []

def emit(s):
    lines.append(s)

emit(f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{total_h}" '
     f'viewBox="0 0 {W} {total_h}">')

emit("""<style>
  .box       { stroke: #444; stroke-width: 1.8; }
  .box-main  { fill: #ffffff; }
  .box-hl    { fill: #fff9db; stroke: #b8860b; stroke-width: 2; }
  .label     { font-family: monospace; font-size: 14px; fill: #222;
               text-anchor: middle; dominant-baseline: central; }
  .arrow-lbl { font-family: sans-serif; font-size: 11px; fill: #666;
               text-anchor: middle; dominant-baseline: central; }
  .badge     { font-family: monospace; font-size: 11px; fill: #b8860b;
               text-anchor: middle; dominant-baseline: central; }
</style>""")

cx           = W // 2              # centre x for boxes and text
RIGHT_COL_X  = BOX_X + BOX_W + 12  # shared x for all right-column annotations

for i, (main, sub, hl) in enumerate(steps):
    y = box_top(i)
    cls = "box box-hl" if hl else "box box-main"
    emit(f'<rect x="{BOX_X}" y="{y}" width="{BOX_W}" height="{BOX_H}" '
         f'rx="{BOX_RX}" class="{cls}"/>')
    emit(f'<text x="{cx}" y="{y + BOX_H // 2}" class="label">{main}</text>')
    if hl:
        # small badge "← this document" to the right of the box
        badge_x = RIGHT_COL_X + 40
        badge_y = y + BOX_H // 2
        emit(f'<text x="{badge_x}" y="{badge_y}" class="badge" '
             f'text-anchor="start">← this document</text>')

    # Draw arrow + label below this box (not after the last one)
    if i < len(arrows):
        arrow_top    = y + BOX_H
        arrow_bottom = arrow_top + GAP
        mid_y        = arrow_top + GAP // 2

        # shaft
        emit(f'<line x1="{cx}" y1="{arrow_top}" x2="{cx}" y2="{arrow_bottom - 8}" '
             f'stroke="{ARROW_COLOR}" stroke-width="1.8"/>')
        # arrowhead
        emit(f'<polygon points="{cx},{arrow_bottom} {cx-5},{arrow_bottom-8} {cx+5},{arrow_bottom-8}" '
             f'fill="{ARROW_COLOR}"/>')
        # label on the right of the shaft
        emit(f'<text x="{RIGHT_COL_X}" y="{mid_y}" class="arrow-lbl" '
             f'text-anchor="start">{arrows[i]}</text>')

emit("</svg>")

svg = "\n".join(lines)
path = os.path.join(OUT, "pipeline.svg")
with open(path, "w") as f:
    f.write(svg)
print(f"  wrote {path}")
