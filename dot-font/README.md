# Dot font

Dot font is a Rust font editor for Playbit. It opens on the lowercase **a** from the supplied DM font. All 1,079 glyphs share a seven-cell advance. Each glyph starts with a point at every occupied cell of the imported bitmap. A glyph stores the lines between its points and an optional tangent at each end. The editor, glyph picker, and specimen share the same glyph data.

Run `pb run` in this directory with the Playbit CLI on your path.

## Editing

- In **Curves**, click a blank cell to add a point. Drag from a point to aim its tangent in one of eight directions. Double-click a point to turn its direction off or back on. Select **Auto** to restore its inferred tangent.
- Select a point and choose **Delete** on the grid or **Delete point** in its context menu. Removing an interior point joins its two neighbors. Removing an endpoint selects the next connected point while the line has at least two points left. At a junction, deletion pairs arms that continue most directly through it. Undo restores the original point and connections. The installed Playbit canvas receives the arrow keys and Space.
- In **Curves**, turn on **Lines** from the grid or its right-click menu and drag between two existing points to connect them. Drag between the same endpoints again to cut the line. The gray centerlines show which points connect. Turn off **Lines** to edit tangents. The installed Playbit canvas exposes primary-button drag gestures.
- In **Pixels**, each anchor occupies one cell and lines fill in the cells between anchors. At a sharp tip, the two legs round outward to keep the opening visible. A lowercase `v` drawn from (1, 5) to (3, 0) to (5, 5) reproduces the eleven pixels of the DM lowercase `v` with three anchors. Start on an empty cell to paint, or on ink to erase. Drag to apply the same override across several cells. Each touched cell keeps its on/off value through later curve edits. A small corner mark identifies overridden cells on the grid.
- To return a cell to its automatic value, turn on **Reset pixels** and click the cell. Drag to reset several cells in one gesture. Click **Reset pixels** again to resume painting.
- The glyph palette lists characters in ascending Unicode code point order. Choose a glyph or page through the list with the chevrons. The **+** button and the Unicode badge open a character by text or code point, such as `U+03A9`. An unused code point creates a blank glyph.
- Click the specimen to edit its text. Click its size to cycle through 11, 16, 22, and 33 px.
- Click anywhere on the circle-and-square toggle in the lower right, or press Space with the canvas focused, to switch rendering modes. The filled icon shows the active mode. The arrow keys browse the palette or aim a selected point. Undo and redo appear in the upper-right corner of the grid after an edit.

Glyph edits are saved by gesture in the root volume’s `glyph-edits` log. Each record identifies the Unicode character and any changes to points, connections, or pixel overrides. Legacy point-only edits are replayed against the imported bitmap. Coordinates use columns 0–6 and baseline-relative rows −3 through 10. Cell values are 0 for empty, 1 for automatic, 2–9 for compass directions clockwise from east, and 10–18 for a disabled tangent with its previous setting. The specimen, selection, and undo history belong to the current window.

## OpenType export

Click **Export…** in the specimen panel and choose **Export Pixels** or **Export Curves**. Both files use OpenType with TrueType outlines and include all current Unicode glyphs. **Export Pixels** includes the per-cell overrides. **Export Curves** uses the points, tangents, and lines. The two styles have separate family names so you can install both.

Without a folder grant, exports go to the app’s Playbit volume. After saving, the dialog displays a copyable `pb file-read` command to retrieve the font into the current folder on your Mac. To export directly to a folder, grant it at launch using `pb run --fs /path/to/folder:Exports:rw`. The dialog selects that folder by default. Repeat exports to a Mac folder create numbered copies such as `DotFont-Curves-2.ttf`. Repeat exports to the volume update the corresponding font file.
