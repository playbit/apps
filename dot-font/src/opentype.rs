//! Static OpenType fonts with TrueType outlines. One grid cell is 100 font units.
use crate::font::{Glyph, WIDTH};
use kurbo::{BezPath, CubicBez, Point};
use std::collections::BTreeMap;
use tiny_skia::{Path, PathSegment};
use write_fonts::{
    FontBuilder,
    tables::{
        cmap::Cmap,
        glyf::{Bbox, GlyfLocaBuilder, SimpleGlyph},
        head::Head,
        hhea::Hhea,
        hmtx::{Hmtx, LongMetric},
        maxp::Maxp,
        name::{Name, NameRecord},
        os2::{Os2, SelectionFlags},
        post::Post,
    },
    types::{Fixed, GlyphId, NameId, Tag, Version16Dot16},
};

const UNIT: i16 = 100;
const ADVANCE: i16 = WIDTH as i16 * UNIT;
pub fn filename(curves: bool) -> &'static str {
    if curves {
        "DotFont-Curves.ttf"
    } else {
        "DotFont-Pixels.ttf"
    }
}

fn point(p: tiny_skia::Point) -> Point {
    Point::new(
        f64::from(p.x) * f64::from(UNIT),
        -f64::from(p.y) * f64::from(UNIT),
    )
}

fn quadratic_outline(path: &Path) -> BezPath {
    let mut out = BezPath::new();
    let mut pen = Point::ZERO;
    for segment in path.segments() {
        match segment {
            PathSegment::MoveTo(p) => {
                pen = point(p);
                out.move_to(pen);
            }
            PathSegment::LineTo(p) => {
                pen = point(p);
                out.line_to(pen);
            }
            PathSegment::QuadTo(c, p) => {
                pen = point(p);
                out.quad_to(point(c), pen);
            }
            PathSegment::CubicTo(a, b, p) => {
                let end = point(p);
                // Approximation error is under a quarter of one font unit.
                for (_, _, q) in CubicBez::new(pen, point(a), point(b), end).to_quads(0.25) {
                    out.quad_to(q.p1, q.p2);
                }
                pen = end;
            }
            PathSegment::Close => out.close_path(),
        }
    }
    out
}

fn notdef() -> BezPath {
    let mut p = BezPath::new();
    p.move_to((100., 0.));
    p.line_to((100., 800.));
    p.line_to((600., 800.));
    p.line_to((600., 0.));
    p.close_path();
    p.move_to((200., 100.));
    p.line_to((500., 100.));
    p.line_to((500., 700.));
    p.line_to((200., 700.));
    p.close_path();
    p
}

pub fn encode(glyphs: &BTreeMap<char, Glyph>, curves: bool) -> Result<Vec<u8>, String> {
    let count = u16::try_from(glyphs.len() + 1)
        .map_err(|_| "OpenType supports up to 65,535 glyphs per font.")?;
    let mut glyfs = GlyfLocaBuilder::new();
    let mut metrics = Vec::new();
    let mut bounds = Bbox::default();
    let mut max_points = 0;
    let mut max_contours = 0;
    let mut min_right = ADVANCE;
    let mut add = |path: Option<BezPath>| -> Result<(), String> {
        let mut glyph = match path {
            Some(p) => {
                SimpleGlyph::from_bezpath(&p).map_err(|e| format!("Invalid outline: {e:?}"))?
            }
            None => SimpleGlyph::default(),
        };
        glyph.overlaps = curves && glyph.contours.len() > 1;
        let points = glyph.contours.iter().map(|c| c.len()).sum::<usize>();
        max_points = max_points
            .max(u16::try_from(points).map_err(|_| "Too many outline points in one glyph.")?);
        max_contours = max_contours.max(
            u16::try_from(glyph.contours.len()).map_err(|_| "Too many contours in one glyph.")?,
        );
        let b = glyph.bbox;
        bounds = bounds.union(b);
        min_right = min_right.min(ADVANCE - b.x_max);
        metrics.push(LongMetric::new(ADVANCE as u16, b.x_min));
        let glyph = write_fonts::tables::glyf::Glyph::from(glyph);
        glyfs.add_glyph(&glyph).map_err(|e| e.to_string())?;
        Ok(())
    };
    add(Some(notdef()))?;
    for (&ch, glyph) in glyphs {
        let (pixels, rounded) = glyph.outlines();
        let path = if curves { rounded } else { pixels };
        add(path.as_ref().map(quadratic_outline))
            .map_err(|e| format!("U+{:04X}: {e}", ch as u32))?;
    }
    let (glyf, loca, format) = glyfs.build();
    let cmap = Cmap::from_mappings(
        glyphs
            .keys()
            .enumerate()
            .map(|(i, &ch)| (ch, GlyphId::new(i as u32 + 1))),
    )
    .map_err(|e| e.to_string())?;
    let head = Head {
        font_revision: Fixed::ONE,
        units_per_em: 1100,
        lowest_rec_ppem: 11,
        x_min: bounds.x_min,
        y_min: bounds.y_min,
        x_max: bounds.x_max,
        y_max: bounds.y_max,
        index_to_loc_format: format as i16,
        ..Default::default()
    };
    let hhea = Hhea {
        ascender: 1100.into(),
        descender: (-300).into(),
        advance_width_max: (ADVANCE as u16).into(),
        min_left_side_bearing: bounds.x_min.into(),
        min_right_side_bearing: min_right.into(),
        x_max_extent: bounds.x_max.into(),
        caret_slope_rise: 1,
        number_of_h_metrics: count,
        ..Default::default()
    };
    let maxp = Maxp {
        num_glyphs: count,
        max_points: Some(max_points),
        max_contours: Some(max_contours),
        max_zones: Some(1),
        max_composite_points: Some(0),
        max_composite_contours: Some(0),
        max_twilight_points: Some(0),
        max_storage: Some(0),
        max_function_defs: Some(0),
        max_instruction_defs: Some(0),
        max_stack_elements: Some(0),
        max_size_of_instructions: Some(0),
        max_component_elements: Some(0),
        max_component_depth: Some(0),
    };
    let family = if curves {
        "Dot Font Curves"
    } else {
        "Dot Font Pixels"
    };
    let ps = filename(curves).trim_end_matches(".ttf");
    let mut names = Vec::new();
    for (id, value) in [
        (1, family),
        (2, "Regular"),
        (3, ps),
        (4, family),
        (5, "Version 1.000"),
        (6, ps),
    ] {
        names.push(NameRecord::new(
            3,
            1,
            0x409,
            NameId::new(id),
            value.to_owned().into(),
        ));
    }
    let mut ranges = [0u32; 4];
    for &ch in glyphs.keys() {
        let code = ch as u32;
        if code > 0xffff {
            ranges[1] |= 1 << 25;
        } // OS/2 bit 57 covers non-BMP characters.
        for &(start, end, bit) in &write_fonts::read::tables::os2::OS2_UNICODE_RANGES {
            if (start..=end).contains(&code) {
                ranges[usize::from(bit / 32)] |= 1 << (bit % 32);
            }
        }
    }
    let os2 = Os2 {
        x_avg_char_width: ADVANCE,
        us_weight_class: 400,
        us_width_class: 5,
        fs_selection: SelectionFlags::REGULAR,
        us_first_char_index: glyphs.keys().next().copied().unwrap_or(' ') as u32 as u16,
        us_last_char_index: (glyphs.keys().next_back().copied().unwrap_or(' ') as u32).min(65535)
            as u16,
        s_typo_ascender: 1100,
        s_typo_descender: -300,
        us_win_ascent: 1100.max(bounds.y_max) as u16,
        us_win_descent: 300.max(-bounds.y_min) as u16,
        ach_vend_id: Tag::new(b"NONE"),
        y_subscript_x_size: 650,
        y_subscript_y_size: 650,
        y_subscript_y_offset: 140,
        y_superscript_x_size: 650,
        y_superscript_y_size: 650,
        y_superscript_y_offset: 420,
        y_strikeout_size: 70,
        y_strikeout_position: 300,
        ul_unicode_range_1: ranges[0],
        ul_unicode_range_2: ranges[1],
        ul_unicode_range_3: ranges[2],
        ul_unicode_range_4: ranges[3],
        ul_code_page_range_1: Some(1),
        ul_code_page_range_2: Some(0),
        sx_height: Some(600),
        s_cap_height: Some(800),
        us_default_char: Some(0),
        us_break_char: Some(32),
        us_max_context: Some(1),
        ..Default::default()
    };
    let post = Post {
        version: Version16Dot16::VERSION_3_0,
        underline_position: (-125).into(),
        underline_thickness: 75.into(),
        is_fixed_pitch: 1,
        ..Default::default()
    };
    let mut font = FontBuilder::new();
    macro_rules! add_table {
        ($table:expr) => {
            font.add_table(&$table).map_err(|e| e.to_string())?
        };
    }
    add_table!(head);
    add_table!(hhea);
    add_table!(maxp);
    add_table!(Hmtx::new(metrics, vec![]));
    add_table!(Name::new(names));
    add_table!(os2);
    add_table!(post);
    add_table!(cmap);
    add_table!(glyf);
    add_table!(loca);
    Ok(font.build())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::{self, Cell, Link};
    use write_fonts::read::{FontRef, TableProvider};

    #[test]
    fn exported_font_maps_every_character_and_reflects_line_edits() {
        let mut glyphs = font::seed();
        let pixel_bytes = encode(&glyphs, false).unwrap();
        let curve_bytes = encode(&glyphs, true).unwrap();
        for bytes in [&pixel_bytes, &curve_bytes] {
            let font = FontRef::new(bytes).unwrap();
            let cmap = font.cmap().unwrap();
            let glyf = font.glyf().unwrap();
            let loca = font.loca(None).unwrap();
            assert_eq!(
                usize::from(font.maxp().unwrap().num_glyphs()),
                glyphs.len() + 1
            );
            for (&ch, g) in &glyphs {
                let id = cmap.map_codepoint(ch).unwrap();
                let outline = loca.get(id, &glyf).unwrap().into_glyph();
                assert_eq!(outline.is_some(), g.points().next().is_some(), "{ch:?}");
            }
        }
        glyphs
            .get_mut(&'s')
            .unwrap()
            .set_link(Link::between(Cell(5, 4), Cell(4, 3)), true);
        assert_eq!(encode(&glyphs, false).unwrap(), pixel_bytes);
        assert_ne!(encode(&glyphs, true).unwrap(), curve_bytes);
    }
}
