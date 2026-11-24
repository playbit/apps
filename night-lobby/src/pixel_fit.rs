use std::borrow::Cow;

/// Canvas bounds are logical points, while the bitmap is sampled in backing pixels.
pub struct PixelFit {
    pub width: u32,
    pub height: u32,
    pub tex_w: u32,
    pub tex_h: u32,
    pub sample: u32,
}
impl PixelFit {
    pub fn new(area_w: u32, area_h: u32, w: u32, h: u32, density: f32) -> Self {
        let device = |n: u32| (n as f32 * density).ceil() as u32;
        let mut sample = 1;
        while w.div_ceil(sample) > device(area_w) || h.div_ceil(sample) > device(area_h) {
            sample *= 2;
        }
        let (sw, sh) = (w.div_ceil(sample), h.div_ceil(sample));
        let max_zoom = (device(area_w) / sw).min(device(area_h) / sh);
        for z in (1..=max_zoom).rev() {
            let axis = |texels: u32, bound: u32| {
                let start = (((texels * z - 1) as f32 / density) as u32).max(1);
                (start..=bound).find_map(|logical| {
                    let backing = device(logical);
                    (backing >= texels * z && backing % z == 0).then_some((logical, backing / z))
                })
            };
            if let (Some((width, tex_w)), Some((height, tex_h))) =
                (axis(sw, area_w), axis(sh, area_h))
            {
                return Self {
                    width,
                    height,
                    tex_w,
                    tex_h,
                    sample,
                };
            }
        }
        unreachable!("zoom one fits both sampled axes")
    }
    pub fn pixels<'a>(&self, rgba: &'a [u8], w: u32, h: u32) -> Cow<'a, [u8]> {
        if self.sample == 1 && (w, h) == (self.tex_w, self.tex_h) {
            return Cow::Borrowed(rgba);
        }
        let mut out = vec![0; (4 * self.tex_w * self.tex_h) as usize];
        for y in 0..h.div_ceil(self.sample) {
            let sy = (y * self.sample + self.sample / 2).min(h - 1);
            for x in 0..w.div_ceil(self.sample) {
                let sx = (x * self.sample + self.sample / 2).min(w - 1);
                let src = (4 * (sy * w + sx)) as usize;
                let dst = (4 * (y * self.tex_w + x)) as usize;
                out[dst..dst + 4].copy_from_slice(&rgba[src..src + 4]);
            }
        }
        Cow::Owned(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pixels::{H, W};
    #[test]
    fn every_source_texel_occupies_a_whole_square_of_backing_pixels() {
        for density in [0.75, 1., 1.25, 1.5, 2., 2.5, 3.] {
            for (w, h) in [
                (1025, 524),
                (608, 301),
                (387, 225),
                (176, 90),
                (9, 5),
                (1, 1),
            ] {
                let f = PixelFit::new(w, h, W as u32, H as u32, density);
                let dw = (f.width as f32 * density).ceil() as u32;
                let dh = (f.height as f32 * density).ceil() as u32;
                assert!(f.width <= w && f.height <= h);
                assert_eq!(dw % f.tex_w, 0);
                assert_eq!(dh % f.tex_h, 0);
                assert_eq!(dw / f.tex_w, dh / f.tex_h);
                assert!(
                    f.tex_w >= (W as u32).div_ceil(f.sample)
                        && f.tex_h >= (H as u32).div_ceil(f.sample)
                );
            }
        }
    }
}
