// The face byte is shared with older clients. Each menu owns only its bits.
use crate::guest::{Guest, LOOKS};
use playbit_api::ui::{ButtonStyle, TextStyle, ViewEncoder};

const OUTFIT: u32 = 20;

struct Feature {
    menu: u32,
    label: &'static str,
    mask: u8,
    choices: &'static [(&'static str, u8)],
}

const FEATURES: &[Feature] = &[
    Feature {
        menu: 30,
        label: "Skin tone",
        mask: 0x03,
        choices: &[
            ("Light", 0),
            ("Medium light", 1),
            ("Medium", 2),
            ("Deep", 3),
        ],
    },
    Feature {
        menu: 40,
        label: "Hair color",
        mask: 0x0c,
        choices: &[
            ("Dark brown", 0),
            ("Chestnut", 4),
            ("Plum", 8),
            ("Honey", 12),
        ],
    },
    Feature {
        menu: 50,
        label: "Hairstyle",
        mask: 0x90,
        choices: &[
            ("Fringe", 0),
            ("Curls", 0x80),
            ("Fringe with ponytail", 0x10),
            ("Curls with ponytail", 0x90),
        ],
    },
    Feature {
        menu: 60,
        label: "Accessories",
        mask: 0x60,
        choices: &[
            ("None", 0),
            ("Cap", 0x20),
            ("Glasses", 0x40),
            ("Cap and glasses", 0x60),
        ],
    },
];

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct Appearance {
    outfit: u8,
    face: u8,
}

impl From<&Guest> for Appearance {
    fn from(guest: &Guest) -> Self {
        Self {
            outfit: guest.look,
            face: guest.face,
        }
    }
}

impl Appearance {
    pub fn apply(self, guest: &mut Guest) {
        guest.look = self.outfit;
        guest.face = self.face;
    }

    pub fn refresh(&mut self, previous: Self, saved: Self) {
        if self.outfit == previous.outfit {
            self.outfit = saved.outfit;
        }
        for feature in FEATURES {
            if self.face & feature.mask == previous.face & feature.mask {
                self.face = (self.face & !feature.mask) | (saved.face & feature.mask);
            }
        }
    }

    pub fn click(&mut self, action: u32, expanded: &mut Option<u32>) {
        if action == OUTFIT || FEATURES.iter().any(|field| field.menu == action) {
            *expanded = (Some(action) != *expanded).then_some(action);
            return;
        }
        let Some(menu) = *expanded else { return };
        let Some(i) = action.checked_sub(menu + 1) else {
            return;
        };
        if menu == OUTFIT && (i as usize) < LOOKS.len() {
            self.outfit = i as u8;
        } else if let Some(feature) = FEATURES.iter().find(|field| field.menu == menu)
            && let Some(&(_, bits)) = feature.choices.get(i as usize)
        {
            self.face = (self.face & !feature.mask) | bits;
        } else {
            return;
        }
        *expanded = None;
    }

    pub fn controls(self, view: &mut ViewEncoder, expanded: Option<u32>) {
        menu(
            view,
            OUTFIT,
            "Outfit",
            LOOKS.iter().map(|choice| choice.0),
            self.outfit as usize,
            expanded == Some(OUTFIT),
        );
        for feature in FEATURES {
            let selected = feature
                .choices
                .iter()
                .position(|&(_, bits)| bits == self.face & feature.mask)
                .expect("every saved face has a matching option");
            menu(
                view,
                feature.menu,
                feature.label,
                feature.choices.iter().map(|choice| choice.0),
                selected,
                expanded == Some(feature.menu),
            );
        }
    }
}

fn menu(
    view: &mut ViewEncoder,
    id: u32,
    label: &str,
    options: impl Iterator<Item = &'static str> + Clone,
    selected: usize,
    expanded: bool,
) {
    let value = options
        .clone()
        .nth(selected)
        .expect("saved option is in range");
    view.row_start();
    view.text(TextStyle::Label, label);
    let arrow = if expanded { "▴" } else { "▾" };
    view.button(
        id,
        &format!("{value}  {arrow}"),
        ButtonStyle::Default,
        false,
    );
    view.row_end();
    if expanded {
        let mut choices = options.enumerate().peekable();
        while choices.peek().is_some() {
            view.row_start();
            for (i, name) in choices.by_ref().take(2) {
                let style = if i == selected {
                    ButtonStyle::Primary
                } else {
                    ButtonStyle::Default
                };
                view.button(id + 1 + i as u32, name, style, false);
            }
            view.row_end();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn incoming_appearance_updates_preserve_the_edit_in_progress() {
        let previous = Appearance { outfit: 0, face: 0 };
        let saved = Appearance {
            outfit: 4,
            face: 255,
        };
        let mut draft = previous;
        let mut expanded = None;
        draft.click(40, &mut expanded);
        draft.click(42, &mut expanded); // Chestnut.
        draft.refresh(previous, saved);
        assert_eq!(draft.face, (saved.face & !0x0c) | 4);
        assert_eq!(draft.outfit, 4);
    }
}
