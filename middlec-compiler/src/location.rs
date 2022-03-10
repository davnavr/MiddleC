//! Contains code for keeping track of line and column numbers.

pub use std::num::NonZeroUsize as Number;

pub const DEFAULT: Number = unsafe { Number::new_unchecked(1) };

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct Location {
    pub line: Number,
    pub column: Number,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.line, self.column)
    }
}

impl Default for Location {
    fn default() -> Self {
        Self {
            line: DEFAULT,
            column: DEFAULT,
        }
    }
}

#[derive(Clone, Debug)]
struct MapEntry {
    offset: usize,
    line: Number,
}

/// An iterator over the line and column numbers of a source file.
///
/// See [`Map::iter_over`] for more information.
#[derive(Clone, Debug)]
pub struct Iter<'a> {
    lookup: &'a Map,
    range: std::ops::Range<usize>,
}

impl std::iter::Iterator for Iter<'_> {
    type Item = Location;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.lookup.get_location(self.range.next()?))
    }
}

impl std::iter::ExactSizeIterator for Iter<'_> {
    fn len(&self) -> usize {
        self.range.len()
    }
}

impl std::iter::FusedIterator for Iter<'_> {}

/// Maps a byte offset into a UTF-8 source file to a line and column number.
#[derive(Clone, Debug)]
pub struct Map {
    entries: Vec<MapEntry>,
}

impl Map {
    pub fn get_location(&self, offset: usize) -> Location {
        let entry_index = self
            .entries
            .binary_search_by(|entry| entry.offset.cmp(&offset));

        let line;
        // NOTE: Currently, the column number is not calculated correctly for multi-byte characters.
        let column;

        match entry_index {
            Ok(exact_index) => {
                // The offset is to a new line character, so the index is guaranteed to point to a valid entry.
                let entry = unsafe { self.entries.get_unchecked(exact_index) };
                line = entry.line;
                column = offset - entry.offset + 1;
            }
            Err(index) => {
                if index == 0 || self.entries.is_empty() {
                    // Offset is to a character in the first line of the source file.
                    line = DEFAULT;
                    column = offset + 1;
                } else {
                    let entry = self.entries.get(index - 1).expect("valid entry");
                    column = offset - entry.offset + 1;

                    line = if index < self.entries.len() {
                        // Offset is to a character that is not in the last line.
                        entry.line
                    } else {
                        Number::new(entry.line.get() + 1).expect("line number overflow")
                    };
                }
            }
        }

        Location {
            line,
            column: Number::new(column).expect("column number overflow"),
        }
    }

    pub fn iter_over(&self, offsets: std::ops::Range<usize>) -> Iter {
        Iter {
            lookup: self,
            range: offsets,
        }
    }
}

pub struct MapBuilder {
    lookup: Map,
    next_line_number: Number,
}

impl MapBuilder {
    pub(crate) fn push(&mut self, offset: usize) {
        self.next_line_number =
            Number::new(self.next_line_number.get() + 1).expect("line number overflow");

        self.lookup.entries.push(MapEntry {
            offset,
            line: self.next_line_number,
        });
    }

    pub(crate) fn finish(self) -> Map {
        self.lookup
    }
}

impl Default for MapBuilder {
    fn default() -> Self {
        Self {
            lookup: Map {
                entries: Vec::default(),
            },
            next_line_number: DEFAULT,
        }
    }
}
