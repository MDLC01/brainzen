//! This module contains types related to memory management.
//!
//! There is a distinction between *abstract* and *concrete* memory: abstract memory corresponds to
//! the abstract model that is used to interact with the concrete memory—the actual cells on the
//! tape when executing the final Brainfuck program.
//!
//! # Abstract memory
//!
//! The abstract memory model consists of cells and pages.
//!
//! A *page* is a block of adjacent cells, conceptually infinitely expandable to the right. Pages do
//! not overlap. A *cell* is the information of a page and a (positive) offset within this page.
//!
//! Both [`Page`] and [`Cell`] types do not contain any data. Instead, they can be though of as
//! pointers to chunks in the concrete memory.
//!
//! ## Special pages
//!
//! Most pages belong to the *internal memory* of a subroutine. That is, its variables and temporary
//! results. There are two exceptions to that: the input and output pages.
//!
//! The *input page* is a page that already exists before a subroutine is invoked, and contains its
//! arguments.
//!
//! The *output page* is a page that is never freed within the execution of a subroutine, and in
//! which its return value is written.
//!
//! # Concrete memory
//!
//! The concrete memory consists of a one-dimensional tape of 8-bit integers, infinitely expandable
//! to the right. An [`Allocator`] is used to decide what part of the tape to associate with each
//! page, and thus, where each abstract cell is stored.
//!
//! When allocating memory for a page, the `Allocator` needs to know the size of the page. This
//! means the amount of cells that are actually used in a, theoretically infinite, page has to be
//! tracked somewhere else. Similarly, the `Allocator` does not free unused memory automatically, so
//! the lifetime of each page needs to be determined by the used of the `Allocator`.
//!
//! ## A note on semantics
//!
//! In the context of the Brainfuck tape, "to the right" or "after" refer to the part of the tape
//! with greater indices than the part currently being considered. Whether or not the later is
//! included depends on the context.
//!
//! We index cells of the tape such that:
//! - The pointer starts at cell 0 (the *origin*), and
//! - All considered cells have positive (or null), consecutive, indices.


use std::collections::HashMap;
use std::ops::Add;

use crate::generator::memory::chunk_manager::AvailableChunkManager;

/// The identifier of a [`Page`] in the internal memory of a subroutine.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct InternalPage(usize);

/// An abstract block of adjacent cells in memory, with no right-bound.
///
/// See [module level documentation](self) for more details.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum Page {
    /// The page containing the inputs of a subroutine.
    Input,
    /// The page containing the return value of a function.
    Output,
    /// A page in the internal memory of a subroutine.
    Internal(InternalPage),
}

impl Page {
    /// Creates a new `Page` with the specified `id`.
    pub fn with_id(id: usize) -> Self {
        Self::Internal(InternalPage(id))
    }

    /// Returns the cell at a given offset in this page.
    pub fn at(self, offset: usize) -> Cell {
        Cell {
            page: self,
            offset,
        }
    }

    /// Returns the first cell of this page.
    pub fn base(self) -> Cell {
        self.at(0)
    }

    /// If this page belongs to the internal memory of a subroutine, returns the corresponding
    /// [`InternalPage`].
    pub fn get_internal_page_id(self) -> Option<InternalPage> {
        match self {
            Self::Internal(id) => Some(id),
            _ => None,
        }
    }
}

impl From<InternalPage> for Page {
    fn from(value: InternalPage) -> Self {
        Self::Internal(value)
    }
}


/// An abstract cell in memory, belonging to a [`Page`].
///
/// See [module level documentation](self) for more details.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct Cell {
    /// The the page this cell belongs to.
    pub page: Page,
    /// The offset of this cell within its page.
    pub offset: usize,
}

impl Cell {
    /// Returns a cell that is a specific amount to the right of this cell.
    pub fn offset(self, offset: usize) -> Self {
        Self {
            page: self.page,
            offset: self.offset + offset,
        }
    }
}


/// Represents known information about the value of a cell.
///
/// Each `CellValue` can be interpreted as a set of possible values for the cell.
#[derive(Copy, Clone, Debug)]
pub enum CellValue {
    /// A known exact value.
    ///
    /// This is a singleton.
    Known(u8),
    /// Unknown value.
    ///
    /// This is the set containing all values.
    Unknown,
}

impl Default for CellValue {
    /// Returns the set of possible default values of a cell: [`CellValue::Known(0)`].
    fn default() -> Self {
        Self::Known(0)
    }
}

impl Add<u8> for CellValue {
    type Output = Self;

    /// Adds a specific amount to this value.
    fn add(self, rhs: u8) -> Self::Output {
        match self {
            Self::Known(value) => Self::Known(value.wrapping_add(rhs)),
            Self::Unknown => Self::Unknown,
        }
    }
}

impl Add<CellValue> for CellValue {
    type Output = Self;

    /// Adds another value to this value.
    fn add(self, rhs: CellValue) -> Self::Output {
        match (self, rhs) {
            (Self::Known(value), Self::Known(other_value)) => Self::Known(value.wrapping_add(other_value)),
            _ => Self::Unknown,
        }
    }
}


/// The index of a chunk (or a concrete cell) on the tape.
pub type ChunkIndex = usize;
/// The size of a memory chunk.
pub type ChunkSize = usize;


/// Contains [`AvailableChunkManager`].
mod chunk_manager {
    use std::collections::{BTreeMap, BTreeSet};
    use std::num::NonZeroUsize;

    use crate::generator::memory::{ChunkIndex, ChunkSize};
    use crate::utils::with_owned::WithOwned;

    /// Common behavior for [`Chunk`] and [`NonEmptyChunk`].
    trait ChunkDescriptor: Copy {
        /// The type of the size of a chunk.
        type Size: Copy + Into<ChunkSize>;

        /// Creates a new chunk with the given parameters.
        fn new(start: ChunkIndex, size: Self::Size) -> Self;

        /// Returns the index of the first cell of this chunk.
        fn start(self) -> ChunkIndex;

        /// Returns the number of cells in this chunk.
        fn size(self) -> Self::Size;

        /// Returns the index of the first cell after this chunk.
        ///
        /// If this chunk is zero-sized, that is the first cell after this chunk's
        /// [start](Self::start). Otherwise, that is the first cell after this chunk's last cell.
        #[inline]
        fn upper_bound(self) -> ChunkIndex {
            self.start() + self.size().into()
        }

        /// Tests if this chunk contains a specific cell.
        #[inline]
        fn contains(self, index: ChunkIndex) -> bool {
            (self.start()..self.upper_bound()).contains(&index)
        }

        /// Tests if this chunk is fully contained within another chunk.
        ///
        /// If this chunk is empty, it is considered to be a subchunk of any chunk.
        #[inline]
        fn is_subchunk(self, other: impl ChunkDescriptor) -> bool {
            other.start() <= self.start() && self.upper_bound() <= other.upper_bound()
        }
    }

    /// A chunk is a block of adjacent concrete memory cells.
    ///
    /// This structure is used internally by [`AvailableChunkManager`].
    ///
    /// If you want a chunk whose size is guaranteed to be non-zero, use [`NonEmptyChunk`].
    #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
    struct Chunk {
        /// The index of the first cell of the chunk.
        index: ChunkIndex,
        /// The amount of cells in the chunk.
        size: ChunkSize,
    }

    impl ChunkDescriptor for Chunk {
        type Size = ChunkSize;

        #[inline]
        fn new(start: ChunkIndex, size: Self::Size) -> Self {
            Self { index: start, size }
        }

        #[inline]
        fn start(self) -> ChunkIndex {
            self.index
        }

        #[inline]
        fn size(self) -> Self::Size {
            self.size
        }
    }

    impl Chunk {
        /// Creates a new chunk that starts at `start` and whose size is `upper_bound - start`.
        ///
        /// Note that `start` is included within the chunk, while `upper_bound` is not.
        pub fn with_bounds(start: usize, upper_bound: usize) -> Self {
            Self {
                index: start,
                size: upper_bound - start,
            }
        }

        /// If possible, converts this [`Chunk`] to a [`NonEmptyChunk`]. Otherwise, returns
        /// [`None`].
        #[inline]
        pub fn into_non_empty(self) -> Option<NonEmptyChunk> {
            NonZeroUsize::new(self.size)
                .map(|size| NonEmptyChunk { index: self.index, size })
        }
    }

    /// A [`Chunk`] whose size is not zero.
    #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
    struct NonEmptyChunk {
        /// The index of the first cell of the chunk.
        index: ChunkIndex,
        /// The amount of cells in the chunk.
        size: NonZeroUsize,
    }

    impl ChunkDescriptor for NonEmptyChunk {
        type Size = NonZeroUsize;

        #[inline]
        fn new(start: ChunkIndex, size: Self::Size) -> Self {
            Self { index: start, size }
        }

        #[inline]
        fn start(self) -> ChunkIndex {
            self.index
        }

        #[inline]
        fn size(self) -> Self::Size {
            self.size
        }
    }

    impl From<(ChunkIndex, ChunkSize)> for Chunk {
        /// Converts a `(start, size)` pair to a [`Chunk`].
        #[inline]
        fn from((index, size): (ChunkIndex, ChunkSize)) -> Self {
            Self { index, size }
        }
    }

    impl From<(ChunkIndex, NonZeroUsize)> for NonEmptyChunk {
        /// Converts a `(start, size)` pair to a [`NonEmptyChunk`].
        #[inline]
        fn from((index, size): (usize, NonZeroUsize)) -> Self {
            Self { index, size }
        }
    }

    impl From<NonEmptyChunk> for Chunk {
        #[inline]
        fn from(NonEmptyChunk { index, size }: NonEmptyChunk) -> Self {
            Self { index, size: size.get() }
        }
    }


    /// A set of chunks of adjacent cells on the tape. Always contain an unbounded chunk.
    ///
    /// Adjacent chunks are merged automatically each operation, so that each chunk has a maximal
    /// size.
    #[derive(Default, Debug)]
    pub struct AvailableChunkManager {
        /// Available chunks by first cell index.
        by_start: BTreeMap<ChunkIndex, NonZeroUsize>,
        /// Available chunks by size.
        by_size: BTreeMap<NonZeroUsize, BTreeSet<ChunkIndex>>,
        /// The index of the first available cell such that all cells to its right are available.
        ///
        /// The, conceptually infinite to the right, chunk starting on this cell is called the
        /// *unbounded chunk*.
        unbounded_start: ChunkIndex,
    }

    impl AvailableChunkManager {
        // We define the following invariants:
        //  (1a) For any entry `(start, size)` of `by_start`, `size` is a key of `by_size` and its
        //       value contains the element `start`;
        //  (1b) For any entry `(size, starts)` of `by_size`, and any element `start` of `starts`,
        //       `(start, size)` is an entry of `by_start`;
        //   (2) For any entry key `size` of `by_size`, there exists an entry of `by_start` whose
        //       value is `size`;
        //   (3) Chunks do not overlap (and cannot be further merged): for any entries
        //       `(start1, size1)` and `(start2, size2)` of `by_start`, `start1 + size1 < start2` or
        //       `start2 + size2 < start1`, or `(start1, size1) == (start2, size2)`;
        //   (4) Same, with the unbounded chunk: for any entry `(start, size)` of `by_start`,
        //       `start + size < unbounded_start`.
        //
        // Note that (2), together with (1a), requires that all sets of `by_size` are non-empty.
        //
        // Any public method preserves all invariants. Any method, public or private, requires all
        // invariants to hold when called, unless explicitly mentioned otherwise in its
        // documentation.

        /// Tests if invariants (1a) and (1b) hold.
        #[cfg(debug_assertions)]
        fn test_invariants_1(&self) -> bool {
            let test_1a = self.by_start.iter()
                .all(|(start, size)| {
                    self.by_size.get(size)
                        .is_some_and(|starts| starts.contains(start))
                });
            let test_1b = self.by_size.iter()
                .all(|(size, starts)| {
                    starts.iter()
                        .all(|start| {
                            self.by_start.get(start) == Some(size)
                        })
                });
            test_1a && test_1b
        }

        /// Tests if invariant (2) holds.
        #[cfg(debug_assertions)]
        fn test_invariant_2(&self) -> bool {
            self.by_size.keys()
                .all(|size| {
                    self.by_start.iter()
                        .any(|(_, s)| s == size)
                })
        }

        /// Tests if invariant (3) holds.
        #[cfg(debug_assertions)]
        fn test_invariant_3(&self) -> bool {
            self.by_start.iter()
                .all(|(start1, size1)| {
                    self.by_start.iter()
                        .all(|(start2, size2)| {
                            (start1, size1) == (start2, size2)
                                || start1 + size1.get() < *start2
                                || start2 + size2.get() < *start1
                        })
                })
        }

        /// Tests if invariant (4) holds.
        #[cfg(debug_assertions)]
        fn test_invariant_4(&self) -> bool {
            self.by_start.iter()
                .all(|(start, size)| {
                    start + size.get() < self.unbounded_start
                })
        }

        /// Creates a new [`AvailableChunkManager`] containing a single, unbounded, chunk, starting
        /// on the first cell.
        pub fn new() -> Self {
            // All invariants are trivially verified, as all maps are empty.
            let manager = Self::default();
            debug_assert!(manager.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(manager.test_invariant_2(), "invariant (2) should hold");
            debug_assert!(manager.test_invariant_3(), "invariant (3) should hold");
            debug_assert!(manager.test_invariant_4(), "invariant (4) should hold");
            manager
        }

        /// Tests if a chunk is fully contained within chunks of this set.
        ///
        /// Note that, by (3) and (4), this is equivalent to testing if the chunk is fully contained
        /// within a single chunk.
        ///
        /// A zero-sized chunk is always considered covered.
        fn is_covered(&self, chunk: Chunk) -> bool {
            debug_assert!(self.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
            debug_assert!(self.test_invariant_3(), "invariant (3) should hold");
            debug_assert!(self.test_invariant_4(), "invariant (4) should hold");
            if chunk.size() == 0 || chunk.start() >= self.unbounded_start {
                return true;
            }
            if let Some(potential_container) =
                self.by_start
                    .range(..=chunk.start())
                    .next_back()
                    .with_owned()
            {
                chunk.is_subchunk(NonEmptyChunk::from(potential_container))
            } else {
                false
            }
        }

        /// Returns the index and size of a bounded chunk containing the specified index. If no such
        /// chunk exists, returns [`None`].
        ///
        /// Note that, by invariant (3), if such a chunk exists, it is unique.
        ///
        /// # Invariants
        ///
        /// This method only requires invariants (3) and (4) to hold when called.
        fn get_bounded_container(&self, index: ChunkIndex) -> Option<NonEmptyChunk> {
            debug_assert!(self.test_invariant_3(), "invariant (3) should hold");
            debug_assert!(self.test_invariant_4(), "invariant (4) should hold");
            let chunk = NonEmptyChunk::from(
                self.by_start.range(..=index)
                    .next_back()?
                    .with_owned()
            );
            if chunk.contains(index) {
                Some(chunk)
            } else {
                None
            }
        }

        /// Adds a chunk without trying to merge it with nearby chunks.
        ///
        /// This method is intended to be used to safely add a chunk whose size was already
        /// maximized.
        ///
        /// # Invariants
        ///
        /// This method preserves invariants (1a), (1b) and (2). (3) and (4) may be broken if the
        /// caller is not careful to call this method with a chunk that does not overlap any chunk
        /// in this set. It is the caller's responsibility to call this method with such a chunk.
        #[inline]
        fn add_merged(&mut self, chunk: NonEmptyChunk) {
            debug_assert!(self.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
            // Invariants:
            //  - (1a) and (1b) are both preserved because we insert `(start, size)` in `by_start`
            //    and `start` into the set corresponding to `size` in `by_size`;
            //  - (2) is preserved because we never remove an element from any value of `by_size`.
            let previous = self.by_start.insert(chunk.start(), chunk.size());
            debug_assert!(previous.is_none());
            self.by_size.entry(chunk.size()).or_default().insert(chunk.start());
            debug_assert!(self.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
        }

        /// Removes the chunk that starts at a specific index. If no such chunk exist, the behavior
        /// is undefined.
        ///
        /// # Invariants
        ///
        /// This method preserves all invariants.
        ///
        /// This method only requires (1a) and (1b) to hold when called. It will preserve any other
        /// invariant that holds when called.
        fn remove(&mut self, start: ChunkIndex) {
            debug_assert!(self.test_invariants_1(), "invariant (1a) and (1b) should hold");
            #[cfg(debug_assertions)] let had_invariant_2 = self.test_invariant_2();
            #[cfg(debug_assertions)] let had_invariant_3 = self.test_invariant_3();
            #[cfg(debug_assertions)] let had_invariant_4 = self.test_invariant_4();
            // Invariants:
            //  - (1a) is preserved because, if we remove an entry `(start, size)` from `by_start`,
            //    we remove the corresponding element in `by_size` immediately after;
            //  - (1b) is preserved because we only remove an element `start` from `by_size`, under
            //    the key `size`, if we removed `(start, key)` from `by_start` before;
            //  - (2) is preserved because we remove the value associated with `start` in `by_size`
            //    if it becomes empty;
            //  - (3) is preserved because removing a chunk cannot cause chunks to overlap;
            //  - (4) is preserved because the chunk we remove is a bounded chunk, which is
            //    separated from the unbounded chunk by (4) at the start of the call.
            let Some(size) = self.by_start.remove(&start) else {
                panic!("the index passed to `remove` should be the start of a chunk")
            };
            // SAFETY: By invariant (1a), `size` is a key of `by_size`.
            let starts = self.by_size.get_mut(&size).unwrap();
            starts.remove(&start);
            if starts.is_empty() {
                self.by_size.remove(&size);
            }
            debug_assert!(self.test_invariants_1(), "invariant (1a) and (1b) should hold");
            #[cfg(debug_assertions)]
            if had_invariant_2 {
                debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
            }
            #[cfg(debug_assertions)]
            if had_invariant_3 {
                debug_assert!(self.test_invariant_3(), "invariant (3) should hold");
            }
            #[cfg(debug_assertions)]
            if had_invariant_4 {
                debug_assert!(self.test_invariant_4(), "invariant (4) should hold");
            }
        }

        /// Adds a new chunk to the set, merging it with adjacent chunks if possible.
        ///
        /// If the chunk is zero-sized, or a subchunk of another chunk, nothing happens.
        pub fn make_available(&mut self, start: ChunkIndex, size: ChunkSize) {
            debug_assert!(self.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
            debug_assert!(self.test_invariant_3(), "invariant (3) should hold");
            debug_assert!(self.test_invariant_4(), "invariant (4) should hold");
            // Invariants:
            //  - (1a), (1b) and (2) are preserved because we delegate all mutable operations to
            //    methods that preserve those invariants;
            //  - (3) is preserved thanks to parts I–IV;
            //  - (4) is preserved because we update `unbounded_start` appropriately in part V.
            // Also note that the required invariants hold when a method is called (see their
            // respective documentations).
            let chunk = Chunk::new(start, size);
            // Part I: Test if the chunk to add is already covered.
            if self.is_covered(chunk) {
                return;
            }
            // Part II: Remove subchunks of the chunk to add.
            let subchunks = self.by_start
                .range(chunk.start()..chunk.upper_bound())
                .filter(|&potential_subchunk| {
                    !NonEmptyChunk::from(potential_subchunk.with_owned()).is_subchunk(chunk)
                })
                .map(|(&i, _)| i)
                .collect::<Vec<_>>();
            for subchunk in subchunks {
                self.remove(subchunk)
            }
            // Part III: Remove the chunk the chunk to add starts in (or right after).
            let final_start =
                if let Some(previous_index) = chunk.start().checked_sub(1) {
                    if let Some(left_chunk) = self.get_bounded_container(previous_index) {
                        self.remove(left_chunk.start());
                        left_chunk.start()
                    } else {
                        chunk.start()
                    }
                } else {
                    chunk.start()
                };
            // Part IV: Remove the chunk the chunk to add ends in (or right before).
            let final_upper_bound =
                if let Some(right_chunk) = self.get_bounded_container(chunk.upper_bound()) {
                    self.remove(right_chunk.start());
                    right_chunk.upper_bound()
                } else {
                    chunk.upper_bound()
                };
            // Part V: Add the final chunk.
            let final_chunk = Chunk::with_bounds(final_start, final_upper_bound);
            if final_chunk.upper_bound() >= self.unbounded_start {
                self.unbounded_start = final_chunk.start();
            } else if let Some(non_empty_final_chunk) = final_chunk.into_non_empty() {
                self.add_merged(non_empty_final_chunk)
            }
            debug_assert!(self.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
            debug_assert!(self.test_invariant_3(), "invariant (3) should hold");
            debug_assert!(self.test_invariant_4(), "invariant (4) should hold");
        }

        /// Removes and returns a bounded chunk with the desired size. If no chunk with this exact
        /// size exists, creates one by breaking down a larger chunk. If all bounded chunks have a
        /// size smaller than the desired size, returns [`None`].
        ///
        /// # Invariants
        ///
        /// This method preserves all invariants.
        #[inline]
        fn pop_bounded_chunk(&mut self, desired_size: NonZeroUsize) -> Option<ChunkIndex> {
            debug_assert!(self.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
            debug_assert!(self.test_invariant_3(), "invariant (3) should hold");
            debug_assert!(self.test_invariant_4(), "invariant (4) should hold");
            // Part I: Find a large enough chunk.
            // Invariants for part I:
            //  - (1a) is preserved because, if we remove an entry `(start, size)` from `by_start`,
            //    we remove the corresponding element in `by_size` immediately after;
            //  - (1b) is preserved because we only remove an element `start` from `by_size`, under
            //    the key `size`, if we removed `(start, key)` from `by_start` before;
            //  - (2) is preserved because we remove the value associated with `start` in `by_size`
            //    if it becomes empty;
            //  - (3) is preserved because removing a chunk cannot cause chunks to overlap;
            //  - (4) is preserved because the chunk we remove is a bounded chunk, which is
            //    separated from the unbounded chunk by (4) at the start of the call.
            let (&size, potential_starts) = self.by_size.range_mut(desired_size..).next()?;
            // SAFETY: By invariant (2), `potential_starts`, as value of `by_size`, cannot be empty.
            let start = potential_starts.pop_first().unwrap();
            if potential_starts.is_empty() {
                self.by_size.remove(&size);
            }
            self.by_start.remove(&start);
            // Part II: Mark the remaining cells as available.
            // Invariants for part II:
            //  - (1a), (1b) and (2) are preserved because we only mutate `by_start` and `by_size`
            //    through `add_merge`, which preserves (1a), (1b) and (2);
            //  - (3) is preserved because we call `add_merged` with a chunk whose size is
            //    maximized, as its size is maximized within a super-chunk whose size was already
            //    maximized, by (3) at the beginning of the call;
            //  - (4) is preserved because the only chunk we add is a subchunk of a chunk whose
            //    existence did not break (4) at the beginning of the call, and `unbounded_start`
            //    was not updated since then.
            if let Some(remaining_size) = NonZeroUsize::new(size.get() - desired_size.get()) {
                let remaining_chunk = NonEmptyChunk::new(start + desired_size.get(), remaining_size);
                self.add_merged(remaining_chunk);
            }
            debug_assert!(self.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
            debug_assert!(self.test_invariant_3(), "invariant (3) should hold");
            debug_assert!(self.test_invariant_4(), "invariant (4) should hold");
            Some(start)
        }

        /// Removes a chunk of a specific size from this set and returns its first index.
        ///
        /// If the size is null, nothing changes and an arbitrary index is returned.
        pub fn require(&mut self, size: ChunkSize) -> ChunkIndex {
            debug_assert!(self.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
            debug_assert!(self.test_invariant_3(), "invariant (3) should hold");
            debug_assert!(self.test_invariant_4(), "invariant (4) should hold");
            // Invariants:
            //  - (1a), (1b), (2) and (3) are preserved because we only mutate `by_start` and
            //    `by_size` through `pop_bounded_chunk`, which preserves all invariants;
            //  - (4) is preserved because `pop_bounded_chunk` preserves all invariants,
            let Some(size) = NonZeroUsize::new(size) else {
                return 0;
            };
            let index =
                if let Some(index) = self.pop_bounded_chunk(size) {
                    index
                } else {
                    let index = self.unbounded_start;
                    self.unbounded_start += size.get();
                    index
                };
            debug_assert!(self.test_invariants_1(), "invariants (1a) and (1b) should hold");
            debug_assert!(self.test_invariant_2(), "invariant (2) should hold");
            debug_assert!(self.test_invariant_3(), "invariant (3) should hold");
            debug_assert!(self.test_invariant_4(), "invariant (4) should hold");
            index
        }

        /// Returns the index of the first cell of the unbounded chunk.
        pub fn get_unbounded_chunk_start(&self) -> ChunkIndex {
            self.unbounded_start
        }
    }
}


/// An `Allocator` is used to decide where each abstract [`Cell`] will be stored concretely in the
/// tape when executing the final Brainfuck program.
///
/// See [module level documentation](self) for more details.
#[derive(Debug)]
pub struct Allocator {
    /// For each used chunk, its size.
    used_chunks: HashMap<Page, (ChunkIndex, ChunkSize)>,
    available_chunks: AvailableChunkManager,
}

impl Allocator {
    /// Creates a new allocator, with reserved chunks for the input and output.
    pub fn new(input_size: usize, output_size: usize) -> Self {
        let mut used_chunks = HashMap::new();
        let mut available_chunks = AvailableChunkManager::new();
        let input_index = available_chunks.require(input_size);
        used_chunks.insert(Page::Input, (input_index, input_size));
        let output_index = available_chunks.require(input_size);
        used_chunks.insert(Page::Output, (output_index, output_size));
        Self { used_chunks, available_chunks }
    }

    /// Returns the index of an abstract cell on the tape.
    pub fn index(&self, cell: Cell) -> ChunkIndex {
        let index = self.used_chunks.get(&cell.page)
            .map(|(index, _)| *index)
            .expect("`cell` should belong to an allocated page");
        index + cell.offset
    }

    /// Returns the index of the first cell of the input on the tape.
    ///
    /// Note that the input might be zero-sized.
    pub fn input_index(&self) -> ChunkIndex {
        self.index(Page::Input.base())
    }

    /// Returns the index of the first cell of the output on the tape.
    ///
    /// Note that the output might be zero-sized.
    pub fn output_index(&self) -> ChunkIndex {
        self.index(Page::Output.base())
    }

    /// Allocates a chunk of cells for a page and returns the index of its first cell on the tape.
    ///
    /// The chunk is allowed to be zero-sized.
    pub fn alloc(&mut self, page: InternalPage, size: ChunkSize) -> ChunkIndex {
        let index = self.available_chunks.require(size);
        self.used_chunks.insert(page.into(), (index, size));
        index
    }

    /// Frees a chunk of cells, making it reusable for chunks allocated later on.
    pub fn free(&mut self, id: InternalPage) {
        let (index, size) = self.used_chunks.remove(&id.into())
            .expect("an unused chunk should not be freed");
        self.available_chunks.make_available(index, size);
    }

    /// Returns the size of the stack frame.
    ///
    /// In other words, this is the index of the first concrete cell after which no cell is
    /// allocated on the tape.
    ///
    /// When a subroutine is called, its origin should be a concrete cell to the right of the cell
    /// returned by this method.
    pub fn frame_size(&self) -> usize {
        self.available_chunks.get_unbounded_chunk_start()
    }
}
