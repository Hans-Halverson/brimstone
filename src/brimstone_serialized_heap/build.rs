use std::{
    env, fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use brimstone_core::{
    common::options::OptionsBuilder,
    runtime::{gc::HeapSerializer, ContextBuilder},
};

/// Generate the default serialized heap and embed into the binary.
fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let out_path = Path::new(&out_dir);
    let dest_path = out_path.join("generated_serialized_heap.rs");

    let serialized_heap_file = gen_serialized_heap_file(out_path);

    fs::write(&dest_path, &serialized_heap_file).unwrap();
}

/// A 4 MB heap is sufficient
const HEAP_SIZE: usize = 4 * 1024 * 1024;

fn gen_serialized_heap_file(out_path: &Path) -> String {
    let options = OptionsBuilder::new().heap_size(HEAP_SIZE).build();
    let cx = ContextBuilder::new().set_options(Rc::new(options)).build();

    let serializer = HeapSerializer::serialize(cx);
    let serialized_heap = serializer.as_serialized();

    let permanent_space_file =
        write_bytes_to_file(serialized_heap.permanent_space.bytes, out_path, "permanent_space.in");
    let current_space_file =
        write_bytes_to_file(serialized_heap.current_space.bytes, out_path, "current_space.in");

    let root_offsets_file = write_bytes_to_file(
        usize_slice_to_u8_slice(serialized_heap.root_offsets),
        out_path,
        "root_offsets.in",
    );

    format!(
        r#"
use brimstone_core::common::serialized_heap::{{SerializedHeap, SerializedSemispace}};

/// Align a sequence of bytes to a the alignment of the given type.
/// 
/// Needs #[repr(C)] to guarantee that bytes comes after `_align`.
#[repr(C)]
struct AlignedBytes<Align, Bytes: ?Sized = [u8]> {{
    _align: [Align; 0],
    bytes: Bytes, 
}}

const SERIALIZED_PERMANENT_SPACE: &[u8] = include_bytes!("{}");
const SERIALIZED_CURRENT_SPACE: &[u8] = include_bytes!("{}");
const SERIALIZED_ROOT_OFFSETS: &[usize] = u8_slice_to_usize_slice(&ALIGNED_SERIALIZED_ROOT_OFFSETS.bytes);

const fn u8_slice_to_usize_slice(slice: &[u8]) -> &[usize] {{
    let num_usizes = slice.len() / std::mem::size_of::<usize>();
    unsafe {{ std::slice::from_raw_parts(slice.as_ptr().cast(), num_usizes) }}
}}

/// Must make sure that byte sequence has the right alignment so that it can be read as a &[usize].
static ALIGNED_SERIALIZED_ROOT_OFFSETS: &AlignedBytes<usize> = &AlignedBytes {{
    _align: [],
    bytes: *include_bytes!("{}"),
}};

/// The default serialized heap embedded in the binary.
pub const SERIALIZED_HEAP: SerializedHeap<'static> = SerializedHeap {{
    permanent_space: SerializedSemispace {{ bytes: SERIALIZED_PERMANENT_SPACE, start_offset: {} }},
    current_space: SerializedSemispace {{ bytes: SERIALIZED_CURRENT_SPACE, start_offset: {} }},
    root_offsets: SERIALIZED_ROOT_OFFSETS,
    heap_info_size: {},
}};
"#,
        permanent_space_file.to_string_lossy(),
        current_space_file.to_string_lossy(),
        root_offsets_file.to_string_lossy(),
        serialized_heap.permanent_space.start_offset,
        serialized_heap.current_space.start_offset,
        serialized_heap.heap_info_size,
    )
}

fn write_bytes_to_file(bytes: &[u8], out_path: &Path, file_name: &str) -> PathBuf {
    let file = out_path.join(file_name).to_path_buf();
    fs::write(&file, bytes).unwrap();
    file
}

const fn usize_slice_to_u8_slice(slice: &[usize]) -> &[u8] {
    let num_bytes = std::mem::size_of_val(slice);
    unsafe { std::slice::from_raw_parts(slice.as_ptr().cast(), num_bytes) }
}
