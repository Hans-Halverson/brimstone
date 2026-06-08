use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    sync::Mutex,
};

/// Utility for reserving unique file paths.
pub struct FileNameReserver {
    used_paths: Mutex<HashSet<PathBuf>>,
}

impl FileNameReserver {
    pub fn new() -> Self {
        Self { used_paths: Mutex::new(HashSet::new()) }
    }

    /// Reserve a unique file path given the directory, file name, and extension.
    ///
    /// Aggressively sanitizes the file name. Attempts to use the provided name but may need to
    /// append suffixes for uniqueness.
    pub fn reserve_unique_path(
        &self,
        directory: &Path,
        file_name: &str,
        extension: &str,
    ) -> PathBuf {
        let sanitized = sanitize_file_name(file_name);

        let mut used_paths = self.used_paths.lock().unwrap();

        let mut path = directory.join(&sanitized).with_extension(extension);
        let mut suffix = 2;
        while used_paths.contains(&path) {
            path = directory
                .join(format!("{sanitized}_{suffix}"))
                .with_extension(extension);
            suffix += 1;
        }

        used_paths.insert(path.clone());

        path
    }
}

/// Sanitize a name into a filesystem-safe name by replacing non-alphanumeric characters and
/// truncating to a reasonable length.
fn sanitize_file_name(name: &str) -> String {
    const MAX_LEN: usize = 128;

    let sanitized: String = name
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .take(MAX_LEN)
        .collect();

    if sanitized.is_empty() {
        "EMPTY".to_owned()
    } else {
        sanitized
    }
}
