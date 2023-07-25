use std::time::{SystemTime, UNIX_EPOCH};

pub fn get_current_unix_time() -> f64 {
    let now = SystemTime::now();
    let unix_time = now.duration_since(UNIX_EPOCH).unwrap();
    unix_time.as_secs_f64() * 1000.0
}
