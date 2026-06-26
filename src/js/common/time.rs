use std::time::{SystemTime, UNIX_EPOCH};

pub fn get_current_unix_time_millis() -> u128 {
    let now = SystemTime::now();
    let unix_time = now.duration_since(UNIX_EPOCH).unwrap();
    unix_time.as_millis()
}

pub fn get_current_unix_time_nanos() -> u128 {
    let now = SystemTime::now();
    let unix_time = now.duration_since(UNIX_EPOCH).unwrap();
    unix_time.as_nanos()
}
