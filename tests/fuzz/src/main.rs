use std::os::fd::RawFd;

#[link(name = "coverage", kind = "dylib")]
extern "C" {
    fn __sanitizer_cov_reset_edgeguards();
}

const REPRL_CRFD: RawFd = 100;
const REPRL_CWFD: RawFd = 101;
const REPRL_DRFD: RawFd = 102;
const REPRL_DWFD: RawFd = 103;

fn main() {
    let mut i = 0;
    loop {
        if i % 2 == 0 {
            println!("one");
        } else {
            println!("two");
        }

        unsafe {
            __sanitizer_cov_reset_edgeguards();
        }

        i += 1;
    }
}
