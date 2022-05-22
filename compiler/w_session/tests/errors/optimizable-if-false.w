fn main(a: u32, b: u32): u32 {
    if 0 {
        a
    } else {
        b
    }
}

// more complex case
fn bar(a: u32, b: u32): u32 {
    let c = 1;
    if c ^ c {
        a
    } else {
        b
    }
}