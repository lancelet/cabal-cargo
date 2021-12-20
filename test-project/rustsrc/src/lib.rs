
#[no_mangle]
pub extern "C" fn succ_i8(x: i8) -> i8 {
    x + 1
}

#[no_mangle]
pub extern "C" fn succ_i16(x: i16) -> i16 {
    x + 1
}

#[no_mangle]
pub extern "C" fn succ_i32(x: i32) -> i32 {
    x + 1
}
