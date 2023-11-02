const c = @import("c.zig");

/// Macro to check ABI compatibility (same major revision number)
pub inline fn WEBP_ABI_IS_INCOMPATIBLE(a: anytype, b: anytype) @TypeOf((a >> @as(c_int, 8)) != (b >> @as(c_int, 8))) {
    return (a >> @as(c_int, 8)) != (b >> @as(c_int, 8));
}

/// Allocates 'size' bytes of memory. Memory must be deallocated by calling
/// `webpFree()`. This function is made available by the core 'libwebp'
/// library.
pub fn webpMalloc(size: usize) error{OutOfMemory}!*anyopaque {
    return c.WebPMalloc(size) orelse error.OutOfMemory;
}

/// Releases memory returned by the `decode*()` functions (from `decode`
/// namespace).
pub inline fn webpFree(ptr: ?*anyopaque) void {
    return c.WebPFree(ptr);
}
