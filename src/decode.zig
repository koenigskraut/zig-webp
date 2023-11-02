const std = @import("std");

const c = @import("c.zig");
const types = @import("types.zig");

pub const DECODER_ABI_VERSION = 0x0209;

/// Enumeration of the status codes
pub const VP8Status = enum(c_uint) {
    Ok = 0,
    OutOfMemory,
    InvalidParam,
    BitstreamError,
    UnsupportedFeature,
    Suspended,
    UserAbort,
    NotEnoughData,

    pub const VP8Error = error{
        OutOfMemory,
        InvalidParam,
        BitstreamError,
        UnsupportedFeature,
        Suspended,
        UserAbort,
        NotEnoughData,
    };

    pub fn toErr(self: VP8Status) VP8Error!VP8Status {
        return switch (self) {
            .Ok => .Ok,
            .OutOfMemory => VP8Error.OutOfMemory,
            .InvalidParam => VP8Error.InvalidParam,
            .BitstreamError => VP8Error.BitstreamError,
            .UnsupportedFeature => VP8Error.UnsupportedFeature,
            .Suspended => VP8Error.Suspended,
            .UserAbort => VP8Error.UserAbort,
            .NotEnoughData => VP8Error.NotEnoughData,
        };
    }
};

/// Colorspaces
/// Note: the naming describes the byte-ordering of packed samples in memory.
/// For instance, `BGRA` relates to samples ordered as B,G,R,A,B,G,R,A,...
/// Non-capital names (e.g.:`Argb`) relates to pre-multiplied RGB channels.
/// RGBA-4444 and RGB-565 colorspaces are represented by following byte-order:
///
/// RGBA-4444: [r3 r2 r1 r0 g3 g2 g1 g0], [b3 b2 b1 b0 a3 a2 a1 a0], ...
///
/// RGB-565: [r4 r3 r2 r1 r0 g5 g4 g3], [g2 g1 g0 b4 b3 b2 b1 b0], ...
///
/// In the case `WEBP_SWAP_16BITS_CSP` is defined, the bytes are swapped for
/// these two modes:
///
/// RGBA-4444: [b3 b2 b1 b0 a3 a2 a1 a0], [r3 r2 r1 r0 g3 g2 g1 g0], ...
///
/// RGB-565: [g2 g1 g0 b4 b3 b2 b1 b0], [r4 r3 r2 r1 r0 g5 g4 g3], ...
pub const ColorspaceMode = enum(c_uint) {
    RGB = 0,
    RGBA = 1,
    BGR = 2,
    BGRA = 3,
    ARGB = 4,
    RGBA_4444 = 5,
    RGB_565 = 6,
    // RGB-premultiplied transparent modes (alpha value is preserved)
    rgbA = 7,
    bgrA = 8,
    Argb = 9,
    rgbA_4444 = 10,
    // YUV modes must come after RGB ones.
    YUV = 11,
    YUVA = 12, // yuv 4:2:0
    LAST = 13,

    pub inline fn isPremultipliedMode(self: ColorspaceMode) bool {
        return switch (self) {
            .rgbA, .bgrA, .Argb, .rgbA_444 => true,
            else => false,
        };
    }

    pub inline fn isAlphaMode(self: ColorspaceMode) bool {
        return switch (self) {
            .RGBA, .BGRA, .ARGB, .RGBA_4444, .YUVA => true,
            else => self.isPremultipliedMode(),
        };
    }

    pub inline fn isRGBMode(self: ColorspaceMode) bool {
        return @intFromEnum(self) < @intFromEnum(ColorspaceMode.YUV);
    }
};

/// Generic structure for describing the output sample buffer.
pub const RGBABuffer = extern struct { // view as RGBA
    /// pointer to RGBA samples
    rgba: [*c]u8,
    /// stride in bytes from one scanline to the next.
    stride: c_int,
    /// total size of the `rgba` buffer.
    size: usize,
};

/// view as YUVA
pub const YUVABuffer = extern struct {
    /// pointer to luma samples
    y: [*c]u8,
    /// pointer to chroma U samples
    u: [*c]u8,
    /// pointer to chroma V samples
    v: [*c]u8,
    /// pointer to alpha samples
    a: [*c]u8,
    /// luma stride
    y_stride: c_int,
    /// chroma U stride
    u_stride: c_int,
    /// chroma V stride
    v_stride: c_int,
    /// alpha stride
    a_stride: c_int,
    /// luma plane size
    y_size: usize,
    /// chroma U plane size
    u_size: usize,
    /// chroma V plane size
    v_size: usize,
    /// alpha-plane size
    a_size: usize,
};

/// Output buffer
pub const DecBuffer = extern struct {
    /// Colorspace.
    colorspace: ColorspaceMode,
    /// X dimension.
    width: c_int,
    /// Y dimension.
    height: c_int,
    /// If non-zero, 'internal_memory' pointer is not used. If value is '2' or
    /// more, the external memory is considered 'slow' and multiple read/write
    /// will be avoided.
    is_external_memory: c_int,
    /// Nameless union of buffer parameters.
    u: extern union {
        RGBA: RGBABuffer,
        YUVA: YUVABuffer,
    },
    /// padding for later use
    pad: [4]u32,
    /// Internally allocated memory (only when is_external_memory is 0). Should
    /// not be used externally, but accessed via the buffer union.
    private_memory: [*c]u8,

    /// Initialize the structure as empty. Must be called before any other use.
    /// Returns false in case of version mismatch
    pub inline fn init(self: *DecBuffer) c_int {
        return self.initInternal(DECODER_ABI_VERSION);
    }

    /// Internal, version-checked, entry point
    pub fn initInternal(self: *DecBuffer, version: c_int) c_int {
        return c.WebPInitDecBufferInternal(self, version);
    }

    /// Free any memory associated with the buffer. Must always be called last.
    ///
    /// Note: doesn't free the 'buffer' structure itself.
    pub fn free(self: *DecBuffer) void {
        c.WebPFreeDecBuffer(self);
    }
};

/// Features gathered from the bitstream
pub const BitstreamFeatures = extern struct {
    /// Width in pixels, as read from the bitstream.
    width: c_int,
    /// Height in pixels, as read from the bitstream.
    height: c_int,
    /// True if the bitstream contains an alpha channel.
    has_alpha: c_int,
    /// True if the bitstream is an animation.
    has_animation: c_int,
    /// 0 = undefined (/mixed), 1 = lossy, 2 = lossless
    format: c_int,
    /// padding for later use
    pad: [5]u32,

    /// Internal, version-checked, entry point
    pub fn getFeaturesInternal(data: []const u8, features: *BitstreamFeatures, version: c_int) VP8Status {
        return c.WebPGetFeaturesInternal(data.ptr, data.len, features, version);
    }
};

/// Retrieve features from the bitstream. The `features.*` structure is filled
/// with information gathered from the bitstream.
///
/// Returns `VP8StatusCode.Ok` when the features are successfully retrieved.
/// Returns `VP8StatusCode.NotEnoughData` when more data is needed to retrieve
/// the features from headers. Returns error in other cases.
///
/// Note: The following chunk sequences (before the raw VP8/VP8L data) are
/// considered valid by this function:
/// - RIFF + VP8(L)
/// - RIFF + VP8X + (optional chunks) + VP8(L)
/// - ALPH + VP8 <-- Not a valid WebP format: only allowed for internal
/// purpose.
/// - VP8(L)     <-- Not a valid WebP format: only allowed for internal
/// purpose.
pub inline fn getFeatures(data: []const u8, features: *BitstreamFeatures) VP8Status.VP8Error!VP8Status {
    const status = BitstreamFeatures.getFeaturesInternal(data, features, DECODER_ABI_VERSION);
    return if (status == .NotEnoughData) status else status.toErr();
}

/// Decoding options.
pub const DecoderOptions = extern struct {
    /// if true, skip the in-loop filtering
    bypass_filtering: c_int,
    /// if true, use faster pointwise upsampler
    no_fancy_upsampling: c_int,
    /// if true, cropping is applied _first_
    use_cropping: c_int,
    /// left position for cropping. Will be snapped to even values.
    crop_left: c_int,
    /// top position for cropping. Will be snapped to even values.
    crop_top: c_int,
    /// X dimension of the cropping area
    crop_width: c_int,
    /// Y dimension of the cropping area
    crop_height: c_int,
    /// if true, scaling is applied _afterward_
    use_scaling: c_int,
    /// final resolution width
    scaled_width: c_int,
    /// final resolution height
    scaled_height: c_int,
    /// if true, use multi-threaded decoding
    use_threads: c_int,
    /// dithering strength (0=Off, 100=full)
    dithering_strength: c_int,
    /// if true, flip output vertically
    flip: c_int,
    /// alpha dithering strength in [0..100]
    alpha_dithering_strength: c_int,
    /// padding for later use
    pad: [5]u32,
};

/// Main object storing the configuration for advanced decoding.
pub const DecoderConfig = extern struct {
    /// Immutable bitstream features (optional)
    input: BitstreamFeatures,
    /// Output buffer (can point to external mem)
    output: DecBuffer,
    /// Decoding options
    options: DecoderOptions,

    /// Internal, version-checked, entry point
    pub inline fn initInternal(self: *DecoderConfig, version: c_int) c_int {
        return c.WebPInitDecoderConfigInternal(self, version);
    }

    /// Initialize the configuration as empty. This function must always be
    /// called first, unless `getFeatures()` is to be called.
    pub inline fn init(self: *DecoderConfig) error{VersionMismatch}!void {
        if (initInternal(self, DECODER_ABI_VERSION) == 0) return error.VersionMismatch;
    }
};

/// Return the decoder's version number, packed in hexadecimal using 8bits for
/// each of major/minor/revision. E.g: v2.5.7 is 0x020507.
pub inline fn getDecoderVersion() u24 {
    return @truncate(@as(u32, @bitCast(c.WebPGetDecoderVersion())));
}

/// Retrieve basic header information: `struct { width, height }`.
///
/// This function will also validate the header, returning error on failure.
///
/// Note: The following chunk sequences (before the raw VP8/VP8L data) are
/// considered valid by this function:
/// ```code
/// RIFF + VP8(L)
/// RIFF + VP8X + (optional chunks) + VP8(L)
/// ALPH + VP8 <-- Not a valid WebP format: only allowed for internal purpose.
/// VP8(L)     <-- Not a valid WebP format: only allowed for internal purpose.
/// ```
pub fn getInfo(data: []const u8) error{MalformedHeader}!struct { u32, u32 } {
    var width: c_int, var height: c_int = .{ undefined, undefined };
    if (c.WebPGetInfo(data.ptr, data.len, &width, &height) == 0) return error.MalformedHeader;
    return .{ @intCast(width), @intCast(height) };
}

inline fn cToZ(i: c_int) u32 {
    return @intCast(i);
}

pub const RGBImage = struct {
    data: []u8,
    width: u32,
    height: u32,
    mode: ColorspaceMode,

    pub fn init(ptr: [*]u8, width_c: c_int, height_c: c_int, mode: ColorspaceMode) RGBImage {
        const width, const height = .{ cToZ(width_c), cToZ(height_c) };
        return .{ .data = ptr[0 .. width * height * 4], .width = width, .height = height, .mode = mode };
    }

    pub fn deinit(self: *const RGBImage) void {
        types.webpFree(self.data.ptr);
    }

    pub fn tuple(self: RGBImage) struct { []u8, u32, u32 } {
        return .{ self.data, self.width, self.height };
    }
};

/// Decodes WebP images pointed to by `data` and returns RGBA samples, along
/// with the dimensions in `RGBImage` structure. The ordering of samples
/// in memory is R, G, B, A, R, G, B, A... in scan order (endian-independent).
///
/// **NOTE**: The returned data should be deleted with `RGBImage.deinit()`.
pub fn decodeRGBA(data: []const u8) error{DecodingError}!RGBImage {
    var width_c: c_int, var height_c: c_int = .{ undefined, undefined };
    const ptr = c.WebPDecodeRGBA(data.ptr, data.len, &width_c, &height_c) orelse return error.DecodingError;
    return RGBImage.init(ptr, width_c, height_c, .RGBA);
}

/// Same as `decodeRGBA()`, but returning A, R, G, B, A, R, G, B... ordered
/// data.
pub fn decodeARGB(data: []const u8) error{DecodingError}!RGBImage {
    var width_c: c_int, var height_c: c_int = .{ undefined, undefined };
    const ptr = c.WebPDecodeARGB(data.ptr, data.len, &width_c, &height_c) orelse return error.DecodingError;
    return RGBImage.init(ptr, width_c, height_c, .ARGB);
}

/// Same as `decodeRGBA()`, but returning B, G, R, A, B, G, R, A...
/// ordered data.
pub fn decodeBGRA(data: []const u8) error{DecodingError}!RGBImage {
    var width_c: c_int, var height_c: c_int = .{ undefined, undefined };
    const ptr = c.WebPDecodeBGRA(data.ptr, data.len, &width_c, &height_c) orelse return error.DecodingError;
    return RGBImage.init(ptr, width_c, height_c, .BGRA);
}

/// Same as `decodeRGBA()`, but returning R, G, B, R, G, B... ordered data.
///
/// If the bitstream contains transparency, it is ignored.
pub fn decodeRGB(data: []const u8) error{DecodingError}!RGBImage {
    var width_c: c_int, var height_c: c_int = .{ undefined, undefined };
    const ptr = c.WebPDecodeRGB(data.ptr, data.len, &width_c, &height_c) orelse return error.DecodingError;
    return RGBImage.init(ptr, width_c, height_c, .RGB);
}

/// Same as `decodeRGB()`, but returning B, G, R, B, G, R... ordered data.
pub fn decodeBGR(data: []const u8) error{DecodingError}!RGBImage {
    var width_c: c_int, var height_c: c_int = .{ undefined, undefined };
    const ptr = c.WebPDecodeBGR(data.ptr, data.len, &width_c, &height_c) orelse return error.DecodingError;
    return RGBImage.init(ptr, width_c, height_c, .BGR);
}

pub const YUVImage = struct {
    data_y: []u8,
    data_u: []u8,
    data_v: []u8,
    width: u32,
    height: u32,
    stride: u32,
    uv_stride: u32,
    mode: ColorspaceMode,

    pub fn deinit(self: *const YUVImage) void {
        types.webpFree(self.data_y.ptr);
    }
};

/// Decode WebP images pointed to by `data` to Y'UV format (also named
/// [Y'CbCr](https://en.wikipedia.org/wiki/YCbCr)). The dimension of the U and
/// V planes are both `(width + 1) / 2` and `(height + 1) / 2`.
///
/// Upon return, the Y buffer has a stride returned as `YUVImage.stride`, while
/// U and V have a common stride returned as `YUVImage.uv_stride`.
///
/// **NOTE**: The returned data should be deleted with `YUVImage.deinit()`.
pub fn decodeYUV(data: []const u8) error{DecodingError}!YUVImage {
    var width_c: c_int, var height_c: c_int = .{ undefined, undefined };
    var ptr_u: [*c]u8, var ptr_v: [*c]u8 = .{ undefined, undefined };
    var stride_c: c_int, var uv_stride_c: c_int = .{ undefined, undefined };
    const ptr_y = c.WebPDecodeYUV(data.ptr, data.len, &width_c, &height_c, &ptr_u, &ptr_v, &stride_c, &uv_stride_c) orelse return error.DecodingError;
    const width, const height, const stride, const uv_stride = .{ cToZ(width_c), cToZ(height_c), cToZ(stride_c), cToZ(uv_stride_c) };
    return YUVImage{
        .data_y = ptr_y[0 .. height * stride],
        .data_u = ptr_u[0 .. (height + 1) / 2 * uv_stride],
        .data_v = ptr_v[0 .. (height + 1) / 2 * uv_stride],
        .width = width,
        .height = height,
        .stride = stride,
        .uv_stride = uv_stride,
        .mode = .YUV,
    };
}

/// Variant of `decodeRGBA()`, that decode the image directly into a
/// pre-allocated buffer `output_buffer`.
///
/// The parameter `output_stride` specifies the distance (in bytes) between
/// scanlines. Hence, `output_buffer` is expected to be at least
/// `output_stride` x picture-height in size.
pub fn decodeRGBAInto(data: []const u8, output_buffer: []u8, output_stride: u32) error{DecodingError}!void {
    _ = c.WebPDecodeRGBAInto(data.ptr, data.len, output_buffer.ptr, output_buffer.len, @intCast(output_stride)) orelse
        return error.DecodingError;
}

/// Same as `decodeRGBAInto()`, but returning A, R, G, B, A, R, G, B... ordered
/// data.
pub fn decodeARGBInto(data: []const u8, output_buffer: []u8, output_stride: u32) error{DecodingError}!void {
    _ = c.WebPDecodeARGBInto(data.ptr, data.len, output_buffer.ptr, output_buffer.len, @intCast(output_stride)) orelse
        return error.DecodingError;
}

/// Same as `decodeRGBAInto()`, but returning B, G, R, A, B, G, R, A...
/// ordered data.
pub fn decodeBGRAInto(data: []const u8, output_buffer: []u8, output_stride: u32) error{DecodingError}!void {
    _ = c.WebPDecodeBGRAInto(data.ptr, data.len, output_buffer.ptr, output_buffer.len, @intCast(output_stride)) orelse
        return error.DecodingError;
}

/// Same as `decodeRGBAInto()`, but returning R, G, B, R, G, B... ordered data.
///
/// If the bitstream contains transparency, it is ignored.
pub fn decodeRGBInto(data: []const u8, output_buffer: []u8, output_stride: u32) error{DecodingError}!void {
    _ = c.WebPDecodeRGBInto(data.ptr, data.len, output_buffer.ptr, output_buffer.len, @intCast(output_stride)) orelse
        return error.DecodingError;
}

/// Same as `decodeRGBInto()`, but returning B, G, R, B, G, R... ordered data.
pub fn decodeBGRInto(data: []const u8, output_buffer: []u8, output_stride: u32) error{DecodingError}!void {
    _ = c.WebPDecodeBGRInto(data.ptr, data.len, output_buffer.ptr, output_buffer.len, @intCast(output_stride)) orelse
        return error.DecodingError;
}

/// `decodeYUVInto()` is a variant of `decodeYUV()` that operates directly into
/// pre-allocated luma/chroma plane buffers. This function requires the strides
/// to be passed: one for the luma plane and one for each of the chroma ones.
///
/// Pointer to the luma plane ('*luma') is returned or NULL if an error
/// occurred during decoding (or because some buffers were found to be too
/// small).
pub fn decodeYUVInto(data: []const u8, luma: []u8, luma_stride: c_int, u: []u8, u_stride: c_int, v: []u8, v_stride: c_int) error{DecodingError}!void {
    _ = c.WebPDecodeYUVInto(data.ptr, data.len, luma.ptr, luma.len, luma_stride, u.ptr, u.len, u_stride, v.ptr, v.len, v_stride) orelse
        return error.DecodingError;
}

//------------------------------------------------------------------------------
// Incremental decoding
//
// This API allows streamlined decoding of partial data.
// Picture can be incrementally decoded as data become available thanks to the
// WebPIDecoder object. This object can be left in a SUSPENDED state if the
// picture is only partially decoded, pending additional input.
// Code example:
//  WebPInitDecBuffer(&output_buffer);
//  output_buffer.colorspace = mode;
//  ...
//  WebPIDecoder* idec = WebPINewDecoder(&output_buffer);
//  while (additional_data_is_available) {
//    // ... (get additional data in some new_data[] buffer)
//    status = WebPIAppend(idec, new_data, new_data_size);
//    if (status != VP8_STATUS_OK && status != VP8_STATUS_SUSPENDED) {
//      break;    // an error occurred.
//    }

//    // The above call decodes the current available buffer.
//    // Part of the image can now be refreshed by calling
//    // WebPIDecGetRGB()/WebPIDecGetYUVA() etc.
//  }
//  WebPIDelete(idec);

pub const IDecoder = opaque {
    /// Creates a new incremental decoder with the supplied buffer parameter.
    ///
    /// This `output_buffer` can be passed `null`, in which case a default
    /// output buffer is used (with `ColorspaceMode.RGB`). Otherwise, an
    /// internal reference to 'output_buffer' is kept, which means that the
    /// lifespan of 'output_buffer' must be larger than that of the returned
    /// `IDecoder` object.
    ///
    /// The supplied `output_buffer` content MUST NOT be changed between calls
    /// to `IDecoder.append()` or `IDecoder.update()` unless
    /// `output_buffer.is_external_memory` is not set to 0. In such a case, it
    /// is allowed to modify the pointers, size and stride of
    /// `output_buffer.u.RGBA` or `output_buffer.u.YUVA`, provided they remain
    /// within valid bounds.
    ///
    /// All other fields of `DecBuffer` MUST remain constant between calls.
    pub fn new(output_buffer: ?*DecBuffer) error{OutOfMemory}!*IDecoder {
        return c.WebPINewDecoder(output_buffer) orelse error.OutOfMemory;
    }

    /// This function allocates and initializes an incremental-decoder object,
    /// which will output the RGB/A samples specified by `csp` into a
    /// preallocated buffer `output_buffer`. The stride (distance in bytes
    /// between two scanlines) of this buffer is specified by `output_stride`.
    /// Additionally, `output_buffer` can be passed `null` in which case the
    /// output buffer will be allocated automatically when the decoding starts.
    /// The colorspace `csp` is taken into account for allocating this buffer.
    /// All other parameters are ignored.
    ///
    /// Possible failure is either allocation or parameter error.
    pub fn newRGB(csp: ColorspaceMode, output_buffer: ?[]u8, output_stride: u17) error{IDecoderCreateError}!*IDecoder {
        return c.WebPINewRGB(
            csp,
            if (output_buffer) |o| o.ptr else null,
            if (output_buffer) |o| o.len else 0,
            @intCast(output_stride),
        ) orelse error.IDecoderCreateError;
    }

    /// This function allocates and initializes an incremental-decoder object,
    /// which will output the raw luma/chroma samples into a preallocated
    /// planes if supplied. The luma plane is specified by the slice `luma` and
    /// its stride `luma_stride`. Similarly, the chroma-u plane is specified by
    /// the `u` and `u_stride` parameters, and the chroma-v plane by `v` and
    /// `v_stride`. And same for the alpha-plane. The `a` can be pass `null` in
    /// case one is not interested in the transparency plane. Conversely,
    /// `luma` can be passed `null` if no preallocated planes are supplied. In
    /// this case, the output buffer will be automatically allocated (using
    /// `ColorspaceMode.YUVA`) when decoding starts. All parameters are then
    /// ignored.
    ///
    /// Possible failure is either allocation or parameter error.
    pub fn newYUVA(luma: ?[]u8, luma_stride: u17, u: ?[]u8, u_stride: u17, v: ?[]u8, v_stride: u17, a: ?[]u8, a_stride: u17) error{IDecoderCreateError}!*IDecoder {
        return c.WebPINewYUVA(
            if (luma) |l| l.ptr else null,
            if (luma) |l| l.len else 0,
            @intCast(luma_stride),
            if (luma) |_| u.ptr else null,
            if (luma) |_| u.len else 0,
            @intCast(u_stride),
            if (luma) |_| v.ptr else null,
            if (luma) |_| v.len else 0,
            @intCast(v_stride),
            if (a) |a_| a_.ptr else null,
            if (a) |a_| a_.len else 0,
            @intCast(a_stride),
        ) orelse error.IDecoderCreateError;
    }

    /// Deprecated version of the `IDecoder.newYUVA()`, without the alpha
    /// plane.
    ///
    /// Kept for backward compatibility.
    pub fn newYUV(luma: ?[]u8, luma_stride: u17, u: ?[]u8, u_stride: u17, v: ?[]u8, v_stride: u17) error{IDecoderCreateError}!*IDecoder {
        c.WebPINewYUV(
            if (luma) |l| l.ptr else null,
            if (luma) |l| l.len else 0,
            @intCast(luma_stride),
            if (luma) |_| u.ptr else null,
            if (luma) |_| u.len else 0,
            @intCast(u_stride),
            if (luma) |_| v.ptr else null,
            if (luma) |_| v.len else 0,
            @intCast(v_stride),
        ) orelse error.IDecoderCreateError;
    }

    /// Deletes the `IDecoder` object and associated memory. Must always be
    /// called if `IDecoder.new()`, `IDecoder.newRGB()` or `IDecoder.newYUV()`
    /// succeeded.
    pub fn delete(self: *IDecoder) void {
        c.WebPIDelete(self);
    }

    /// Copies and decodes the next available data. Returns `.Ok` when the
    /// image is successfully decoded. Returns `.Suspended` when more data is
    /// expected. Returns error in other cases.
    pub fn append(self: *IDecoder, data: []const u8) VP8Status.VP8Error!VP8Status {
        const status = c.WebPIAppend(self, data.ptr, data.len);
        return if (status == .Suspended) status else status.toErr();
    }

    /// A variant of the `IDecoder.append()` to be used when data buffer
    /// contains partial data from the beginning. In this case data buffer is
    /// not copied to the internal memory.
    ///
    /// Note that the value of the `data` slice can change between calls to
    /// `IDecoder.update()`, for instance when the data buffer is resized to
    /// fit larger data.
    pub fn update(self: *IDecoder, data: []const u8) VP8Status.VP8Error!VP8Status {
        const status = c.WebPIUpdate(self, data.ptr, data.len);
        return if (status == .Suspended) status else status.toErr();
    }

    pub const PartialImageRGB = struct {
        /// index of last decoded row in raster scan order
        last_y: u16,
        width: u16,
        height: u16,
        stride: u17,
        data: []u8,
    };

    /// Returns the RGB/A image decoded so far. Returns
    /// `error.OutputNotInitialized` if output params are not initialized yet.
    /// The RGB/A output type corresponds to the colorspace specified during
    /// call to `IDecoder.new()` or `IDecoder.newRGB()`.
    ///
    /// `PartialImage.last_y` is the index of last decoded row in raster scan
    /// order.
    pub fn getRGB(self: *const IDecoder) error{OutputNotInitialized}!PartialImageRGB {
        var r: struct { last_y: c_int, width: c_int, height: c_int, stride: c_int } = undefined;
        const ptr = c.WebPIDecGetRGB(self, &r.last_y, &r.width, &r.height, &r.stride) orelse return error.OutputNotInitialized;
        const last_y: u16 = @truncate(@as(c_uint, @intCast(r.last_y)));
        const width: u16 = @truncate(@as(c_uint, @intCast(r.width)));
        const height: u16 = @truncate(@as(c_uint, @intCast(r.height)));
        const stride: u17 = @truncate(@as(c_uint, @intCast(r.stride)));
        return PartialImageRGB{ .last_y = last_y, .width = width, .height = height, .stride = stride, .data = ptr[0 .. height * stride] };
    }

    pub const PartialImageYUVA = struct {
        /// index of last decoded row in raster scan order
        last_y: u16,
        width: u16,
        height: u16,
        stride: u17,
        uv_stride: u17,
        a_stride: u17,
        y: []u8,
        u: []u8,
        v: []u8,
        a: ?[]u8,
    };

    /// Same as `IDecoder.getRGB()` but to get a YUVA image. If there is no
    /// alpha information the alpha slice `PartialImageYUVA.a` will be `null`.
    pub fn getYUVA(self: *const IDecoder) error{OutputNotInitialized}!PartialImageYUVA {
        var r: struct {
            last_y: c_int,
            width: c_int,
            height: c_int,
            stride: c_int,
            uv_stride: c_int,
            a_stride: c_int,
        } = undefined;
        var u: ?[*]u8 = undefined;
        var v: ?[*]u8 = undefined;
        var a: ?[*]u8 = undefined;
        const y: [*]u8 = c.WebPIDecGetYUVA(self, &r.last_y, &u, &v, &a, &r.width, &r.height, &r.stride, &r.uv_stride, &r.a_stride) orelse
            return error.OutputNotInitialized;
        const last_y: u16 = @truncate(@as(c_uint, @intCast(r.last_y)));
        const width: u16 = @truncate(@as(c_uint, @intCast(r.width)));
        const height: u16 = @truncate(@as(c_uint, @intCast(r.height)));
        const stride: u17 = @truncate(@as(c_uint, @intCast(r.stride)));
        const uv_stride: u17 = @truncate(@as(c_uint, @intCast(r.uv_stride)));
        const a_stride: u17 = @truncate(@as(c_uint, @intCast(r.a_stride)));

        return PartialImageYUVA{
            .last_y = last_y,
            .width = width,
            .height = height,
            .stride = stride,
            .uv_stride = uv_stride,
            .a_stride = a_stride,
            .y = y[0 .. height * stride],
            .u = y[0 .. height * uv_stride],
            .v = y[0 .. height * uv_stride],
            .a = if (a) |a_| a_[0 .. height * a_stride] else null,
        };
    }

    pub const VisibleArea = struct {
        left: u16,
        right: u16,
        width: u16,
        height: u16,
    };

    /// Generic call to retrieve information about the displayable area.
    ///
    /// Fails in case the incremental decoder object is in an invalid state.
    /// Otherwise returns tuple `struct {VisibleArea, *const DecBuffer }`.
    /// `VisibleArea` describes the visible rectangular area so far, second
    /// value is the pointer to the internal representation.
    ///
    /// This pointed-to structure is read-only, tied to `IDecoder`'s lifespan
    /// and should not be modified.
    pub fn decodedArea(self: *const IDecoder) error{InvalidState}!struct { VisibleArea, *const DecBuffer } {
        var r: struct { left: c_int, right: c_int, width: c_int, height: c_int } = undefined;
        const ptr = c.WebPIDecodedArea(self, &r.left, &r.top, &r.width, &r.height) orelse return error.InvalidState;
        return .{ VisibleArea{
            .left = @truncate(@as(c_uint, @intCast(r.left))),
            .right = @truncate(@as(c_uint, @intCast(r.right))),
            .width = @truncate(@as(c_uint, @intCast(r.width))),
            .height = @truncate(@as(c_uint, @intCast(r.height))),
        }, ptr };
    }
};

/// Instantiate a new incremental decoder object with the requested
/// configuration. The bitstream can be passed using `data` parameter, in which
/// case the features will be parsed and stored into `config.input`. Otherwise,
/// `data` can be `null` and no parsing will occur.
///
/// Note that `config` can be `null` too, in which case a default configuration
/// is used. If `config` is not `null`, it must outlive the `IDecoder` object
/// as some references to its fields will be used. No internal copy of `config`
/// is made.
///
/// The return `IDecoder` object must always be deleted calling
/// `IDecoder.delete()`.
pub fn iDecode(data: ?[]const u8, config: ?*DecoderConfig) error{DecodingError}!*IDecoder {
    // TODO: doc says "Returns NULL in case of error (and config->status will
    // then reflect the error condition, if available)." What's this
    // config->status?
    return c.WebPIDecode(
        if (data) |d| d.ptr else null,
        if (data) |d| d.size else 0,
        config,
    ) orelse error.DecodingError;
}

/// Non-incremental version of `iDecode()`. This version decodes the full data
/// at once, taking `config` into account. Returns `void` if the decoding was
/// successful, respective error otherwise.
pub fn decode(data: []const u8, config: *DecoderConfig) VP8Status.VP8Error!void {
    _ = try c.WebPDecode(data.ptr, data.len, config).toErr();
}
