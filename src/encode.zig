const std = @import("std");

const c = @import("c.zig");

const ENCODER_ABI_VERSION = 0x020f;

/// Image characteristics hint for the underlying encoder.
pub const ImageHint = enum(c_uint) {
    /// default preset.
    default = 0,
    /// digital picture, like portrait, inner shot
    picture,
    /// outdoor photograph, with natural lighting
    photo,
    /// Discrete tone image (graph, map-tile etc).
    graph,
    last,
};

/// Color spaces.
pub const EncCSP = enum(c_uint) {
    /// 4:2:0
    pub const YUV420: c_uint = 0;
    /// alpha channel variant
    pub const YUV420A: c_uint = 4;
    /// bit-mask to get the UV sampling factors
    pub const CSP_UV_MASK: c_uint = 3;
    /// bit that is set if alpha is present
    pub const CSP_ALPHA_BIT: c_uint = 4;
};

/// Enumerate some predefined settings for `Config`, depending on the type of
/// source picture. These presets are used when calling `WebPConfigPreset()`.
pub const Preset = enum(c_uint) {
    /// default preset
    default = 0,
    /// digital picture, like portrait, inner shot
    picture,
    /// outdoor photograph, with natural lighting
    photo,
    /// hand or line drawing, with high-contrast details
    drawing,
    /// small-sized colorful images
    icon,
    /// text-like
    text,
};

/// Encoding error conditions.
pub const EncodingError = enum(c_uint) {
    Ok = 0,
    /// memory error allocating objects
    OutOfMemory,
    /// memory error while flushing bits
    BitstreamOutOfMemory,
    /// a pointer parameter is NULL
    NullParameter,
    /// configuration is invalid
    InvalidConfiguration,
    /// picture has invalid width/height
    BadDimension,
    /// partition is bigger than 512k
    Partition0Overflow,
    /// partition is bigger than 16M
    PartitionOverflow,
    /// error while flushing bytes
    BadWrite,
    /// file is bigger than 4G
    FileTooBig,
    /// abort request by user
    UserAbort,
    /// list terminator. always last.
    Last,

    pub const Error = error{
        /// memory error allocating objects
        OutOfMemory,
        /// memory error while flushing bits
        BitstreamOutOfMemory,
        /// a pointer parameter is NULL
        NullParameter,
        /// configuration is invalid
        InvalidConfiguration,
        /// picture has invalid width/height
        BadDimension,
        /// partition is bigger than 512k
        Partition0Overflow,
        /// partition is bigger than 16M
        PartitionOverflow,
        /// error while flushing bytes
        BadWrite,
        /// file is bigger than 4G
        FileTooBig,
        /// abort request by user
        UserAbort,
        /// list terminator. always last.
        Last,
    };

    pub fn toErr(self: EncodingError) Error!void {
        return switch (self) {
            .Ok => void,
            .OutOfMemory => Error.OutOfMemory,
            .BitstreamOutOfMemory => Error.BitstreamOutOfMemory,
            .NullParameter => Error.NullParameter,
            .InvalidConfiguration => Error.InvalidConfiguration,
            .BadDimension => Error.BadDimension,
            .Partition0Overflow => Error.Partition0Overflow,
            .PartitionOverflow => Error.PartitionOverflow,
            .BadWrite => Error.BadWrite,
            .FileTooBig => Error.FileTooBig,
            .UserAbort => Error.UserAbort,
            .Last => Error.Last,
        };
    }
};

/// Compression parameters.
pub const Config = extern struct {
    /// Lossless encoding (0=lossy(default), 1=lossless).
    lossless: c_int,
    /// between 0 and 100. For lossy, 0 gives the smallest size and 100 the
    /// largest. For lossless, this parameter is the amount of effort put into
    /// the compression: 0 is the fastest but gives larger files compared to
    /// the slowest, but best, 100.
    quality: f32,
    /// quality/speed trade-off (0=fast, 6=slower-better)
    method: c_int,
    /// Hint for image type (lossless only for now).
    image_hint: ImageHint,
    /// if non-zero, set the desired target size in bytes.
    ///
    /// Takes precedence over the 'compression' parameter.
    target_size: c_int,
    /// if non-zero, specifies the minimal distortion to try to achieve.
    ///
    /// Takes precedence over `target_size`.
    target_PSNR: f32,
    /// maximum number of segments to use, in [1..4]
    segments: c_int,
    /// Spatial Noise Shaping.
    ///
    /// 0=off, 100=maximum.
    sns_strength: c_int,
    /// range: [0 = off .. 100 = strongest]
    filter_strength: c_int,
    /// range: [0 = off .. 7 = least sharp]
    filter_sharpness: c_int,
    /// filtering type: 0 = simple, 1 = strong (only used if
    /// `filter_strength` > 0 or `autofilter` > 0)
    filter_type: c_int,
    /// Auto adjust filter's strength [0 = off, 1 = on]
    autofilter: c_int,
    /// Algorithm for encoding the alpha plane (0 = none, 1 = compressed with
    /// WebP lossless).
    ///
    /// Default is 1.
    alpha_compression: c_int,
    /// Predictive filtering method for alpha plane.
    ///
    /// 0: none, 1: fast, 2: best. Default if 1.
    alpha_filtering: c_int,
    /// Between 0 (smallest size) and 100 (lossless).
    ///
    /// Default is 100.
    alpha_quality: c_int,
    /// number of entropy-analysis passes (in [1..10]).
    pass: c_int,
    /// if not 0, export the compressed picture back. In-loop filtering is not
    /// applied.
    show_compressed: c_int,
    /// preprocessing filter:
    ///
    /// 0=none, 1=segment-smooth, 2=pseudo-random dithering
    preprocessing: c_int,
    /// log2(number of token partitions) in [0..3].
    ///
    /// Default is set to 0 for easier progressive decoding.
    partitions: c_int,
    /// quality degradation allowed to fit the 512k limit on prediction modes
    /// coding (0: no degradation, 100: maximum possible degradation).
    partition_limit: c_int,
    /// If not 0, compression parameters will be remapped to better match the
    /// expected output size from JPEG compression. Generally, the output size
    /// will be similar but the degradation will be lower.
    emulate_jpeg_size: c_int,
    /// If non-zero, try and use multi-threaded encoding.
    thread_level: c_int,
    /// If set, reduce memory usage (but increase CPU use).
    low_memory: c_int,
    /// Near lossless encoding [0 = max loss .. 100 = off (default)].
    near_lossless: c_int,
    /// if non-zero, preserve the exact RGB values under transparent area.
    /// Otherwise, discard this invisible RGB information for better
    /// compression.
    ///
    /// The default value is 0.
    exact: c_int,
    /// reserved for future lossless feature
    use_delta_palette: c_int,
    /// if needed, use sharp (and slow) RGB->YUV conversion
    use_sharp_yuv: c_int,
    /// minimum permissible quality factor
    qmin: c_int,
    /// maximum permissible quality factor
    qmax: c_int,

    /// Internal, version-checked, entry point
    pub fn initInternal(self: *Config, preset: Preset, quality: f32, abi_version: c_int) c_int {
        return c.WebPConfigInitInternal(self, preset, quality, abi_version);
    }

    /// Should always be called, to create an initialized `Config` structure.
    /// Returns error in case of version mismatch. `Config.init()` must have
    /// succeeded before using the `config` object.
    ///
    /// Note that the default values are lossless=0 and quality=75.
    pub inline fn init() error{ConfigInitError}!Config {
        var config = Config{};
        if (config.initInternal(.default, 75.0, @intCast(ENCODER_ABI_VERSION)) == 0)
            return error.ConfigInitError
        else
            return config;
    }

    /// This function will create the initialized configuration according to a
    /// predefined set of parameters (referred to by `preset`) and a given
    /// quality factor.
    ///
    /// This function can be called as a replacement to `Config.init()`.
    pub inline fn initPreset(preset: Preset, quality: f32) error{ConfigInitError}!Config {
        var config = Config{};
        if (config.initInternal(preset, quality, @intCast(ENCODER_ABI_VERSION)) == 0)
            return error.ConfigInitError
        else
            return config;
    }

    /// Activate the lossless compression mode with the desired efficiency
    /// level between 0 (fastest, lowest compression) and 9 (slower, best
    /// compression). A good default level is '6', providing a fair tradeoff
    /// between compression speed and final compressed size.
    ///
    /// This function will overwrite several fields from config: `method`,
    /// `quality` and `lossless`.
    pub fn losslessPreset(self: *Config, level: c_int) error{ParameterError}!void {
        if (c.WebPConfigLosslessPreset(self, level) == 0) return error.ParameterError;
    }

    /// Returns `true` if all configuration parameters are within their valid
    /// ranges.
    pub fn validate(self: *const Config) bool {
        return c.WebPValidateConfig(self) != 0;
    }
};

/// maximum width/height allowed (inclusive), in pixels
pub const MAX_DIMENSION = 16383;

/// Main exchange structure (input samples, output bytes, statistics)
///
/// Once WebPPictureInit() has been called, it's ok to make all the INPUT
/// fields (use_argb, y/u/v, argb, ...) point to user-owned data, even if
/// WebPPictureAlloc() has been called. Depending on the value use_argb,
/// it's guaranteed that either *argb or *y/*u/*v content will be kept
/// untouched.
pub const Picture = extern struct {
    //   INPUT
    //////////////

    /// Main flag for encoder selecting between ARGB or YUV input. It is
    /// recommended to use ARGB input (`*argb`, `argb_stride`) for lossless
    /// compression, and YUV input (`*y`, `*u`, `*v`, etc.) for lossy
    /// compression since these are the respective native colorspace for these
    /// formats.
    use_argb: c_int,

    // YUV input (mostly used for input to lossy compression)

    /// colorspace: should be `EncCSP.YUV420` for now (=Y'CbCr)
    colorspace: EncCSP,
    /// X dimension (less or equal to `MAX_DIMENSION`)
    width: c_int,
    /// Y dimension (less or equal to `MAX_DIMENSION`)
    height: c_int,
    /// pointer to luma plane
    y: [*c]u8,
    /// pointer to U chroma plane
    u: [*c]u8,
    /// pointer to V chroma plane
    v: [*c]u8,
    /// luma stride.
    y_stride: c_int,
    /// chroma stride
    uv_stride: c_int,
    /// pointer to the alpha plane
    a: [*c]u8,
    /// stride of the alpha plane
    a_stride: c_int,
    /// padding for later use
    pad1: [2]u32,

    // ARGB input (mostly used for input to lossless compression)

    /// pointer to argb (32 bit) plane
    argb: [*c]u32,
    /// this is stride in pixels units, not bytes
    argb_stride: c_int,
    /// padding for later use
    pad2: [3]u32,

    //   OUTPUT
    ///////////////

    // Byte-emission hook, to store compressed bytes as they are ready.
    //
    // Can be `null`.
    // writer: WriterFunction,
    /// can be used by the writer
    custom_ptr: ?*anyopaque,
    /// map for extra information (only for lossy compression mode)
    /// - `1`: intra type
    /// - `2`: segment
    /// - `3`: quant
    /// - `4`: intra-16 prediction mode,
    /// - `5`: chroma prediction mode,
    /// - `6`: bit cost, 7: distortion
    extra_info_type: c_int,
    extra_info: [*c]u8,

    //   STATS AND REPORTS
    ///////////////////////////

    /// Pointer to side statistics (updated only if not `null`)
    stats: [*c]AuxStats,
    /// Error code for the latest error encountered during encoding
    error_code: EncodingError,
    // If not `null`, report progress during encoding.
    // progress_hook: ProgressHook,
    /// this field is free to be set to any value and used during callbacks
    /// (like progress-report e.g.)
    user_data: ?*anyopaque,
    /// padding for later use
    pad3: [3]u32,
    /// Unused for now
    pad4: [*c]u8,
    /// Unused for now
    pad5: [*c]u8,
    /// padding for later use
    pad6: [8]u32,

    // PRIVATE FIELDS
    ////////////////////

    /// row chunk of memory for yuva planes
    memory_: ?*anyopaque,
    /// and for argb too
    memory_argb_: ?*anyopaque,
    /// padding for later use
    pad7: [2]?*anyopaque,

    /// Internal, version-checked, entry point
    pub fn initInternal(self: *Picture, arg: c_int) c_int {
        return c.WebPPictureInitInternal(self, arg);
    }

    /// Should always be called, to initialize the structure. Returns false in
    /// case of version mismatch. WebPPictureInit() must have succeeded before
    /// using the `picture` object.
    ///
    /// Note that, by default, `use_argb` is `false` and colorspace is
    /// `EncCSP.YUV420`.
    pub inline fn init(self: *Picture) c_int {
        // TODO return struct?
        return self.initInternal(ENCODER_ABI_VERSION);
    }

    /// Convenience allocation / deallocation based on picture->width/height:
    ///
    /// Allocate y/u/v buffers as per colorspace/width/height specification.
    ///
    /// **Note!** This function will free the previous buffer if needed.
    pub fn alloc(self: *Picture) error{MemoryError}!void {
        if (c.WebPPictureAlloc(self) == 0) return error.MemoryError;
    }

    /// Release the memory allocated by `Picture.alloc()` or WebPPictureImport*().
    ///
    /// Note that this function does _not_ free the memory used by the `self`
    /// object itself.
    ///
    /// Besides memory (which is reclaimed) all other fields of `self` are
    /// preserved.
    pub fn free(self: *Picture) void {
        c.WebPPictureFree(self);
    }

    /// Copy the pixels of `self` into `other`, using `Picture.alloc()`. Upon
    /// return, `other` will fully own the copied pixels (this is not a view).
    /// The `other` picture need not be initialized as its content is
    /// overwritten.
    pub fn copy(self: *const Picture, other: *Picture) error{OutOfMemory}!void {
        if (c.WebPPictureCopy(self, other) == 0) return error.OutOfMemory;
    }

    /// self-crops a picture to the rectangle defined by top/left/width/height.
    /// Fails in case of memory allocation error, or if the rectangle is
    /// outside of the source picture.
    ///
    /// The rectangle for the view is defined by the top-left corner pixel
    /// coordinates (left, top) as well as its width and height. This rectangle
    /// must be fully be comprised inside the `self` source picture. If the source
    /// picture uses the YUV420 colorspace, the top and left coordinates will be
    /// snapped to even values.
    pub fn crop(self: *Picture, left: u16, top: u16, width: u16, height: u16) error{CropError}!void {
        if (c.WebPPictureCrop(self, @intCast(left), @intCast(top), @intCast(width), @intCast(height)) == 0)
            return error.CropError;
    }

    /// Extracts a view from `self` picture into `dst`. The rectangle for the
    /// view is defined by the top-left corner pixel coordinates (left, top) as
    /// well as its width and height. This rectangle must be fully be comprised
    /// inside the `self` source picture. If the source picture uses the YUV420
    /// colorspace, the top and left coordinates will be snapped to even
    /// values. Picture `self` must out-live `dst` picture. Self-extraction of
    /// view is allowed (`self` equal to `dst`) as a mean of fast-cropping (but
    /// note that doing so, the original dimension will be lost). Picture `dst`
    /// need not be initialized with `Picture.init()` if it is different from
    /// `src`, since its content will be overwritten.
    pub fn view(self: [*c]const Picture, left: u16, top: u16, width: u16, height: u16, dst: *Picture) error{InvalidParameters}!void {
        if (c.WebPPictureView(self, @intCast(left), @intCast(top), @intCast(width), @intCast(height), dst) == 0)
            return error.InvalidParameters;
    }

    /// Returns `true` if the `self` is actually a view and therefore does not
    /// own the memory for pixels.
    pub fn isView(self: *const Picture) bool {
        return c.WebPPictureIsView(self) != 0;
    }

    /// Rescale a picture to new dimension width x height.
    ///
    /// If either `width` or `height` (but not both) is 0 the corresponding
    /// dimension will be calculated preserving the aspect ratio.
    ///
    /// No gamma correction is applied.
    pub fn rescale(self: *Picture, width: u16, height: u16) error{RescalingError}!void {
        if (c.WebPPictureRescale(self, @intCast(width), @intCast(height)) == 0) return error.RescalingError;
    }

    /// Colorspace conversion function to import RGB samples.
    ///
    /// Previous buffer will be free'd, if any.
    ///
    /// `rgb` buffer should have a size of at least `height * rgb_stride`.
    pub fn importRGB(self: *Picture, rgb: []const u8, rgb_stride: u17) error{MemoryError}!void {
        if (c.WebPPictureImportRGB(self, rgb.ptr, @intCast(rgb_stride)) == 0) return error.MemoryError;
    }

    /// Same as `Picture.importRGB()`, but for RGBA buffer.
    pub fn importRGBA(self: *Picture, rgba: []const u8, rgba_stride: u17) error{MemoryError}!void {
        if (c.WebPPictureImportRGBA(self, rgba.ptr, @intCast(rgba_stride)) == 0) return error.MemoryError;
    }

    /// Same as `Picture.importRGB()`, but for RGBA buffer. Imports the RGB
    /// direct from the 32-bit format input buffer ignoring the alpha channel.
    /// Avoids needing to copy the data to a temporary 24-bit RGB buffer to
    /// import the RGB only.
    pub fn importRGBX(self: *Picture, rgbx: []const u8, rgbx_stride: u17) error{MemoryError}!void {
        if (c.WebPPictureImportRGBX(self, rgbx.ptr, @intCast(rgbx_stride)) == 0) return error.MemoryError;
    }

    /// Same as `Picture.importRGB()`, but taking BGR input.
    pub fn importBGR(self: *Picture, bgr: []const u8, bgr_stride: u17) error{MemoryError}!void {
        if (c.WebPPictureImportBGR(self, bgr.ptr, @intCast(bgr_stride)) == 0) return error.MemoryError;
    }

    /// Same as `Picture.importRGBA()`, but taking BGRA input.
    pub fn importBGRA(self: *Picture, bgra: []const u8, bgra_stride: u17) error{MemoryError}!void {
        if (c.WebPPictureImportBGRA(self, bgra.ptr, @intCast(bgra_stride)) == 0) return error.MemoryError;
    }

    /// Same as `Picture.importRGBX()`, but taking BGRX input.
    pub fn importBGRX(self: *Picture, bgrx: []const u8, bgrx_stride: u17) error{MemoryError}!void {
        if (c.WebPPictureImportBGRX(self, bgrx.ptr, @intCast(bgrx_stride)) == 0) return error.MemoryError;
    }

    /// Converts `self.argb` data to the YUV420A format.
    ///
    /// Upon return, `picture.use_argb` is set to `0`. The presence of real
    /// non-opaque transparent values is detected. Note that this method is
    /// lossy.
    pub fn ARGBToYUVA(self: *Picture) error{ConvertingError}!void {
        if (c.WebPPictureARGBToYUVA(self, EncCSP.YUV420) == 0) return error.ConvertingError;
    }

    /// Same as `Picture.ARGBToYUVA()`, but the conversion is done using
    /// pseudo-random dithering with a strength `dithering` between `0.0` (no
    /// dithering) and `1.0` (maximum dithering). This is useful for
    /// photographic picture.
    pub fn ARGBToYUVADithered(self: *Picture, dithering: f32) error{ConvertingError}!void {
        if (c.WebPPictureARGBToYUVADithered(self, EncCSP.YUV420, dithering) == 0) return error.ConvertingError;
    }

    /// Performs `sharp` RGBA->YUVA420 downsampling and colorspace conversion.
    ///
    /// Downsampling is handled with extra care in case of color clipping. This
    /// method is roughly 2x slower than `Picture.ARGBToYUVA()` but produces
    /// better and sharper YUV representation.
    pub fn sharpARGBToYUVA(self: *Picture) error{ConvertingError}!void {
        if (c.WebPPictureSharpARGBToYUVA(self) == 0) return error.ConvertingError;
    }

    /// kept for backward compatibility:
    pub fn smartARGBToYUVA(self: *Picture) error{ConvertingError}!void {
        if (c.WebPPictureSmartARGBToYUVA(self) == 0) return error.ConvertingError;
    }

    /// Converts `self.yuv` to `self.argb` and sets `self.use_argb` to `1`.
    ///
    /// The input format must be `YUV_420` or `YUV_420A`. The conversion from
    /// YUV420 to ARGB incurs a small loss too.
    ///
    /// Note that the use of this colorspace is discouraged if one has access
    /// to the raw ARGB samples, since using YUV420 is comparatively lossy.
    pub fn YUVAToARGB(self: *Picture) error{ConvertingError}!void {
        if (c.WebPPictureYUVAToARGB(self) == 0) return error.ConvertingError;
    }

    /// Helper function: given a width x height plane of RGBA or YUV(A) samples
    /// clean-up or smoothen the YUV or RGB samples under fully transparent
    /// area, to help compressibility (no guarantee, though).
    pub fn cleanupTransparentArea(self: *Picture) void {
        c.WebPCleanupTransparentArea(self);
    }

    /// Scan the picture `self` for the presence of non fully opaque alpha
    /// values. Returns `true` in such case. Otherwise returns `false`
    /// (indicating that the alpha plane can be ignored altogether e.g.).
    pub fn hasTransparency(self: *const Picture) bool {
        return c.WebPPictureHasTransparency(self) != 0;
    }

    /// Remove the transparency information (if present) by blending the color
    /// with the background color `background_rgb` (specified as 24bit RGB
    /// triplet).
    ///
    /// After this call, all alpha values are reset to `0xff`.
    pub fn blendAlpha(self: *Picture, background_rgb: u24) void {
        c.WebPBlendAlpha(self, @intCast(background_rgb));
    }
};

pub const DistortionMetric = enum(c_int) {
    PSNR,
    SSIM,
    LSIM,
};

/// Compute the single distortion for packed planes of samples.
///
/// `src` will be compared to `ref`, and the raw distortion stored into
/// `*distortion`. The refined metric (log(MSE), log(1 - ssim), ...) will be
/// stored in `*result`.
///
/// `x_step` is the horizontal stride (in bytes) between samples.
///
/// `src/ref_stride` is the byte distance between rows.
pub fn planeDistortion(src: [*c]const u8, src_stride: usize, ref: [*c]const u8, ref_stride: usize, width: c_int, height: c_int, x_step: usize, metric_type: DistortionMetric, distortion: [*c]f32, result: [*c]f32) error{DistortionError}!void {
    if (c.WebPPlaneDistortion(src, src_stride, ref, ref_stride, width, height, x_step, metric_type, distortion, result) == 0) return error.DistortionError;
}

/// Compute PSNR, SSIM or LSIM distortion metric between two pictures. Results
/// are in dB, stored in result[] in the B/G/R/A/All order. The distortion is
/// always performed using ARGB samples. Hence if the input is YUV(A), the
/// picture will be internally converted to ARGB (just for the measurement).
///
/// **Warning**: this function is rather CPU-intensive.
pub fn pictureDistortion(src: *const Picture, ref: *const Picture, metric_type: DistortionMetric, result: [*c]f32) error{DistortionError}!void {
    if (c.WebPPictureDistortion(src, ref, metric_type, result) == 0) return error.DistortionError;
}

/// Structure for storing auxiliary statistics.
pub const AuxStats = extern struct {
    /// final size
    coded_size: c_int,
    /// peak-signal-to-noise ratio for Y/U/V/All/Alpha
    PSNR: [5]f32,
    /// number of intra4/intra16/skipped macroblocks
    block_count: [3]c_int,
    // approximate number of bytes spent for header and mode-partition #0
    header_bytes: [2]c_int,
    /// approximate number of bytes spent for DC/AC/uv coefficients for each
    /// (0..3) segments
    residual_bytes: [3][4]c_int,
    /// number of macroblocks in each segments
    segment_size: [4]c_int,
    /// quantizer values for each segments
    segment_quant: [4]c_int,
    /// filtering strength for each segments [0..63]
    segment_level: [4]c_int,
    /// size of the transparency data
    alpha_data_size: c_int,
    /// size of the enhancement layer data
    layer_data_size: c_int,

    // lossless encoder statistics

    /// - bit 0: predictor
    /// - bit 1: cross-color transform
    /// - bit 2: subtract-green
    /// - bit 3: color indexing
    lossless_features: u32,
    /// number of precision bits of histogram
    histogram_bits: c_int,
    /// precision bits for transform
    transform_bits: c_int,
    /// number of bits for color cache lookup
    cache_bits: c_int,
    /// number of color in palette, if used
    palette_size: c_int,
    /// final lossless size
    lossless_size: c_int,
    /// lossless header (transform, huffman etc) size
    lossless_hdr_size: c_int,
    /// lossless image data size
    lossless_data_size: c_int,
    /// padding for later use
    pad: [2]u32,
};

/// Signature for output function. Should return `1` if writing was
/// successful.
///
/// `data`/`data_size` is the segment of data to write, and `picture` is for
/// reference (and so one can make use of `picture.custom_ptr`).
pub const WriterFunction = ?*const fn (data: [*c]const u8, data_size: usize, picture: [*c]const Picture) callconv(.C) c_int;

// The custom writer to be used with `MemoryWriter` as `custom_ptr`. Upon
// completion, `writer.mem` and `writer.size` will hold the coded data.
// `writer.mem` must be freed by calling `.clear()` on it.
pub const MemoryWriter = extern struct {
    /// final buffer (of size `max_size`, larger than `size`).
    mem: [*c]u8,
    /// final size
    size: usize,
    /// total capacity
    max_size: usize,
    /// padding for later use
    pad: [1]u32,

    /// The following must be called first before any use.
    pub fn init(self: *MemoryWriter) void {
        c.WebPMemoryWriterInit(self);
    }

    /// The following must be called to deallocate `writer.mem` memory. The
    /// `self` object itself is not deallocated.
    pub fn clear(self: *MemoryWriter) void {
        c.WebPMemoryWriterClear(self);
    }
};

/// `memoryWrite`: a special `WriterFunction` that writes to memory using
/// `MemoryWriter` object (to be set as a `picture.custom_ptr`).
pub const memoryWrite = c.WebPMemoryWrite;

/// Progress hook, called from time to time to report progress. It can return
/// `0` to request an abort of the encoding process, or `1` otherwise if
/// everything is OK.
pub const ProgressHook = ?*const fn (c_int, [*c]const Picture) callconv(.C) c_int;

/// Return the encoder's version number, packed in hexadecimal using 8bits for
/// each of major/minor/revision. E.g: v2.5.7 is 0x020507.
pub inline fn getEncoderVersion() u24 {
    return @truncate(@as(u32, @bitCast(c.WebPGetEncoderVersion())));
}

/// Returns the compressed data if no errors occurred. The compressed data must
/// be released by the caller using the call `webpFree()`.
///
/// This function compress using the lossy format, and the `quality_factor` can
/// go from `0` (smaller output, lower quality) to `100` (best quality, larger
/// output).
pub fn encodeRGB(rgb: []const u8, width: u16, height: u16, stride: u17, quality_factor: f32) error{EncodingError}![]u8 {
    var output: ?[*]u8 = null;
    const size = c.WebPEncodeRGB(rgb.ptr, @intCast(width), @intCast(height), @intCast(stride), quality_factor, &output);
    if (size == 0 or output == null) return error.EncodingError;
    return output.?[0..size];
}

/// Same as `encodeRGB()`, but for BGR data.
pub fn encodeBGR(bgr: []const u8, width: u16, height: u16, stride: u17, quality_factor: f32) error{EncodingError}![]u8 {
    var output: ?[*]u8 = null;
    const size = c.WebPEncodeBGR(bgr.ptr, @intCast(width), @intCast(height), @intCast(stride), quality_factor, &output);
    if (size == 0 or output == null) return error.EncodingError;
    return output.?[0..size];
}

/// Same as `encodeRGB()`, but for RGBA data.
pub fn encodeRGBA(rgba: []const u8, width: u16, height: u16, stride: u17, quality_factor: f32) error{EncodingError}![]u8 {
    var output: ?[*]u8 = null;
    const size = c.WebPEncodeRGBA(rgba.ptr, @intCast(width), @intCast(height), @intCast(stride), quality_factor, &output);
    if (size == 0 or output == null) return error.EncodingError;
    return output.?[0..size];
}

/// Same as `encodeRGB()`, but for BGRA data.
pub fn encodeBGRA(bgra: []const u8, width: u16, height: u16, stride: u17, quality_factor: f32) error{EncodingError}![]u8 {
    var output: ?[*]u8 = null;
    const size = c.WebPEncodeBGRA(bgra.ptr, @intCast(width), @intCast(height), @intCast(stride), quality_factor, &output);
    if (size == 0 or output == null) return error.EncodingError;
    return output.?[0..size];
}

/// This function is the equivalent of `encodeRGB()`, but compressing in a
/// lossless manner. Files are usually larger than lossy format, but will not
/// suffer any compression loss.
///
/// Note this function, like the lossy version, use the library's default
/// settings. For lossless this means `Config.exact` is disabled. RGB values in
/// transparent areas will be modified to improve compression. To avoid this,
/// use `encode()` and set `Config.exact` to `1`.
pub fn encodeLosslessRGB(rgb: []const u8, width: u16, height: u16, stride: u17) error{EncodingError}![]u8 {
    var output: ?[*]u8 = null;
    const size = c.WebPEncodeLosslessRGB(rgb.ptr, @intCast(width), @intCast(height), @intCast(stride), &output);
    if (size == 0 or output == null) return error.EncodingError;
    return output.?[0..size];
}

/// Same as `encodeLosslessRGB()` but for BGR data.
pub fn encodeLosslessBGR(bgr: []const u8, width: u16, height: u16, stride: u17) error{EncodingError}![]u8 {
    var output: ?[*]u8 = null;
    const size = c.WebPEncodeLosslessBGR(bgr.ptr, @intCast(width), @intCast(height), @intCast(stride), &output);
    if (size == 0 or output == null) return error.EncodingError;
    return output.?[0..size];
}

/// Same as `encodeLosslessRGB()` but for RGBA data.
pub fn encodeLosslessRGBA(rgba: []const u8, width: u16, height: u16, stride: u17) error{EncodingError}![]u8 {
    var output: ?[*]u8 = null;
    const size = c.WebPEncodeLosslessRGBA(rgba.ptr, @intCast(width), @intCast(height), @intCast(stride), &output);
    if (size == 0 or output == null) return error.EncodingError;
    return output.?[0..size];
}

/// Same as `encodeLosslessRGB()` but for BGRA data.
pub fn encodeLosslessBGRA(bgra: []const u8, width: u16, height: u16, stride: u17) error{EncodingError}![]u8 {
    var output: ?[*]u8 = null;
    const size = c.WebPEncodeLosslessBGRA(bgra.ptr, @intCast(width), @intCast(height), @intCast(stride), &output);
    if (size == 0 or output == null) return error.EncodingError;
    return output.?[0..size];
}

/// Main encoding call, after config and picture have been initialized.
/// `picture` must be less than 16384x16384 in dimension (cf `MAX_DIMENSION`),
/// and the `config` object must be a valid one.
///
/// In case of error, `picture.error_code` is updated accordingly.
/// `picture` can hold the source samples in both YUV(A) or ARGB input,
/// depending on the value of `picture.use_argb`. It is highly recommended to
/// use the former for lossy encoding, and the latter for lossless encoding
/// (when `config.lossless` is `1`). Automatic conversion from one format to
/// another is provided but they both incur some loss.
pub fn encode(config: *const Config, picture: *Picture) EncodingError.Error!void {
    if (c.WebPEncode(config, picture) == 0) return picture.error_code.toErr();
}
