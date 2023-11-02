# zig-webp

Zig binding for libwebp, libwebp used is a fork with added Zig build system. It is added as a dependency and will be downloaded and built from source.

## Quick start

1. Add zig-webp as a dependency in your build.zig.zon as follows:

    ```diff
    .{
        .name = "your-project",
        .version = "1.0.0",
        .dependencies = .{
    +       .zig_webp = .{
    +           .url = "https://github.com/koenigskraut/zig-webp/archive/6453d26c947fe679a99b7c24bc3cb9f916474e91.tar.gz",
    +           .hash = "1220f5d2221040b2baa61f564d1d03791c6a7d17b831435c6300d1756c6b1d4a0437",
    +       },
        },
    }
    ```

2. In your build.zig add zig-webp as a dependency and attach its modules to your project:

    ```diff
    const std = @import("std");

    pub fn build(b: *std.Build) void {
        const target = b.standardTargetOptions(.{});
        const optimize = b.standardOptimizeOption(.{});

    +   const opts = .{ .target = target, .optimize = optimize };
    +   const dep = b.dependency("zig_webp", opts);
    +
    +   const webp_module = dep.module("zig-webp");
    +   const webp_lib = dep.artifact("webp");

        const exe = b.addExecutable(.{
            .name = "test",
            .root_source_file = .{ .path = "src/main.zig" },
            .target = target,
            .optimize = optimize,
        });
    +   exe.addModule("webp", webp_module);
    +   exe.linkLibrary(webp_lib);
        exe.install();

        ...
    }
    ```

3. Import and use it in your project! Note that currently zig-webp keeps the structure of original header files like `webp/encode.h` and `webp/decode.h`:

    ```zig
    const std = @import("std");
    const webp = struct {
        const decode = @import("webp").decode;
        const encode = @import("webp").encode;
    };

    pub fn main() !void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const alloc = arena.allocator();

        var in_file = try std.fs.openFileAbsolute("path_to_file.webp", .{});
        defer in_file.close();
        var data = try in_file.readToEndAlloc(alloc, 1<<24);

        const rgb = try webp.decode.decodeRGB(data);
        defer rgb.deinit();

        var out_file = try std.fs.cwd().createFile("out.ppm", .{});
        defer out_file.close();
        const writer = out_file.writer();
        try writer.print("P6\n{} {}\n255\n", .{ rgb.width, rgb.height });
        for (0..rgb.height) |h| {
            try writer.writeAll(rgb.data[rgb.width * 3 * h ..][0 .. rgb.width * 3]);
        }
    }
    ```