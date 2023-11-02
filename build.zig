const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("zig-webp", .{ .source_file = .{ .path = "src/library.zig" }, .dependencies = &.{} });

    const lib = b.addStaticLibrary(.{
        .name = "webp",
        .root_source_file = b.addWriteFiles().add("empty.c", ""),
        .target = target,
        .optimize = optimize,
    });
    link(b, lib);
    b.installArtifact(lib);
}

pub fn link(b: *std.Build, step: *std.build.CompileStep) void {
    const libwebp_dep = b.dependency("libwebp", .{
        .target = step.target,
        .optimize = step.optimize,
    });
    step.linkLibrary(libwebp_dep.artifact("webp"));
}
