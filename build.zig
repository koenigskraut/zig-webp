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
    var found: ?*std.Build.Step.Compile = null;
    for (libwebp_dep.builder.install_tls.step.dependencies.items) |dep_step| {
        const inst = dep_step.cast(std.Build.Step.InstallArtifact) orelse continue;
        if (std.mem.eql(u8, inst.artifact.name, "webp")) {
            found = if (inst.artifact.isStaticLibrary()) inst.artifact else continue;
        }
    }
    step.linkLibrary(found orelse std.debug.panic("webp not found in artifacts\n", .{}));
}
