const std = @import("std");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const scanner = @import("scanner.zig");
const unit = @import("unit.zig");
const CompileInfo = unit.CompileInfo;

const args = @import("args.zig");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    if (argv.len < 2) {
        args.usage();
        return;
    }

    const info = args.parse_command(allocator, argv) catch |err| {
        if (err != error.Canceled) std.debug.print("error: failed to parse arguments\n", .{});
        return;
    };

    switch (info) {
        .compile => |data| {
            for (data.files) |f| {
                _ = try unit.compile(allocator, f);
            }
        },
        else => return,
    }
}
