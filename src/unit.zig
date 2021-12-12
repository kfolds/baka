const std = @import("std");

const lexer = @import("lexer.zig");
const _ast = @import("ast.zig");
const scanner = @import("scanner.zig");
const read_file = @import("util.zig").read_file;

pub const BuildMode = enum {
    debug,
    debug_fast,
    release,
    release_safe,
};

pub const OptLevel = enum {
    no_opt,
    l1_opt,
    l2_opt,
    l3_opt,
    l4_opt,
};

pub const CompileInfo = struct {
    files: []const []u8,
    link_libraries: []const []u8,
    opt_level: OptLevel,
    build_mode: BuildMode,
};

pub const Module = struct {
    filename: []const u8,
    src: [:0]const u8,

    tokens: []lexer.Token,
    ast: _ast.Ast,
};

pub fn compile(allocator: *std.mem.Allocator, file: []const u8) anyerror!Module {
    var module: Module = undefined;
    module.filename = file;
    module.src = read_file(allocator, file) catch |err| {
        std.debug.warn("error: could not read file \"{s}\"; reason {}\n", .{ file, err });
        return err;
    };
    module.tokens = try lexer.tokenize(allocator, module.src);
    module.ast = try scanner.parse(allocator, module.tokens);

    return module;
}
