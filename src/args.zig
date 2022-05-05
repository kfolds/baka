const std = @import("std");
const unit = @import("unit.zig");
const CompileInfo = unit.CompileInfo;

pub fn usage() void {
    std.debug.print("usage text\n", .{});
}

pub fn help() void {
    std.debug.print("help text\n", .{});
}

const ArgType = enum {
    file_name,
    show_help,
    show_usage,
    opt_level,
    build_mode,
    link_library,
};

const long_args = std.ComptimeStringMap(ArgType, .{
    .{ "help", .show_help },
    .{ "usage", .show_usage },
    .{ "opt-level", .opt_level },
    .{ "build-mode", .build_mode },
    .{ "link-library", .link_library },
});

const short_args = std.ComptimeStringMap(ArgType, .{
    .{ "h", .show_help },
    .{ "O", .opt_level },
    .{ "l", .link_library },
    .{ "b", .build_mode },
});

const JobType = enum {
    none,
    build,
    run,
    compile,
};

const JobData = union(JobType) {
    none: void,
    build: void,
    run: void,
    compile: CompileInfo,
};

pub fn parse_command(allocator: std.mem.Allocator, args: []const []u8) !JobData {
    const cmd = args[1];
    if (std.mem.eql(u8, cmd, "help")) {
        help();
        return error.Canceled;
    } else if (std.mem.eql(u8, cmd, "build")) {
        return error.NotImplemented;
    } else if (std.mem.eql(u8, cmd, "run")) {
        return error.NotImplemented;
    } else if (std.mem.eql(u8, cmd, "compile")) {
        const build_info = try parse_compile_args(allocator, args);
        return JobData{ .compile = build_info };
    } else {
        std.debug.print("error: invalid command \"{s}\"\n", .{cmd});
        return error.InvalidCommand;
    }
}

pub fn parse_compile_args(allocator: std.mem.Allocator, args: []const []u8) !CompileInfo {
    var info = CompileInfo{
        .files = undefined,
        .link_libraries = undefined,
        .opt_level = .no_opt,
        .build_mode = .debug,
    };
    var i: usize = 2;

    var files = std.ArrayList([]u8).init(allocator);
    errdefer files.deinit();

    while (i < args.len) : (i += 1) {
        var arg: ?ArgType = null;
        if (std.mem.startsWith(u8, args[i], "--")) {
            arg = long_args.get(args[i][2..]);
        } else if (std.mem.startsWith(u8, args[i], "-")) {
            arg = short_args.get(args[i][1..2]);
        } else {
            arg = ArgType.file_name;
        }

        if (arg == null) {
            std.debug.print("error: unrecognized argument \"{s}\"\n", .{args[i]});
            return error.UnrecognizedArgument;
        }

        switch (arg.?) {
            // TODO(mia): make printing help/usage cancel
            .show_help => {
                help();
                return error.Canceled;
            },
            .show_usage => {
                usage();
                return error.Canceled;
            },
            .file_name => try files.append(args[i]),
            else => return error.NotImplemented,
        }
    }

    info.files = files.toOwnedSlice();

    return info;
}
