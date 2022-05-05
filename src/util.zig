const std = @import("std");
const mem = std.mem;

pub const IndexedSlice = struct {
    offset: usize,
    len: usize,

    pub const EMPTY = IndexedSlice{ .offset = 0, .len = 0 };

    pub fn make(offset: usize, len: usize) IndexedSlice {
        return IndexedSlice{ .offset = offset, .len = len };
    }

    pub fn to_slice(self: *IndexedSlice, comptime T: type, ptr: [*]T) []T {
        return ptr[self.offset .. self.offset + self.len];
    }
};

/// read a file into a buffer and return it. caller must free allocated memory.
pub fn read_file(allocator: mem.Allocator, fname: []const u8) ![:0]const u8 {
    const f = try std.fs.cwd().openFile(fname, .{ .mode = .read_only });
    defer f.close();

    const fsize = try f.getEndPos();
    var buf = try allocator.alloc(u8, fsize + 1);
    errdefer allocator.free(buf);

    _ = try f.reader().readAll(buf);
    buf[fsize] = '\x00';
    return buf[0..fsize :0];
}
