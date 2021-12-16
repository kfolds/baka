//! BAKA LEXER :O

const std = @import("std");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.lexer);

const Error = error{
    InvalidToken,
    ReachedEof,
};

pub const TokenId = enum(u16) {
    identifier = 255,
    literal_int,
    literal_uint,
    literal_float,
    literal_char,
    literal_string,
    literal_hex,
    literal_oct,
    literal_bin,

    kwd_include,
    kwd_if,
    kwd_else,
    kwd_while,
    kwd_for,
    kwd_switch,
    kwd_break,
    kwd_continue,
    kwd_return,
    kwd_inline,
    kwd_extern,
    kwd_struct,
    kwd_union,
    kwd_enum,
    kwd_int,
    kwd_uint,
    kwd_float,
    kwd_byte,
    kwd_char,
    kwd_void,
    kwd_null,
    kwd_true,
    kwd_false,
    kwd_and,
    kwd_or,
    kwd_error,
    kwd_type,
    kwd_fn,
    kwd_const,

    op_assign,
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_mod,
    op_at,
    op_add_inline,
    op_sub_inline,
    op_mul_inline,
    op_div_inline,
    op_mod_inline,
    op_at_inline,
    op_ampersand,
    op_cond_not,
    op_cond_eq,
    op_cond_neq,
    op_cond_lt,
    op_cond_leq,
    op_cond_gt,
    op_cond_geq,
    op_bit_lshift,
    op_bit_rshift,
    op_bit_or,
    // op_bit_and,  // op_ampersand, gotta do extra work in the parser ;)
    op_bit_xor,
    op_bit_not,
    op_bit_lshift_inline,
    op_bit_rshift_inline,
    op_bit_and_inline,
    op_bit_or_inline,
    op_bit_xor_inline,
    op_question,

    sep_arrow,
    sep_big_arrow,
    sep_dot, // could be operator
    sep_semicolon,
    sep_brace_l,
    sep_brace_r,
    sep_bracket_l,
    sep_bracket_r,
    sep_paren_l,
    sep_paren_r,
    sep_comma,
    sep_colon,
    sep_double_colon,
    sep_colon_equals,
    end_of_file,
};

const Reserved = struct { src: []const u8, tag: TokenId };
const reserved_atoms = .{
    Reserved{ .src = "@import", .tag = .kwd_include },
    Reserved{ .src = "type", .tag = .kwd_type },
    Reserved{ .src = "if", .tag = .kwd_if },
    Reserved{ .src = "else", .tag = .kwd_else },
    Reserved{ .src = "while", .tag = .kwd_while },
    Reserved{ .src = "for", .tag = .kwd_for },
    Reserved{ .src = "switch", .tag = .kwd_switch },
    Reserved{ .src = "continue", .tag = .kwd_continue },
    Reserved{ .src = "break", .tag = .kwd_break },
    Reserved{ .src = "return", .tag = .kwd_return },
    Reserved{ .src = "fn", .tag = .kwd_fn },
    Reserved{ .src = "inline", .tag = .kwd_inline },
    Reserved{ .src = "extern", .tag = .kwd_extern },
    Reserved{ .src = "struct", .tag = .kwd_struct },
    Reserved{ .src = "union", .tag = .kwd_union },
    Reserved{ .src = "enum", .tag = .kwd_enum },
    Reserved{ .src = "const", .tag = .kwd_const },
    //Reserved{ .src = "int", .tag = .kwd_int },
    //Reserved{ .src = "uint", .tag = .kwd_uint },
    //Reserved{ .src = "float", .tag = .kwd_float },
    //Reserved{ .src = "byte", .tag = .kwd_byte },
    //Reserved{ .src = "char", .tag = .kwd_char },
    //Reserved{ .src = "void", .tag = .kwd_void },
    Reserved{ .src = "null", .tag = .kwd_null },
    Reserved{ .src = "true", .tag = .kwd_true },
    Reserved{ .src = "false", .tag = .kwd_false },
    Reserved{ .src = "and", .tag = .kwd_and },
    Reserved{ .src = "or", .tag = .kwd_or },
};

pub const Token = struct {
    tag: TokenId,
    pos: usize,
    len: usize,

    line: usize = 0,
    col: usize = 0,

    str: []const u8,
    data: ?Data = null,

    const Data = union {
        str_value: []const u8,
        f32_value: f32,
        f64_value: f64,
        uint_value: u128,
        int_value: i128,
    };
};

fn match_op_or_sep(src: [:0]const u8, pos: usize) ?Token {
    if (pos >= src.len) return null;

    const c = src[pos];
    const has_next = pos + 1 < src.len;
    const has_next_equals = has_next and src[pos + 1] == '=';

    var t: Token = undefined;
    t.pos = pos;
    t.len = 1;

    switch (c) {
        ':' => {
            const has_next_colon = has_next and src[pos + 1] == ':';
            if (has_next_colon) {
                t.tag = .sep_double_colon;
                t.len += 1;
            } else if (has_next_equals) {
                t.tag = .sep_colon_equals;
                t.len += 1;
            } else {
                t.tag = .sep_colon;
            }
        },
        '=' => {
            if (has_next_equals) {
                t.tag = .op_cond_eq;
                t.len += 1;
            } else if (has_next and src[pos + 1] == '>') {
                t.tag = .sep_big_arrow;
                t.len += 1;
            } else {
                t.tag = .op_assign;
            }
        },
        '-' => {
            if (has_next_equals) {
                t.tag = .op_sub_inline;
                t.len += 1;
            } else if (has_next and src[pos + 1] == '>') {
                t.tag = .sep_arrow;
                t.len += 1;
            } else {
                t.tag = .op_sub;
            }
        },
        '<' => {
            if (has_next_equals) {
                t.tag = .op_cond_leq;
                t.len += 1;
            } else if (has_next and src[pos + 1] == '<') {
                t.tag = .op_bit_lshift;
                t.len += 1;
            } else {
                t.tag = .op_cond_lt;
            }
        },
        '>' => {
            if (has_next_equals) {
                t.tag = .op_cond_geq;
                t.len += 1;
            } else if (has_next and src[pos + 1] == '>') {
                t.tag = .op_bit_rshift;
                t.len += 1;
            } else {
                t.tag = .op_cond_gt;
            }
        },
        '!' => {
            if (has_next_equals) {
                t.tag = .op_cond_neq;
                t.len += 1;
            } else {
                t.tag = .op_cond_not;
            }
        },
        '+' => {
            if (has_next_equals) {
                t.tag = .op_add_inline;
                t.len += 1;
            } else {
                t.tag = .op_add;
            }
        },
        '*' => {
            if (has_next_equals) {
                t.tag = .op_mul_inline;
                t.len += 1;
            } else {
                t.tag = .op_mul;
            }
        },
        '/' => {
            if (has_next_equals) {
                t.tag = .op_div_inline;
                t.len += 1;
            } else {
                t.tag = .op_div;
            }
        },
        '%' => {
            if (has_next_equals) {
                t.tag = .op_mod_inline;
                t.len += 1;
            } else {
                t.tag = .op_mod;
            }
        },
        '@' => {
            if (has_next_equals) {
                t.tag = .op_at_inline;
                t.len += 1;
            } else {
                t.tag = .op_at;
            }
        },
        '&' => {
            if (has_next_equals) {
                t.tag = .op_bit_and_inline;
                t.len += 1;
            } else {
                t.tag = .op_ampersand;
            }
        },
        '|' => {
            if (has_next_equals) {
                t.tag = .op_bit_or_inline;
                t.len += 1;
            } else {
                t.tag = .op_bit_or;
            }
        },
        '^' => {
            if (has_next_equals) {
                t.tag = .op_bit_xor_inline;
                t.len += 1;
            } else {
                t.tag = .op_bit_xor;
            }
        },
        '(' => {
            t.tag = .sep_paren_l;
        },
        ')' => {
            t.tag = .sep_paren_r;
        },
        '[' => {
            t.tag = .sep_bracket_l;
        },
        ']' => {
            t.tag = .sep_bracket_r;
        },
        '{' => {
            t.tag = .sep_brace_l;
        },
        '}' => {
            t.tag = .sep_brace_r;
        },
        ';' => {
            t.tag = .sep_semicolon;
        },
        '.' => {
            t.tag = .sep_dot;
        },
        ',' => {
            t.tag = .sep_comma;
        },
        '?' => {
            t.tag = .op_question;
        },
        '\x00' => {
            t.tag = .end_of_file;
        },
        else => return null,
    }
    t.str = src[pos .. pos + t.len];
    return t;
}

fn is_identifier_char(c: u8) bool {
    return std.ascii.isAlNum(c) or c == '_';
}

/// match integer literals prefixed with `0b`, `0o`, or `0x` for binary, octal, and hexadecimal literals, respectively.
fn match_int_literal(src: [:0]const u8, pos: usize) !?Token {
    if (src[pos] != '0' or pos + 2 >= src.len) return null;
    var token: Token = undefined;
    token.tag = switch (src[pos + 1]) {
        'b' => .literal_bin,
        'o' => .literal_oct,
        'x' => .literal_hex,
        else => return null,
    };
    token.pos = pos;
    token.len = 2;

    while (pos + token.len < src.len and is_identifier_char(src[pos + token.len])) token.len += 1;

    token.data = Token.Data{ .uint_value = try std.fmt.parseUnsigned(u128, src[pos .. pos + token.len], 0) };
    token.str = src[pos .. pos + token.len];
    return token;
}

/// match literal integers and floats.
fn match_numeric_literal(src: [:0]const u8, pos: usize) !?Token {
    if (try match_int_literal(src, pos)) |t| return t;

    const has_next = pos + 1 < src.len;
    var seen_dot = src[pos] == '.';
    if ((!std.ascii.isDigit(src[pos]) and !seen_dot) or (has_next and seen_dot and !std.ascii.isDigit(src[pos + 1]))) {
        return null;
    }
    var token: Token = undefined;
    token.pos = pos;
    token.len = 0;

    seen_dot = false;
    var seen_e = false;

    while (pos + token.len < src.len) {
        switch (src[pos + token.len]) {
            '.' => if (seen_dot or seen_e) return Error.InvalidToken,
            'e', 'E' => if (seen_e) return Error.InvalidToken,
            '-' => {
                const prev_char = src[pos + token.len - 1];
                if (!seen_e or (prev_char != 'e' and prev_char != 'E')) break;
            },
            '_', '0'...'9' => {},
            'a'...'d', 'f'...'z', 'A'...'D', 'F'...'Z' => return Error.InvalidToken,
            else => break,
        }
        seen_dot = seen_dot or src[pos + token.len] == '.';
        seen_e = seen_e or src[pos + token.len] == 'e' or src[pos + token.len] == 'E';

        token.len += 1;
    }

    token.tag = if (seen_dot or seen_e) .literal_float else .literal_int;
    token.data = if (token.tag == .literal_float) .{
        .f64_value = try std.fmt.parseFloat(f64, src[pos .. pos + token.len]),
    } else .{
        .uint_value = try std.fmt.parseUnsigned(u128, src[pos .. pos + token.len], 10),
    };
    token.str = src[pos .. pos + token.len];

    return token;
}

fn match_string_or_char_literal(src: [:0]const u8, pos: usize) !?Token {
    if (pos >= src.len or (src[pos] != '"' and src[pos] != '\'')) return null;
    var token: Token = undefined;
    token.pos = pos;
    token.len = 1;

    const end_char = src[pos];
    token.tag = if (end_char == '"') .literal_string else .literal_char;

    while (pos + token.len < src.len and src[pos + token.len] != end_char) {
        token.len += 1 + @intCast(usize, @boolToInt(src[pos + token.len] == '\\'));
    }
    if (src[pos + token.len] != end_char) return Error.ReachedEof;

    token.len += 1;
    token.data = Token.Data{ .str_value = src[pos .. pos + token.len] };
    token.str = src[pos .. pos + token.len];

    return token;
}

fn match_identifier_or_kwd(src: [:0]const u8, pos: usize) ?Token {
    // zig fmt: off
    if (pos >= src.len
        or (!std.ascii.isAlpha(src[pos]) and src[pos] != '_' and src[pos] != '@')
        or (pos + 1 < src.len and src[pos] == '@' and !std.ascii.isAlpha(src[pos + 1])) 
    ) return null;
    // zig fmt: on

    var token: Token = undefined;
    token.pos = pos;
    token.len = 1;

    while (pos + token.len < src.len and is_identifier_char(src[pos + token.len])) {
        token.len += 1;
    }
    token.str = src[pos .. pos + token.len];

    // TODO(mia): use comptime string hash map
    token.tag = .identifier;
    inline for (reserved_atoms) |a| {
        if (a.src.len == token.len and std.mem.eql(u8, a.src, token.str)) {
            token.tag = a.tag;
            break;
        }
    }

    return token;
}

pub const Lexer = struct {
    src: [:0]const u8,

    pos: usize = 0,
    cur_line: usize = 0,
    cur_col: usize = 0,

    pub fn get_next_token(l: *Lexer) !Token {
        var pos = l.pos;
        var line = l.cur_line;
        var col = l.cur_col;
        const src = l.src;

        var token = Token{ .tag = .end_of_file, .pos = pos, .len = 1, .data = null, .str = "" };

        while (pos < src.len) {
            // skip whitespace
            while (pos < src.len and std.ascii.isSpace(src[pos])) {
                if (src[pos] == '\n') {
                    line += 1;
                    col = 0;
                } else {
                    col += 1;
                }
                pos += 1;
            }

            // skip comments
            if (pos + 1 < src.len and src[pos] == '/' and src[pos + 1] == '/') {
                while (pos < src.len and src[pos] != '\n') {
                    pos += 1;
                }
                line += 1;
                col = 0;
                pos += 1;
                continue;
            }

            if (pos >= src.len) break;

            if (try match_numeric_literal(src, pos)) |t| {
                token = t;
                break;
            }

            if (try match_string_or_char_literal(src, pos)) |t| {
                token = t;
                break;
            }

            if (match_identifier_or_kwd(src, pos)) |t| {
                token = t;
                break;
            }

            if (match_op_or_sep(src, pos)) |t| {
                token = t;
                break;
            }

            pos += 1;
            col += 1;
        }

        token.line = line;
        token.col = col;

        l.pos = pos + token.len;
        l.cur_line = line;
        l.cur_col = col + token.len;

        return token;
    }
};

pub fn tokenize(allocator: *Allocator, src: [:0]const u8) ![]Token {
    var tokens = std.ArrayList(Token).init(allocator);
    errdefer tokens.deinit();

    var l = Lexer{ .src = src };
    while (l.pos <= l.src.len) {
        const t = try l.get_next_token();
        try tokens.append(t);
    }

    log.debug("tokens:", .{});
    for (tokens.items) |t| {
        log.debug("{?}", .{t});
    }

    return tokens.toOwnedSlice();
}

test "basic lexing" {
    const src =
        \\rand :: @import("std").rand;
        \\foo :: inline (x: int) -> int {
        \\    y := rand();
        \\    if (y < 0.5) {
        \\        return x + 1;
        \\    } else {
        \\        // hot dog
        \\        print("hot dog\n");
        \\        return x - 1;
        \\    }
        \\}
    ;
    var l = Lexer{ .src = src };
    while (l.pos <= l.src.len) {
        const t = try l.get_next_token();
        std.debug.warn("{}\n", .{t});
    }
}

test "basic lexing into buffer" {
    const src =
        \\include rand;
        \\foo :: inline (x: int) -> int {
        \\    y := rand();
        \\    if (y < 0.5) {
        \\        return x + 1;
        \\    } else {
        \\        // hot dog
        \\        print("hot dog\n");
        \\        return x - 1;
        \\    }
        \\}
    ;
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = &allocator.allocator;

    const tokens = try tokenize(gpa, src);
}

fn assert_match(src: [:0]const u8, tags: []const TokenId) !void {
    var l = Lexer{ .src = src };
    var i: usize = 0;

    while (l.pos < l.src.len and i < tags.len) : (i += 1) {
        const t = try l.get_next_token();
        std.debug.warn("{}\n", .{t});
        if (t.tag != tags[i]) {
            std.debug.panic("expected {s}, got {s}\n", .{ tags[i], t.tag });
        }
    }
    if (i == tags.len) std.debug.panic("reached end of string before finishing\n", .{});
}

test "lex literals" {
    const src =
        \\0 1 1 0b10 0x29a 0o400 1.3 0.1 .32 1e2 9.42e-3 "hello\n" '0'
    ;
    const tags = &[_]TokenId{
        .literal_int,
        .literal_int,
        .literal_int,
        .literal_bin,
        .literal_hex,
        .literal_oct,
        .literal_float,
        .literal_float,
        .literal_float,
        .literal_float,
        .literal_float,
        .literal_string,
        .literal_char,
        .end_of_file,
    };

    try assert_match(src, tags[0..]);
}

test "lex identifiers" {
    const src =
        \\foo foo0 foo_ _foo _foo0 _foo _foo0_ foo_0 _foo_0_ _foo_0 foo_0_
    ;
    const tags = &[_]TokenId{
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .end_of_file,
    };

    try assert_match(src, tags[0..]);
}
