//! BAKA LEXER :O

const std = @import("std");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.lexer);

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
    Reserved{ .src = "include", .tag = .kwd_include },
    Reserved{ .src = "if", .tag = .kwd_if },
    Reserved{ .src = "else", .tag = .kwd_else },
    Reserved{ .src = "while", .tag = .kwd_while },
    Reserved{ .src = "for", .tag = .kwd_for },
    Reserved{ .src = "switch", .tag = .kwd_switch },
    Reserved{ .src = "continue", .tag = .kwd_continue },
    Reserved{ .src = "break", .tag = .kwd_break },
    Reserved{ .src = "return", .tag = .kwd_return },
    Reserved{ .src = "inline", .tag = .kwd_inline },
    Reserved{ .src = "extern", .tag = .kwd_extern },
    Reserved{ .src = "struct", .tag = .kwd_struct },
    Reserved{ .src = "union", .tag = .kwd_union },
    Reserved{ .src = "enum", .tag = .kwd_enum },
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
    if (pos >= src.len) {
        return null;
    }
    const c = src[pos];
    const has_next = pos + 1 < src.len;
    const has_next_equals = has_next and src[pos + 1] == '=';

    var t: Token = undefined;
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
        '\x00' => {
            t.tag = .end_of_file;
        },
        else => {
            return null;
        },
    }
    t.pos = pos;
    return t;
}

fn match_reserved(src: [:0]const u8, pos: usize, len: usize) ?Token {
    inline for (reserved_atoms) |a| {
        var l = a.src.len;
        if (l == len and pos + l <= src.len and std.mem.eql(u8, a.src, src[pos .. pos + l])) {
            return Token{ .tag = a.tag, .len = a.src.len, .pos = pos, .data = null };
        }
    }
    return null;
}

fn match_identifier_chars(src: [:0]const u8, pos: usize) bool {
    return (std.ascii.isAlNum(src[pos]) or src[pos] == '_');
}

/// TODO(mia): this is so ugly
fn match_numeric_literal(src: [:0]const u8, pos: usize) ?Token {
    const has_next = pos + 1 < src.len;
    if ((!std.ascii.isDigit(src[pos]) and src[pos] != '.') or (has_next and src[pos] == '.' and !std.ascii.isDigit(src[pos + 1]))) {
        return null;
    }
    var token: ?Token = null;
    var token_len: usize = 1;
    const c = src[pos];
    if (has_next) {
        const next_c = src[pos + 1];
        if (c == '.' and std.ascii.isDigit(next_c)) {
            // handle float literal
            while (pos + token_len < src.len and (std.ascii.isDigit(src[pos + token_len]))) {
                token_len += 1;
            }
            // handle float literal
            const value = std.fmt.parseFloat(f64, src[pos .. pos + token_len]) catch unreachable;
            token = Token{
                .tag = .literal_float,
                .pos = pos,
                .len = token_len,
                .data = Token.Data{ .f64_value = value },
            };
        } else if (c == '0' and next_c == 'x') {
            // handle hexadecimal literal
            token_len += 1;
            while (pos + token_len < src.len and ((src[pos + token_len] >= 'a' and src[pos + token_len] <= 'f') or (src[pos + token_len] >= 'A' and src[pos + token_len] <= 'F') or (src[pos + token_len] >= '0' and src[pos + token_len] <= '9'))) {
                token_len += 1;
            }
            const value = std.fmt.parseUnsigned(u128, src[pos .. pos + token_len - 1], 16) catch unreachable;
            token = Token{
                .tag = .literal_hex,
                .pos = pos,
                .len = token_len,
                .data = Token.Data{ .uint_value = value },
            };
        } else if (c == '0' and next_c == 'b') {
            // handle binary literal
            token_len += 1;
            while (pos + token_len < src.len and (src[pos + token_len] == '0' or src[pos + token_len] == '1')) {
                token_len += 1;
            }
            //std.debug.warn("{s}\n", .{src[pos + 2 .. pos + 2 + token_len]});
            const value = std.fmt.parseUnsigned(u128, src[pos .. pos + token_len - 1], 0) catch unreachable;
            token = Token{
                .tag = .literal_bin,
                .pos = pos,
                .len = token_len,
                .data = Token.Data{ .uint_value = value },
            };
        } else {
            // handle numeric literal (could be int or float)
            var seen_dot = false;
            var seen_e = false;
            while (pos + token_len < src.len and (std.ascii.isDigit(src[pos + token_len]) or (!seen_dot and src[pos + token_len] == '.') or (!seen_e and src[pos + token_len] == 'e'))) {
                seen_dot = seen_dot or src[pos + token_len] == '.';
                seen_e = seen_e or src[pos + token_len] == 'e';
                token_len += 1;
            }
            if (seen_dot or seen_e) {
                // handle float literal
                const value = std.fmt.parseFloat(f64, src[pos .. pos + token_len]) catch unreachable;
                token = Token{
                    .tag = .literal_float,
                    .pos = pos,
                    .len = token_len,
                    .data = Token.Data{ .f64_value = value },
                };
            } else {
                // handle integer literal
                const value = std.fmt.parseUnsigned(u128, src[pos .. pos + token_len], 10) catch unreachable;
                token = Token{
                    .tag = .literal_int,
                    .pos = pos,
                    .len = token_len,
                    .data = Token.Data{ .uint_value = value },
                };
            }
        }
    } else {
        if (std.ascii.isDigit(c)) {
            // handle single-digit number that is probably an error??
            const value = std.fmt.parseUnsigned(u128, src[pos .. pos + token_len], 10) catch unreachable;
            token = Token{
                .tag = .literal_int,
                .pos = pos,
                .len = token_len,
                .data = Token.Data{ .uint_value = value },
            };
        }
    }

    return token;
}

fn match_string_literal(src: [:0]const u8, pos: usize) ?Token {
    if (pos >= src.len or src[pos] != '"') {
        return null;
    }

    var token_len: usize = 1;
    while (pos + token_len < src.len and src[pos + token_len] != '"') {
        token_len += @as(usize, 1) + @boolToInt(src[pos + token_len] == '\\');
    }
    if (src[pos + token_len] != '"') {
        return null;
    }

    return Token{
        .tag = .literal_string,
        .len = token_len + 1,
        .pos = pos,
        .data = Token.Data{ .str_value = src[pos .. pos + token_len + 1] },
    };
}

fn match_char_literal(src: [:0]const u8, pos: usize) ?Token {
    if (pos >= src.len or src[pos] != '\'') {
        return null;
    }

    var token_len: usize = 1;
    while (pos + token_len < src.len and src[pos + token_len] != '\'') {
        token_len += 1 + @boolToInt(src[pos + token_len] == '\\');
    }

    return Token{
        .tag = .literal_string,
        .len = token_len + 1,
        .pos = pos,
        .data = Token.Data{ .str_value = src[pos .. pos + token_len] },
    };
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
        var token_len: usize = 1;
        const src = l.src;

        var token = Token{ .tag = .end_of_file, .pos = pos, .len = token_len, .data = null };

        while (pos < src.len) {
            while (pos < src.len and std.ascii.isSpace(src[pos])) {
                if (src[pos] == '\n') {
                    line += 1;
                    col = 0;
                } else {
                    col += 1;
                }
                pos += 1;
            }

            if (pos < src.len - 1 and src[pos] == '/' and src[pos + 1] == '/') {
                while (pos < src.len and src[pos] != '\n') {
                    pos += 1;
                }
                line += 1;
                col = 0;
                pos += 1;
            }

            if (pos >= src.len) break;

            if (std.ascii.isAlpha(src[pos]) or src[pos] == '_') {
                while (pos + token_len < src.len and match_identifier_chars(src, pos + token_len)) {
                    token_len += 1;
                }
                if (match_reserved(src, pos, token_len)) |t| {
                    token.tag = t.tag;
                } else {
                    token.tag = .identifier;
                }
                break;
            }

            if (match_numeric_literal(src, pos)) |t| {
                token.tag = t.tag;
                token.data = t.data;
                token_len = t.len;
                break;
            }

            if (match_string_literal(src, pos)) |t| {
                token.tag = t.tag;
                token.data = t.data;
                token_len = t.len;
                break;
            }

            if (match_char_literal(src, pos)) |t| {
                token.tag = t.tag;
                token.data = t.data;
                token_len = t.len;
                break;
            }

            if (match_op_or_sep(src, pos)) |t| {
                token.tag = t.tag;
                token_len = t.len;
                break;
            }

            pos += 1;
            col += 1;
        }

        token.len = token_len;
        token.pos = pos;
        token.line = line;
        token.col = col;

        l.pos = pos + token_len;
        l.cur_line = line;
        l.cur_col = col + token_len;

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
        log.debug("{?}\n", .{t});
    }

    return tokens.toOwnedSlice();
}

test "basic lexing" {
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
    var l = Lexer{ .src = src };
    while (l.pos <= l.src.len) {
        const t = try l.get_next_token();
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
    while (l.pos <= l.src.len) : (i += 1) {
        const t = try l.get_next_token();
        if (t.tag != tags[i]) return error.MatchError;
    }
}

test "lex numeric literals" {
    const src = "0 1 1 0b10 0x29a 0o400 1.3 0.1 .32";
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
    };

    try assert_match(src, tags[0..]);
}
