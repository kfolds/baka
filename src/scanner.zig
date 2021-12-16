//! BAKA SCANNER :x
//!
//! TODO:
//! - multiple variable declarations: `x, y: i32;`
//! - multiple assignment: `x, y = 1, 2.0;`

const std = @import("std");

const _ast = @import("ast.zig");
const Node = _ast.Node;
const Tag = _ast.Node.Tag;
const NodeList = _ast.NodeList;
const RefList = _ast.RefList;

const lexer = @import("lexer.zig");
const Lexer = lexer.Lexer;
const Token = lexer.Token;
const TokenId = lexer.TokenId;

pub const Error = error{
    NotImplemented,
    UnexpectedToken,
    RedundantToken,
    UnmatchedParenthesis,
    InvalidConstExpr,
    ExpectedExpression,
    ExpectedParenthesis,
    ExpectedBrace,
    ExpectedFnDecl,
    ExpectedIdentifier,
    ExpectedVarDefn,
    ExpectedSemicolon,
    ExpectedColon,
    ExpectedPipe,
    ExpectedBlock,
    ExpectedStatement,
    UnknownError,
} || _ast.Error;

const log = std.log.scoped(.parser);

const Scanner = struct {
    tokens: []Token,
    token_i: usize,

    allocator: *std.mem.Allocator,
    ast: _ast.Ast,

    _lcstr: [512]u8 = undefined,

    fn linecol(self: *Scanner) []const u8 {
        const t = self.tokens[self.token_i];
        const s = std.fmt.bufPrint(self._lcstr[0..], "{}:{}", .{ t.line + 1, t.col + 1 }) catch unreachable;
        return s;
    }

    fn log_error(self: *Scanner, comptime fmt: []const u8, args: anytype) void {
        var msg: [1024:0]u8 = undefined;
        _ = std.fmt.bufPrint(msg[0..], fmt, args) catch unreachable;
        _ = std.fmt.bufPrint(msg[0..], "{s}: error: {s}", .{ self.linecol(), msg }) catch unreachable;
        log.err("{s}", .{msg});
    }

    pub fn init(allocator: *std.mem.Allocator, tokens: []Token) Scanner {
        return Scanner{
            .tokens = tokens,
            .token_i = 0,
            .allocator = allocator,
            .ast = _ast.Ast.init(allocator),
        };
    }

    pub fn deinit(self: *Scanner) void {
        self.ast.deinit();
    }

    fn eat(p: *Scanner) Token {
        const t = p.tokens[p.token_i];
        if (p.token_i < p.tokens.len - 1) p.token_i += 1;
        return t;
    }

    fn match(p: *Scanner, tags: []const TokenId) bool {
        for (tags) |t| {
            if (t == p.peek(0).tag) {
                return true;
            }
        }
        return false;
    }

    fn consume(p: *Scanner, tag: TokenId, err: Error) !Token {
        if (p.peek(0).tag != tag) {
            return err;
        }
        return p.eat();
    }

    fn peek(p: *Scanner, offset: usize) Token {
        const offs = p.token_i + offset;
        const clamp_offs = if (offs < p.tokens.len - 1) offs else p.tokens.len - 1;
        return p.tokens[offs];
    }

    fn match_sequence(p: *Scanner, tags: []TokenId) bool {
        for (tags) |t, i| {
            if (p.token_i + i > p.tokens.len or t != p.tokens[p.token_i + i].tag) {
                return false;
            }
        }
        return true;
    }

    fn parse_type_prefix(p: *Scanner) Error!Node.Index {
        const tags = &[_]TokenId{ .sep_bracket_l, .op_mul, .op_question };
        while (p.match(tags[0..])) {
            const op = p.eat();
            switch (op) {
                .sep_bracket_l => {},
                .op_mul => {},
                .op_question => {},
                else => unreachable,
            }
            var rexpr = try p.parse_type_prefix();
            expr = try p.ast.add_type(op, expr, rexpr);
        }
    }

    fn parse_type_expr(p: *Scanner) Error!Node.Index {}

    fn parse_primary(p: *Scanner) Error!Node.Index {
        const t = p.peek(0);
        switch (t.tag) {
            .kwd_false,
            .kwd_true,
            .kwd_null,
            .literal_char,
            .literal_uint,
            .literal_int,
            .literal_float,
            .literal_string,
            .literal_hex,
            .literal_oct,
            .literal_bin,
            => {
                _ = p.eat();
                return try p.ast.add_literal(t);
            },
            .identifier => {
                _ = p.eat();
                return try p.ast.add_identifier(t);
            },
            .sep_paren_l => {
                _ = p.eat();
                var expr = try p.parse_expr();
                _ = try p.consume(.sep_paren_r, Error.UnmatchedParenthesis);
                return expr;
            },
            else => return Error.UnexpectedToken,
        }
    }

    fn parse_call(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_primary();

        const tags = &[2]TokenId{ .sep_paren_l, .sep_dot };

        var t = p.peek(0);
        std.debug.warn("{}\n", .{t});
        while (p.match(tags[0..])) {
            t = p.eat();
            switch (t.tag) {
                .sep_paren_l => {
                    var args = std.ArrayList(Node.Index).init(p.allocator);
                    if (p.peek(0).tag != .sep_paren_r) {
                        try args.append(try p.parse_expr());
                        while (p.peek(0).tag == .sep_comma) {
                            t = p.eat();
                            try args.append(try p.parse_expr());
                        }
                    }
                    _ = try p.consume(.sep_paren_r, Error.ExpectedParenthesis);
                    expr = try p.ast.add_func_call(expr, args.items);
                },
                .sep_dot => {
                    const id = try p.consume(.identifier, Error.ExpectedIdentifier);
                    expr = try p.ast.add_member_access(expr, id);
                },
                else => break,
            }
        }

        return expr;
    }

    fn parse_unary(p: *Scanner) Error!Node.Index {
        const tags = &[3]TokenId{ .op_cond_not, .op_sub, .op_bit_not };
        if (p.match(tags[0..])) {
            const op = p.eat();
            var rexpr = try p.parse_unary();
            return try p.ast.add_unary_op(op, rexpr);
        }

        return p.parse_call();
    }

    fn parse_factor(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_unary();
        const tags = &[4]TokenId{ .op_div, .op_mul, .op_mod, .op_at };

        while (p.match(tags[0..])) {
            const op = p.eat();
            var rexpr = try p.parse_unary();
            expr = try p.ast.add_binary_op(op, expr, rexpr);
        }

        return expr;
    }

    fn parse_term(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_factor();
        const tags = &[2]TokenId{ .op_sub, .op_add };

        while (p.match(tags[0..])) {
            const op = p.eat();
            var rexpr = try p.parse_factor();
            expr = try p.ast.add_binary_op(op, expr, rexpr);
        }

        return expr;
    }

    fn parse_bitshift(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_term();
        const tags = &[2]TokenId{ .op_bit_lshift, .op_bit_rshift };

        while (p.match(tags[0..])) {
            const op = p.eat();
            var rexpr = try p.parse_term();
            expr = try p.ast.add_binary_op(op, expr, rexpr);
        }

        return expr;
    }

    fn parse_bitwise_comparison(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_bitshift();
        const tags = &[3]TokenId{ .op_bit_or, .op_bit_xor, .op_ampersand };

        while (p.match(tags[0..])) {
            const op = p.eat();
            var rexpr = try p.parse_bitshift();
            expr = try p.ast.add_binary_op(op, expr, rexpr);
        }

        return expr;
    }

    fn parse_comparison(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_bitwise_comparison();

        while (p.match(&[4]TokenId{ .op_cond_lt, .op_cond_leq, .op_cond_gt, .op_cond_geq })) {
            const op = p.eat();
            var rexpr = try p.parse_bitwise_comparison();
            expr = try p.ast.add_binary_op(op, expr, rexpr);
        }

        return expr;
    }

    fn parse_equality(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_comparison();

        while (p.match(&[2]TokenId{ .op_cond_neq, .op_cond_eq })) {
            const op = p.eat();
            var rexpr = try p.parse_comparison();
            expr = try p.ast.add_binary_op(op, expr, rexpr);
        }

        return expr;
    }

    fn parse_or(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_and();

        while (p.peek(0).tag == .kwd_or) {
            const op = p.eat();
            const rexpr = try p.parse_and();
            expr = try p.ast.add_binary_op(op, expr, rexpr);
        }

        return expr;
    }

    fn parse_and(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_equality();

        while (p.peek(0).tag == .kwd_and) {
            const op = p.eat();
            const rexpr = try p.parse_equality();
            expr = try p.ast.add_binary_op(op, expr, rexpr);
        }

        return expr;
    }

    fn parse_assignment_expr(p: *Scanner) Error!Node.Index {
        var expr = try p.parse_expr();

        switch (p.peek(0).tag) {
            .op_assign,
            .op_add_inline,
            .op_sub_inline,
            .op_mul_inline,
            .op_div_inline,
            .op_mod_inline,
            .op_at_inline,
            .op_bit_and_inline,
            .op_bit_or_inline,
            .op_bit_xor_inline,
            .op_bit_lshift_inline,
            .op_bit_rshift_inline,
            => {
                const op = p.eat();
                const value = try p.parse_expr();

                // if (p.ast.nodes[expr].tag != .uhh) return Error.Wtf;

                return try p.ast.add_binary_op(op, expr, value);
            },
            else => return expr,
        }
    }

    fn parse_expr(p: *Scanner) Error!Node.Index {
        return try p.parse_or();
    }

    fn parse_func_decl(p: *Scanner, id: Token) Error!Node.Index {
        var t = p.peek(0);

        var is_extern: bool = false;
        var is_inline: bool = false;

        switch (t.tag) {
            .kwd_inline,
            .kwd_extern,
            => {
                const tags = &[2]TokenId{ .kwd_inline, .kwd_extern };
                while (p.match(tags[0..])) {
                    if ((is_extern and p.peek(0).tag == .kwd_extern) or (is_inline and p.peek(0).tag == .kwd_inline)) {
                        return Error.RedundantToken;
                    }
                    is_extern = is_extern or p.peek(0).tag == .kwd_extern;
                    is_inline = is_inline or p.peek(0).tag == .kwd_inline;
                    _ = p.eat();
                }
            },
            .sep_paren_l => {},
            else => return Error.ExpectedFnDecl,
        }

        _ = try p.consume(TokenId.sep_paren_l, Error.ExpectedParenthesis);

        var params = std.ArrayList(Node.Index).init(p.allocator);
        defer params.deinit();

        t = p.eat();
        while (t.tag != .sep_paren_r) {
            if (t.tag == .identifier) {
                const param_id = t;
                _ = try p.consume(TokenId.sep_colon, Error.ExpectedColon);

                var ptr: usize = 0;
                while (p.peek(0).tag == .op_mul) : (t = p.eat()) ptr += 1;

                const param_type = try p.consume(.identifier, Error.ExpectedIdentifier);
                const param = try p.ast.add_func_param(
                    param_id,
                    try p.ast.add_identifier(param_type),
                    ptr,
                );
                try params.append(param);
                if (p.peek(0).tag == .sep_comma) _ = p.eat();
            } else return Error.ExpectedIdentifier;

            t = p.eat();
        }

        var return_type: ?Node.Index = null;

        if (p.peek(0).tag == .sep_arrow) {
            _ = p.eat();
            const type_id = try p.consume(.identifier, Error.ExpectedIdentifier);
            return_type = try p.ast.add_identifier(type_id);
        }

        const fn_body = try p.parse_compound_statement();

        return try p.ast.add_func_decl(
            id,
            params.items,
            return_type,
            fn_body,
            is_extern,
            is_inline,
        );
    }

    fn parse_structured_decl(p: *Scanner, id: Token) Error!Node.Index {
        const is_union = p.peek(0).tag == .kwd_union;
        if (!is_union and p.peek(0).tag != .kwd_struct) {
            p.log_error("expected \"struct\" or \"union\"; got {}", .{p.peek(0)});
            return Error.UnexpectedToken;
        }
        _ = p.eat();
        _ = try p.consume(TokenId.sep_brace_l, Error.ExpectedBrace);

        var members = std.ArrayList(Node.Index).init(p.allocator);
        defer members.deinit();

        var t = p.peek(0);
        while (t.tag != .sep_brace_r) {
            const member = try p.parse_decl(true);
            try members.append(member);
            t = p.peek(0);
        }

        _ = p.eat();

        return try p.ast.add_struct_decl(id, members.items, is_union);
    }

    /// parse a declaration
    ///
    /// <declaration> ::= <identifier> ( ( "::" ( <struct_decl> | <union_decl> | <func_decl> | <expression> ";" )
    ///                                 | ( ":=" <expression> ";" ) 
    ///                                 | ( ":" <identifier> ( ( "=" | ":" ) <expression> )? ";" ) ) )
    fn parse_decl(p: *Scanner, is_root: bool) Error!Node.Index {
        const id = p.peek(0); //try p.consume(.identifier, Error.ExpectedIdentifier);
        var t = p.peek(1);

        switch (t.tag) {
            .sep_colon => {
                // expect identifier then optional ( ( EQUALS | COLON ) Expr)
                _ = p.eat();
                _ = p.eat();
                t = p.eat();
                var ptr: usize = 0;
                while (t.tag == .op_mul) {
                    ptr += 1;
                    t = p.eat();
                }
                if (t.tag != .identifier) return Error.ExpectedIdentifier;

                var type_ = try p.ast.add_identifier(t);
                var expr: ?Node.Index = null;
                t = p.eat();

                if (is_root and t.tag == .sep_comma) {
                    return try p.ast.add_struct_member(id, type_, null);
                }

                const is_const = t.tag == .sep_colon;
                if (t.tag == .op_assign or t.tag == .sep_colon) {
                    expr = try p.parse_expr();
                } else {
                    return Error.ExpectedVarDefn;
                }

                t = p.eat();
                if (t.tag == .sep_comma) {
                    if (is_const) return Error.InvalidConstExpr;
                    return try p.ast.add_struct_member(id, type_, expr);
                }

                if (t.tag != .sep_semicolon) {
                    return Error.ExpectedSemicolon;
                }

                return try p.ast.add_var_decl(id, type_, expr, is_const);
            },
            .sep_double_colon => {
                // could be a type-inferred const decl, func decl, struct, or union
                _ = p.eat();
                _ = p.eat();
                t = p.peek(0);
                switch (t.tag) {
                    .kwd_struct, .kwd_union => {
                        return try p.parse_structured_decl(id);
                    },
                    .kwd_inline, .kwd_extern => {
                        // function decl
                        return try p.parse_func_decl(id);
                    },
                    .sep_paren_l => {
                        // could be an Expr or ParamList
                        if ((p.peek(1).tag == .sep_paren_r and (p.peek(2).tag == .sep_arrow or p.peek(2).tag == .sep_brace_l)) or (p.peek(1).tag == .identifier and p.peek(2).tag == .sep_colon)) {
                            return try p.parse_func_decl(id);
                        }

                        var expr = try p.parse_expr();
                        return try p.ast.add_auto_var_decl(id, expr, false);
                    },
                    else => {
                        // type-inferred const decl
                        var expr = try p.parse_expr();
                        t = p.eat();
                        if (t.tag != .sep_semicolon) {
                            return Error.ExpectedSemicolon;
                        }
                        return try p.ast.add_auto_var_decl(id, expr, true);
                    },
                }
            },
            .sep_colon_equals => {
                // expect Expr
                _ = p.eat();
                _ = p.eat();
                var expr = try p.parse_expr();
                t = p.eat();
                if (t.tag != .sep_semicolon) {
                    return Error.ExpectedSemicolon;
                }
                return try p.ast.add_auto_var_decl(id, expr, false);
            },
            else => return Error.ExpectedVarDefn,
        }
    }

    /// parse a block of code
    ///
    /// <block> ::= "{" <statement>* "}"
    fn parse_compound_statement(p: *Scanner) Error!Node.Index {
        var t = try p.consume(.sep_brace_l, Error.ExpectedBrace);

        var statements = std.ArrayList(Node.Index).init(p.allocator);
        defer statements.deinit();

        while (t.tag != .sep_brace_r) {
            const statement = try p.parse_statement();
            try statements.append(statement);
            t = p.peek(0);
        }
        _ = p.eat();

        return p.ast.add_block(statements.items);
    }

    /// parse an if statement
    ///
    /// <if_statement> ::= "if" "(" <expression> ")" ( <statement> ( "else" <statement> )? ";" 
    ///                     | <block> ( "else" ( <if_statement> | <block> | <statement> ";" ) )? )
    ///
    /// TODO(mia): support if statements as expressions?
    fn parse_if_statement(p: *Scanner) Error!Node.Index {
        _ = try p.consume(.kwd_if, Error.UnexpectedToken);
        _ = try p.consume(.sep_paren_l, Error.ExpectedParenthesis);
        const condition = try p.parse_expr();
        _ = try p.consume(.sep_paren_r, Error.ExpectedParenthesis);

        // TODO(mia): this could just be a line statement
        const true_block = try p.parse_compound_statement();

        var false_block: ?Node.Index = null;
        if (p.peek(0).tag == .kwd_else) {
            _ = p.eat();

            if (p.peek(0).tag == .kwd_if) {
                false_block = try p.parse_if_statement();
            } else if (p.peek(0).tag == .sep_brace_l) {
                false_block = try p.parse_compound_statement();
            } else {
                return Error.UnexpectedToken;
            }
        }

        return try p.ast.add_if_block(condition, true_block, false_block);
    }

    /// parse a for loop
    ///
    /// <for_loop> ::= "for" "(" <expression> ")" <ptr_idx_payload>? ( <statement> ";" | <block> )
    fn parse_for_statement(p: *Scanner) Error!Node.Index {
        _ = try p.consume(.kwd_for, Error.UnexpectedToken);
        _ = try p.consume(.sep_paren_l, Error.ExpectedParenthesis);
        const condition = try p.parse_expr() catch Error.ExpectedExpression;
        _ = try p.consume(.sep_paren_r, Error.ExpectedParenthesis);
    }

    /// parse a while loop
    ///
    /// <while_loop> ::= "while" "(" <expression> ")" <ptr_payload>? <while_update>? ( <statement> ";" | <block> )
    /// <while_update> :: ":" "(" <assign_expr> ")"
    fn parse_while_statement(p: *Scanner) Error!Node.Index {
        _ = try p.consume(.kwd_while, Error.UnexpectedToken);
        _ = try p.consume(.sep_paren_l, Error.ExpectedParenthesis);
        const condition = try p.parse_expr() catch Error.ExpectedExpression;
        _ = try p.consume(.sep_paren_r, Error.ExpectedParenthesis);

        // TODO: use ptr
        var payload: ?Node.Index = null;
        var ptr: usize = 0;
        if (p.peek(0).tag == .op_bit_or) {
            _ = p.eat();
            if (p.peek(0).tag == .op_mul) {
                ptr += 1;
                _ = p.eat();
            }
            const id = try p.consume(.identifier, Error.ExpectedIdentifier);
            _ = try p.consume(.op_bit_or, Error.ExpectedPipe);
            payload = try p.ast.add_auto_var_decl(id, condition, false);
        }

        var update_expr: ?Node.Index = null;
        if (p.peek(0).tag == .sep_colon) {
            _ = p.eat();
            _ = try p.consume(.sep_paren_l, Error.ExpectedParenthesis);
            update_expr = try p.parse_assignment_expr() catch Error.ExpectedExpression;
            _ = try p.consume(.sep_paren_r, Error.ExpectedParenthesis);
        }

        // TODO(mia): this could just be a line statement
        const block = try p.parse_compound_statement();

        return try p.ast.add_while_loop(condition, update_expr, block);
    }

    /// parse a statement
    ///
    /// <statement> ::= (( <assignment> | <inline_op> | <expression> | ( "return" <expression> ) | "break" | "continue" ) ";" )
    ///                 | <decl> | <while_loop> | <for_loop> | <if_statement> | <switch_statement> | <block>
    /// 2-token lookahead
    fn parse_statement(p: *Scanner) Error!Node.Index {
        var t = p.peek(0);
        switch (t.tag) {
            .identifier => {
                switch (p.peek(1).tag) {
                    .sep_colon,
                    .sep_double_colon,
                    .sep_colon_equals,
                    => {
                        // some scoped declaration
                        return try p.parse_decl(false);
                    },
                    else => {
                        // last chance to parse an expression before peacing out
                        const expr = p.parse_assignment_expr() catch |err| return Error.ExpectedStatement;
                        _ = try p.consume(.sep_semicolon, Error.ExpectedSemicolon);
                        return expr;
                    },
                }
            },
            .kwd_while => {
                return try p.parse_while_statement();
            },
            .kwd_if => {
                return try p.parse_if_statement();
            },
            .kwd_break,
            .kwd_continue,
            => {
                _ = p.eat();
                _ = try p.consume(.sep_semicolon, Error.ExpectedSemicolon);
                return try p.ast.add_statement(if (t.tag == .kwd_break) .break_stmt else .continue_stmt, null);
            },
            .kwd_return => {
                _ = p.eat();
                var expr: ?Node.Index = if (p.peek(0).tag != .sep_semicolon) try p.parse_expr() else null;
                _ = try p.consume(.sep_semicolon, Error.ExpectedSemicolon);
                return try p.ast.add_statement(.return_stmt, expr);
            },
            .sep_brace_l => return try p.parse_compound_statement(),
            .kwd_switch, // => {},
            .kwd_for,
            => return Error.NotImplemented,
            else => return Error.ExpectedStatement,
        }
    }

    fn parse_root_decl(p: *Scanner) Error!Node.Index {
        var root_items = std.ArrayList(Node.Index).init(p.allocator);
        var t = p.peek(0);

        while (t.tag != .end_of_file) {
            if (t.tag != .identifier) return Error.ExpectedIdentifier;
            const root_stmt = try p.parse_decl(true);
            try root_items.append(root_stmt);
            t = p.peek(0);
        }

        return try p.ast.add_root(root_items.items[0..]);
    }
};

pub fn parse(allocator: *std.mem.Allocator, tokens: []Token) Error!_ast.Ast {
    var p = Scanner.init(allocator, tokens);
    const root = p.parse_root_decl() catch |err| {
        log.err("error {s}: {}\n", .{ p.linecol(), err });
        log.err("  got: {}\n", .{p.peek(0).tag});
        return err;
    };

    p.ast.print();

    return p.ast;
}

test "simple parse" {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = &allocator.allocator;
    const src =
        \\ foo :: (bar: T) -> T {
        \\     i := bar * 2;
        \\ }
        \\
        \\ Bruh :: struct {
        \\    foo: T,
        \\    bar: T,
        \\
        \\    frob :: (i: T) -> T {
        \\      i := 1;
        \\    }
        \\ }
    ;
    const tokens = try lexer.tokenize(gpa, src);
    _ = try parse(gpa, tokens);
}
