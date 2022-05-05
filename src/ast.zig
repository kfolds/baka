//! BAKA AST :S

const std = @import("std");
const Token = @import("lexer.zig").Token;
const Slice = @import("util.zig").IndexedSlice;

pub const Error = error{
    InvalidTokenConversion,
    InvalidArgs,
} || std.mem.Allocator.Error;

pub const NodeList = std.ArrayList(Node);
pub const RefList = std.ArrayList(Node.Index);

const log = std.log.scoped(.ast);

pub const Node = struct {
    tag: Tag,
    token: Token,
    children: Slice,

    pub const Index = u32;
    pub const Tag = enum {
        root,
        block,

        decl_var,
        decl_const,
        decl_var_auto,
        decl_const_auto,
        decl_fn,
        decl_fn_param,
        decl_struct,
        decl_union,
        decl_struct_member,

        for_stmt,
        while_stmt,
        if_stmt,
        else_stmt,
        return_stmt,
        break_stmt,
        continue_stmt,
        switch_stmt,
        switch_prong,

        binary_expr,
        unary_negate,
        unary_bit_negate,
        unary_cond_negate,
        unary_deref,
        unary_ref,
        binary_add,
        binary_sub,
        binary_mul,
        binary_div,
        binary_mod,
        binary_matmul,
        binary_bit_and,
        binary_bit_or,
        binary_bit_xor,
        binary_bit_lshift,
        binary_bit_rshift,
        binary_cond_and,
        binary_cond_or,
        binary_cond_eq,
        binary_cond_neq,
        binary_cond_lt,
        binary_cond_leq,
        binary_cond_gt,
        binary_cond_geq,
        member_access,
        array_access,

        fn_call,
        assign_expr,
        assign_add_expr,
        assign_sub_expr,
        assign_mul_expr,
        assign_div_expr,
        assign_mod_expr,
        assign_at_expr,
        assign_bit_and_expr,
        assign_bit_or_expr,
        assign_bit_xor_expr,
        assign_bit_lshift_expr,
        assign_bit_rshift_expr,

        literal_int,
        literal_float,
        literal_char,
        literal_string,
        literal_hex,
        literal_oct,
        literal_bin,
        identifier,
    };
};

pub const Ast = struct {
    nodes: NodeList,
    refs: RefList,

    pub fn init(allocator: std.mem.Allocator) Ast {
        const ast = Ast{
            .nodes = std.ArrayList(Node).init(allocator),
            .refs = std.ArrayList(Node.Index).init(allocator),
        };
        return ast;
    }

    pub fn deinit(self: *Ast) void {
        self.nodes.deinit();
        self.refs.deinit();
    }

    pub fn add_literal(self: *Ast, token: Token) !Node.Index {
        const n_nodes = self.nodes.items.len;

        try self.nodes.append(Node{
            .tag = switch (token.tag) {
                .literal_int => Node.Tag.literal_int,
                .literal_float => Node.Tag.literal_float,
                .literal_char => Node.Tag.literal_char,
                .literal_string => Node.Tag.literal_string,
                .literal_hex => Node.Tag.literal_hex,
                .literal_oct => Node.Tag.literal_oct,
                .literal_bin => Node.Tag.literal_bin,
                else => return Error.InvalidTokenConversion,
            },
            .token = token,
            .children = Slice.EMPTY,
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_identifier(self: *Ast, token: Token) !Node.Index {
        const n_nodes = self.nodes.items.len;

        try self.nodes.append(Node{
            .tag = .identifier,
            .token = token,
            .children = Slice.EMPTY,
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_unary_op(self: *Ast, op: Token, rhs: Node.Index) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(rhs);

        try self.nodes.append(Node{
            .tag = switch (op.tag) {
                .op_sub => Node.Tag.unary_negate,
                .op_bit_not => Node.Tag.unary_bit_negate,
                .op_cond_not => Node.Tag.unary_cond_negate,
                .op_ampersand => Node.Tag.unary_ref,
                .op_mul => Node.Tag.unary_deref,
                else => return Error.InvalidTokenConversion,
            },
            .token = op,
            .children = Slice.make(@intCast(usize, n_refs), 1),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_binary_op(self: *Ast, op: Token, lhs: Node.Index, rhs: Node.Index) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(lhs);
        try self.refs.append(rhs);

        try self.nodes.append(Node{
            .tag = switch (op.tag) {
                .op_add => Node.Tag.binary_add,
                .op_sub => Node.Tag.binary_sub,
                .op_mul => Node.Tag.binary_mul,
                .op_div => Node.Tag.binary_div,
                .op_mod => Node.Tag.binary_mod,
                .op_at => Node.Tag.binary_matmul,
                .op_ampersand => Node.Tag.binary_bit_and,
                .op_bit_or => Node.Tag.binary_bit_or,
                .op_bit_xor => Node.Tag.binary_bit_xor,
                .op_bit_lshift => Node.Tag.binary_bit_lshift,
                .op_bit_rshift => Node.Tag.binary_bit_rshift,
                .op_cond_eq => Node.Tag.binary_cond_eq,
                .op_cond_neq => Node.Tag.binary_cond_neq,
                .op_cond_lt => Node.Tag.binary_cond_lt,
                .op_cond_leq => Node.Tag.binary_cond_leq,
                .op_cond_gt => Node.Tag.binary_cond_gt,
                .op_cond_geq => Node.Tag.binary_cond_geq,
                .kwd_and => Node.Tag.binary_cond_and,
                .kwd_or => Node.Tag.binary_cond_or,
                .op_assign => Node.Tag.assign_expr,
                .op_add_inline => Node.Tag.assign_add_expr,
                .op_sub_inline => Node.Tag.assign_sub_expr,
                .op_mul_inline => Node.Tag.assign_mul_expr,
                .op_div_inline => Node.Tag.assign_div_expr,
                .op_mod_inline => Node.Tag.assign_mod_expr,
                .op_at_inline => Node.Tag.assign_at_expr,
                .op_bit_and_inline => Node.Tag.assign_bit_and_expr,
                .op_bit_or_inline => Node.Tag.assign_bit_or_expr,
                .op_bit_xor_inline => Node.Tag.assign_bit_xor_expr,
                .op_bit_lshift_inline => Node.Tag.assign_bit_lshift_expr,
                .op_bit_rshift_inline => Node.Tag.assign_bit_rshift_expr,
                else => return Error.InvalidTokenConversion,
            },
            .token = op,
            .children = Slice.make(n_refs, 2),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_statement(
        self: *Ast,
        tag: Node.Tag,
        expr: ?Node.Index,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        switch (tag) {
            .return_stmt,
            .break_stmt,
            .continue_stmt,
            => {},
            else => return Error.InvalidArgs,
        }
        if (expr != null and tag != .return_stmt) return Error.InvalidArgs;
        if (expr != null) try self.refs.append(expr.?);

        try self.nodes.append(Node{
            .tag = tag,
            .token = undefined,
            .children = if (expr != null) Slice.make(n_refs, 1) else Slice.EMPTY,
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_func_call(
        self: *Ast,
        callee: Node.Index,
        args: []Node.Index,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(callee);

        for (args) |a| {
            try self.refs.append(a);
        }

        try self.nodes.append(Node{
            .tag = .fn_call,
            .token = undefined,
            .children = Slice.make(n_refs, args.len + 1),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_var_decl(
        self: *Ast,
        identifier: Token,
        var_type: ?Node.Index,
        expr: ?Node.Index,
        is_const: bool,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        if (is_const and expr == null) {
            return Error.InvalidArgs;
        }

        try self.refs.append(if (var_type != null) var_type.? else 0);
        try self.refs.append(if (expr != null) expr.? else 0);

        try self.nodes.append(Node{
            .tag = if (is_const) .decl_const else .decl_var,
            .token = identifier,
            .children = Slice.make(n_refs, 2),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_auto_var_decl(
        self: *Ast,
        identifier: Token,
        expr: Node.Index,
        is_const: bool,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(expr);

        try self.nodes.append(Node{
            .tag = if (is_const) .decl_const_auto else .decl_var_auto,
            .token = identifier,
            .children = Slice.make(n_refs, 1),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_struct_member(
        self: *Ast,
        identifier: Token,
        member_type: Node.Index,
        default_value: ?Node.Index,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(member_type);
        try self.refs.append(if (default_value != null) default_value.? else 0);

        try self.nodes.append(Node{
            .tag = .decl_struct_member,
            .token = identifier,
            .children = Slice.make(n_refs, 2),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_struct_decl(
        self: *Ast,
        identifier: Token,
        members: []Node.Index,
        is_union: bool,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        for (members) |m| {
            try self.refs.append(m);
        }

        try self.nodes.append(Node{
            .tag = if (is_union) .decl_union else .decl_struct,
            .token = identifier,
            .children = Slice.make(n_refs, members.len),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_member_access(
        self: *Ast,
        object: Node.Index,
        member: Token,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(object);
        try self.refs.append(try self.add_identifier(member));

        try self.nodes.append(Node{
            .tag = .member_access,
            .token = member,
            .children = Slice.make(n_refs, 2),
        });

        return @intCast(Node.Index, n_nodes + 1); // adding an id so add 1
    }

    pub fn add_func_param(
        self: *Ast,
        identifier: Token,
        param_type: Node.Index,
        ptr_level: usize,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(param_type);
        try self.refs.append(@intCast(Node.Index, ptr_level)); // TODO: hack

        try self.nodes.append(Node{
            .tag = .decl_fn_param,
            .token = identifier,
            .children = Slice.make(n_refs, 2),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_func_decl(
        self: *Ast,
        identifier: Token,
        params: []Node.Index,
        return_type: ?Node.Index,
        fn_body: Node.Index,
        is_extern: bool,
        is_inline: bool,
    ) !Node.Index {
        // TODO(mia): uwu don't do this
        _ = is_extern;
        _ = is_inline;
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(if (return_type != null) return_type.? else 0);
        try self.refs.append(fn_body);

        for (params) |p| {
            try self.refs.append(p);
        }

        try self.nodes.append(Node{
            .tag = .decl_fn,
            .token = identifier,
            .children = Slice.make(n_refs, 2 + params.len),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_while_loop(
        self: *Ast,
        condition: Node.Index,
        update: ?Node.Index,
        block: Node.Index,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(condition);
        try self.refs.append(if (update != null) update.? else 0);
        try self.refs.append(block);

        try self.nodes.append(Node{
            .tag = .while_stmt,
            .token = undefined,
            .children = Slice.make(n_refs, 3),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_if_block(
        self: *Ast,
        condition: Node.Index,
        true_block: Node.Index,
        false_block: ?Node.Index,
    ) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        try self.refs.append(condition);
        try self.refs.append(true_block);
        try self.refs.append(if (false_block != null) false_block.? else 0);

        try self.nodes.append(Node{
            .tag = .if_stmt,
            .token = undefined,
            .children = Slice.make(n_refs, 3),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_switch(self: *Ast, condition: Node.Index, prongs: []Node.Index) !Node.Index {
        _ = self;
        _ = condition;
        _ = prongs;
        // const n_nodes = self.nodes.items.len;
        // const n_refs = self.refs.items.len;
        return 0;
    }

    pub fn add_block(self: *Ast, statements: []Node.Index) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        for (statements) |s| {
            try self.refs.append(s);
        }

        try self.nodes.append(Node{
            .tag = .block,
            .token = undefined,
            .children = Slice.make(n_refs, statements.len),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn add_root(self: *Ast, statements: []Node.Index) !Node.Index {
        const n_nodes = self.nodes.items.len;
        const n_refs = self.refs.items.len;

        for (statements) |stmt| {
            try self.refs.append(stmt);
        }

        try self.nodes.append(Node{
            .tag = .root,
            .token = undefined,
            .children = Slice.make(n_refs, statements.len),
        });

        return @intCast(Node.Index, n_nodes);
    }

    pub fn print(self: *Ast) void {
        var root = self.nodes.items[self.nodes.items.len - 1];
        log.info("printing AST:", .{});
        std.debug.print("{}: {}", .{ self.nodes.items.len - 1, root.tag });
        self._print_level(1, &root);
        std.debug.print("\n", .{});
    }

    fn _print_level(self: *Ast, level: usize, node: *Node) void {
        const children = node.*.children.to_slice(Node.Index, self.refs.items.ptr);

        for (children) |c| {
            if (c == self.nodes.items.len) continue;
            var i: usize = 0;
            while (i < level) : (i += 1) {
                std.debug.print(" ", .{});
            }
            std.debug.print("{}: {} -> [ ]\n", .{ c, self.nodes.items[c].tag });
            var next_node = self.nodes.items[c];
            self._print_level(level + 1, &next_node);
        }
    }
};

test "init and add" {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = allocator.allocator();
    var ast = Ast.init(gpa);
    defer ast.deinit();
    // TODO(mia): add
}
