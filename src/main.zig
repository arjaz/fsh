pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var gpaAllocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(gpaAllocator);
    const arenaAllocator = arena.allocator();
    defer arena.deinit();

    var argsIterator = process.argsWithAllocator(gpaAllocator) catch oom();
    // The first one is the binary name
    _ = argsIterator.next();
    const filename = argsIterator.next() orelse exit("provide the input file", .{});
    const input = std.fs.cwd().readFileAllocOptions(
        gpaAllocator,
        filename,
        4194304,
        null,
        .of(u8),
        0,
    ) catch |err| switch (err) {
        error.OutOfMemory => oom(),
        else => exit("Something is wrong with your file", .{}),
    };
    defer gpaAllocator.free(input);
    argsIterator.deinit();

    const words = lex(arenaAllocator, input);
    var machine = Machine.init(arenaAllocator);
    try interpret(arenaAllocator, &machine, words);
}

const STACK_CAP = 2 << 16;

// TODO: Refcount

const Value = struct {
    payload: Inner,
    // refcount: u64 = 0,

    const Inner = union(enum) {
        int: i64,
        float: f64,
        string: []const u8,
        identifier: []const u8,
        array: []Value,
    };

    fn fromInt(i: i64) Value {
        return .{ .payload = .{ .int = i } };
    }

    fn fromFloat(f: f64) Value {
        return .{ .payload = .{ .float = f } };
    }

    fn fromString(str: []const u8) Value {
        return .{ .payload = .{ .string = str } };
    }

    fn fromIdentifier(identifier: []const u8) Value {
        return .{ .payload = .{ .identifier = identifier } };
    }

    fn fromArray(values: []Value) Value {
        return .{ .payload = .{ .array = values } };
    }

    fn fromBool(b: bool) Value {
        // true == -1 because all 1s
        return if (b) fromInt(-1) else fromInt(0);
    }

    fn sameTag(this: Value, other: Value) bool {
        return meta.activeTag(this.payload) == meta.activeTag(other.payload);
    }

    fn print(value: Value) void {
        switch (value.payload) {
            .int => printStderr("{d}", .{value.payload.int}),
            .float => printStderr("{d}", .{value.payload.float}),
            .identifier => printStderr("'{s}", .{value.payload.identifier}),
            .string => printStderr("{s}", .{value.payload.string}),
            .array => {
                printStderr("[", .{});
                for (value.payload.array, 0..) |v, i| {
                    v.print();
                    if (i + 1 != value.payload.array.len)
                        printStderr(" ", .{});
                }
                printStderr("]", .{});
            },
        }
    }
};

// Exactly the same name is also used for parsing
const Builtin = enum {
    // booleans
    @"=",
    @"!=",
    @"<",
    @"<=",
    @">",
    @">=",
    // arithmetic
    @"+",
    @"*",
    xor,
    @"or",
    @"and",
    @"-",
    @"<<",
    @">>",
    @"/",
    @"%",
    // stack manipulation
    swap,
    dup,
    over,
    rot,
    @"-rot",
    drop,
    nip,
    tuck,
    // printing
    @".",
    @".b",
    @".o",
    @".x",
    // OS interactions
    ls,
    sh,
    // last stdout
    // stdout,
    // last stderr
    // stderr,
};

const Word = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    identifier: []const u8,
    quoted: []const u8,
    builtin: Builtin,

    fn print(word: Word) void {
        switch (word) {
            .int => printStderr("{d} ", .{word.int}),
            .float => printStderr("{d} ", .{word.float}),
            .string => printStderr("{s} ", .{word.string}),
            .identifier => printStderr("{s} ", .{word.identifier}),
            .quoted => printStderr("'{s} ", .{word.quoted}),
        }
    }
};

const Definition = struct {
    const empty = Definition{};

    name: []const u8 = &.{},
    codeLen: u8 = 0,
    code: [64]Word = .{Word{ .int = 0 }} ** 64,
};

const Machine = struct {
    arena: Allocator,
    dataStackLen: u64 = 0,
    dataStack: []Value,
    callStackLen: u64 = 0,
    callStack: []u64,
    dictionaryLen: u64 = 0,
    dictionary: []Definition,
    wp: u32 = 0,

    fn init(arena: Allocator) Machine {
        errdefer oom();
        const dataStackPtr = try arena.alloc(Value, STACK_CAP);
        @memset(dataStackPtr, .fromInt(0));
        const callStackPtr = try arena.alloc(u64, STACK_CAP);
        @memset(callStackPtr, 0);
        const dictionaryPtr = try arena.alloc(Definition, STACK_CAP);
        @memset(dictionaryPtr, .empty);
        return .{
            .arena = arena,
            .dataStack = dataStackPtr,
            .callStack = callStackPtr,
            .dictionary = dictionaryPtr,
        };
    }

    fn push(machine: *Machine, value: Value) void {
        machine.dataStackLen += 1;
        machine.dataStack[machine.dataStackLen - 1] = value;
    }

    fn pop(machine: *Machine) Value {
        const top1 = machine.top();
        machine.dataStackLen -= 1;
        return top1;
    }

    fn top(machine: Machine) Value {
        return machine.dataStack[machine.dataStackLen - 1];
    }

    fn top2(machine: Machine) Value {
        return machine.dataStack[machine.dataStackLen - 2];
    }

    fn top3(machine: Machine) Value {
        return machine.dataStack[machine.dataStackLen - 3];
    }

    fn dictionaryLookup(machine: Machine, name: []const u8) ?Definition {
        for (machine.dictionary) |def|
            if (mem.eql(u8, name, def.name))
                return def;
        return null;
    }

    fn pushDefinition(machine: *Machine, definition: Definition) u32 {
        assert(machine.dictionaryLen >= std.math.maxInt(u32), "dictionary overflow", .{});
        machine.dictionaries[machine.dictionaryLen] = definition;
        const index = machine.dictionaryLen;
        machine.dictionaryLen += 1;
        return index;
    }
};

const ErrorType = enum { typeError, todo, undefined };
fn reportError(e: ErrorType) !void {
    switch (e) {
        .typeError => return error.TypeError,
        .todo => return error.Todo,
        .undefined => return error.Undefined,
    }
}

fn interpertBuiltin(arena: Allocator, machine: *Machine, builtin: Builtin) !void {
    switch (builtin) {
        .@"=" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            machine.push(Value.fromBool(meta.eql(arg1, arg2)));
        },

        .@"!=" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            machine.push(Value.fromBool(!meta.eql(arg1, arg2)));
        },

        .@"<" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(Value.fromBool(arg1.payload.int < arg2.payload.int)),
                .float => machine.push(Value.fromBool(arg1.payload.float < arg2.payload.float)),
                else => try reportError(.typeError),
            }
        },

        .@"<=" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(Value.fromBool(arg1.payload.int <= arg2.payload.int)),
                .float => machine.push(Value.fromBool(arg1.payload.float <= arg2.payload.float)),
                else => try reportError(.typeError),
            }
        },

        .@">" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(Value.fromBool(arg1.payload.int > arg2.payload.int)),
                .float => machine.push(Value.fromBool(arg1.payload.float > arg2.payload.float)),
                else => try reportError(.typeError),
            }
        },

        .@">=" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(Value.fromBool(arg1.payload.int >= arg2.payload.int)),
                .float => machine.push(Value.fromBool(arg1.payload.float >= arg2.payload.float)),
                else => try reportError(.typeError),
            }
        },

        .@"+" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int + arg2.payload.int)),
                .float => machine.push(.fromFloat(arg1.payload.float + arg2.payload.float)),
                else => try reportError(.typeError),
            }
        },

        .@"*" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int * arg2.payload.int)),
                .float => machine.push(.fromFloat(arg1.payload.float * arg2.payload.float)),
                else => try reportError(.typeError),
            }
        },

        .xor => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int ^ arg2.payload.int)),
                else => try reportError(.typeError),
            }
        },

        .@"or" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int | arg2.payload.int)),
                else => try reportError(.typeError),
            }
        },

        .@"and" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int & arg2.payload.int)),
                else => try reportError(.typeError),
            }
        },

        .@"-" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int - arg2.payload.int)),
                .float => machine.push(.fromFloat(arg1.payload.float - arg2.payload.float)),
                else => try reportError(.typeError),
            }
        },

        .@"<<" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int << @intCast(arg2.payload.int))),
                else => try reportError(.typeError),
            }
        },

        .@">>" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int >> @intCast(arg2.payload.int))),
                else => try reportError(.typeError),
            }
        },

        .@"/" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(@divFloor(arg1.payload.int, arg2.payload.int))),
                .float => machine.push(.fromFloat(arg1.payload.float / arg2.payload.float)),
                else => try reportError(.typeError),
            }
        },

        .@"%" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.typeError);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(@mod(arg1.payload.int, arg2.payload.int))),
                .float => machine.push(.fromFloat(@mod(arg1.payload.float, arg2.payload.float))),
                else => try reportError(.typeError),
            }
        },

        .swap => {
            const top = machine.pop();
            const top2 = machine.pop();
            machine.push(top);
            machine.push(top2);
        },

        .dup => {
            machine.push(machine.top());
        },

        .over => {
            machine.push(machine.top2());
        },

        .rot => {
            const top = machine.pop();
            const top2 = machine.pop();
            const top3 = machine.pop();
            machine.push(top2);
            machine.push(top);
            machine.push(top3);
        },

        .@"-rot" => {
            const top = machine.pop();
            const top2 = machine.pop();
            const top3 = machine.pop();
            machine.push(top);
            machine.push(top3);
            machine.push(top2);
        },

        .drop => {
            _ = machine.pop();
        },

        .nip => {
            const top = machine.pop();
            _ = machine.pop();
            machine.push(top);
        },

        .tuck => {
            const top = machine.pop();
            const top2 = machine.pop();
            machine.push(top);
            machine.push(top2);
            machine.push(top);
        },

        .@"." => {
            const top = machine.pop();
            top.print();
            printStderr(" ", .{});
        },

        .@".b" => {
            const top = machine.pop();
            switch (top.payload) {
                .int => printStderr("{b} ", .{top.payload.int}),
                else => try reportError(.typeError),
            }
        },

        .@".o" => {
            const top = machine.pop();
            switch (top.payload) {
                .int => printStderr("{o} ", .{top.payload.int}),
                else => try reportError(.typeError),
            }
        },

        .@".x" => {
            const top = machine.pop();
            switch (top.payload) {
                .int => printStderr("{x} ", .{top.payload.int}),
                else => try reportError(.typeError),
            }
        },

        .ls => {
            var array = ArrayList(Value).init(arena);
            var dir = std.fs.cwd().openDir(".", .{ .iterate = true }) catch unreachable;
            defer dir.close();
            var iterator = dir.iterateAssumeFirstIteration();
            while (iterator.next() catch null) |entry| {
                const owned = arena.alloc(u8, entry.name.len) catch oom();
                @memcpy(owned, entry.name);
                array.append(.fromString(owned)) catch oom();
            }
            machine.dataStackLen += 1;
            machine.dataStack[machine.dataStackLen - 1] = .fromArray(array.items);
        },

        .sh => {
            // TODO: capture stdout/stderr?
            const top = machine.pop();
            switch (top.payload) {
                .identifier => process.execv(arena, &.{top.payload.identifier}) catch {},
                .array => try reportError(.todo),
                else => try reportError(.typeError),
            }
        },
    }
}

// TODO: memory management, some other time
fn interpret(arena: Allocator, machine: *Machine, words: []const Word) !void {
    while (true) {
        switch (words[machine.wp]) {
            .int => |num| machine.push(.fromInt(num)),
            .float => |num| machine.push(.fromFloat(num)),
            .string => |str| machine.push(.fromString(str)),
            .quoted => |quoted| machine.push(.fromIdentifier(quoted)),
            .builtin => |builtin| try interpertBuiltin(arena, machine, builtin),
            .identifier => |id| {
                if (machine.dictionaryLookup(id)) |def| {
                    assert(false, "todo", .{});
                    _ = def;
                } else try reportError(.undefined);
            },
        }

        machine.wp += 1;
        if (machine.wp >= words.len) break;
    }
}

fn parse_escape_sequence(char: u8) ?u8 {
    return switch (char) {
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        '\\' => '\\',
        '"' => '"',
        '0' => 0,
        else => null,
    };
}

fn lex(arena: Allocator, input: [:0]const u8) []const Word {
    var words = ArrayListUnmanaged(Word).empty;
    var index: usize = 0;

    while (index < input.len) {
        // Skip whitespace
        while (index < input.len and
            (input[index] == ' ' or input[index] == '\n' or input[index] == '\t'))
            index += 1;

        if (index >= input.len) break;

        if (input[index] == '"') {
            // Skip opening quote
            index += 1;
            var string_buf = ArrayListUnmanaged(u8).empty;

            while (index < input.len and input[index] != '"') : (index += 1) {
                if (input[index] == '\\' and index + 1 < input.len) {
                    index += 1;
                    if (parse_escape_sequence(input[index])) |escaped| {
                        string_buf.append(arena, escaped) catch oom();
                    } else {
                        // for invalid escape sequences add the character anyway
                        string_buf.append(arena, input[index]) catch oom();
                    }
                } else {
                    string_buf.append(arena, input[index]) catch oom();
                }
            }

            if (index < input.len) {
                // Skip closing quote
                index += 1;
            } else {
                assert(false, "unterminated string literal", .{});
            }

            words.append(arena, .{
                .string = string_buf.toOwnedSlice(arena) catch oom(),
            }) catch oom();
        } else if (input[index] == '\'') {
            // Skip the quote
            index += 1;
            const wordStart = index;
            while (index < input.len and input[index] != ' ' and input[index] != '\n' and
                input[index] != '\t')
                index += 1;
            const word = input[wordStart..index];
            words.append(arena, .{ .quoted = word }) catch oom();
        } else {
            // Not a string, find the end of the word
            const wordStart = index;
            while (index < input.len and
                input[index] != ' ' and input[index] != '\n' and
                input[index] != '\t' and input[index] != '"')
                index += 1;

            const word = input[wordStart..index];

            // Try to parse as int, float, builtin, or identifier
            if (std.fmt.parseInt(i64, word, 0) catch null) |num| {
                words.append(arena, .{ .int = num }) catch oom();
            } else if (std.fmt.parseFloat(f64, word) catch null) |num| {
                words.append(arena, .{ .float = num }) catch oom();
            } else parsed: {
                inline for (meta.fields(Builtin)) |enumField| {
                    if (mem.eql(u8, word, enumField.name)) {
                        words.append(arena, .{
                            .builtin = @field(Builtin, enumField.name),
                        }) catch oom();
                        break :parsed;
                    }
                }
                words.append(arena, .{ .identifier = word }) catch oom();
            }
        }
    }

    return words.toOwnedSlice(arena) catch oom();
}

fn assert(condition: bool, comptime format: []const u8, args: anytype) void {
    if (!condition) std.debug.panic(format ++ "\n", args);
}

fn oom() noreturn {
    std.debug.panic("OOM\n", .{});
}

fn exit(comptime format: []const u8, args: anytype) noreturn {
    std.debug.print(format ++ "\n", args);
    process.exit(1);
}

fn printStderr(comptime format: []const u8, args: anytype) void {
    var buffer: [64]u8 = undefined;
    const bw = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();
    nosuspend bw.print(format, args) catch return;
}

test "lex integers" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic integers
    {
        const words = lex(allocator, "0 42 -17 +99");
        try std.testing.expectEqual(@as(usize, 4), words.len);
        try std.testing.expectEqual(@as(i64, 0), words[0].int);
        try std.testing.expectEqual(@as(i64, 42), words[1].int);
        try std.testing.expectEqual(@as(i64, -17), words[2].int);
        try std.testing.expectEqual(@as(i64, 99), words[3].int);
    }

    // Different bases
    {
        const words = lex(allocator, "0x1F 0xFF 0b1010 0o77");
        try std.testing.expectEqual(@as(usize, 4), words.len);
        try std.testing.expectEqual(@as(i64, 31), words[0].int);
        try std.testing.expectEqual(@as(i64, 255), words[1].int);
        try std.testing.expectEqual(@as(i64, 10), words[2].int);
        try std.testing.expectEqual(@as(i64, 63), words[3].int);
    }

    // Large numbers
    {
        const words = lex(allocator, "9223372036854775807 -9223372036854775808");
        try std.testing.expectEqual(@as(usize, 2), words.len);
        try std.testing.expectEqual(@as(i64, std.math.maxInt(i64)), words[0].int);
        try std.testing.expectEqual(@as(i64, std.math.minInt(i64)), words[1].int);
    }
}

test "lex floats" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic floats
    {
        const words = lex(allocator, "3.14 -2.5 0.0 +1.23");
        try std.testing.expectEqual(@as(usize, 4), words.len);
        try std.testing.expectApproxEqAbs(@as(f64, 3.14), words[0].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, -2.5), words[1].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 0.0), words[2].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 1.23), words[3].float, 0.001);
    }

    // Scientific notation
    {
        const words = lex(allocator, "1e10 1.5e-5 -3.14e+2");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectApproxEqAbs(@as(f64, 1e10), words[0].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 1.5e-5), words[1].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, -3.14e+2), words[2].float, 0.001);
    }

    // Edge cases
    {
        const words = lex(allocator, ".5 0. 1.");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectApproxEqAbs(@as(f64, 0.5), words[0].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 0.0), words[1].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 1.0), words[2].float, 0.001);
    }
}

test "lex strings" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic strings
    {
        const words = lex(allocator, "\"hello\" \"world\" \"\"");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectEqualStrings("hello", words[0].string);
        try std.testing.expectEqualStrings("world", words[1].string);
        try std.testing.expectEqualStrings("", words[2].string);
    }

    // Escape sequences
    {
        const words = lex(allocator, "\"\\n\\t\\r\" \"\\\"quoted\\\"\" \"back\\\\slash\" \"null\\0char\"");
        try std.testing.expectEqual(@as(usize, 4), words.len);
        try std.testing.expectEqualStrings("\n\t\r", words[0].string);
        try std.testing.expectEqualStrings("\"quoted\"", words[1].string);
        try std.testing.expectEqualStrings("back\\slash", words[2].string);
        try std.testing.expectEqualStrings("null\x00char", words[3].string);
    }

    // Strings with spaces
    {
        const words = lex(allocator, "\"hello world\" \"multiple   spaces\" \"tabs\\there\"");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectEqualStrings("hello world", words[0].string);
        try std.testing.expectEqualStrings("multiple   spaces", words[1].string);
        try std.testing.expectEqualStrings("tabs\there", words[2].string);
    }

    // Invalid escape sequences
    {
        const words = lex(allocator, "\"\\x\" \"\\q\" \"\\1\"");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectEqualStrings("x", words[0].string);
        try std.testing.expectEqualStrings("q", words[1].string);
        try std.testing.expectEqualStrings("1", words[2].string);
    }
}

test "lex identifiers" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic identifiers and operators
    {
        const words = lex(allocator, "+ - * / dup drop swap over");
        try std.testing.expectEqual(@as(usize, 8), words.len);
        try std.testing.expectEqual(Builtin.@"+", words[0].builtin);
        try std.testing.expectEqual(Builtin.@"-", words[1].builtin);
        try std.testing.expectEqual(Builtin.@"*", words[2].builtin);
        try std.testing.expectEqual(Builtin.@"/", words[3].builtin);
        try std.testing.expectEqual(Builtin.dup, words[4].builtin);
        try std.testing.expectEqual(Builtin.drop, words[5].builtin);
        try std.testing.expectEqual(Builtin.swap, words[6].builtin);
        try std.testing.expectEqual(Builtin.over, words[7].builtin);
    }

    // Complex identifiers
    {
        const words = lex(allocator, "foo123 _bar baz_ under_score CamelCase");
        try std.testing.expectEqual(@as(usize, 5), words.len);
        try std.testing.expectEqualStrings("foo123", words[0].identifier);
        try std.testing.expectEqualStrings("_bar", words[1].identifier);
        try std.testing.expectEqualStrings("baz_", words[2].identifier);
        try std.testing.expectEqualStrings("under_score", words[3].identifier);
        try std.testing.expectEqualStrings("CamelCase", words[4].identifier);
    }

    // Funny identifiers
    {
        const words = lex(allocator, "1+ 2/ works?");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectEqualStrings("1+", words[0].identifier);
        try std.testing.expectEqualStrings("2/", words[1].identifier);
        try std.testing.expectEqualStrings("works?", words[2].identifier);
    }
}

test "lex quotes" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    {
        const words = lex(allocator, "'test");
        try std.testing.expectEqual(@as(usize, 1), words.len);
        try std.testing.expectEqualStrings("test", words[0].quoted);
    }
}

test "lex mixed input" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Mixed types
    {
        const words = lex(allocator, "42 3.14 \"hello\" + -17 \"world\" swap 'swap");
        try std.testing.expectEqual(@as(usize, 8), words.len);
        try std.testing.expectEqual(@as(i64, 42), words[0].int);
        try std.testing.expectApproxEqAbs(@as(f64, 3.14), words[1].float, 0.001);
        try std.testing.expectEqualStrings("hello", words[2].string);
        try std.testing.expectEqual(Builtin.@"+", words[3].builtin);
        try std.testing.expectEqual(@as(i64, -17), words[4].int);
        try std.testing.expectEqualStrings("world", words[5].string);
        try std.testing.expectEqual(Builtin.swap, words[6].builtin);
        try std.testing.expectEqualStrings("swap", words[7].quoted);
    }

    // Different whitespace
    {
        const words = lex(allocator, "1\n2\t3  4\n\t  5");
        try std.testing.expectEqual(@as(usize, 5), words.len);
        for (words, 1..) |word, i| {
            try std.testing.expectEqual(@as(i64, @intCast(i)), word.int);
        }
    }

    // Adjacent strings and numbers
    {
        const words = lex(allocator, "\"no\"\"space\"42\"between\"3.14");
        try std.testing.expectEqual(@as(usize, 5), words.len);
        try std.testing.expectEqualStrings("no", words[0].string);
        try std.testing.expectEqualStrings("space", words[1].string);
        try std.testing.expectEqual(@as(i64, 42), words[2].int);
        try std.testing.expectEqualStrings("between", words[3].string);
        try std.testing.expectApproxEqAbs(@as(f64, 3.14), words[4].float, 0.001);
    }

    // Empty input
    {
        const words = lex(allocator, "");
        try std.testing.expectEqual(@as(usize, 0), words.len);
    }

    // Only whitespace
    {
        const words = lex(allocator, "   \n\t  \n  ");
        try std.testing.expectEqual(@as(usize, 0), words.len);
    }
}

const std = @import("std");
const meta = std.meta;
const process = std.process;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const ArrayList = std.ArrayList;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const StaticStringMap = std.StaticStringMap;
