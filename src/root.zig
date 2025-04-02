const std = @import("std");
const testing = std.testing;

const Direction = enum {
    add,
    remove,
};

const SymbolMapping = struct {
    source_idx: usize,
    coded_idx: usize,
};

pub fn CodingWindow(comptime T: type, comptime Context: type, comptime hashFn: fn (context: Context, item: T) u64) type {
    return struct {
        const Self = @This();

        const Encoder = struct {
            window: *Self,

            pub fn produceNextCodedSymbol(self: Encoder) CodedSymbol(T) {
                const out = CodedSymbol(T){
                    .symbol = 0,
                    .hash = 0,
                    .count = 0,
                };

                return self.window.applyWindow(
                    out,
                    .add,
                );
            }
        };

        const Decoder = struct {
            // coded symbols received so far
            cs: std.ArrayList(CodedSymbol(T)),

            // set of source symbols that the decoder initially has
            window: *Self,

            // set of source symbols that are exclusive to the decoder
            local: Self,

            // set of source symbols that are exclusive to the encoder
            remote: Self,

            decodable: std.ArrayList(usize),
            decoded: usize,

            fn init(allocator: std.mem.Allocator, self: *Self) Decoder {
                return .{
                    .window = self,
                    .cs = .init(allocator),
                    .local = .init(allocator),
                    .remote = .init(allocator),
                    .decodable = .init(allocator),
                    .decoded = 0,
                };
            }

            pub fn deinit(self: *Decoder) void {
                self.cs.deinit();
                self.decodable.deinit();

                self.local.deinit();
                self.remote.deinit();
            }

            pub fn isDecoded(self: *Decoder) bool {
                return self.decoded == self.cs.items.len;
            }

            pub fn addCodedSymbol(self: *Decoder, context: Context, sym: CodedSymbol(T)) !void {
                // scan through decoded symbols to peel off matching ones
                var next_sym = self.window.applyWindow(sym, .remove);
                next_sym = self.remote.applyWindow(next_sym, .remove);
                next_sym = self.local.applyWindow(next_sym, .add);

                // insert the new coded symbol
                try self.cs.append(next_sym);

                // check if the coded symbol is decodable, and insert into decodable list if so
                if ((next_sym.count == 1 or next_sym.count == -1) and (next_sym.hash == hashFn(context, next_sym.symbol))) {
                    try self.decodable.append(self.cs.items.len - 1);
                }

                if (next_sym.count == 0 and next_sym.hash == 0) {
                    try self.decodable.append(self.cs.items.len - 1);
                }
            }

            fn applyNewSymbol(self: *Decoder, context: Context, sym: HashedSymbol(T), direction: Direction) !RandomMapping {
                var m = RandomMapping{
                    .prng = sym.hash,
                    .last_idx = 0,
                };

                while (m.last_idx < self.cs.items.len) {
                    const cidx = m.last_idx;

                    self.cs.items[cidx] = self.cs.items[cidx].apply(sym, direction);

                    // Check if the coded symbol is now decodable. We do not want to insert
                    // a decodable symbol into the list if we already did, otherwise we
                    // will visit the same coded symbol twice. To see how we achieve that,
                    // notice the following invariant: if a coded symbol becomes decodable
                    // with degree D (obviously -1 <= D <=1), it will stay that way, except
                    // for that it's degree may become 0. For example, a decodable symbol
                    // of degree -1 may not later become undecodable, or become decodable
                    // but of degree 1. This is because each peeling removes a source
                    // symbol from the coded symbol. So, if a coded symbol already contains
                    // only 1 or 0 source symbol (the definition of decodable), the most we
                    // can do is to peel off the only remaining source symbol.
                    //
                    // Meanwhile, notice that if a decodable symbol is of degree 0, then
                    // there must be a point in the past when it was of degree 1 or -1 and
                    // decodable, at which time we would have inserted it into the
                    // decodable list. So, we do not insert degree-0 symbols to avoid
                    // duplicates. On the other hand, it is fine that we insert all
                    // degree-1 or -1 decodable symbols, because we only see them in such
                    // state once.

                    if ((self.cs.items[cidx].count == -1 or self.cs.items[cidx].count == 1) and
                        self.cs.items[cidx].hash == hashFn(context, self.cs.items[cidx].symbol))
                    {
                        try self.decodable.append(cidx);
                    }
                    _ = m.nextIndex();
                }

                return m;
            }

            pub fn tryDecode(self: *Decoder, context: Context) !void {
                var didx: usize = 0;
                while (didx < self.decodable.items.len) {
                    const cidx = self.decodable.items[didx];
                    const cs = self.cs.items[cidx];
                    switch (cs.count) {
                        1 => {
                            const new_sym = HashedSymbol(T){
                                .symbol = cs.symbol,
                                .hash = cs.hash,
                            };

                            const m = try self.applyNewSymbol(context, new_sym, .remove);

                            try self.remote.addHashedSymbolWithMapping(new_sym, m);
                        },
                        -1 => {
                            const new_sym = HashedSymbol(T){
                                .symbol = cs.symbol,
                                .hash = cs.hash,
                            };

                            const m = try self.applyNewSymbol(context, new_sym, .add);
                            try self.local.addHashedSymbolWithMapping(new_sym, m);
                        },
                        0 => {},
                        else => unreachable,
                    }

                    self.decoded += 1;
                    didx += 1;
                }

                self.decodable.clearAndFree();
            }
        };

        allocator: std.mem.Allocator,
        symbols: std.ArrayList(HashedSymbol(T)),
        mappings: std.ArrayList(RandomMapping),
        queue: std.PriorityQueue(SymbolMapping, void, lessThan),
        next_idx: usize,

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .allocator = allocator,
                .symbols = .init(allocator),
                .mappings = .init(allocator),
                .queue = .init(allocator, {}),
                .next_idx = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.queue.deinit();
            self.symbols.deinit();
            self.mappings.deinit();
        }

        fn lessThan(context: void, a: SymbolMapping, b: SymbolMapping) std.math.Order {
            _ = context;
            return std.math.order(a.coded_idx, b.coded_idx);
        }

        /// inserts a symbol to the CodingWindow
        pub fn addSymbol(self: *Self, ctx: Context, t: T) !void {
            const hashed_symbol = HashedSymbol(T){
                .symbol = t,
                .hash = hashFn(ctx, t),
            };

            try self.addHashedSymbol(hashed_symbol);
        }

        /// inserts a HashedSymbol to the CodingWindow
        fn addHashedSymbol(self: *Self, t: HashedSymbol(T)) !void {
            try self.addHashedSymbolWithMapping(t, .{ .prng = t.hash, .last_idx = 0 });
        }

        /// inserts a HashedSymbol and the current state of its mapping generator to the CodingWindow
        fn addHashedSymbolWithMapping(self: *Self, t: HashedSymbol(T), m: RandomMapping) !void {
            try self.symbols.append(t);
            try self.mappings.append(m);
            try self.queue.add(.{ .source_idx = self.symbols.items.len - 1, .coded_idx = m.last_idx });
        }

        /// maps the source symbols to the next coded symbol they should be
        /// mapped to, given as cw. The parameter direction controls how the counter
        /// of cw should be modified.
        fn applyWindow(self: *Self, cs: CodedSymbol(T), direction: Direction) CodedSymbol(T) {
            var next_cs = cs;
            if (self.queue.count() == 0) {
                self.next_idx += 1;
                return next_cs;
            }

            while (self.queue.peek()) |head| {
                if (head.coded_idx != self.next_idx)
                    break;

                next_cs = next_cs.apply(self.symbols.items[head.source_idx], direction);

                const next_map = self.mappings.items[head.source_idx].nextIndex();
                var copy = head;
                copy.coded_idx = next_map;

                self.queue.update(head, copy) catch unreachable;
            }

            self.next_idx += 1;
            return next_cs;
        }

        pub fn encoder(self: *Self) Encoder {
            return .{
                .window = self,
            };
        }

        pub fn decoder(self: *Self) Decoder {
            return Decoder.init(self.allocator, self);
        }
    };
}

fn HashedSymbol(T: type) type {
    return struct {
        symbol: T,
        hash: u64,
    };
}

pub fn CodedSymbol(T: type) type {
    return struct {
        symbol: T,
        hash: u64,
        count: i64,

        fn apply(self: CodedSymbol(T), s: HashedSymbol(T), direction: Direction) CodedSymbol(T) {
            return .{
                .symbol = self.symbol ^ s.symbol,
                .hash = self.hash ^ s.hash,
                .count = switch (direction) {
                    .add => self.count + 1,
                    .remove => self.count - 1,
                },
            };
        }
    };
}

const RandomMapping = struct {
    // PRNG state
    prng: u64,

    // the last index the symbol was mapped to
    last_idx: u64,

    fn nextIndex(self: *RandomMapping) u64 {
        const r = self.prng *% 0xda942042e4dd58b5;
        self.prng = r;

        const value = (@as(f64, @floatFromInt(self.last_idx)) + 1.5) * (@as(f64, @floatCast(1 << 32)) / std.math.sqrt(@as(f64, @floatFromInt(r)) + 1) - 1);

        const diff: f64 = @ceil(value);
        const next_diff: u64 = @intFromFloat(@mod(diff, 1 << 64));
        self.last_idx = self.last_idx +% next_diff;

        return self.last_idx;
    }
};

fn hash(ctx: void, input: u64) u64 {
    _ = ctx;
    return std.hash.Murmur2_64.hashUint64(input);
}

test "example" {
    const alice = [_]u64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    const bob = [_]u64{ 1, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
    const Window = CodingWindow(u64, void, hash);

    var enc = Window.init(std.testing.allocator);
    defer enc.deinit();
    for (alice) |item| {
        try enc.addSymbol({}, item);
    }

    var dec = Window.init(std.testing.allocator);
    defer dec.deinit();
    for (bob) |item| {
        try dec.addSymbol({}, item);
    }

    var cost: u32 = 0;

    var encoder = enc.encoder();
    var decoder = dec.decoder();
    defer decoder.deinit();

    while (true) {
        cost += 1;

        const s = encoder.produceNextCodedSymbol();
        try decoder.addCodedSymbol({}, s);
        try decoder.tryDecode({});

        if (decoder.isDecoded()) {
            break;
        }
    }

    try testing.expect(decoder.remote.symbols.items.len == 1);
    try testing.expectEqual(2, decoder.remote.symbols.items[0].symbol);
    try testing.expect(decoder.local.symbols.items.len == 1);
    try testing.expectEqual(11, decoder.local.symbols.items[0].symbol);
    try testing.expect(cost == 2);
}
