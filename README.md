# riblt

Zig port of [RIBLT library](https://github.com/yangl1996/riblt) by yang1996.

Implementation of Rateless Invertible Bloom Lookup Tables (Rateless IBLTs), as
proposed in paper Practical Rateless Set Reconciliation by Lei Yang, Yossi
Gilad, and Mohammad Alizadeh. Preprint available at
[arxiv.org/abs/2402.02668](https://arxiv.org/abs/2402.02668).

## Example usage

```zig
    const alice = [_]u64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    const bob = [_]u64{ 1, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
    const Window = CodingWindow(u64, Ctx, hash);

    var enc = Window.init(std.testing.allocator);
    defer enc.deinit();
    for (alice) |item| {
        try enc.addSymbol(.{}, item);
    }

    var dec = Window.init(std.testing.allocator);
    defer dec.deinit();
    for (bob) |item| {
        try dec.addSymbol(.{}, item);
    }

    var encoder = enc.encoder();
    var decoder = dec.decoder();
    defer decoder.deinit();

    while (true) {
        const sym = encoder.produceNextCodedSymbol();
        try decoder.addCodedSymbol(sym);
        try decoder.tryDecode();

        if (decoder.isDecoded()) {
            break;
        }
    }

    // remote: decoder.remote.symbols (exclusive to alice)
    // local: decoder.local.symbols (exclusive to bob)
```

For the complete example see test `example` in `src/root.zig`.
