# `board` – Retrieve order-book information

```
POST /kabue/rpc/board
```

Returns the latest **order-book (board)** data for a single cash-equity symbol.

Internally the function transparently chooses one of two data sources:

1. **WebSocket cache** – If the symbol is already subscribed through
   `kabue_mufje_ws_apic`, the in-memory snapshot is returned immediately
   (`is_ws_data = true`).
2. **REST fallback** – Otherwise the function:
   * Requests `/board` from the kabu Station REST API.
   * Registers the symbol for WebSocket streaming so that subsequent calls are
     served from cache (`is_ws_data = false` for the first call only).

## Request body

| field  | type   | required | description                       |
|--------|--------|----------|-----------------------------------|
| symbol | string | Yes      | Four-digit stock code (e.g. "0000") |

```jsonc
{
  "symbol": "0000"
}
```

## Response body

```jsonc
{
  "is_ws_data": true,          // or false – which data source was used
  "board": { /* same shape as kabu REST /board */ },
  "time": 1714452123456        // UNIX epoch (milliseconds)
}
```

* The structure of `board` is identical to the kabu Station REST API
  `/board` response.
* `time` is the timestamp of retrieval in milliseconds since the epoch.

### Example

```jsonc
{
  "is_ws_data": true,
  "board": {
    "Symbol": "0000",
    "CurrentPrice": 2030.5,
    "Bids": [ { "Price": 2030.0, "Qty": 1200 } ],
    "Asks": [ { "Price": 2031.0, "Qty": 800 } ]
  },
  "time": 1714452123456
}
```

## Notes

* Clients do **not** need to subscribe to WebSocket themselves—first access
  automatically triggers registration.
* On the very first call the REST path may add a few hundred milliseconds of
  latency before WebSocket data becomes available.

---

## Full JSON structure of `kabue_mufje_types:board()`

When the Erlang `board()` map is converted to JSON using `jsone:encode/1`,
every atom key becomes a snake-case string with the **exact name of the atom**
and enum atoms are rendered as their textual atom names.  The example below
shows all keys with illustrative values so you can see the full layout.

```jsonc
{
  "symbol": "0000",
  "symbol_name": "example ticker",
  "exchange": "tokyo",          // enum atom → string
  "exchange_name": "Tokyo",

  "current_price": 2030.5,
  "current_price_time": "2025-04-30T13:23:45+09:00",
  "current_price_change_status": "up",
  "current_price_status": "trading",
  "calc_price": 2030.5,

  "previous_close": 2010.0,
  "previous_close_time": "2025-04-26T15:00:00+09:00",
  "change_previous_close": 20.5,
  "change_previous_close_per": 1.02,

  "opening_price": 2025.0,
  "opening_price_time": "2025-04-30T09:00:00+09:00",
  "high_price": 2044.0,
  "high_price_time": "2025-04-30T10:42:13+09:00",
  "low_price": 2018.5,
  "low_price_time": "2025-04-30T09:10:07+09:00",

  "trading_volume": 18543200,
  "trading_volume_time": "2025-04-30T13:23:00+09:00",
  "vwap": 2028.4,
  "trading_value": 37596288000,

  // best quote (kabu’s Bid/Ask naming is swapped)
  "bid_qty": 800,
  "bid_price": 2031.0,
  "bid_time": "13:23:45",
  "bid_sign": "normal",
  "market_order_sell_qty": 300,

  // sell depth levels 1-10
  "sell1_time": "13:23:45",
  "sell1_sign": "normal",
  "sell1_price": 2031.0,
  "sell1_qty": 800,
  "sell2_price": 2031.5,
  "sell2_qty": 1200,
  "sell3_price": 2032.0,
  "sell3_qty": 1400,
  "sell4_price": 2032.5,
  "sell4_qty": 2000,
  "sell5_price": 2033.0,
  "sell5_qty": 1400,
  "sell6_price": 2033.5,
  "sell6_qty": 800,
  "sell7_price": 2034.0,
  "sell7_qty": 600,
  "sell8_price": 2034.5,
  "sell8_qty": 300,
  "sell9_price": 2035.0,
  "sell9_qty": 200,
  "sell10_price": 2035.5,
  "sell10_qty": 100,

  // buy depth levels 1-10 (Ask side)
  "ask_qty": 1200,
  "ask_price": 2030.5,
  "ask_time": "13:23:45",
  "ask_sign": "normal",
  "market_order_buy_qty": 500,

  "buy1_time": "13:23:45",
  "buy1_sign": "normal",
  "buy1_price": 2030.5,
  "buy1_qty": 1200,
  "buy2_price": 2030.0,
  "buy2_qty": 1400,
  "buy3_price": 2029.5,
  "buy3_qty": 2000,
  "buy4_price": 2029.0,
  "buy4_qty": 2400,
  "buy5_price": 2028.5,
  "buy5_qty": 2600,
  "buy6_price": 2028.0,
  "buy6_qty": 1800,
  "buy7_price": 2027.5,
  "buy7_qty": 1200,
  "buy8_price": 2027.0,
  "buy8_qty": 900,
  "buy9_price": 2026.5,
  "buy9_qty": 700,
  "buy10_price": 2026.0,
  "buy10_qty": 500,

  // imbalance
  "oversell_qty": 0,
  "underbuy_qty": 200,

  // stock specific
  "total_market_value": 32500000000000,

  // derivatives specific (fields are null for stocks)
  "settlement_price": null,
  "iv": null,
  "gamma": null,
  "theta": null,
  "vega": null,
  "delta": null
}
```

### Encoding characteristics

1. **Enums as strings** – `exchange`, `bid_sign`, etc. originate as atoms
   that come from `kabue_mufje_enum:*()` look-ups.  They appear in JSON as
   those atom names.
2. **Numbers are numbers** – quantities and prices are JSON numbers.  kabu
   sometimes returns decimals (`0.5`).
3. **Nullable fields** – Keys not applicable to the instrument type are
   encoded as `null`.
