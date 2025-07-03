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
| symbol | string | Yes      | Four-digit stock code (e.g. "7203") |

```jsonc
{
  "symbol": "7203"
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
    "Symbol": "7203",
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
