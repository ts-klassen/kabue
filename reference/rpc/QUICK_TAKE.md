# `quick_take` – Proof-of-concept day-trade helper

```
POST /kabue/rpc/quick_take
```

> ⚠️ **WARNING** – This RPC **executes live orders**: it buys 1 share of the
> ETF **1459** at market, then immediately places a take-profit sell order at
> *buy-price + 1 JPY*.  The symbol, quantity and profit distance are
> hard-coded.  Use only in a test environment or at your own risk.

## What the function does

1. Submits a **market buy** for symbol `1459` (cash equity, margin day-trade).
2. Waits one second and fetches the order detail to obtain the executed price.
3. Calculates `price + 1`-JPY and submits a **sell (repay)** order of type
   `nonlimit_open_afternoon` *(Japanese term: **不成 — funari**)* for the same
   quantity.
4. Returns both order IDs and the profit-taking price.

## Request body

| field  | type   | required | description                                                      |
|--------|--------|----------|------------------------------------------------------------------|
| symbol | string | Yes      | Must be exactly **"1459"**. Any other value triggers HTTP 500. |

```jsonc
{
  "symbol": "1459"
}
```

## Response body

| field            | type   | description                                   |
|------------------|--------|-----------------------------------------------|
| order_id         | string | The initial buy order ID                      |
| close_order_id   | string | The profit-taking sell order ID               |
| close_order_price| number | The limit price used for the sell order (JPY) |

```jsonc
{
  "order_id": "202504240001",
  "close_order_id": "202504240002",
  "close_order_price": 2032.5
}
```

## Important considerations

* Trade type is fixed to **margin / general day-trade**.
* The sell order uses `nonlimit_open_afternoon` (不成). Depending on market
  conditions the order might not execute.
* The implementation is meant as a **proof of concept**.  Modify the Erlang
  code if you need different symbols, quantities, or exit strategy.
