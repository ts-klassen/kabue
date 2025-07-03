# `wallet` – Cash & margin balance snapshot

```
POST /kabue/rpc/wallet
```

Returns the current **cash** and **margin** wallet information as provided by
the kabu Station REST endpoints **/wallet/cash** and **/wallet/margin**.  The
RPC performs two REST calls internally and combines their payloads into one
convenient JSON object.

Typical use-cases:

* Showing available buying power in a trading UI.
* Monitoring remaining margin when running an automated strategy.

---

## Request body

Send an empty object – no parameters are required:

```json
{}
```

---

## Response body

```jsonc
{
  "cash": {
    "stock_account_wallet": 1_234_567.0,
    "au_kc_stock_account_wallet": 0.0,
    "au_jbn_stock_account_wallet": 0.0
  },
  "margin": {
    "margin_account_wallet": 850_000.0,
    "depositkeep_rate": 0.25,
    "consignment_deposit_rate": 0.3,
    "cash_of_consignment_deposit_rate": 255_000.0
  }
}
```

* The exact fields match the kabu Station API **WalletCashSuccess** and
  **WalletMarginSuccess** schemas.
* Unavailable values are omitted from the embedded maps – the key set may
  therefore vary between brokers or account types.

---

## Notes

* Only **cash** and **margin** wallets are queried here.  Futures / options
  wallets are outside the scope of this RPC; extend the Erlang implementation
  if you need them.
* All numeric values are returned as JSON numbers (floating point).
* The RPC simply relays the REST data – no additional calculations are
  performed.
