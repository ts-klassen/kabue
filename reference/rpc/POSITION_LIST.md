# `position_list` – Snapshot of current positions

```
POST /kabue/rpc/position_list
```

Returns the list of **open positions** exactly as provided by the kabu Station
REST endpoint **/positions** – but already converted to the more ergonomic
snake-case keys and enum text values that `kabue_mufje_rest_apic` exposes.

Typical usage scenarios:

* Display all holdings in a trading dashboard.
* Feed the data into P/L tracking or risk-management tools.

---

## Request body

No parameters are required. Send an empty object:

```json
{}
```

---

## Response body

```jsonc
{
  "position_list": [
    {
      "execution_id": "202504240001",
      "account_type": "margin",
      "symbol": "0000",
      "symbol_name": "Example Co.",
      "exchange": "tokyo",
      "exchange_name": "Tokyo",
      "security_type": "stock",
      "execution_day": "2025-04-24",
      "price": 2030.5,
      "leaves_qty": 0,
      "hold_qty": 100,
      "side": "buy",
      "expenses": 0.0,
      "commission": 0.0,
      "commission_tax": 0.0,
      "expire_day": null,
      "margin_trade_type": "system",
      "current_price": 2044.0,
      "valuation": 204400.0,
      "profit_loss": 1350.0,
      "profit_loss_rate": 0.68
    }
  ]
}
```

* Each object inside `position_list` is one element of the `/positions` array
  with keys converted to snake-case and enum numeric codes replaced by their
  descriptive text values.
* All numeric values are returned as JSON numbers (floating point).

---

## Notes

* When the account has **no open positions** the array is simply empty:

  ```json
  {
    "position_list": []
  }
  ```
* The RPC performs **one** REST call only – no additional calculations or
  filtering are applied.
