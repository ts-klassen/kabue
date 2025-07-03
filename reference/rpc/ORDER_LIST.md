# `order_list` – Categorise current orders

```
POST /kabue/rpc/order_list
```

Returns the current order list together with three **ID buckets** that make it
easy to display status in a UI.

| bucket   | criteria                                               |
|----------|--------------------------------------------------------|
| `all`    | Completely filled (`order_qty - cum_qty ≈ 0`)          |
| `partial`| Partially filled (cum_qty > 0 but not full)            |
| `none`   | No fill at all (cum_qty ≈ 0)                           |

## Request body

Send an empty object – no parameters are required:

```json
{}
```

## Response body

```jsonc
{
  "order_list": [ /* full REST /orders payload */ ],
  "id_buckets": {
    "all":     ["202504240001"],
    "partial": ["202504240002"],
    "none":    ["202504240003", "202504240004"]
  }
}
```

* `order_list` is exactly what kabu Station REST API `/orders` returns.
* `id_buckets` contains only the order IDs – handy for coloring rows or quick
  filtering.
