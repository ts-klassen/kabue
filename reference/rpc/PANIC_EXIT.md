# `panic_exit` – Emergency exit helper

```
POST /kabue/rpc/panic_exit
```

Cancels every open order **and** immediately closes all remaining positions
at market price.  Use this RPC when you need to flatten exposure as fast as
possible.

## Processing steps

1. Fetch the order list via REST `/orders`.
2. For any order whose `state` is *not* `finished`, issue `/cancelorder`.
3. Fetch positions via REST `/positions`.
4. For each position with `leaves_qty > 0` submit a market order on the
   opposite side (cash-margin logic handled by the function).
5. Aggregate the results and return a summary JSON.

## Request body

Send an empty object:

```json
{}
```

## Response body

```jsonc
{
  "cancel_list": ["202504240001", "202504240002"],
  "exit_list": [ /* order objects used for close-out */ ],
  "cancel_result": [
    { "success": true,  "payload": { /* REST /cancelorder response */ } },
    { "success": false, "payload": { "Code": 4001020, "Message": "Expired" } }
  ],
  "exit_result": [
    { "success": true,  "payload": { /* REST /sendorder response */ } }
  ]
}
```

* Each element in `cancel_result` / `exit_result` is created from an `either`
  tuple returned by the underlying library:
  * `success = true`   → `{right, Payload}`
  * `success = false`  → `{left,  Payload}`

## Caveats

* Works across **both** cash-equity and margin positions.
* Orders that cannot be canceled (already fully filled or expired) appear in
  `cancel_result` with `success = false`; this is expected and does not stop
  processing.
* If there are no positions the arrays `exit_list` and `exit_result` are empty.
