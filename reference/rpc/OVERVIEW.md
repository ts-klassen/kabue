# Kabue RPC API – Overview

Kabue exposes a small **RPC-style** HTTP endpoint that allows front-end code
or automation scripts to invoke server-side functions that are **not** part of
the official **kabu Station** REST / WebSocket API.

*   Base URL (default build): `http://<host>:<port>/kabue/rpc/{id}`
*   HTTP Method: **POST** *(only)*
*   Content-Type: any (the server always tries to decode the body as JSON)
*   Request Body: JSON object that is passed as Erlang map argument to the
    corresponding function `kabue_rpc:{id}/1` on the server.

If the *id* segment in the path matches an exported function of
`kabue_rpc.erl` (arity = 1) the request is accepted, the function is executed
and its return value is encoded back to JSON and sent in the response body
(HTTP 200).

When the *id* does not exist or  the method is not `POST`, the server responds
with **404 Not Found**.

---

## Available RPC methods

| id (function) | Summary |
|---------------|---------|
| `echo` | Echos back the request payload. Mainly for connectivity test |
| `board` | Returns board (order-book) information for one symbol |
| `quick_take` | *Demo/utility*: buys 1 share of 1459 and immediately places a take-profit order |
| `order_list` | Lists current orders and groups their IDs by filled state |
| `panic_exit` | Cancels all open orders **and** immediately closes all positions |

Detailed request / response specification for every RPC is available in the
individual markdown files placed next to this one.

---

### Error handling

* Each RPC function is free to design its own **success / error payload**.
* If an un-handled Erlang exception occurs inside the function, Cowboy will
  automatically respond with **HTTP 500 Internal Server Error** and an empty
  body.

In other words *application-level* errors are communicated by the individual
function (and are documented in its dedicated markdown file), whereas
unexpected crashes surface as HTTP 500.
